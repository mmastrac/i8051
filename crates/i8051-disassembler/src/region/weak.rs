//! Auto-disassembly derivation: roots produce a weak code layer computed from
//! the bytes and the barriers. [`Region`](super::Region) owns the strong layer
//! and the [`CodeSource`] here, and composes strong over weak on read.

use std::collections::{BTreeMap, BTreeSet};

use crate::address::AddressValue;
use crate::db::{Equivalent, EquivalentKind, EquivalentRange};
use crate::platform::{ControlFlow, DecodedInsn};

/// What the traversal reads from the region. Strong *code* is intentionally not
/// exposed, so the traversal flows through code islands and re-derives them.
pub(crate) trait CodeSource {
    fn decode(&self, addr: AddressValue) -> Option<DecodedInsn>;
    /// The kind of the first barrier overlapping `[addr, addr + len)`, if any.
    fn barrier_in(&self, addr: AddressValue, len: AddressValue) -> Option<EquivalentKind>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AutoDisassembleError {
    /// Flow reached bytes that could not be claimed as code: a barrier, or an
    /// overlap with other code.
    Overlapped(EquivalentKind),
}

#[must_use]
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct AutoDisassembleResult {
    pub success: Vec<AddressValue>,
    pub errors: Vec<(AddressValue, AutoDisassembleError)>,
}

impl AutoDisassembleResult {
    pub fn is_success(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn unwrap_success(self) -> Vec<AddressValue> {
        if let Some((addr, _)) = self.errors.first() {
            panic!("auto-disassembly failed at {addr:04X}");
        }
        self.success
    }
}

/// The derived layer: the roots and the code reachable from them.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub(crate) struct Weak {
    roots: BTreeSet<AddressValue>,
    code: BTreeMap<AddressValue, EquivalentRange>,
}

impl Weak {
    pub fn code(&self) -> &BTreeMap<AddressValue, EquivalentRange> {
        &self.code
    }

    pub fn roots(&self) -> impl Iterator<Item = AddressValue> + '_ {
        self.roots.iter().copied()
    }

    pub fn contains_root(&self, root: AddressValue) -> bool {
        self.roots.contains(&root)
    }

    pub fn remove_root(&mut self, root: AddressValue) -> bool {
        self.roots.remove(&root)
    }

    /// Any derived code intersecting `[offset, offset + span)`.
    pub fn overlaps(&self, offset: AddressValue, span: AddressValue) -> bool {
        let end = offset.saturating_add(span);
        self.code.range(offset..end).next().is_some()
            || matches!(self.code.range(..offset).next_back(), Some((_, r)) if r.end > offset)
    }

    /// Re-derive from scratch for changes that can shrink the layer: a barrier
    /// added over existing code, bytes changed, or a root removed.
    pub fn refresh(&mut self, src: &impl CodeSource) {
        self.code.clear();
        let roots: Vec<AddressValue> = self.roots.iter().copied().collect();
        for root in roots {
            let _ = self.trace(src, root);
        }
    }

    /// Record `root` and fold in its newly-reached code, halting at code already
    /// derived so a root costs only its new reach. Returns the diagnostics.
    pub fn extend(&mut self, src: &impl CodeSource, root: AddressValue) -> AutoDisassembleResult {
        self.roots.insert(root);
        self.trace(src, root)
    }

    fn trace(&mut self, src: &impl CodeSource, root: AddressValue) -> AutoDisassembleResult {
        let overlap = |addr, kind| (addr, AutoDisassembleError::Overlapped(kind));
        let mut result = AutoDisassembleResult::default();
        let mut queue = vec![root];
        while let Some(addr) = queue.pop() {
            if let Some((start, _)) = covering(&self.code, addr) {
                if start != addr {
                    result.errors.push(overlap(addr, EquivalentKind::Code));
                }
                continue;
            }
            if let Some(kind) = src.barrier_in(addr, 1) {
                result.errors.push(overlap(addr, kind));
                continue;
            }
            let Some(insn) = src.decode(addr) else {
                result.errors.push(overlap(addr, EquivalentKind::Code));
                continue;
            };
            let len = insn.len() as AddressValue;
            if let Some(kind) = src.barrier_in(addr, len) {
                result.errors.push(overlap(addr, kind));
                continue;
            }
            if self.overlaps(addr, len) {
                result.errors.push(overlap(addr, EquivalentKind::Code));
                continue;
            }
            self.code.insert(
                addr,
                EquivalentRange {
                    end: addr + len,
                    equivalent: Equivalent::Code,
                },
            );
            result.success.push(addr);
            match insn.control_flow() {
                ControlFlow::Continue { next } => queue.push(next),
                ControlFlow::Jump { target } => queue.push(target),
                ControlFlow::Call { target, return_pc } => {
                    queue.push(return_pc);
                    queue.push(target);
                }
                ControlFlow::Choice {
                    fall_through,
                    branch_target,
                } => {
                    queue.push(fall_through);
                    queue.push(branch_target);
                }
                ControlFlow::Diverge => {}
            }
        }
        result
    }
}

/// The entry covering `offset`: an exact start, or a preceding range containing it.
fn covering(
    code: &BTreeMap<AddressValue, EquivalentRange>,
    offset: AddressValue,
) -> Option<(AddressValue, &EquivalentRange)> {
    if let Some(range) = code.get(&offset) {
        return Some((offset, range));
    }
    match code.range(..=offset).next_back() {
        Some((&start, range)) if offset < range.end => Some((start, range)),
        _ => None,
    }
}
