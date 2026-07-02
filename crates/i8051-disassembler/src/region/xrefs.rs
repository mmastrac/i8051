//! Reverse cross-reference index: for each target address, the source
//! instructions that reference it. Derived from decoded code like
//! [`Weak`](super::weak::Weak), so it is never serialized. [`Region`](super::Region)
//! holds it lazily and rebuilds it on first read after a code change.

use std::collections::BTreeMap;

use crate::address::{AddressValue, PhysicalAddr, XrefType, xrefs_from_instruction};

use super::Region;

/// One inbound reference: the source instruction offset and the edge kind.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Edge {
    pub from: AddressValue,
    pub kind: XrefType,
}

#[derive(Debug)]
pub(crate) struct Xrefs {
    reverse: BTreeMap<PhysicalAddr, Vec<Edge>>,
}

impl Xrefs {
    /// Build the reverse map from every decoded instruction in `region`. Holds
    /// only byte-derived facts (never labels or other annotations), and must
    /// not read the index it is initializing.
    pub fn build(region: &Region) -> Self {
        // `code_offsets` lists strong starts then weak starts. Sort once so
        // each target's edges come out in source-offset order.
        let mut offsets = region.code_offsets();
        offsets.sort_unstable();

        let mut reverse: BTreeMap<PhysicalAddr, Vec<Edge>> = BTreeMap::new();
        for offset in offsets {
            let Some(insn) = region.decode_at(offset) else {
                continue;
            };
            let source = PhysicalAddr {
                space: region.space,
                offset,
            };
            for xref in xrefs_from_instruction(&insn, source) {
                reverse.entry(xref.to).or_default().push(Edge {
                    from: offset,
                    kind: xref.xref_type,
                });
            }
        }
        Self { reverse }
    }

    /// Inbound edges to `target`, in source-offset order.
    pub fn to(&self, target: &PhysicalAddr) -> &[Edge] {
        self.reverse.get(target).map_or(&[][..], Vec::as_slice)
    }

    /// Every target with at least one inbound edge, paired with its edges.
    pub fn targets(&self) -> impl Iterator<Item = (&PhysicalAddr, &[Edge])> {
        self.reverse.iter().map(|(t, e)| (t, e.as_slice()))
    }
}
