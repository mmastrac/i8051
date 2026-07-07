//! Processor drivers.
//!
//! The disassembler is generic over the CPU it decodes. A [`Platform`] driver
//! turns raw bytes into a CPU-neutral [`DecodedInsn`] and declares the address
//! [`regions`](Platform::regions) that CPU exposes, so the rest of the crate
//! never names a concrete instruction set. The one built-in driver lives in
//! [`i8051`], a caller can supply its own (including user-defined regions).

use std::sync::Arc;

use crate::address::{AddressSpace, AddressValue, PhysicalAddr, Xref, XrefType};

pub mod i8051;
pub mod mos6502;
pub mod m6805;

/// How execution proceeds after an instruction. CPU-neutral: every driver maps
/// its own control flow onto these cases.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ControlFlow {
    /// Fall through to `next`.
    Continue { next: AddressValue },
    /// Unconditional jump to `target`.
    Jump { target: AddressValue },
    /// Call `target`, returning to `return_pc`.
    Call {
        target: AddressValue,
        return_pc: AddressValue,
    },
    /// Branch to `branch_target` or fall through to `fall_through`.
    Choice {
        fall_through: AddressValue,
        branch_target: AddressValue,
    },
    /// Unknown or data-dependent control flow.
    Diverge,
}

/// A static data reference an instruction makes, already classified into an
/// address space by the driver (which owns the CPU's memory-map semantics).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DataRef {
    pub space: AddressSpace,
    pub offset: AddressValue,
    pub kind: XrefType,
}

/// A CPU-neutral decoded instruction: everything the disassembler needs to
/// render, trace, and cross-reference an instruction without knowing the ISA.
#[derive(Debug, Clone)]
pub struct DecodedInsn {
    /// Byte length of the instruction.
    pub len: u8,
    /// The instruction bytes (length `len`).
    pub bytes: Vec<u8>,
    /// Canonical text (mnemonic + operands), before label substitution.
    pub text: String,
    /// Control flow after this instruction.
    pub control_flow: ControlFlow,
    /// Which operand (0-based) holds the branch/jump target, for label
    /// substitution during rendering. `None` if the instruction has no target.
    pub branch_operand_index: Option<usize>,
    /// A register-file address to surface by name in listings (the 8051 SFR
    /// naming hook). `None` on CPUs without one.
    pub named_register: Option<u8>,
    /// Static data references, space-classified by the driver.
    pub data_refs: Vec<DataRef>,
}

impl DecodedInsn {
    #[inline]
    pub fn len(&self) -> usize {
        self.len as usize
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    #[inline]
    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    #[inline]
    pub fn control_flow(&self) -> ControlFlow {
        self.control_flow
    }

    #[inline]
    pub fn as_string(&self) -> &str {
        &self.text
    }

    /// The register-file address to name, if any (see [`named_register`]).
    ///
    /// [`named_register`]: DecodedInsn::named_register
    #[inline]
    pub fn direct_addr(&self) -> Option<u8> {
        self.named_register
    }
}

/// What a region holds, advisory metadata a driver attaches to each space.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegionKind {
    /// Executable code.
    Code,
    /// A register file (the 8051 `SFR`/`BIT` spaces, memory-mapped I/O, ...).
    Register,
    /// Plain data memory.
    Data,
}

/// A driver-declared address region.
#[derive(Debug, Clone)]
pub struct RegionDef {
    /// The space this region occupies.
    pub space: AddressSpace,
    /// What the region holds.
    pub kind: RegionKind,
    /// The `sdas` `.area` header emitted for this region.
    pub area_header: &'static str,
}

/// A processor driver: decodes bytes and declares the CPU's address regions.
///
/// Held behind an `Arc<dyn Platform>` so a [`Db`](crate::db::Db) can be built
/// for any CPU chosen at runtime without threading a type parameter everywhere.
pub trait Platform: Send + Sync {
    /// A short identifier for the CPU (`"i8051"`, `"mos6502"`, ...).
    fn name(&self) -> &str;

    /// The address regions this CPU exposes, in emission order. May include
    /// user-defined regions.
    fn regions(&self) -> &[RegionDef];

    /// The longest instruction in bytes, the fetch window for decoding.
    fn max_insn_len(&self) -> usize;

    /// Decode the instruction starting at `pc` from `bytes` (a fetch window of
    /// up to [`max_insn_len`](Platform::max_insn_len) bytes). Returns an
    /// instruction even for unknown opcodes (with its length), mirroring the
    /// underlying decoder, the caller rejects it if it runs past `bytes`.
    fn decode(&self, pc: AddressValue, bytes: &[u8]) -> DecodedInsn;

    /// The `.area` header for `space`, if this driver declares it.
    fn area_header(&self, space: AddressSpace) -> Option<&'static str> {
        self.regions()
            .iter()
            .find(|r| r.space == space)
            .map(|r| r.area_header)
    }
}

/// The control-flow and data cross-references an instruction makes from
/// `source`. Control edges come from the neutral control flow, data edges are
/// the driver's pre-classified [`DataRef`]s.
pub fn xrefs_from_instruction(insn: &DecodedInsn, source: PhysicalAddr) -> Vec<Xref> {
    let mut xrefs = Vec::new();
    let mut push = |space, offset, xref_type| {
        xrefs.push(Xref {
            from: source,
            to: PhysicalAddr { space, offset },
            xref_type,
        });
    };

    match insn.control_flow {
        ControlFlow::Jump { target } => push(source.space, target, XrefType::Jump),
        ControlFlow::Call { target, .. } => push(source.space, target, XrefType::Call),
        ControlFlow::Choice { branch_target, .. } => {
            push(source.space, branch_target, XrefType::Jump)
        }
        _ => {}
    }

    for data_ref in &insn.data_refs {
        push(data_ref.space, data_ref.offset, data_ref.kind);
    }

    xrefs
}

/// The absolute branch/jump/call target of `insn`, if it has one.
pub fn branch_target(insn: &DecodedInsn) -> Option<AddressValue> {
    match insn.control_flow {
        ControlFlow::Jump { target } | ControlFlow::Call { target, .. } => Some(target),
        ControlFlow::Choice { branch_target, .. } => Some(branch_target),
        _ => None,
    }
}

/// Which operand holds the branch target (for label substitution), or `None`.
pub fn branch_target_operand_index(insn: &DecodedInsn) -> Option<usize> {
    insn.branch_operand_index
}

/// Handy alias for a shared driver.
pub type PlatformRef = Arc<dyn Platform>;

/// The built-in driver names, in a stable order.
pub const BUILTIN_PLATFORMS: &[&str] = &["i8051", "mos6502", "m6805"];

/// Look up a built-in driver by name (see [`BUILTIN_PLATFORMS`]).
pub fn by_name(name: &str) -> Option<PlatformRef> {
    match name {
        "i8051" => Some(i8051::platform()),
        "mos6502" => Some(mos6502::platform()),
        "m6805" => Some(m6805::platform()),
        _ => None,
    }
}

/// Shared helpers for the per-driver test modules.
#[cfg(test)]
pub(crate) mod test_util {
    use super::*;

    /// Decode `bytes` with `driver` (at pc 0) and return the cross-reference
    /// edges it makes from a source at offset 0, as `(space, offset, kind)`.
    pub(crate) fn edges(
        driver: &dyn Platform,
        bytes: &[u8],
    ) -> Vec<(AddressSpace, AddressValue, XrefType)> {
        let source = PhysicalAddr {
            space: driver.regions()[0].space,
            offset: 0,
        };
        xrefs_from_instruction(&driver.decode(0, bytes), source)
            .into_iter()
            .map(|x| (x.to.space, x.to.offset, x.xref_type))
            .collect()
    }
}
