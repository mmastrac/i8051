//! The Intel 8051 (MCS-51) driver.
//!
//! Adapts the concrete [`::i8051`] decoder to the neutral [`DecodedInsn`] and
//! owns every 8051-specific decision the rest of the crate used to hardcode:
//! the `SFR`/`IDATA` split at `0x80`, the per-mnemonic memory-access roles, and
//! the branch-target operand index.

use std::sync::Arc;

use ::i8051::{Instruction, Mnemonic, Operand};

pub use ::i8051::format_direct;

use crate::address::{AddressSpace, AddressValue, XrefType};

use super::{
    CanonicalAddr, Certainty, ControlFlow, DataRef, DecodedInsn, EntryPoint, Platform, PlatformRef,
    RegionDef, RegionKind,
};

/// External program memory — where code lives.
pub const CODE: AddressSpace = AddressSpace::new("CODE");
/// Internal RAM (`0x00`-`0xFF`), including the register banks and stack.
pub const IDATA: AddressSpace = AddressSpace::new("IDATA");
/// Special Function Registers (`0x80`-`0xFF`).
pub const SFR: AddressSpace = AddressSpace::new("SFR");
/// Bit-addressable space.
pub const BIT: AddressSpace = AddressSpace::new("BIT");
/// External data memory, reached via `MOVX`.
pub const XDATA: AddressSpace = AddressSpace::new("XDATA");

/// The 8051 regions, in `sdas` emission order.
static REGIONS: &[RegionDef] = &[
    RegionDef {
        space: CODE,
        kind: RegionKind::Code,
        area_header: ".area CODE (CODE,ABS)\n",
    },
    RegionDef {
        space: IDATA,
        kind: RegionKind::Data,
        area_header: ".area IDATA (IDATA,ABS)\n",
    },
    RegionDef {
        space: SFR,
        kind: RegionKind::Register,
        area_header: ".area SFR (SFR,ABS)\n",
    },
    RegionDef {
        space: BIT,
        kind: RegionKind::Register,
        area_header: ".area BIT (BIT,ABS)\n",
    },
    RegionDef {
        space: XDATA,
        kind: RegionKind::Data,
        area_header: ".area XDATA (XDATA,ABS)\n",
    },
];

/// The MCS-51 vectors.
static ENTRY_POINTS: &[EntryPoint] = &[
    EntryPoint { space: CODE, offset: 0x00, name: "INT_reset", reason: "power-on reset" },
    EntryPoint { space: CODE, offset: 0x03, name: "INT_ext0", reason: "external interrupt 0" },
    EntryPoint { space: CODE, offset: 0x0B, name: "INT_timer0", reason: "timer 0 overflow" },
    EntryPoint { space: CODE, offset: 0x13, name: "INT_ext1", reason: "external interrupt 1" },
    EntryPoint { space: CODE, offset: 0x1B, name: "INT_timer1", reason: "timer 1 overflow" },
    EntryPoint { space: CODE, offset: 0x23, name: "INT_serial", reason: "serial port" },
];

/// The 8051 driver.
#[derive(Debug, Default, Clone, Copy)]
pub struct I8051;

/// A shared 8051 driver.
pub fn platform() -> PlatformRef {
    Arc::new(I8051)
}

impl Platform for I8051 {
    fn name(&self) -> &str {
        "i8051"
    }

    fn regions(&self) -> &[RegionDef] {
        REGIONS
    }

    fn entry_points(&self) -> &[EntryPoint] {
        ENTRY_POINTS
    }

    /// Bit addresses are a second name for certain SFRs.
    fn canonicalize(&self, space: AddressSpace, offset: AddressValue) -> CanonicalAddr {
        if space != BIT || offset > 0xFF {
            return CanonicalAddr { space, offset, bit: None };
        }
        let n = offset as u8;
        if n < 0x80 {
            CanonicalAddr {
                space: IDATA,
                offset: AddressValue::from(0x20 + n / 8),
                bit: Some(n % 8),
            }
        } else {
            CanonicalAddr {
                space: SFR,
                offset: AddressValue::from(n & 0xF8),
                bit: Some(n & 0x07),
            }
        }
    }

    fn max_insn_len(&self) -> usize {
        Instruction::MAX_LENGTH
    }

    fn decode(&self, pc: u32, bytes: &[u8]) -> DecodedInsn {
        let insn = Instruction::decode_from_bytes(pc, bytes);
        let control_flow = map_control_flow(insn.control_flow());
        DecodedInsn {
            len: insn.len() as u8,
            bytes: insn.bytes().to_vec(),
            text: insn.as_string(),
            control_flow,
            branch_operand_index: branch_operand_index(&insn, control_flow),
            named_register: insn.direct_addr(),
            data_refs: data_refs(&insn),
        }
    }
}

fn map_control_flow(cf: ::i8051::ControlFlow) -> ControlFlow {
    use ::i8051::ControlFlow as I;
    match cf {
        I::Continue { next } => ControlFlow::Continue { next },
        I::Jump { target } => ControlFlow::Jump { target },
        I::Call { target, return_pc } => ControlFlow::Call { target, return_pc },
        I::Choice {
            fall_through,
            branch_target,
        } => ControlFlow::Choice {
            fall_through,
            branch_target,
        },
        I::Diverge => ControlFlow::Diverge,
    }
}

/// Which operand holds the branch target. The absolute-address jumps/calls put
/// it first; every other branch (conditional, `CJNE`, `DJNZ`, ...) puts the
/// `rel`/`addr` operand last.
fn branch_operand_index(insn: &Instruction, cf: ControlFlow) -> Option<usize> {
    // Only instructions with a static target name a target operand.
    match cf {
        ControlFlow::Jump { .. } | ControlFlow::Call { .. } | ControlFlow::Choice { .. } => {}
        _ => return None,
    }
    match insn.mnemonic() {
        Mnemonic::LJMP | Mnemonic::LCALL | Mnemonic::AJMP | Mnemonic::ACALL | Mnemonic::SJMP => {
            Some(0)
        }
        _ => {
            let decoded = insn.as_string();
            let operand_count = decoded.split_once(' ').map_or(0, |(_, rest)| {
                if rest.is_empty() {
                    0
                } else {
                    rest.split(',').count()
                }
            });
            operand_count.checked_sub(1)
        }
    }
}

/// Read/write role of a memory operand, from the ISA semantics.
#[derive(Debug, Clone, Copy)]
enum Access {
    Read,
    Write,
    ReadWrite,
}

impl Access {
    fn kind(self) -> XrefType {
        match self {
            Access::Read => XrefType::Read,
            Access::Write => XrefType::Write,
            Access::ReadWrite => XrefType::ReadWrite,
        }
    }
}

/// Access role of a `Direct`/`Bit` operand at `index` (0 = destination, 1 =
/// source on the 8051). `None` for mnemonics that never address memory.
/// Transcribed from the ISA bodies in `i8051::op`: `MOV direct,A` writes,
/// `MOV A,direct` reads, `ANL direct,A` and `INC direct` read-modify-write.
fn memory_access(mnemonic: Mnemonic, index: usize) -> Option<Access> {
    use Mnemonic::*;
    Some(match mnemonic {
        MOV => {
            if index == 0 {
                Access::Write
            } else {
                Access::Read
            }
        }
        ANL | ORL | XRL => {
            if index == 0 {
                Access::ReadWrite
            } else {
                Access::Read
            }
        }
        CJNE | ADD | ADDC | SUBB | PUSH | JB | JNB => Access::Read,
        POP | CLR | SETB => Access::Write,
        INC | DEC | CPL | DJNZ | XCH | JBC => Access::ReadWrite,
        _ => return None,
    })
}

fn is_dptr_load(insn: &Instruction) -> bool {
    insn.mnemonic() == Mnemonic::MOV
        && matches!(
            insn.operands().as_slice(),
            [Operand::Dptr, Operand::Imm16(_)]
        )
}

/// The static data references an instruction makes. 
/// 
/// For 8051: Direct operands split into `SFR` (`>= 0x80`) or `IDATA`. Bit
/// operands land in `BIT`. A `MOV DPTR,#addr` may be either CODE or XDATA.
fn data_refs(insn: &Instruction) -> Vec<DataRef> {
    let mut refs = Vec::new();
    let dptr_load = is_dptr_load(insn);
    for (index, operand) in insn.operands().as_slice().iter().enumerate() {
        match operand {
            Operand::Direct(addr) => {
                if let Some(access) = memory_access(insn.mnemonic(), index) {
                    let space = if *addr >= 0x80 { SFR } else { IDATA };
                    refs.push(DataRef {
                        space,
                        offset: u32::from(*addr),
                        kind: access.kind(),
                        certainty: Certainty::Definite,
                        operand: u8::try_from(index).ok(),
                    });
                }
            }
            Operand::Bit(addr) | Operand::BitNot(addr) => {
                if let Some(access) = memory_access(insn.mnemonic(), index) {
                    refs.push(DataRef {
                        space: BIT,
                        offset: u32::from(*addr),
                        kind: access.kind(),
                        certainty: Certainty::Definite,
                        operand: u8::try_from(index).ok(),
                    });
                }
            }
            Operand::Imm16(value) if dptr_load => {
                for space in [CODE, XDATA] {
                    refs.push(DataRef {
                        space,
                        offset: u32::from(*value),
                        kind: XrefType::Pointer,
                        certainty: Certainty::Inferred,
                        operand: u8::try_from(index).ok(),
                    });
                }
            }
            _ => {}
        }
    }
    refs
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::platform::test_util::edges;
    use pretty_assertions::assert_eq;

    #[test]
    fn data_xrefs_classify_direction_and_space() {
        use XrefType::{Jump, Pointer, Read, ReadWrite, Write};
        let e = |bytes: &[u8]| edges(&I8051, bytes);

        // MOV A,P1 (E5 90) reads SFR 0x90. MOV P1,A (F5 90) writes it.
        assert_eq!(e(&[0xE5, 0x90]), vec![(SFR, 0x90, Read)]);
        assert_eq!(e(&[0xF5, 0x90]), vec![(SFR, 0x90, Write)]);
        // Low direct addresses land in IDATA. INC direct is read-modify-write.
        assert_eq!(e(&[0x05, 0x30]), vec![(IDATA, 0x30, ReadWrite)]);
        // MOV DPTR,#0x1234 is ambiguous: it may be either CODE or XDATA.
        assert_eq!(
            e(&[0x90, 0x12, 0x34]),
            vec![(CODE, 0x1234, Pointer), (XDATA, 0x1234, Pointer)]
        );
        // MOV 0x30,0x40 (85 src dst): a write to the dest, a read of the source.
        assert_eq!(
            e(&[0x85, 0x40, 0x30]),
            vec![(IDATA, 0x30, Write), (IDATA, 0x40, Read)]
        );
        // JB 0x20,rel (20 20 05) is a branch AND a bit read.
        assert_eq!(e(&[0x20, 0x20, 0x05]), vec![(CODE, 8, Jump), (BIT, 0x20, Read)]);
    }

    #[test]
    fn bit_addresses_resolve_to_the_byte_that_holds_them() {
        let p = I8051;
        let bit = |n| p.canonicalize(BIT, n);

        // 0x00-0x7F: bits of internal RAM 0x20-0x2F.
        assert_eq!(bit(0x65), CanonicalAddr { space: IDATA, offset: 0x2C, bit: Some(5) });
        assert_eq!(bit(0x00), CanonicalAddr { space: IDATA, offset: 0x20, bit: Some(0) });
        assert_eq!(bit(0x7F), CanonicalAddr { space: IDATA, offset: 0x2F, bit: Some(7) });

        // 0x80-0xFF: bits of the SFRs on eight-byte boundaries.
        assert_eq!(bit(0x99), CanonicalAddr { space: SFR, offset: 0x98, bit: Some(1) });
        assert_eq!(bit(0xB3), CanonicalAddr { space: SFR, offset: 0xB0, bit: Some(3) });

        assert_eq!(
            p.canonicalize(IDATA, 0x2C),
            CanonicalAddr { space: IDATA, offset: 0x2C, bit: None }
        );
        assert_eq!(p.canonicalize(SFR, 0x99), CanonicalAddr { space: SFR, offset: 0x99, bit: None });

        assert_ne!(bit(0x99).offset, p.canonicalize(SFR, 0x99).offset);
    }
}
