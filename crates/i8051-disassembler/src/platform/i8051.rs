//! The Intel 8051 (MCS-51) driver.
//!
//! Adapts the concrete [`::i8051`] decoder to the neutral [`DecodedInsn`] and
//! owns every 8051-specific decision the rest of the crate used to hardcode:
//! the `SFR`/`IDATA` split at `0x80`, the per-mnemonic memory-access roles, and
//! the branch-target operand index.

use std::sync::Arc;

use ::i8051::{Instruction, Mnemonic, Operand};

use crate::address::{AddressSpace, XrefType};

use super::{ControlFlow, DataRef, DecodedInsn, Platform, PlatformRef, RegionDef, RegionKind};

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

/// The static data references an instruction makes. Direct operands split into
/// `SFR` (`>= 0x80`) or `IDATA`; bit operands land in `BIT`; a `MOV DPTR,#addr`
/// materializes an `XDATA` pointer. Indirect operands have no static target.
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
                    });
                }
            }
            Operand::Bit(addr) | Operand::BitNot(addr) => {
                if let Some(access) = memory_access(insn.mnemonic(), index) {
                    refs.push(DataRef {
                        space: BIT,
                        offset: u32::from(*addr),
                        kind: access.kind(),
                    });
                }
            }
            Operand::Imm16(value) if dptr_load => {
                refs.push(DataRef {
                    space: XDATA,
                    offset: u32::from(*value),
                    kind: XrefType::Pointer,
                });
            }
            _ => {}
        }
    }
    refs
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::address::PhysicalAddr;
    use crate::platform::xrefs_from_instruction;
    use pretty_assertions::assert_eq;

    #[test]
    fn data_xrefs_classify_direction_and_space() {
        let src = PhysicalAddr {
            space: CODE,
            offset: 0,
        };
        let edges = |bytes: &[u8]| xrefs_from_instruction(&I8051.decode(0, bytes), src);
        let edge = |space, offset, xref_type| crate::address::Xref {
            from: src,
            to: PhysicalAddr { space, offset },
            xref_type,
        };
        use XrefType::{Jump, Pointer, Read, ReadWrite, Write};

        // MOV A,P1 (E5 90) reads SFR 0x90. MOV P1,A (F5 90) writes it.
        assert_eq!(edges(&[0xE5, 0x90]), vec![edge(SFR, 0x90, Read)]);
        assert_eq!(edges(&[0xF5, 0x90]), vec![edge(SFR, 0x90, Write)]);
        // Low direct addresses land in IDATA. INC direct is read-modify-write.
        assert_eq!(edges(&[0x05, 0x30]), vec![edge(IDATA, 0x30, ReadWrite)]);
        // MOV DPTR,#0x1234 materializes an address, not a dereference.
        assert_eq!(edges(&[0x90, 0x12, 0x34]), vec![edge(XDATA, 0x1234, Pointer)]);
        // MOV 0x30,0x40 (85 src dst): a write to the dest, a read of the source.
        assert_eq!(
            edges(&[0x85, 0x40, 0x30]),
            vec![edge(IDATA, 0x30, Write), edge(IDATA, 0x40, Read)]
        );
        // JB 0x20,rel (20 20 05) is a branch AND a bit read.
        assert_eq!(
            edges(&[0x20, 0x20, 0x05]),
            vec![edge(CODE, 8, Jump), edge(BIT, 0x20, Read)]
        );
    }
}
