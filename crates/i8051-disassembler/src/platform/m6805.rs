//! The Motorola 6805 (M68HC05) driver.
//!
//! Adapts the [`::m6805`] decoder to the neutral [`DecodedInsn`]. Like the
//! 6502, the 6805 has a single flat address space, data references are
//! classified by the load/store/RMW/bit role of each mnemonic. Used to
//! disassemble the "green LED" DEC LK201 keyboard ROM.

use std::sync::Arc;

use ::m6805::{Instruction, Mnemonic, Operand};

use crate::address::{AddressSpace, XrefType};

use super::{ControlFlow, DataRef, DecodedInsn, Platform, PlatformRef, RegionDef, RegionKind};

/// The 6805's single flat address space (RAM, I/O, and ROM).
pub const CODE: AddressSpace = AddressSpace::new("CODE");

/// The one 6805 region.
static REGIONS: &[RegionDef] = &[RegionDef {
    space: CODE,
    kind: RegionKind::Code,
    area_header: ".area CODE (CODE,ABS)\n",
}];

/// The 6805 driver.
#[derive(Debug, Default, Clone, Copy)]
pub struct M6805;

/// A shared 6805 driver.
pub fn platform() -> PlatformRef {
    Arc::new(M6805)
}

impl Platform for M6805 {
    fn name(&self) -> &str {
        "m6805"
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
        // The target is always the last operand: the lone operand of a branch or
        // JMP/JSR, or the `rel` at the end of a BRSET/BRCLR.
        let branch_operand_index =
            has_target(control_flow).then(|| insn.operands().as_slice().len().checked_sub(1))
                .flatten();
        DecodedInsn {
            len: insn.len() as u8,
            bytes: insn.bytes().to_vec(),
            text: insn.as_string(),
            control_flow,
            branch_operand_index,
            named_register: None,
            data_refs: data_refs(&insn),
        }
    }
}

fn map_control_flow(cf: ::m6805::ControlFlow) -> ControlFlow {
    use ::m6805::ControlFlow as M;
    match cf {
        M::Continue { next } => ControlFlow::Continue { next },
        M::Jump { target } => ControlFlow::Jump { target },
        M::Call { target, return_pc } => ControlFlow::Call { target, return_pc },
        M::Choice {
            fall_through,
            branch_target,
        } => ControlFlow::Choice {
            fall_through,
            branch_target,
        },
        M::Diverge => ControlFlow::Diverge,
    }
}

fn has_target(cf: ControlFlow) -> bool {
    matches!(
        cf,
        ControlFlow::Jump { .. } | ControlFlow::Call { .. } | ControlFlow::Choice { .. }
    )
}

/// Read/write role of a memory-operand instruction.
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

/// The access role of a mnemonic that touches memory. Register-only forms
/// (`NEGA`, `CLRX`, ...) are distinct mnemonics with no memory operand, so they
/// fall through to `None`. Control-flow mnemonics are handled by the neutral
/// control flow, not here.
fn memory_access(mnemonic: Mnemonic) -> Option<Access> {
    use Mnemonic::*;
    Some(match mnemonic {
        STA | STX | CLR => Access::Write,
        NEG | COM | LSR | ROR | ASR | LSL | ROL | DEC | INC | BSET | BCLR => Access::ReadWrite,
        LDA | LDX | CMP | CPX | BIT | SUB | SBC | AND | EOR | ADC | ORA | ADD | TST | BRSET
        | BRCLR => Access::Read,
        _ => return None,
    })
}

/// The static data reference an instruction makes, if any: the direct or
/// extended address it names. Indexed and immediate operands have no static
/// target and are skipped.
fn data_refs(insn: &Instruction) -> Vec<DataRef> {
    let Some(access) = memory_access(insn.mnemonic()) else {
        return Vec::new();
    };
    let offset = insn.operands().as_slice().iter().find_map(|op| match op {
        Operand::Direct(a) => Some(u32::from(*a)),
        Operand::Extended(a) => Some(u32::from(*a)),
        _ => None,
    });
    match offset {
        Some(offset) => vec![DataRef {
            space: CODE,
            offset,
            kind: access.kind(),
        }],
        None => Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::address::PhysicalAddr;
    use crate::platform::{branch_target, xrefs_from_instruction};
    use pretty_assertions::assert_eq;

    fn decode(bytes: &[u8]) -> DecodedInsn {
        M6805.decode(0x1000, bytes)
    }

    #[test]
    fn decodes_and_renders() {
        assert_eq!(decode(&[0xA6, 0x42]).text, "LDA #0x42");
        assert_eq!(decode(&[0xCC, 0x20, 0x00]).text, "JMP 0x2000");
        assert_eq!(decode(&[0x16, 0x50]).text, "BSET #3,0x50");
        assert_eq!(decode(&[0x9D]).text, "NOP");
    }

    #[test]
    fn control_flow_and_targets() {
        let jsr = decode(&[0xCD, 0x0C, 0xB5]);
        assert!(matches!(
            jsr.control_flow,
            ControlFlow::Call { target: 0x0CB5, .. }
        ));
        assert_eq!(branch_target(&jsr), Some(0x0CB5));
        assert_eq!(jsr.branch_operand_index, Some(0));

        // BRSET's branch target is the third operand.
        let brset = decode(&[0x0E, 0x50, 0x10]);
        assert!(matches!(brset.control_flow, ControlFlow::Choice { .. }));
        assert_eq!(brset.branch_operand_index, Some(2));

        // Indexed jump has no static target.
        assert_eq!(decode(&[0xFC]).control_flow, ControlFlow::Diverge);
    }

    #[test]
    fn data_refs_classify_access() {
        let src = PhysicalAddr {
            space: CODE,
            offset: 0,
        };
        let refs = |bytes: &[u8]| xrefs_from_instruction(&decode(bytes), src);
        let to = |offset, kind| crate::address::Xref {
            from: src,
            to: PhysicalAddr {
                space: CODE,
                offset,
            },
            xref_type: kind,
        };

        assert_eq!(refs(&[0xB6, 0x50]), vec![to(0x50, XrefType::Read)]); // LDA 0x50
        assert_eq!(refs(&[0xC7, 0x12, 0x34]), vec![to(0x1234, XrefType::Write)]); // STA ext
        assert_eq!(refs(&[0x3A, 0x50]), vec![to(0x50, XrefType::ReadWrite)]); // DEC 0x50
        assert_eq!(refs(&[0x16, 0x50]), vec![to(0x50, XrefType::ReadWrite)]); // BSET 3,0x50
        // JMP is a control edge, not a data reference.
        assert_eq!(refs(&[0xCC, 0x12, 0x34]), vec![to(0x1234, XrefType::Jump)]);
        // Immediate makes no data reference.
        assert!(refs(&[0xA6, 0x42]).is_empty());
    }
}
