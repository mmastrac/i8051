//! The MOS 6502 driver.
//!
//! Adapts the [`::mos6502`] decoder to the neutral [`DecodedInsn`]. The 6502 has
//! a single flat 16-bit address space, so there is one region, data references
//! are classified by the load/store/RMW role of each mnemonic.

use std::sync::Arc;

use ::mos6502::{Instruction, Mnemonic, Operand};

use crate::address::{AddressSpace, XrefType};

use super::{ControlFlow, DataRef, DecodedInsn, Platform, PlatformRef, RegionDef, RegionKind};

/// The 6502's single flat address space.
pub const CODE: AddressSpace = AddressSpace::new("CODE");

/// The one 6502 region.
static REGIONS: &[RegionDef] = &[RegionDef {
    space: CODE,
    kind: RegionKind::Code,
    area_header: ".area CODE (CODE,ABS)\n",
}];

/// The 6502 driver.
#[derive(Debug, Default, Clone, Copy)]
pub struct Mos6502;

/// A shared 6502 driver.
pub fn platform() -> PlatformRef {
    Arc::new(Mos6502)
}

impl Platform for Mos6502 {
    fn name(&self) -> &str {
        "mos6502"
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
            // Every 6502 branch/jump/call carries its target as its lone operand.
            branch_operand_index: has_target(control_flow).then_some(0),
            named_register: None,
            data_refs: data_refs(&insn),
        }
    }
}

fn map_control_flow(cf: ::mos6502::ControlFlow) -> ControlFlow {
    use ::mos6502::ControlFlow as M;
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

/// The access role of a mnemonic that touches memory. `None` for control-flow,
/// register-only, and implied instructions (whose targets, if any, come from
/// the neutral control flow instead).
fn memory_access(mnemonic: Mnemonic) -> Option<Access> {
    use Mnemonic::*;
    Some(match mnemonic {
        STA | STX | STY => Access::Write,
        ASL | LSR | ROL | ROR | INC | DEC => Access::ReadWrite,
        LDA | LDX | LDY | CMP | CPX | CPY | ADC | SBC | AND | ORA | EOR | BIT => Access::Read,
        _ => return None,
    })
}

/// The static data reference an instruction makes, if any. Direct and
/// absolute-indexed operands name a fixed base address, indirect and immediate
/// operands have no static target and are skipped (like the 8051's `@Ri`).
fn data_refs(insn: &Instruction) -> Vec<DataRef> {
    let Some(access) = memory_access(insn.mnemonic()) else {
        return Vec::new();
    };
    let Some(&operand) = insn.operands().as_slice().first() else {
        return Vec::new();
    };
    let offset = match operand {
        Operand::ZeroPage(a) | Operand::ZeroPageX(a) | Operand::ZeroPageY(a) => u32::from(a),
        Operand::Absolute(a) | Operand::AbsoluteX(a) | Operand::AbsoluteY(a) => u32::from(a),
        _ => return Vec::new(),
    };
    vec![DataRef {
        space: CODE,
        offset,
        kind: access.kind(),
    }]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::address::PhysicalAddr;
    use crate::platform::{branch_target, xrefs_from_instruction};
    use pretty_assertions::assert_eq;

    fn decode(bytes: &[u8]) -> DecodedInsn {
        Mos6502.decode(0x1000, bytes)
    }

    #[test]
    fn decodes_and_renders() {
        assert_eq!(decode(&[0xA9, 0x42]).text, "LDA #0x42");
        assert_eq!(decode(&[0x4C, 0x00, 0x20]).text, "JMP 0x2000");
        assert_eq!(decode(&[0xEA]).text, "NOP");
    }

    #[test]
    fn control_flow_targets() {
        // JMP absolute is an unconditional jump.
        let jmp = decode(&[0x4C, 0x00, 0x20]);
        assert_eq!(jmp.control_flow, ControlFlow::Jump { target: 0x2000 });
        assert_eq!(branch_target(&jmp), Some(0x2000));
        assert_eq!(jmp.branch_operand_index, Some(0));

        // JSR is a call, JMP indirect diverges.
        assert!(matches!(
            decode(&[0x20, 0x00, 0x20]).control_flow,
            ControlFlow::Call { target: 0x2000, .. }
        ));
        assert_eq!(decode(&[0x6C, 0x00, 0x20]).control_flow, ControlFlow::Diverge);
        assert_eq!(decode(&[0x6C, 0x00, 0x20]).branch_operand_index, None);
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

        // LDA 0x1234 reads, STA 0x1234 writes, INC 0x1234 read-modify-writes.
        assert_eq!(refs(&[0xAD, 0x34, 0x12]), vec![to(0x1234, XrefType::Read)]);
        assert_eq!(refs(&[0x8D, 0x34, 0x12]), vec![to(0x1234, XrefType::Write)]);
        assert_eq!(refs(&[0xEE, 0x34, 0x12]), vec![to(0x1234, XrefType::ReadWrite)]);
        // A JMP is a control edge, not a data reference.
        assert_eq!(refs(&[0x4C, 0x34, 0x12]), vec![to(0x1234, XrefType::Jump)]);
        // Immediate operands make no data reference.
        assert!(refs(&[0xA9, 0x42]).is_empty());
    }
}
