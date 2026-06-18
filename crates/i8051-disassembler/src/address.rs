use i8051::{ControlFlow, Instruction, Opcode};
use serde::{Deserialize, Serialize};

pub type AddressValue = u32;

/// Explicit emission order for sdas area headers.
pub const AREA_ORDER: [AddressSpace; 5] = [
    AddressSpace::Code,
    AddressSpace::Idata,
    AddressSpace::Sfr,
    AddressSpace::Bit,
    AddressSpace::Xdata,
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum AddressSpace {
    Code,
    Idata,
    Sfr,
    Bit,
    Xdata,
}

impl AddressSpace {
    pub fn area_header(self) -> &'static str {
        match self {
            Self::Code => ".area CODE (CODE,ABS)\n",
            Self::Idata => ".area IDATA (IDATA,ABS)\n",
            Self::Sfr => ".area SFR (SFR,ABS)\n",
            Self::Bit => ".area BIT (BIT,ABS)\n",
            Self::Xdata => ".area XDATA (XDATA,ABS)\n",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct PhysicalAddr {
    pub space: AddressSpace,
    pub offset: AddressValue,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Xref {
    pub from: PhysicalAddr,
    pub to: PhysicalAddr,
    pub xref_type: XrefType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum XrefType {
    Call,
    Jump,
    Data,
}

pub fn branch_target_operand_index(insn: &Instruction) -> Option<usize> {
    if branch_target(insn).is_none() {
        return None;
    }
    match insn.mnemonic() {
        Opcode::LJMP | Opcode::LCALL | Opcode::AJMP | Opcode::ACALL | Opcode::SJMP => Some(0),
        _ => {
            let decoded = insn.as_string();
            let operand_count = decoded.split_once(' ').map_or(0, |(_, rest)| {
                if rest.is_empty() {
                    0
                } else {
                    rest.split(',').count()
                }
            });
            if operand_count == 0 {
                None
            } else {
                Some(operand_count - 1)
            }
        }
    }
}

pub fn branch_target(insn: &Instruction) -> Option<u32> {
    let next = insn.pc().wrapping_add(insn.len() as u32);
    match insn.control_flow() {
        ControlFlow::Continue(addr) if u32::from(addr) != next => Some(u32::from(addr)),
        ControlFlow::Call(_, addr) => Some(u32::from(addr)),
        ControlFlow::Choice(_, addr) => Some(u32::from(addr)),
        _ => None,
    }
}

pub fn xrefs_from_instruction(instruction: &Instruction, source: PhysicalAddr) -> Vec<Xref> {
    let next = instruction.pc().wrapping_add(instruction.len() as u32);

    let mut xrefs = Vec::new();
    let mut push = |to_offset: u32, xref_type: XrefType| {
        xrefs.push(Xref {
            from: source,
            to: PhysicalAddr {
                space: source.space,
                offset: to_offset,
            },
            xref_type,
        });
    };

    match instruction.control_flow() {
        ControlFlow::Continue(addr) if u32::from(addr) != next => {
            push(u32::from(addr), xref_type_for(instruction.mnemonic()));
        }
        ControlFlow::Call(_, addr) => push(u32::from(addr), XrefType::Call),
        ControlFlow::Choice(_, addr) => push(u32::from(addr), XrefType::Jump),
        _ => {}
    }

    xrefs
}

pub fn xrefs_to_target(
    instruction: &Instruction,
    source: PhysicalAddr,
    target: &PhysicalAddr,
) -> Vec<Xref> {
    xrefs_from_instruction(instruction, source)
        .into_iter()
        .filter(|xref| xref.to == *target)
        .collect()
}

fn xref_type_for(opcode: Opcode) -> XrefType {
    match opcode {
        Opcode::LCALL | Opcode::ACALL => XrefType::Call,
        _ => XrefType::Jump,
    }
}
