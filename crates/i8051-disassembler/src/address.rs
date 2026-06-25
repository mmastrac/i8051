use std::ops::{Bound, RangeBounds};

use i8051::{ControlFlow, Instruction, Mnemonic};
use serde::{Deserialize, Serialize};

pub type SpaceAddressValue = (AddressSpace, AddressValue);
pub type SpaceAddressRange = (AddressSpace, AddressRange);

pub type AddressValue = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct AddressRange {
    pub start: AddressValue,
    pub end: AddressValue,
}

impl AddressRange {
    pub fn new(start_inclusive: AddressValue, end_exclusive: AddressValue) -> Self {
        Self {
            start: start_inclusive,
            end: end_exclusive,
        }
    }
}

impl<T: RangeBounds<AddressValue>> From<T> for AddressRange {
    fn from(range: T) -> Self {
        let range_start_inclusive = match range.start_bound() {
            Bound::Included(addr) => *addr,
            Bound::Excluded(addr) => addr.saturating_add(1),
            Bound::Unbounded => 0,
        };
        let range_end_inclusive = match range.end_bound() {
            Bound::Included(addr) => *addr,
            Bound::Excluded(addr) => addr.saturating_sub(1),
            Bound::Unbounded => AddressValue::MAX,
        };
        Self {
            start: range_start_inclusive,
            end: range_end_inclusive,
        }
    }
}

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
            if operand_count == 0 {
                None
            } else {
                Some(operand_count - 1)
            }
        }
    }
}

pub fn branch_target(insn: &Instruction) -> Option<u32> {
    match insn.control_flow() {
        ControlFlow::Jump { target } => Some(target),
        ControlFlow::Call { target, .. } => Some(target),
        ControlFlow::Choice { branch_target, .. } => Some(branch_target),
        _ => None,
    }
}

pub fn xrefs_from_instruction(instruction: &Instruction, source: PhysicalAddr) -> Vec<Xref> {
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
        ControlFlow::Jump { target } => {
            push(target, xref_type_for(instruction.mnemonic()));
        }
        ControlFlow::Call { target, .. } => push(target, XrefType::Call),
        ControlFlow::Choice { branch_target, .. } => push(branch_target, XrefType::Jump),
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

fn xref_type_for(opcode: Mnemonic) -> XrefType {
    match opcode {
        Mnemonic::LCALL | Mnemonic::ACALL => XrefType::Call,
        _ => XrefType::Jump,
    }
}
