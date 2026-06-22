use std::collections::HashSet;

use i8051::{ControlFlow, Instruction, Mnemonic, Operand};
use leiden_rs::{GraphDataBuilder, Leiden, LeidenConfig, LeidenError};

use crate::address::{AddressSpace, AddressValue};
use crate::db::{DataType, Db};
use crate::render::Line;

const NUMBER_SPACE: usize = 65536;
const SFR_SPACE: usize = 256;

/// Edge weight for direct call cross-references.
const WEIGHT_CALL: f64 = 8.0;
/// Edge weight for jump cross-references.
const WEIGHT_JUMP: f64 = 8.0;
/// Edge weight for jump cross-references.
const WEIGHT_CONTINUE: f64 = 10.0;
/// Edge weight for code that touches an internal RAM direct address.
const WEIGHT_DATA_DIRECT: f64 = 3.0;
/// Edge weight for code that touches an SFR.
const WEIGHT_SFR: f64 = 3.0;
/// Edge weight for code that touches a bit register.
const WEIGHT_BIT: f64 = 2.0;
/// Edge weight for `MOV DPTR, #imm16` and similar 16-bit address loads.
const WEIGHT_DPTR_LOAD: f64 = 5.0;
/// Edge weight for other 16-bit immediates.
const WEIGHT_IMM16: f64 = 2.0;
/// Edge weight for 8-bit immediates treated as data values.
const WEIGHT_IMM8: f64 = 0.01;
/// Weak tie between a code address and the number node at the same value.
const WEIGHT_CODE_NUMBER_WEAK: f64 = 0.01;

struct GraphLayout {
    code_size: usize,
}

impl GraphLayout {
    fn node_count(&self) -> usize {
        self.code_size + NUMBER_SPACE + SFR_SPACE
    }

    fn code(&self, addr: AddressValue) -> Option<usize> {
        ((addr as usize) < self.code_size).then_some(addr as usize)
    }

    fn number(&self, value: u16) -> usize {
        self.code_size + value as usize
    }

    fn sfr(&self, addr: u8) -> usize {
        self.code_size + NUMBER_SPACE + addr as usize
    }

    fn describe(&self, node: usize) -> String {
        if node < self.code_size {
            format!("code:0x{node:04X}")
        } else if node < self.code_size + NUMBER_SPACE {
            format!("num:0x{:04X}", node - self.code_size)
        } else {
            format!("sfr:0x{:02X}", node - self.code_size - NUMBER_SPACE)
        }
    }
}

/// Computes the Leiden communities for the given database from
/// code cross-references, SFRs, bit registers and DPTR loads.
pub fn leiden_communities(db: &Db) -> Result<(), LeidenError> {
    let code_size = code_region_size(db).max(1) as usize;
    let layout = GraphLayout { code_size };
    let mut builder = GraphDataBuilder::new(layout.node_count());

    let mut interesting = HashSet::new();

    for line in db.render(AddressSpace::Code) {
        if let Line::Data {
            addr,
            bytes,
            data_type: DataType::Word,
            ..
        } = line
        {
            for i in 0..bytes.len() / 2 {
                let offset = addr + i as AddressValue * 2;
                let value = db
                    .region(AddressSpace::Code)
                    .unwrap()
                    .read_u16_le(offset)
                    .unwrap();
                if let Some(from) = layout.code(addr) {
                    add_edge(&mut builder, from, layout.number(value), WEIGHT_IMM16)?;
                }
                // Word values are code addresses (jump tables, pointers, etc.).
                interesting.insert(u32::from(value));
            }
            continue;
        }
        let Line::Instruction { addr, bytes, .. } = line else {
            continue;
        };
        let Some(from) = layout.code(addr) else {
            continue;
        };

        add_edge(
            &mut builder,
            from,
            layout.number(addr as u16),
            WEIGHT_CODE_NUMBER_WEAK,
        )?;

        let instruction = Instruction::decode_from_bytes(addr as u32, &bytes);

        match instruction.control_flow() {
            ControlFlow::Continue { next } => {
                if let Some(to) = layout.code(next) {
                    add_edge(&mut builder, from, to, WEIGHT_CONTINUE)?;
                }
            }
            ControlFlow::Jump { target } => {
                if let Some(to) = layout.code(target) {
                    add_edge(&mut builder, from, to, WEIGHT_JUMP)?;
                }
                interesting.insert(target);
            }
            ControlFlow::Call { target, return_pc } => {
                if let Some(to) = layout.code(target) {
                    add_edge(&mut builder, from, to, WEIGHT_CALL)?;
                }
                if let Some(to) = layout.code(return_pc) {
                    add_edge(&mut builder, from, to, WEIGHT_CONTINUE)?;
                }
                interesting.insert(target);
            }
            ControlFlow::Choice {
                fall_through,
                branch_target,
            } => {
                if let Some(to) = layout.code(branch_target) {
                    add_edge(&mut builder, from, to, WEIGHT_JUMP)?;
                }
                if let Some(to) = layout.code(fall_through) {
                    add_edge(&mut builder, from, to, WEIGHT_CONTINUE)?;
                }
                interesting.insert(branch_target);
            }
            ControlFlow::Diverge => {}
        }

        if let Some(dptr_value) = dptr_load_value(&instruction) {
            add_edge(
                &mut builder,
                from,
                layout.number(dptr_value),
                WEIGHT_DPTR_LOAD,
            )?;
        }

        for operand in instruction.operands().as_slice() {
            match operand {
                Operand::Direct(addr) if *addr >= 0x80 => {
                    add_edge(&mut builder, from, layout.sfr(*addr), WEIGHT_SFR)?;
                }
                Operand::Direct(addr) => {
                    add_edge(
                        &mut builder,
                        from,
                        layout.number(u16::from(*addr)),
                        WEIGHT_DATA_DIRECT,
                    )?;
                }
                Operand::Bit(addr) | Operand::BitNot(addr) if *addr >= 0x80 => {
                    add_edge(&mut builder, from, layout.sfr(*addr & 0xF8), WEIGHT_SFR)?;
                }
                Operand::Bit(addr) | Operand::BitNot(addr) => {
                    add_edge(
                        &mut builder,
                        from,
                        layout.number(u16::from(*addr)),
                        WEIGHT_BIT,
                    )?;
                }
                Operand::Imm16(value) if dptr_load_value(&instruction).is_none() => {
                    add_edge(&mut builder, from, layout.number(*value), WEIGHT_IMM16)?;
                }
                Operand::Imm8(value) => {
                    add_edge(
                        &mut builder,
                        from,
                        layout.number(u16::from(*value)),
                        WEIGHT_IMM8,
                    )?;
                }
                _ => {}
            }
        }
    }

    let graph = builder.build()?;
    let leiden = Leiden::new(LeidenConfig::default());
    let result = leiden.run(&graph)?;

    println!(
        "Detected {} communities (quality: {:.4})",
        result.partition.num_communities(),
        result.quality
    );
    let mut communities = result.partition.communities();
    communities.sort_by_key(|(id, _)| *id);
    for (community_id, nodes) in communities {
        if nodes.len() <= 3 {
            continue;
        }
        let members = nodes
            .iter()
            .filter(|node| **node < layout.code_size && interesting.contains(&(**node as u32)))
            .map(|node| layout.describe(*node))
            .collect::<Vec<_>>()
            .join(", ");
        if members.is_empty() {
            continue;
        }
        println!("  community {community_id}: [{members}]");
    }

    Ok(())
}

fn add_edge(
    builder: &mut GraphDataBuilder,
    from: usize,
    to: usize,
    weight: f64,
) -> Result<(), LeidenError> {
    if from != to {
        builder.add_edge(from, to, weight)?;
    }
    Ok(())
}

fn dptr_load_value(instruction: &Instruction) -> Option<u16> {
    if instruction.mnemonic() != Mnemonic::MOV {
        return None;
    }
    let operands = instruction.operands().as_slice();
    match operands {
        [Operand::Dptr, Operand::Imm16(value)] => Some(*value),
        _ => None,
    }
}

fn code_region_size(db: &Db) -> AddressValue {
    let mut end = 0;
    for line in db.render(AddressSpace::Code) {
        match line {
            Line::Instruction { addr, bytes, .. } => {
                end = end.max(addr.saturating_add(bytes.len() as AddressValue));
                let instruction = Instruction::decode_from_bytes(addr as u32, &bytes);
                if let Some(target) = instruction.target() {
                    end = end.max(target.saturating_add(1));
                }
            }
            Line::Data { addr, bytes, .. } | Line::Raw { addr, bytes, .. } => {
                end = end.max(addr.saturating_add(bytes.len() as AddressValue));
            }
            _ => {}
        }
    }
    end
}
