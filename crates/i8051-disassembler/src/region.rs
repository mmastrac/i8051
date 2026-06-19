use i8051::{ControlFlow, Instruction};
use std::collections::BTreeMap;
use std::range::Range;

use crate::address::{
    AddressSpace, AddressValue, PhysicalAddr, Xref, branch_target, branch_target_operand_index,
    xrefs_from_instruction, xrefs_to_target,
};
use crate::command::Command;
use crate::db::{
    Equivalent, EquivalentAt, EquivalentRange, Error, Function, Line, OperandOverride, SpaceUsage,
};
use crate::labels::{ImplicitLabels, LabelCollector, Labels};

#[derive(Debug, Clone)]
pub enum ByteRange {
    Mapped(String, usize, Vec<u8>),
    Constant(AddressValue, u8),
}

pub struct Region {
    byte_ranges: BTreeMap<AddressValue, ByteRange>,
    equivalents: BTreeMap<AddressValue, EquivalentRange>,
    labels: BTreeMap<AddressValue, String>,
    comments: BTreeMap<AddressValue, String>,
    functions: BTreeMap<AddressValue, Function>,
}

impl Region {
    pub fn new() -> Self {
        Self {
            byte_ranges: BTreeMap::new(),
            equivalents: BTreeMap::new(),
            labels: BTreeMap::new(),
            comments: BTreeMap::new(),
            functions: BTreeMap::new(),
        }
    }

    pub fn set_bytes(
        &mut self,
        file: &str,
        file_offset: usize,
        offset: AddressValue,
        bytes: &[u8],
    ) {
        self.map_bytes(file, file_offset, offset, bytes);
    }

    pub fn map_bytes(
        &mut self,
        file: &str,
        file_offset: usize,
        offset: AddressValue,
        bytes: &[u8],
    ) {
        if !bytes.is_empty() {
            self.clear_bytes(offset, bytes.len() as AddressValue);
        }
        self.byte_ranges.insert(
            offset,
            ByteRange::Mapped(file.to_string(), file_offset, bytes.to_vec()),
        );
    }

    pub fn set_constant(&mut self, offset: AddressValue, size: AddressValue, value: u8) {
        if size == 0 {
            return;
        }
        self.clear_bytes(offset, size);
        self.byte_ranges
            .insert(offset, ByteRange::Constant(size, value));
    }

    pub(crate) fn snapshot_byte_ranges(
        &self,
        offset: AddressValue,
        size: AddressValue,
    ) -> Vec<(AddressValue, ByteRange)> {
        if size == 0 {
            return Vec::new();
        }
        let end = offset.saturating_add(size);
        self.byte_ranges
            .iter()
            .filter(|(start, range)| range_end(**start, range) > offset && **start < end)
            .map(|(start, range)| (*start as AddressValue, range.clone()))
            .collect()
    }

    pub fn clear_bytes(&mut self, offset: AddressValue, size: AddressValue) {
        if size == 0 {
            return;
        }
        let end = offset.saturating_add(size);
        let mut kept = BTreeMap::new();

        for (&start, range) in &self.byte_ranges {
            match range {
                ByteRange::Mapped(file, file_offset, data) => {
                    let range_end = start.saturating_add(data.len() as AddressValue);
                    if range_end <= offset || start >= end {
                        kept.insert(start, range.clone());
                        continue;
                    }
                    if start < offset {
                        let keep_len = offset - start;
                        kept.insert(
                            start,
                            ByteRange::Mapped(
                                file.clone(),
                                *file_offset,
                                data[..keep_len as usize].to_vec(),
                            ),
                        );
                    }
                    if range_end > end {
                        let skip = end.saturating_sub(start);
                        kept.insert(
                            end,
                            ByteRange::Mapped(
                                file.clone(),
                                file_offset.saturating_add(skip as usize),
                                data[skip as _..].to_vec(),
                            ),
                        );
                    }
                }
                ByteRange::Constant(count, value) => {
                    let range_end = start.saturating_add(*count);
                    if range_end <= offset || start >= end {
                        kept.insert(start, range.clone());
                        continue;
                    }
                    if start < offset {
                        kept.insert(start, ByteRange::Constant(offset - start, *value));
                    }
                    if range_end > end {
                        kept.insert(end, ByteRange::Constant(range_end - end, *value));
                    }
                }
            }
        }

        self.byte_ranges = kept;
    }

    pub fn set_equivalent(
        &mut self,
        offset: AddressValue,
        equivalent: Equivalent,
    ) -> Result<&EquivalentRange, Error> {
        if self.has_equivalent(offset) {
            return Err(Error::NotUndefined(offset));
        }

        let span = self.equivalent_span(offset, &equivalent)?;
        self.validate_equivalent_bounds(offset, span)?;
        self.validate_no_equivalent_overlap(offset, span)?;

        self.equivalents.insert(
            offset,
            EquivalentRange {
                end: offset.saturating_add(span),
                equivalent,
            },
        );
        Ok(&self.equivalents[&offset])
    }

    pub fn clear_equivalents(&mut self, offset: AddressValue, size: AddressValue) {
        if size == 0 {
            return;
        }
        let end = offset.saturating_add(size);
        self.equivalents
            .retain(|&start, range| range.end <= offset || start >= end);
    }

    pub fn snapshot_equivalents(
        &self,
        offset: AddressValue,
        size: AddressValue,
    ) -> Vec<(AddressValue, EquivalentRange)> {
        if size == 0 {
            return Vec::new();
        }
        let end = offset.saturating_add(size);
        self.equivalents
            .iter()
            .filter(|(start, range)| ranges_overlap(**start, range.end, offset, end))
            .map(|(&start, range)| (start, range.clone()))
            .collect()
    }

    pub fn has_equivalent(&self, offset: AddressValue) -> bool {
        self.equivalent_at(offset).is_defined()
    }

    pub fn get_equivalent(&self, offset: AddressValue) -> EquivalentAt<'_> {
        self.equivalent_at(offset)
    }

    fn equivalent_at(&self, offset: AddressValue) -> EquivalentAt<'_> {
        if let Some(range) = self.equivalents.get(&offset) {
            return EquivalentAt::Defined {
                start: offset,
                range,
            };
        }
        if let Some((&start, range)) = self.equivalents.range(..=offset).next_back() {
            if offset < range.end {
                return EquivalentAt::Defined { start, range };
            }
        }
        EquivalentAt::Undefined(self.undefined_range_at(offset))
    }

    fn undefined_range_at(&self, offset: AddressValue) -> Range<AddressValue> {
        let after = offset.saturating_add(1);
        let next_eq = self.equivalents.range(after..).next().map(|(&k, _)| k);
        let next_lbl = self.labels.range(after..).next().map(|(&k, _)| k);
        let next_cmt = self.comments.range(after..).next().map(|(&k, _)| k);
        let end = [Some(self.end()), next_eq, next_lbl, next_cmt]
            .into_iter()
            .flatten()
            .min()
            .unwrap_or(self.end());
        (offset..end).into()
    }

    pub fn set_label(&mut self, offset: AddressValue, label: &str) {
        self.labels
            .insert(offset as AddressValue, label.to_string());
    }

    pub fn clear_label(&mut self, offset: AddressValue) {
        self.labels.remove(&(offset as AddressValue));
    }

    pub fn get_label(&self, offset: AddressValue) -> Option<&str> {
        self.labels
            .get(&(offset as AddressValue))
            .map(String::as_str)
    }

    pub fn set_comment(&mut self, offset: AddressValue, comment: &str) {
        self.comments
            .insert(offset as AddressValue, comment.to_string());
    }

    pub fn clear_comment(&mut self, offset: AddressValue) {
        self.comments.remove(&(offset as AddressValue));
    }

    pub fn get_comment(&self, offset: AddressValue) -> Option<&str> {
        self.comments
            .get(&(offset as AddressValue))
            .map(String::as_str)
    }

    pub fn set_function(&mut self, function: Function) {
        self.functions
            .insert(function.addr.offset as AddressValue, function);
    }

    pub fn get_function(&self, offset: AddressValue) -> Option<&Function> {
        self.functions.get(&(offset as AddressValue))
    }

    pub fn clear_function(&mut self, offset: AddressValue) {
        self.functions.remove(&(offset as AddressValue));
    }

    pub fn bytes_at(&self, offset: AddressValue, size: AddressValue) -> Vec<u8> {
        (0..size)
            .filter_map(|i| self.read_byte(offset + i))
            .collect()
    }

    /// Count mapped bytes classified as code, data, or undefined (no equivalent).
    pub fn space_usage(&self) -> SpaceUsage {
        let mut usage = SpaceUsage::default();
        for (&start, range) in &self.equivalents {
            let span = range.end.saturating_sub(start);
            match &range.equivalent {
                Equivalent::Code(_) => usage.code += span,
                Equivalent::Data(_, _) => usage.data += span,
            }
        }

        let mapped: AddressValue = self
            .byte_ranges
            .iter()
            .map(|(&start, range)| range_end(start, range).saturating_sub(start))
            .sum();

        usage.undefined = mapped.saturating_sub(usage.code).saturating_sub(usage.data);

        usage
    }

    pub(crate) fn render(
        &self,
        space: AddressSpace,
        implicit_labels: &ImplicitLabels,
    ) -> Vec<Line> {
        let mut lines = Vec::new();
        let start = self.start();
        let end = self.end();
        if start >= end {
            return lines;
        }

        let default_labels = Labels::default();
        let labels = implicit_labels.get(&space).unwrap_or(&default_labels);

        let mut addr = start;
        let mut need_org = true;
        while addr < end {
            if need_org {
                lines.push(Line::Org { addr });
                lines.push(Line::Blank);
                need_org = false;
            }

            if let Some(function) = self.get_function(addr) {
                lines.push(Line::Function {
                    addr,
                    name: function.name.clone(),
                    signature: function.signature.clone(),
                    length: function.length,
                    noreturn: function.noreturn,
                });
            }
            if let Some(comment) = self.get_comment(addr) {
                lines.push(Line::Comment {
                    addr,
                    text: comment.to_string(),
                });
            }
            if let Some(label) = self.get_label(addr) {
                lines.push(Line::Label {
                    addr,
                    name: label.to_string(),
                });
            } else if let Some(label) = labels.get(&addr) {
                lines.push(Line::Label {
                    addr,
                    name: label.to_string(),
                });
            }

            match self.get_equivalent(addr) {
                EquivalentAt::Defined { start: _, range } => match &range.equivalent {
                    Equivalent::Code(overrides) => {
                        let insn = self
                            .decode_at(addr)
                            .expect("validated code equivalent must decode");
                        let text = self.format_instruction(addr, &insn, overrides);
                        lines.push(Line::Instruction {
                            addr,
                            text,
                            bytes: insn.bytes().to_vec(),
                        });
                        addr = range.end;
                    }
                    Equivalent::Data(data_type, size) => {
                        let bytes = self.bytes_at(addr, *size);
                        lines.push(Line::Data {
                            addr,
                            data_type: data_type.clone(),
                            bytes,
                        });
                        addr = range.end;
                    }
                },
                EquivalentAt::Undefined(undefined) => {
                    let span = self.raw_run_until_next_annotation(addr, undefined.end, &labels);
                    if span == 0 {
                        if let Some((&next_mapped, _)) =
                            self.byte_ranges.range(addr.saturating_add(1)..).next()
                        {
                            if next_mapped < undefined.end {
                                addr = next_mapped;
                                need_org = true;
                                continue;
                            }
                        }
                        addr += 1;
                        continue;
                    }
                    let bytes = self.bytes_at(addr, span);
                    lines.push(Line::Raw { addr, bytes });
                    addr += span;
                }
            }
        }

        lines
    }

    pub(crate) fn to_commands(&self, space: AddressSpace) -> Vec<Command> {
        let mut commands = Vec::new();
        for (&offset, range) in &self.byte_ranges {
            match range {
                ByteRange::Mapped(file, file_offset, data) => {
                    commands.push(Command::map_bytes(
                        space,
                        offset,
                        file.clone(),
                        *file_offset,
                        data.len() as AddressValue,
                    ));
                }
                ByteRange::Constant(size, value) => {
                    commands.push(Command::set_constant_bytes(space, offset, *size, *value));
                }
            }
        }
        for (&offset, equivalent_range) in &self.equivalents {
            commands.push(Command::set_equivalent(
                space,
                offset,
                equivalent_range.equivalent.clone(),
            ));
        }
        for (&offset, label) in &self.labels {
            commands.push(Command::set_label(space, offset, label.clone()));
        }
        for (&offset, comment) in &self.comments {
            commands.push(Command::set_comment(space, offset, comment.clone()));
        }
        for (&offset, function) in &self.functions {
            commands.push(Command::set_function(space, offset, function.clone()));
        }
        commands
    }

    pub(crate) fn xrefs_to(&self, space: AddressSpace, target: &PhysicalAddr) -> Vec<Xref> {
        let mut xrefs = Vec::new();
        for (&offset, equivalent_range) in &self.equivalents {
            if !matches!(equivalent_range.equivalent, Equivalent::Code(_)) {
                continue;
            }
            let Some(instruction) = self.decode_at(offset) else {
                continue;
            };
            let source = PhysicalAddr {
                space,
                offset: offset as AddressValue,
            };
            xrefs.extend(xrefs_to_target(&instruction, source, target));
        }
        xrefs
    }

    pub(crate) fn xrefs_from(&self, source: &PhysicalAddr) -> Vec<Xref> {
        let offset = source.offset;
        let Some(equivalent_range) = self.equivalents.get(&offset) else {
            return Vec::new();
        };
        if !matches!(equivalent_range.equivalent, Equivalent::Code(_)) {
            return Vec::new();
        }
        let Some(instruction) = self.decode_at(offset) else {
            return Vec::new();
        };
        xrefs_from_instruction(&instruction, *source)
    }

    /// Collect all the necessary references for this region.
    pub(crate) fn collect_refs(&self, space: AddressSpace, refs: &mut LabelCollector) {
        for (&offset, equivalent_range) in &self.equivalents {
            if !matches!(equivalent_range.equivalent, Equivalent::Code(_)) {
                continue;
            }
            let Some(instruction) = self.decode_at(offset) else {
                continue;
            };
            xrefs_from_instruction(&instruction, PhysicalAddr { space, offset })
                .into_iter()
                .for_each(|xref| {
                    if self.get_label(xref.to.offset).is_none() {
                        refs.collect(xref.to.space, xref.to.offset, None);
                    }
                });
        }
    }

    fn start(&self) -> AddressValue {
        [
            self.byte_ranges.keys().copied().next(),
            self.equivalents.keys().copied().next(),
            self.labels.keys().copied().next(),
            self.comments.keys().copied().next(),
        ]
        .into_iter()
        .flatten()
        .min()
        .unwrap_or(0) as AddressValue
    }

    /// Upper bound of mapped bytes and equivalents. With non-overlapping ranges, the
    /// last entry in each map has the highest end address.
    fn end(&self) -> AddressValue {
        [
            self.byte_ranges.keys().rev().copied().next(),
            self.equivalents.keys().rev().copied().next(),
            self.labels.keys().rev().copied().next(),
            self.comments.keys().rev().copied().next(),
        ]
        .into_iter()
        .flatten()
        .max()
        .unwrap_or(0) as AddressValue
    }

    fn read_byte(&self, offset: AddressValue) -> Option<u8> {
        let (&start, range) = self.byte_ranges.range(..=offset).next_back()?;
        match range {
            ByteRange::Mapped(_, _, data) => data.get((offset - start) as usize).copied(),
            ByteRange::Constant(size, value) if offset - start < *size => Some(*value),
            ByteRange::Constant(_, _) => None,
        }
    }

    fn decode_at(&self, address: AddressValue) -> Option<Instruction> {
        let mut available = Vec::with_capacity(Instruction::MAX_LENGTH);

        for i in 0..Instruction::MAX_LENGTH as AddressValue {
            if let Some(b) = self.read_byte(address + i) {
                available.push(b);
            } else {
                break;
            }
        }
        if available.is_empty() {
            return None;
        }

        let ins = Instruction::decode_from_bytes(address as _, &available);
        if ins.len() > available.len() {
            return None;
        }

        Some(ins)
    }

    pub(crate) fn equivalent_span(
        &self,
        offset: AddressValue,
        equivalent: &Equivalent,
    ) -> Result<AddressValue, Error> {
        match equivalent {
            Equivalent::Code(_) => self
                .decode_at(offset)
                .map(|insn| insn.len() as AddressValue)
                .ok_or(Error::InvalidEquivalent),
            Equivalent::Data(_, size) => Ok(*size),
        }
    }

    fn validate_equivalent_bounds(
        &self,
        offset: AddressValue,
        span: AddressValue,
    ) -> Result<(), Error> {
        for i in 0..span {
            if self.read_byte(offset + i).is_none() {
                return Err(Error::InvalidAddress(offset + i));
            }
        }
        Ok(())
    }

    fn validate_no_equivalent_overlap(
        &self,
        offset: AddressValue,
        span: AddressValue,
    ) -> Result<(), Error> {
        let end = offset.saturating_add(span);
        if let Some((&other_start, other)) = self.equivalents.range(..end).next_back() {
            if other.end > offset {
                return Err(Error::Overlap(other_start));
            }
        }
        Ok(())
    }

    fn raw_run_until_next_annotation(
        &self,
        addr: AddressValue,
        limit: AddressValue,
        implicit_labels: &Labels,
    ) -> AddressValue {
        let after = addr.saturating_add(1);
        let mut boundary = limit;
        if let Some((&start, _)) = self.equivalents.range(after..).next() {
            boundary = boundary.min(start);
        }
        if let Some((&start, _)) = self.labels.range(after..).next() {
            boundary = boundary.min(start);
        }
        if let Some((&start, _)) = self.comments.range(after..).next() {
            boundary = boundary.min(start);
        }
        if let Some((&start, _)) = implicit_labels.range(after..).next() {
            boundary = boundary.min(start);
        }

        let mut end = addr;
        while end < boundary {
            if self.read_byte(end).is_none() {
                break;
            }
            end += 1;
        }
        end - addr
    }

    fn format_instruction(
        &self,
        _addr: AddressValue,
        insn: &Instruction,
        overrides: &[Option<OperandOverride>],
    ) -> String {
        let decoded = insn.as_string();
        let mut merged = overrides.to_vec();

        if let (Some(target), Some(idx)) = (branch_target(insn), branch_target_operand_index(insn))
        {
            while merged.len() <= idx {
                merged.push(None);
            }
            if merged[idx].is_none() {
                if let Some(label) = self.get_label(target) {
                    merged[idx] = Some(OperandOverride::Label(label.to_string()));
                }
            }
        }

        let text = if merged.iter().all(|o| o.is_none()) {
            decoded
        } else {
            apply_operand_overrides(&decoded, &merged)
        };
        sdas_indent_instruction(&text)
    }

    /// Auto-disassembles code addresses recursively. Will not modify any address that already
    /// has an equivalent.
    pub fn auto_disassemble(&mut self, start: u32) -> Vec<AddressValue> {
        let mut addresses = Vec::new();
        let mut queue = Vec::new();
        queue.push(start);
        while let Some(addr) = queue.pop() {
            if self.has_equivalent(addr) {
                continue;
            }
            addresses.push(addr);
            let Ok(_) = self.set_equivalent(addr, Equivalent::Code(vec![])) else {
                return addresses;
            };
            if let Some(ins) = self.decode_at(addr) {
                let flow = ins.control_flow();
                match flow {
                    ControlFlow::Continue(addr) => queue.push(addr),
                    ControlFlow::Call(next, addr) => {
                        queue.push(next);
                        queue.push(addr);
                    }
                    ControlFlow::Choice(next, addr) => {
                        queue.push(next);
                        queue.push(addr);
                    }
                    ControlFlow::Diverge => {
                        continue;
                    }
                }
            }
        }
        addresses
    }
}

fn sdas_indent_instruction(text: &str) -> String {
    if let Some((mnemonic, operands)) = text.split_once(' ') {
        format!("    {mnemonic:<8}{operands}")
    } else {
        format!("    {text}")
    }
}

fn split_instruction(decoded: &str) -> (&str, Vec<&str>) {
    let (mnemonic, rest) = decoded.split_once(' ').unwrap_or((decoded, ""));
    let operands = if rest.is_empty() {
        Vec::new()
    } else {
        rest.split(',').map(str::trim).collect()
    };
    (mnemonic, operands)
}

fn apply_operand_overrides(decoded: &str, overrides: &[Option<OperandOverride>]) -> String {
    if overrides.is_empty() {
        return decoded.to_string();
    }
    let (mnemonic, operands) = split_instruction(decoded);
    let mut out = mnemonic.to_string();
    let operand_count = operands.len().max(overrides.len());
    for idx in 0..operand_count {
        if idx > 0 {
            out.push(',');
        } else {
            out.push(' ');
        }
        match overrides.get(idx).and_then(|o| o.as_ref()) {
            Some(OperandOverride::Label(label)) => out.push_str(label),
            Some(OperandOverride::LabelOffset { label, offset }) => {
                if *offset >= 0 {
                    out.push_str(&format!("{label}+{offset}"));
                } else {
                    out.push_str(&format!("{label}{offset}"));
                }
            }
            Some(OperandOverride::Text(text)) => out.push_str(text),
            None => {
                if let Some(default) = operands.get(idx) {
                    out.push_str(default);
                }
            }
        }
    }
    out
}

fn ranges_overlap(
    a_start: AddressValue,
    a_end: AddressValue,
    b_start: AddressValue,
    b_end: AddressValue,
) -> bool {
    a_start < b_end && b_start < a_end
}

fn range_end(start: AddressValue, range: &ByteRange) -> AddressValue {
    match range {
        ByteRange::Mapped(_, _, data) => start.saturating_add(data.len() as AddressValue),
        ByteRange::Constant(size, _) => start.saturating_add(*size),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::{DataType, OperandOverride, SpaceUsage};

    #[test]
    fn overlapping_equivalents_are_rejected() {
        let mut region = Region::new();
        region.set_bytes(
            "test.bin",
            0,
            0,
            &[0x02, 0x00, 0x10, 0x74, 0x01, 0x00, 0x00, 0x00],
        );
        region.set_equivalent(0, Equivalent::Code(vec![])).unwrap();
        assert!(matches!(
            region.set_equivalent(1, Equivalent::Code(vec![])),
            Err(Error::NotUndefined(1))
        ));
        region.set_equivalent(6, Equivalent::Code(vec![])).unwrap();
        assert!(matches!(
            region.set_equivalent(4, Equivalent::Data(DataType::Byte, 3)),
            Err(Error::Overlap(6))
        ));
    }

    #[test]
    fn clear_bytes_splits_straddling_range() {
        let mut region = Region::new();
        region.set_bytes("test.bin", 0, 0, &[1, 2, 3, 4, 5]);
        region.clear_bytes(1, 2);
        assert_eq!(region.bytes_at(0, 5), vec![1, 4, 5]);
    }

    #[test]
    fn decode_at_does_not_require_bytes_at_zero() {
        let mut region = Region::new();
        region.set_bytes("test.bin", 0, 0x100, &[0x74, 0x42]);
        let insn = region.decode_at(0x100).unwrap();
        assert_eq!(insn.len(), 2);
        assert_eq!(insn.as_string(), "MOV A,#0x42");
    }

    #[test]
    fn decode_at_requires_full_instruction_length() {
        let mut region = Region::new();
        region.set_bytes("test.bin", 0, 0, &[0x02, 0x00]);
        assert!(
            region.decode_at(0).is_none(),
            "Expected None, got {:?}",
            region.decode_at(0).unwrap().as_string()
        );
    }

    #[test]
    fn operand_override_preserves_other_operands() {
        let mut region = Region::new();
        region.set_bytes("test.bin", 0, 0, &[0xB5, 0x20, 0x10]);
        region.set_label(0x13, "target");
        region
            .set_equivalent(
                0,
                Equivalent::Code(vec![
                    None,
                    None,
                    Some(OperandOverride::Label("target".into())),
                ]),
            )
            .unwrap();
        let implicit_labels = ImplicitLabels::default();
        let lines = region.render(AddressSpace::Code, &implicit_labels);
        let insn = lines
            .iter()
            .find_map(|line| match line {
                Line::Instruction { text, .. } => Some(text.clone()),
                _ => None,
            })
            .unwrap();
        assert!(insn.contains("RAM(32),target"));
    }

    #[test]
    fn render_emits_org_after_unmapped_gap() {
        let mut region = Region::new();
        region.set_bytes("test.bin", 0, 0, &[1, 2, 3]);
        region.set_bytes("test.bin", 3, 0x10, &[4, 5]);
        let implicit_labels = ImplicitLabels::default();
        let lines = region.render(AddressSpace::Code, &implicit_labels);
        let orgs: Vec<_> = lines
            .iter()
            .filter_map(|line| match line {
                Line::Org { addr } => Some(*addr),
                _ => None,
            })
            .collect();
        assert_eq!(orgs, vec![0, 0x10]);
    }

    #[test]
    fn space_usage_counts_code_data_and_undefined() {
        let mut region = Region::new();
        region.set_bytes(
            "test.bin",
            0,
            0,
            &[0x02, 0x00, 0x10, 0x74, 0x01, 0xFF, 0xFF],
        );
        region.set_equivalent(0, Equivalent::Code(vec![])).unwrap();
        region.set_equivalent(3, Equivalent::Code(vec![])).unwrap();
        region
            .set_equivalent(5, Equivalent::Data(DataType::Word, 2))
            .unwrap();

        assert_eq!(
            region.space_usage(),
            SpaceUsage {
                code: 5,
                data: 2,
                undefined: 0,
            }
        );

        region.clear_equivalents(5, 2);
        assert_eq!(
            region.space_usage(),
            SpaceUsage {
                code: 5,
                data: 0,
                undefined: 2,
            }
        );
    }
}
