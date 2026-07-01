use i8051::Instruction;
use std::collections::BTreeMap;
use std::ops::RangeBounds;
use std::range::Range;

mod weak;

pub use weak::{AutoDisassembleError, AutoDisassembleResult};
use weak::{CodeSource, Weak};

use crate::address::{
    AddressRange, AddressSpace, AddressValue, PhysicalAddr, Xref, branch_target,
    branch_target_operand_index, xrefs_from_instruction, xrefs_to_target,
};
use crate::commands::{
    AutoDisassemble, Command, DisassembleRange, MapBytes, MarkData, MarkUnknown, OverrideOperand,
    SetComment, SetConstantBytes, SetFunction, SetLabel, boxed,
};
use crate::db::{
    Equivalent, EquivalentAt, EquivalentKind, EquivalentRange, Error, Function, OperandOverride,
    SpaceUsage,
};
use crate::labels::{ImplicitLabels, LabelCollector, Labels};
use crate::pattern::BytePattern;
use crate::render::Line;

#[derive(Debug, Clone)]
pub enum ByteRange {
    Mapped(String, usize, Vec<u8>),
    Constant(AddressValue, u8),
}

impl ByteRange {
    pub fn len(&self) -> AddressValue {
        match self {
            ByteRange::Mapped(_, _, data) => data.len() as AddressValue,
            ByteRange::Constant(size, _) => *size,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

pub struct Region {
    byte_ranges: BTreeMap<AddressValue, ByteRange>,
    equivalents: BTreeMap<AddressValue, EquivalentRange>,
    labels: BTreeMap<AddressValue, String>,
    comments: BTreeMap<AddressValue, String>,
    functions: BTreeMap<AddressValue, Function>,
    /// Operand overrides, keyed by `(instruction address, operand index)`.
    overrides: BTreeMap<(AddressValue, u8), OperandOverride>,
    /// The derived auto-disassembly layer (roots + their code). Reads overlay
    /// `equivalents` over `weak.code()`. Never serialized.
    weak: Weak,
}

impl Default for Region {
    fn default() -> Self {
        Self::new()
    }
}

impl Region {
    pub fn new() -> Self {
        Self {
            byte_ranges: BTreeMap::new(),
            equivalents: BTreeMap::new(),
            labels: BTreeMap::new(),
            comments: BTreeMap::new(),
            functions: BTreeMap::new(),
            overrides: BTreeMap::new(),
            weak: Weak::default(),
        }
    }

    /// Set or clear the override at `(addr, index)`, returning the prior value.
    pub fn set_operand_override(
        &mut self,
        addr: AddressValue,
        index: u8,
        value: Option<OperandOverride>,
    ) -> Option<OperandOverride> {
        match value {
            Some(value) => self.overrides.insert((addr, index), value),
            None => self.overrides.remove(&(addr, index)),
        }
    }

    /// Operand overrides for the instruction at `addr`, as a positional vec.
    fn operand_overrides_at(&self, addr: AddressValue) -> Vec<Option<OperandOverride>> {
        let mut out = Vec::new();
        for (&(_, index), value) in self.overrides.range((addr, 0)..=(addr, u8::MAX)) {
            let index = index as usize;
            if out.len() <= index {
                out.resize(index + 1, None);
            }
            out[index] = Some(value.clone());
        }
        out
    }

    /// Lift the weak layer out to derive against `self` (which supplies the
    /// bytes and barriers via [`CodeSource`]), then put it back. The take avoids
    /// borrowing `self` mutably and immutably at once.
    fn with_weak<R>(&mut self, f: impl FnOnce(&mut Weak, &Self) -> R) -> R {
        let mut weak = std::mem::take(&mut self.weak);
        let result = f(&mut weak, self);
        self.weak = weak;
        result
    }

    fn refresh_weak(&mut self) {
        self.with_weak(|weak, src| weak.refresh(src));
    }

    /// Auto-disassemble from `start`, recording it as a root (so the export is
    /// one `auto_disassemble` command). Returns the traversal diagnostics.
    pub fn auto_disassemble(&mut self, start: AddressValue) -> AutoDisassembleResult {
        self.with_weak(|weak, src| weak.extend(src, start))
    }

    pub fn add_auto_root(&mut self, start: AddressValue) {
        let _ = self.auto_disassemble(start);
    }

    /// Removing a root re-derives weak, so code reachable only through it drops.
    pub fn remove_auto_root(&mut self, start: AddressValue) -> bool {
        let removed = self.weak.remove_root(start);
        if removed {
            self.refresh_weak();
        }
        removed
    }

    pub fn is_auto_root(&self, start: AddressValue) -> bool {
        self.weak.contains_root(start)
    }

    fn is_code_start(&self, offset: AddressValue) -> bool {
        matches!(
            self.equivalents.get(&offset).map(|r| r.equivalent.kind()),
            Some(EquivalentKind::Code)
        ) || self.weak.code().contains_key(&offset)
    }

    /// Every code instruction start across both layers, deduplicated.
    fn code_offsets(&self) -> Vec<AddressValue> {
        let mut offsets: Vec<AddressValue> = self
            .equivalents
            .iter()
            .filter(|(_, range)| matches!(range.equivalent, Equivalent::Code))
            .map(|(&offset, _)| offset)
            .collect();
        for &offset in self.weak.code().keys() {
            if !self.equivalents.contains_key(&offset) {
                offsets.push(offset);
            }
        }
        offsets
    }

    /// Nearest strong or weak start at/after `after`. Bounds undefined runs.
    fn next_equivalent_boundary(&self, after: AddressValue) -> Option<AddressValue> {
        let next_strong = self.equivalents.range(after..).next().map(|(&k, _)| k);
        let next_weak = self.weak.code().range(after..).next().map(|(&k, _)| k);
        [next_strong, next_weak].into_iter().flatten().min()
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
        self.refresh_weak();
    }

    pub fn set_constant(&mut self, offset: AddressValue, size: AddressValue, value: u8) {
        if size == 0 {
            return;
        }
        self.clear_bytes(offset, size);
        self.byte_ranges
            .insert(offset, ByteRange::Constant(size, value));
        self.refresh_weak();
    }

    pub fn find_bytes(&self, pattern: &BytePattern) -> impl Iterator<Item = Range<AddressValue>> {
        self.find_bytes_in(pattern, ..)
    }

    /// Find bytes in a specific range (`..` searches the whole region).
    ///
    /// Cross-byte-range matches and constant-byte-range matches do not
    /// currently work.
    pub fn find_bytes_in(
        &self,
        pattern: &BytePattern,
        range: impl RangeBounds<AddressValue>,
    ) -> impl Iterator<Item = Range<AddressValue>> {
        let range = AddressRange::from(range);
        self.byte_ranges
            .iter()
            .filter_map(move |(addr, byte_range)| match byte_range {
                ByteRange::Mapped(_, _, data) => {
                    if ranges_overlap_inclusive(
                        range.start,
                        range.end,
                        *addr,
                        *addr + data.len() as AddressValue,
                    ) {
                        let data_start = range.start.saturating_sub(*addr) as usize;
                        let data_end = range
                            .end
                            .saturating_sub(*addr)
                            .min((data.len() - 1) as AddressValue)
                            as usize;
                        Some(
                            pattern
                                .find_all(&data[data_start..=data_end])
                                .map(move |range| {
                                    Range::from(
                                        range
                                            .start
                                            .saturating_add(data_start)
                                            .saturating_add(*addr as usize)
                                            ..range
                                                .end
                                                .saturating_add(data_start)
                                                .saturating_add(*addr as usize),
                                    )
                                }),
                        )
                    } else {
                        None
                    }
                }
                ByteRange::Constant(..) => None,
            })
            .flatten()
            .map(|range| Range::from(range.start as AddressValue..range.end as AddressValue))
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
            .filter(|(start, range)| range.len() > offset && **start < end)
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
        self.refresh_weak();
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

        // Only a barrier landing on already-derived code forces a re-derive (a
        // chop). A code island, or a barrier over undefined bytes, leaves weak as-is.
        let chops_weak =
            equivalent.kind() != EquivalentKind::Code && self.weak.overlaps(offset, span);
        self.equivalents.insert(
            offset,
            EquivalentRange {
                end: offset.saturating_add(span),
                equivalent,
            },
        );
        if chops_weak {
            self.refresh_weak();
        }
        Ok(&self.equivalents[&offset])
    }

    pub fn clear_equivalents(&mut self, offset: AddressValue, size: AddressValue) {
        if size == 0 {
            return;
        }
        let end = offset.saturating_add(size);
        self.equivalents
            .retain(|&start, range| range.end <= offset || start >= end);
        self.refresh_weak();
    }

    /// Linear-sweep `[start, end)` as strong code. Errors (mutating nothing) on
    /// a byte that doesn't decode or a strong overlap. The final instruction may
    /// run past `end`. Returns the actual end.
    pub(crate) fn disassemble_linear(
        &mut self,
        start: AddressValue,
        end: AddressValue,
    ) -> Result<AddressValue, Error> {
        // Validate up front so the inserts below can't fail, then re-derive weak once.
        let mut addr = start;
        let mut spans = Vec::new();
        while addr < end {
            let insn = self.decode_at(addr).ok_or(Error::InvalidEquivalent)?;
            let insn_end = addr.saturating_add(insn.len() as AddressValue);
            if !self.snapshot_equivalents(addr, insn_end - addr).is_empty() {
                return Err(Error::Overlap(addr));
            }
            spans.push((addr, insn_end));
            addr = insn_end;
        }
        for (offset, insn_end) in spans {
            self.equivalents.insert(
                offset,
                EquivalentRange {
                    end: insn_end,
                    equivalent: Equivalent::Code,
                },
            );
        }
        self.refresh_weak();
        Ok(addr)
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

    /// Whether a *strong* equivalent covers `offset`. Strong-only: a barrier may
    /// overwrite weak code, which then re-derives.
    pub fn has_equivalent(&self, offset: AddressValue) -> bool {
        lookup_in(&self.equivalents, offset).is_some()
    }

    pub fn get_equivalent_kind(&self, offset: AddressValue) -> Option<EquivalentKind> {
        match self.equivalent_at(offset) {
            EquivalentAt::Defined { range, .. } => Some(range.equivalent.kind()),
            EquivalentAt::Undefined(_) => None,
        }
    }

    pub fn get_equivalent(&self, offset: AddressValue) -> EquivalentAt<'_> {
        self.equivalent_at(offset)
    }

    fn equivalent_at(&self, offset: AddressValue) -> EquivalentAt<'_> {
        // Strong (data / unknown / code islands) overlays weak (derived code).
        match lookup_in(&self.equivalents, offset).or_else(|| lookup_in(self.weak.code(), offset)) {
            Some((start, range)) => EquivalentAt::Defined { start, range },
            None => EquivalentAt::Undefined(self.undefined_range_at(offset)),
        }
    }

    fn undefined_range_at(&self, offset: AddressValue) -> Range<AddressValue> {
        let after = offset.saturating_add(1);
        let next_eq = self.next_equivalent_boundary(after);
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

    /// Remove every label in `range`, returning the removed `(address, label)`
    /// pairs (for undo).
    pub fn clear_labels_in(
        &mut self,
        range: impl RangeBounds<AddressValue>,
    ) -> Vec<(AddressValue, String)> {
        let keys: Vec<AddressValue> = self.labels.range(range).map(|(&k, _)| k).collect();
        keys.into_iter()
            .filter_map(|k| self.labels.remove(&k).map(|v| (k, v)))
            .collect()
    }

    /// Remove every comment in `range`, returning the removed pairs.
    pub fn clear_comments_in(
        &mut self,
        range: impl RangeBounds<AddressValue>,
    ) -> Vec<(AddressValue, String)> {
        let keys: Vec<AddressValue> = self.comments.range(range).map(|(&k, _)| k).collect();
        keys.into_iter()
            .filter_map(|k| self.comments.remove(&k).map(|v| (k, v)))
            .collect()
    }

    /// Remove every function in `range`, returning the removed `(address,
    /// function)` pairs.
    pub fn clear_functions_in(
        &mut self,
        range: impl RangeBounds<AddressValue>,
    ) -> Vec<(AddressValue, Function)> {
        let keys: Vec<AddressValue> = self.functions.range(range).map(|(&k, _)| k).collect();
        keys.into_iter()
            .filter_map(|k| self.functions.remove(&k).map(|v| (k, v)))
            .collect()
    }

    pub fn byte_at(&self, offset: AddressValue) -> Option<u8> {
        self.read_byte(offset)
    }

    pub fn read_u16_le(&self, offset: AddressValue) -> Option<u16> {
        let low = self.read_byte(offset)?;
        let high = self.read_byte(offset.saturating_add(1))?;
        Some((low as u16) | ((high as u16) << 8))
    }

    pub fn read_u16_be(&self, offset: AddressValue) -> Option<u16> {
        let high = self.read_byte(offset)?;
        let low = self.read_byte(offset.saturating_add(1))?;
        Some((high as u16) | ((low as u16) << 8))
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
                Equivalent::Code => usage.code += span,
                Equivalent::Data(_, _) => usage.data += span,
                Equivalent::Unknown(_) => {} // counts as undefined, via subtraction below
            }
        }
        // Weak code, minus strong islands already counted.
        for (&start, range) in self.weak.code() {
            if !self.equivalents.contains_key(&start) {
                usage.code += range.end.saturating_sub(start);
            }
        }

        let mapped: AddressValue = self
            .byte_ranges
            .iter()
            .map(|(&start, range)| range.len().saturating_sub(start))
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
                    Equivalent::Code => {
                        let insn = self
                            .decode_at(addr)
                            .expect("validated code equivalent must decode");
                        let overrides = self.operand_overrides_at(addr);
                        let text = self.format_instruction(addr, &insn, &overrides, labels);
                        lines.push(Line::Instruction {
                            addr,
                            direct: insn.direct_addr(),
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
                    Equivalent::Unknown(size) => {
                        let bytes = self.bytes_at(addr, *size);
                        lines.push(Line::Raw { addr, bytes });
                        addr = range.end;
                    }
                },
                EquivalentAt::Undefined(undefined) => {
                    let span = self.raw_run_until_next_annotation(addr, undefined.end, labels);
                    if span == 0 {
                        if let Some((&next_mapped, _)) =
                            self.byte_ranges.range(addr.saturating_add(1)..).next()
                            && next_mapped < undefined.end
                        {
                            addr = next_mapped;
                            need_org = true;
                            continue;
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

    pub(crate) fn to_commands(&self, space: AddressSpace) -> Vec<Box<dyn Command>> {
        let mut commands = Vec::new();
        for (&offset, range) in &self.byte_ranges {
            match range {
                ByteRange::Mapped(file, file_offset, data) => {
                    commands.push(boxed(MapBytes::new(
                        (space, offset),
                        file.clone(),
                        *file_offset,
                        data.len() as AddressValue,
                    )));
                }
                ByteRange::Constant(size, value) => {
                    commands.push(boxed(SetConstantBytes::new(
                        (space, offset..offset + *size),
                        *value,
                    )));
                }
            }
        }

        // Barriers must precede the roots so they're in place when reload re-derives.
        for (&offset, equivalent_range) in &self.equivalents {
            match &equivalent_range.equivalent {
                Equivalent::Data(data_type, _) => {
                    commands.push(boxed(MarkData::new(
                        (space, offset..equivalent_range.end),
                        data_type.clone(),
                    )));
                }
                Equivalent::Unknown(_) => {
                    commands.push(boxed(MarkUnknown::new((space, offset..equivalent_range.end))));
                }
                Equivalent::Code => {}
            }
        }

        for root in self.weak.roots() {
            commands.push(boxed(AutoDisassemble::new((space, root))));
        }

        // Manual code islands (strong code no root covers), coalesced into one
        // `disassemble_range` per contiguous run.
        let mut run: Option<(AddressValue, AddressValue)> = None;
        for (&offset, equivalent_range) in &self.equivalents {
            let island = matches!(equivalent_range.equivalent, Equivalent::Code)
                && !self.weak.code().contains_key(&offset);
            match (run, island) {
                (Some((start, end)), true) if end == offset => {
                    run = Some((start, equivalent_range.end));
                }
                (Some((start, end)), _) => {
                    commands.push(boxed(DisassembleRange::new((space, start..end))));
                    run = island.then_some((offset, equivalent_range.end));
                }
                (None, true) => run = Some((offset, equivalent_range.end)),
                (None, false) => {}
            }
        }
        if let Some((start, end)) = run {
            commands.push(boxed(DisassembleRange::new((space, start..end))));
        }

        for (&(offset, index), value) in &self.overrides {
            commands.push(boxed(OverrideOperand::new(
                (space, offset),
                index,
                Some(value.clone()),
            )));
        }

        for (&offset, label) in &self.labels {
            commands.push(boxed(SetLabel::new((space, offset), label.clone())));
        }
        for (&offset, comment) in &self.comments {
            commands.push(boxed(SetComment::new((space, offset), comment.clone())));
        }
        for (&offset, function) in &self.functions {
            commands.push(boxed(SetFunction::new((space, offset), function.clone())));
        }
        commands
    }

    pub(crate) fn xrefs_to(&self, space: AddressSpace, target: &PhysicalAddr) -> Vec<Xref> {
        let mut xrefs = Vec::new();
        for offset in self.code_offsets() {
            let Some(instruction) = self.decode_at(offset) else {
                continue;
            };
            let source = PhysicalAddr { space, offset };
            xrefs.extend(xrefs_to_target(&instruction, source, target));
        }
        xrefs
    }

    pub(crate) fn xrefs_from(&self, source: &PhysicalAddr) -> Vec<Xref> {
        let offset = source.offset;
        if !self.is_code_start(offset) {
            return Vec::new();
        }
        let Some(instruction) = self.decode_at(offset) else {
            return Vec::new();
        };
        xrefs_from_instruction(&instruction, *source)
    }

    /// Collect all the necessary references for this region.
    pub(crate) fn collect_refs(&self, space: AddressSpace, refs: &mut LabelCollector) {
        for offset in self.code_offsets() {
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

    /// Upper bound (exclusive) of mapped bytes and equivalents.
    fn end(&self) -> AddressValue {
        [
            self.byte_ranges
                .iter()
                .next_back()
                .map(|(&start, range)| start + range.len()),
            self.equivalents
                .iter()
                .next_back()
                .map(|(&start, range)| start + range.end),
            self.labels.keys().next_back().copied(),
            self.comments.keys().next_back().copied(),
            self.functions.keys().next_back().copied(),
        ]
        .into_iter()
        .flatten()
        .max()
        .map(|addr| addr + 1)
        .unwrap_or(0)
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
            Equivalent::Code => self
                .decode_at(offset)
                .map(|insn| insn.len() as AddressValue)
                .ok_or(Error::InvalidEquivalent),
            Equivalent::Data(_, size) => Ok(*size),
            Equivalent::Unknown(size) => Ok(*size),
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
        if let Some((&other_start, other)) = self.equivalents.range(..end).next_back()
            && other.end > offset
        {
            return Err(Error::Overlap(other_start));
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
        if let Some(start) = self.next_equivalent_boundary(after) {
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
        implicit_labels: &Labels,
    ) -> String {
        let decoded = insn.as_string();
        let mut merged = overrides.to_vec();

        if let (Some(target), Some(idx)) = (branch_target(insn), branch_target_operand_index(insn))
        {
            while merged.len() <= idx {
                merged.push(None);
            }
            if merged[idx].is_none() {
                let label = self
                    .get_label(target)
                    .or_else(|| implicit_labels.get(&target).map(String::as_str));
                if let Some(label) = label {
                    merged[idx] = Some(OperandOverride::Label(label.to_string()));
                }
            }
        }

        let text = if merged.iter().all(|o| o.is_none()) {
            decoded.to_string()
        } else {
            apply_operand_overrides(&decoded, &merged)
        };
        sdas_indent_instruction(&text)
    }

}

impl CodeSource for Region {
    fn decode(&self, addr: AddressValue) -> Option<Instruction> {
        self.decode_at(addr)
    }

    fn barrier_in(&self, addr: AddressValue, len: AddressValue) -> Option<EquivalentKind> {
        let end = addr.saturating_add(len);
        // Strong equivalents don't overlap, so scanning back from `end` until one
        // ends at/before `addr` covers every entry touching the range.
        for (_, range) in self.equivalents.range(..end).rev() {
            if range.end <= addr {
                break;
            }
            let kind = range.equivalent.kind();
            if kind != EquivalentKind::Code {
                return Some(kind);
            }
        }
        None
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

/// Find the entry covering `offset` in one equivalent layer: an exact start, or
/// a preceding range that still contains it.
fn lookup_in(
    map: &BTreeMap<AddressValue, EquivalentRange>,
    offset: AddressValue,
) -> Option<(AddressValue, &EquivalentRange)> {
    if let Some(range) = map.get(&offset) {
        return Some((offset, range));
    }
    match map.range(..=offset).next_back() {
        Some((&start, range)) if offset < range.end => Some((start, range)),
        _ => None,
    }
}

/// A and B are inclusive start, exclusive end.
fn ranges_overlap(
    a_start: AddressValue,
    a_end: AddressValue,
    b_start: AddressValue,
    b_end: AddressValue,
) -> bool {
    a_start < b_end && b_start < a_end
}

/// A and B are inclusive start, inclusive end.
fn ranges_overlap_inclusive(
    a_start: AddressValue,
    a_end: AddressValue,
    b_start: AddressValue,
    b_end: AddressValue,
) -> bool {
    a_start <= b_end && b_start <= a_end
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::address::AddressSpace;
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
        region.set_equivalent(0, Equivalent::Code).unwrap();
        assert!(matches!(
            region.set_equivalent(1, Equivalent::Code),
            Err(Error::NotUndefined(1))
        ));
        region.set_equivalent(6, Equivalent::Code).unwrap();
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
    fn branch_target_uses_implicit_label() {
        let mut region = Region::new();
        region.set_bytes("test.bin", 0, 0, &[0x12, 0x01, 0x6D, 0x02, 0x03, 0x04]);
        region.set_equivalent(0, Equivalent::Code).unwrap();
        region.set_equivalent(3, Equivalent::Code).unwrap();

        let mut collector = LabelCollector::default();
        region.collect_refs(AddressSpace::Code, &mut collector);
        let implicit_labels = collector.into_implicit_labels();

        let lines = region.render(AddressSpace::Code, &implicit_labels);
        let insn = lines
            .iter()
            .find_map(|line| match line {
                Line::Instruction { addr: 0, text, .. } => Some(text.clone()),
                _ => None,
            })
            .unwrap();
        assert!(insn.contains("LCALL"), "{insn}");
        assert!(insn.contains("loc_016D"), "{insn}");
        assert!(!insn.contains("#0x016D"), "{insn}");
    }

    #[test]
    fn operand_override_preserves_other_operands() {
        let mut region = Region::new();
        region.set_bytes("test.bin", 0, 0, &[0xB5, 0x20, 0x10]);
        region.set_label(0x13, "target");
        region.set_equivalent(0, Equivalent::Code).unwrap();
        region.set_operand_override(0, 2, Some(OperandOverride::Label("target".into())));
        let implicit_labels = ImplicitLabels::default();
        let lines = region.render(AddressSpace::Code, &implicit_labels);
        let insn = lines
            .iter()
            .find_map(|line| match line {
                Line::Instruction { text, .. } => Some(text.clone()),
                _ => None,
            })
            .unwrap();
        assert!(insn.contains("0x20,target"));
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
        region.set_equivalent(0, Equivalent::Code).unwrap();
        region.set_equivalent(3, Equivalent::Code).unwrap();
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
