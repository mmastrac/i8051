use i8051_disassembler::address::{
    AddressRange, AddressSpace, AddressValue, PhysicalAddr, SpaceAddressValue, Xref, XrefType,
};
use i8051_disassembler::analysis::completeness::{self, Gate, Item, Phase};
use i8051_disassembler::db::{Db, Note, ScratchDecode};
use i8051_disassembler::render::Line;
use i8051_disassembler::store::{from_dsl, from_dsl_value, to_dsl};

pub use i8051_disassembler::commands::Environment;

mod verbs;
pub use verbs::{ArgType, Category, VerbArg, VerbInfo};

mod bridge;
pub(crate) use bridge::{build_command_dsl, command_focus};

mod controller;
pub use controller::{Controller, EditResult, Location};

mod db;
pub use db::{DbFileError, SaveReport};

#[cfg(feature = "autosave")]
pub mod autosave;

mod complete;
pub use complete::{Candidate, Completion, ValueSource, complete};

mod human;
pub use human::{caret_diagnostic, render_human};

mod dto;
pub use dto::*;

/// Worklist page size when omitted.
pub const DEFAULT_WORKLIST_LIMIT: usize = 20;

/// Instruction count when omitted.
pub const DEFAULT_PEEK_LINES: usize = 24;

mod file;
pub use file::{FsEnvironment, MemoryEnvironment};

#[derive(Debug)]
/// A failed service call, by cause.
pub enum ServiceError {
    Parse(String),
    Apply(String),
}

impl std::fmt::Display for ServiceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Parse(m) => write!(f, "{m}"),
            Self::Apply(m) => write!(f, "{m}"),
        }
    }
}

impl std::error::Error for ServiceError {}

/// A database plus its file-access environment.
pub struct Session {
    db: Db,
    env: Box<dyn Environment + Send + Sync>,
    source: Option<db::Source>,
}

impl Session {
    /// Wrap an existing database.
    pub fn new(db: Db, env: Box<dyn Environment + Send + Sync>) -> Self {
        Self { db, env, source: None }
    }

    /// Build by applying DSL commands in order.
    pub fn from_commands(
        commands: impl IntoIterator<Item = impl AsRef<str>>,
        env: Box<dyn Environment + Send + Sync>,
    ) -> Result<Self, ServiceError> {
        let mut db = Db::new();
        for (i, dsl) in commands.into_iter().enumerate() {
            let command =
                from_dsl(dsl.as_ref()).map_err(|e| ServiceError::Parse(format!("record {i}: {e}")))?;
            db.apply(command, Some(env.as_ref()))
                .map_err(|e| ServiceError::Apply(format!("record {i}: {e}")))?;
        }
        Ok(Self { db, env, source: None })
    }

    /// Apply one command, returning its undo DSL.
    pub fn apply(&mut self, dsl: &str) -> Result<Vec<String>, ServiceError> {
        let command = from_dsl(dsl).map_err(|e| ServiceError::Parse(e.to_string()))?;
        let undo = self
            .db
            .apply(command, Some(self.env.as_ref()))
            .map_err(|e| ServiceError::Apply(e.to_string()))?;
        Ok(undo.iter().map(|c| to_dsl(c.as_ref())).collect())
    }

    /// The whole listing as assembler text.
    pub fn disassembly(&self) -> String {
        self.db.to_sdas()
    }

    fn listing_lines(&self, space: AddressSpace) -> Vec<ListingRow> {
        use i8051_disassembler::render::data::{DataChunk, DataHeuristics};
        let heur = DataHeuristics::default();
        let mut out = Vec::new();
        for line in self.db.render(space) {
            match line {
                Line::Raw { addr, bytes } if bytes.len() > heur.block_size => {
                    out.push(ListingRow::Fold(FoldRow::Region {
                        addr,
                        kind: RegionKind::Unknown,
                    }));
                    let mut at = addr;
                    for chunk in heur.iterate(addr, None, &bytes) {
                        match chunk {
                            DataChunk::Literal(span) => {
                                for row in heur.literal_rows(span) {
                                    out.push(ListingRow::Line(Line::Raw {
                                        addr: at,
                                        bytes: row.to_vec(),
                                    }));
                                    at += row.len() as AddressValue;
                                }
                            }
                            DataChunk::Run(value, len) => {
                                out.push(ListingRow::Fold(FoldRow::Run {
                                    addr: at,
                                    value,
                                    len: len as AddressValue,
                                }));
                                at += len as AddressValue;
                            }
                            DataChunk::BlockRun(unit, count) => {
                                out.push(ListingRow::Fold(FoldRow::Block {
                                    addr: at,
                                    unit: unit.to_vec(),
                                    count,
                                }));
                                at += (unit.len() * count) as AddressValue;
                            }
                        }
                    }
                }
                Line::Data { addr, data_type, bytes } if bytes.len() > heur.block_size => {
                    let mut at = addr;
                    for row in heur.literal_rows(&bytes) {
                        out.push(ListingRow::Line(Line::Data {
                            addr: at,
                            data_type: data_type.clone(),
                            bytes: row.to_vec(),
                        }));
                        at += row.len() as AddressValue;
                    }
                }
                other => out.push(ListingRow::Line(other)),
            }
        }
        out
    }

    /// A window of rendered listing rows.
    pub fn listing(
        &self,
        space: &str,
        start: usize,
        count: usize,
    ) -> Result<Listing, ServiceError> {
        let space = self.parse_space(space)?;
        let all = self.listing_lines(space);
        let total = all.len();
        let end = start.saturating_add(count).min(total);
        let lines = all
            .into_iter()
            .enumerate()
            .skip(start)
            .take(end.saturating_sub(start))
            .map(|(index, line)| LineInfo {
                index,
                addr: addr_dsl(space, line.addr()),
                offset: line.addr(),
                line,
            })
            .collect();
        Ok(Listing {
            space: space.dsl_name().to_string(),
            total,
            start,
            lines,
        })
    }

    /// Decode without committing, to judge bytes.
    pub fn peek(&self, address: &str, lines: Option<usize>) -> Result<PeekInfo, ServiceError> {
        let addr = parse_addr(address)?;
        let count = lines.unwrap_or(DEFAULT_PEEK_LINES).max(1);
        let decode = self.db.peek(addr.space, addr.offset, count);
        let text = decode
            .lines
            .iter()
            .map(|insn| {
                let bytes = insn
                    .bytes
                    .iter()
                    .map(|b| format!("{b:02x}"))
                    .collect::<Vec<_>>()
                    .join(" ");
                let mut line =
                    format!("{}  {bytes:<8}  {}", addr_dsl(addr.space, insn.addr), insn.text);
                if let Some(target) = insn.target {
                    if !insn.target_mapped {
                        line.push_str(&format!(
                            "   ; target {} is outside the loaded image",
                            addr_dsl(addr.space, target)
                        ));
                    } else if insn.target_misaligned {
                        line.push_str(&format!(
                            "   ; target {} lands inside an existing instruction",
                            addr_dsl(addr.space, target)
                        ));
                    }
                }
                line
            })
            .collect::<Vec<_>>()
            .join("\n");
        let (verdict, note) = peek_verdict(&decode);
        let rows = decode
            .lines
            .iter()
            .map(|insn| PeekLine {
                addr: addr_dsl(addr.space, insn.addr),
                bytes: insn.bytes.clone(),
                text: insn.text.trim_start().to_string(),
                target: insn.target.map(|t| addr_dsl(addr.space, t)),
                target_mapped: insn.target_mapped,
                target_misaligned: insn.target_misaligned,
            })
            .collect();
        let commit_with = (verdict == "likely_code")
            .then(|| format!("auto_disassemble(address={})", addr_dsl(addr.space, addr.offset)));
        Ok(PeekInfo {
            committed: false,
            commit_with,
            address: addr_dsl(addr.space, addr.offset),
            verdict,
            note,
            terminates: decode.terminates,
            out_of_range_targets: decode.out_of_range_targets,
            lines: decode.lines.len(),
            text,
            rows,
        })
    }

    fn touch_name(&self, space: AddressSpace, offset: AddressValue) -> String {
        let Some(platform) = self.db.platform() else {
            return addr_dsl(space, offset);
        };
        let canon = platform.canonicalize(space, offset);
        let byte = if canon.space == i8051_disassembler::platform::i8051::SFR {
            i8051_disassembler::platform::i8051::format_direct(canon.offset as u8)
        } else {
            addr_dsl(canon.space, canon.offset)
        };
        match canon.bit {
            Some(bit) => format!("{byte}.{bit}"),
            None => byte,
        }
    }

    fn routine_context(
        &self,
        space: AddressSpace,
        offset: AddressValue,
        lines: &[Line],
    ) -> Option<RoutineContext> {
        let mut entry = None;
        let mut name = None;
        for line in lines {
            match line {
                Line::Label { addr, name: n } | Line::Function { addr, name: n, .. }
                    if *addr <= offset =>
                {
                    entry = Some(*addr);
                    name = Some(n.clone());
                }
                _ => {}
            }
        }
        let (entry, name) = (entry?, name?);

        let mut instructions = 0usize;
        let mut body: Vec<AddressValue> = Vec::new();
        let mut calls: Vec<AddressValue> = Vec::new();
        let mut end = entry;
        for line in lines {
            match line {
                Line::Label { addr, .. } | Line::Function { addr, .. } if *addr > entry => break,
                Line::Instruction { addr, target, .. } if *addr >= entry => {
                    instructions += 1;
                    end = *addr;
                    body.push(*addr);
                    if let Some(t) = target {
                        calls.push(*t);
                    }
                }
                _ => {}
            }
        }
        if instructions == 0 {
            return None;
        }

        let named = |offset: AddressValue| {
            self.db
                .region(space)
                .and_then(|r| r.get_label(offset).map(str::to_string))
                .unwrap_or_else(|| addr_dsl(space, offset))
        };

        let mut callers: Vec<String> = self
            .db
            .xrefs_to(&PhysicalAddr { space, offset: entry })
            .into_iter()
            .map(|x| named(x.from.offset))
            .collect();
        callers.sort();
        callers.dedup();

        let mut calls: Vec<String> = calls.into_iter().map(named).collect();
        calls.sort();
        calls.dedup();
        calls.retain(|c| Some(c.as_str()) != Some(name.as_str()));

        let mut touches: Vec<String> = body
            .iter()
            .flat_map(|&at| self.db.xrefs_from(&PhysicalAddr { space, offset: at }))
            .filter(|x| x.to.space != space)
            .map(|x| self.touch_name(x.to.space, x.to.offset))
            .collect();
        touches.sort();
        touches.dedup();

        let notes: Vec<String> = self
            .db
            .get_notes_overlapping(space, entry..end.saturating_add(1))
            .into_iter()
            .map(|n| n.content.clone())
            .collect();

        Some(RoutineContext {
            entry: addr_dsl(space, entry),
            name,
            instructions,
            callers,
            calls,
            touches,
            notes,
        })
    }

    /// Per-space totals for the whole database.
    pub fn disassembly_overview(&self) -> Result<DisassemblyOverview, ServiceError> {
        let symbols = self.symbols(None)?.len();
        let spaces = self
            .memory_map()
            .into_iter()
            .map(|usage| {
                let space = self.parse_space(&usage.space)?;
                let lines = self.db.render(space).len();
                Ok(SpaceOverview {
                    space: usage.space,
                    lines,
                    code: usage.code,
                    data: usage.data,
                    undefined: usage.undefined,
                    total: usage.total,
                })
            })
            .collect::<Result<Vec<_>, ServiceError>>()?;
        Ok(DisassemblyOverview {
            hint: "Use `listing` for a sdas window and `symbols` for the program map. \
                   Call `disassembly` with `full: true` only on small images.",
            spaces,
            symbols,
        })
    }

    /// Row index of an address, if listed.
    pub fn locate(&self, address: &str) -> Result<Option<usize>, ServiceError> {
        let addr = parse_addr(address)?;
        let rows = self.listing_lines(addr.space);
        let contains = |row: &ListingRow| {
            let len = row.byte_len();
            len > 0 && row.addr() <= addr.offset && addr.offset < row.addr() + len
        };
        if let Some(i) = rows.iter().position(contains) {
            return Ok(Some(i));
        }
        Ok(rows.iter().position(|row| row.addr() >= addr.offset))
    }

    /// Coarse bands for a scrollbar map.
    pub fn listing_overview(&self, space: &str) -> Result<Vec<OverviewBand>, ServiceError> {
        let space = self.parse_space(space)?;
        let mut bands: Vec<OverviewBand> = Vec::new();
        for (i, row) in self.listing_lines(space).iter().enumerate() {
            let kind = row.kind();
            match bands.last_mut() {
                Some(last) if last.kind == kind => last.len += 1,
                _ => bands.push(OverviewBand { start: i, len: 1, kind }),
            }
        }
        Ok(bands)
    }

    fn spaces(&self) -> Vec<AddressSpace> {
        self.db.spaces()
    }

    fn parse_space(&self, name: &str) -> Result<AddressSpace, ServiceError> {
        let space = AddressSpace::from_dsl_name(name)
            .ok_or_else(|| ServiceError::Parse(format!("invalid address space `{name}`")))?;
        if self.spaces().contains(&space) {
            Ok(space)
        } else {
            Err(ServiceError::Parse(format!(
                "unknown address space `{name}`"
            )))
        }
    }

    /// Mapped, code and data bytes per space.
    pub fn memory_map(&self) -> Vec<SpaceUsageInfo> {
        self.spaces()
            .into_iter()
            .filter_map(|space| {
                let usage = self.db.space_usage(space);
                let total = usage.total();
                (total > 0).then_some(SpaceUsageInfo {
                    space: space.dsl_name().to_string(),
                    code: usage.code,
                    data: usage.data,
                    undefined: usage.undefined,
                    total,
                })
            })
            .collect()
    }

    /// Every named address, optionally one space.
    pub fn symbols(&self, space: Option<&str>) -> Result<Vec<SymbolInfo>, ServiceError> {
        let spaces: Vec<AddressSpace> = match space {
            Some(name) => vec![self.parse_space(name)?],
            None => self.spaces(),
        };
        let mut symbols = Vec::new();
        for space in spaces {
            let Some(region) = self.db.region(space) else {
                continue;
            };
            for (addr, func) in region.functions() {
                symbols.push(SymbolInfo {
                    addr: addr_dsl(space, addr),
                    space: space.dsl_name().to_string(),
                    name: func.name.clone(),
                    kind: "function",
                    signature: func.signature.clone(),
                });
            }
            for (addr, name) in region.labels() {
                if region.get_function(addr).is_some() {
                    continue;
                }
                symbols.push(SymbolInfo {
                    addr: addr_dsl(space, addr),
                    space: space.dsl_name().to_string(),
                    name: name.to_string(),
                    kind: "label",
                    signature: None,
                });
            }
        }
        Ok(symbols)
    }

    /// Cross-references *to* an address (who calls/jumps/reads/writes it).
    pub fn xrefs_to(&self, address: &str) -> Result<Vec<XrefInfo>, ServiceError> {
        let target = parse_addr(address)?;
        Ok(self
            .db
            .xrefs_to(&PhysicalAddr {
                space: target.space,
                offset: target.offset,
            })
            .iter()
            .map(xref_info)
            .collect())
    }

    /// References this address makes.
    pub fn xrefs_from(&self, address: &str) -> Result<Vec<XrefInfo>, ServiceError> {
        let source = parse_addr(address)?;
        Ok(self
            .db
            .xrefs_from(&PhysicalAddr {
                space: source.space,
                offset: source.offset,
            })
            .iter()
            .map(xref_info)
            .collect())
    }

    /// Basic blocks of the routine here.
    pub fn cfg(&self, address: &str) -> Result<Vec<BlockInfo>, ServiceError> {
        let entry = parse_addr(address)?;
        Ok(self
            .db
            .basic_blocks(entry.space, entry.offset)
            .into_iter()
            .map(|block| BlockInfo {
                start: addr_dsl(entry.space, block.start),
                end: addr_dsl(entry.space, block.end),
                successors: block
                    .successors
                    .iter()
                    .map(|&s| addr_dsl(entry.space, s))
                    .collect(),
            })
            .collect())
    }

    /// Notes covering or adjoining an address.
    pub fn notes_near(
        &self,
        address: &str,
        window: Option<u64>,
    ) -> Result<Vec<NoteInfo>, ServiceError> {
        let probe = parse_addr(address)?;
        let window = window.unwrap_or(0x40) as AddressValue;
        Ok(self
            .db
            .notes_near(probe.space, probe.offset, window)
            .into_iter()
            .map(|p| note_info(p.note, Some((probe.space, p.range)), Some(p.distance as u64)))
            .collect())
    }

    /// Notes whose text matches.
    pub fn notes_search(&self, query: &str) -> Vec<NoteInfo> {
        self.db
            .search_notes(query)
            .into_iter()
            .map(|note| note_info(note, self.db.note_location(&note.id), None))
            .collect()
    }

    /// Everything known about one address.
    pub fn context(&self, address: &str) -> Result<AddressContext, ServiceError> {
        let addr = parse_addr(address)?;
        let lines = self.db.render(addr.space);
        let mut label = None;
        let mut text = None;
        let mut kind = "empty";
        for line in lines.iter().filter(|l| l.addr() == addr.offset) {
            match line {
                Line::Label { name, .. } => label = Some(name.clone()),
                Line::Function { name, .. } => label = Some(name.clone()),
                Line::Instruction { text: t, .. } => {
                    text = Some(t.trim().to_string());
                    kind = "code";
                }
                Line::Data { bytes, .. } => {
                    text = Some(format!(".db {}", hex_bytes(bytes)));
                    kind = "data";
                }
                Line::Raw { bytes, .. } => {
                    text = Some(format!(".db {}", hex_bytes(bytes)));
                    kind = "raw";
                }
                _ => {}
            }
        }
        let comment = self
            .db
            .region(addr.space)
            .and_then(|r| r.get_comment(addr.offset))
            .map(str::to_string);
        let routine = self.routine_context(addr.space, addr.offset, &lines);
        Ok(AddressContext {
            address: addr_dsl(addr.space, addr.offset),
            comment,
            routine,
            label,
            text,
            kind,
        })
    }

    /// Coverage and what still blocks done.
    pub fn status(&self, gate: Option<&str>) -> Result<StatusInfo, ServiceError> {
        let gate = parse_gate(gate)?;
        let report = completeness::assess_at(&self.db, gate);
        let next = report.items.first().map(|item| crate::NextStep {
            item: item.id.clone(),
            run: item.suggested.first().cloned(),
            rest: "next() for the ordered worklist",
        });
        Ok(StatusInfo {
            gate: report.gate,
            done: report.done,
            phase: report.phase,
            blocking: report.blocking,
            coverage: report.coverage,
            counts: report.counts,
            next,
        })
    }

    /// A page of outstanding work items.
    pub fn worklist(
        &self,
        gate: Option<&str>,
        phase: Option<&str>,
        kind: Option<&str>,
        limit: Option<usize>,
        after: Option<&str>,
    ) -> Result<WorklistPage, ServiceError> {
        let gate = parse_gate(gate)?;
        let phase = phase.map(parse_phase).transpose()?;
        let report = completeness::assess_at(&self.db, gate);

        let mut items: Vec<Item> = report
            .items
            .into_iter()
            .filter(|it| phase.is_none_or(|p| it.phase == p))
            .filter(|it| kind.is_none_or(|k| it.kind == k))
            .collect();

        if let Some(after) = after
            && let Some(pos) = items.iter().position(|it| it.id == after)
        {
            items.drain(..=pos);
        }

        let remaining = items.len();
        let limit = limit.unwrap_or(DEFAULT_WORKLIST_LIMIT);
        let cursor = (remaining > limit)
            .then(|| items.get(limit - 1).map(|it| it.id.clone()))
            .flatten();
        items.truncate(limit);

        Ok(WorklistPage {
            done: report.done,
            remaining,
            returned: items.len(),
            cursor,
            items,
        })
    }
}

fn parse_addr(address: &str) -> Result<SpaceAddressValue, ServiceError> {
    from_dsl_value(address).map_err(|e| ServiceError::Parse(e.to_string()))
}

fn peek_verdict(decode: &ScratchDecode) -> (&'static str, String) {
    if decode.lines.is_empty() {
        return ("empty", "no bytes to decode here".to_string());
    }
    if decode.out_of_range_targets > 0 {
        return (
            "suspect",
            format!(
                "{} branch target(s) fall outside the loaded image; these bytes are probably data or filler, not code",
                decode.out_of_range_targets
            ),
        );
    }
    if decode.misaligned_targets > 0 {
        return (
            "suspect",
            format!(
                "{} branch target(s) land inside existing instructions; these bytes are probably data or filler, not code",
                decode.misaligned_targets
            ),
        );
    }
    if decode.self_misaligned_targets > 0 {
        return (
            "suspect",
            format!(
                "{} branch target(s) land midway through another instruction in this same decode; \
                 real code branches to instruction boundaries, so these bytes are probably data or filler",
                decode.self_misaligned_targets
            ),
        );
    }
    if decode.ran_out {
        return (
            "suspect",
            "decoding ran past the mapped bytes without reaching a return or jump".to_string(),
        );
    }
    if decode.terminates {
        return (
            "likely_code",
            "decodes cleanly to a return or unconditional jump".to_string(),
        );
    }
    (
        "likely_code",
        format!("decodes cleanly for {} instructions", decode.lines.len()),
    )
}

fn parse_gate(gate: Option<&str>) -> Result<Gate, ServiceError> {
    match gate {
        None | Some("documented") => Ok(Gate::Documented),
        Some("named") => Ok(Gate::Named),
        Some("structural") => Ok(Gate::Structural),
        Some(other) => Err(ServiceError::Parse(format!(
            "unknown gate `{other}` (expected `structural`, `named`, or `documented`)"
        ))),
    }
}

/// Parse a completeness phase name.
fn parse_phase(phase: &str) -> Result<Phase, ServiceError> {
    match phase {
        "decode" => Ok(Phase::Decode),
        "classify" => Ok(Phase::Classify),
        "name" => Ok(Phase::Name),
        "document" => Ok(Phase::Document),
        other => Err(ServiceError::Parse(format!(
            "unknown phase `{other}` (expected `decode`, `classify`, `name`, or `document`)"
        ))),
    }
}

/// Render bytes as a comma-separated `0xNN` list.
fn hex_bytes(bytes: &[u8]) -> String {
    bytes
        .iter()
        .map(|b| format!("{b:#04x}"))
        .collect::<Vec<_>>()
        .join(", ")
}

fn addr_dsl(space: AddressSpace, offset: AddressValue) -> String {
    format!("{}:{:#x}", space.dsl_name(), offset)
}

fn xref_kind_name(kind: XrefType) -> &'static str {
    match kind {
        XrefType::Call => "call",
        XrefType::Jump => "jump",
        XrefType::Read => "read",
        XrefType::Write => "write",
        XrefType::ReadWrite => "rw",
        XrefType::Pointer => "pointer",
    }
}

fn xref_info(xref: &Xref) -> XrefInfo {
    XrefInfo {
        from: addr_dsl(xref.from.space, xref.from.offset),
        to: addr_dsl(xref.to.space, xref.to.offset),
        kind: xref_kind_name(xref.xref_type),
    }
}

fn location_string(space: AddressSpace, range: AddressRange) -> String {
    let space = space.dsl_name();
    if range.end == range.start + 1 {
        format!("{space}:{:#x}", range.start)
    } else {
        format!("{space}:{:#x}..{:#x}", range.start, range.end)
    }
}

fn note_info(
    note: &Note,
    location: Option<(AddressSpace, AddressRange)>,
    distance: Option<u64>,
) -> NoteInfo {
    NoteInfo {
        id: note.id.to_string(),
        location: location.map(|(space, range)| location_string(space, range)),
        distance,
        content: note.content.clone(),
        tags: note.tags.iter().cloned().collect(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample() -> Session {
        let env = Box::new(MemoryEnvironment::new().with_file("fw.bin", vec![0x00, 0x00, 0x22]));
        Session::from_commands(
            [
                r#"set_cpu(name="i8051")"#,
                r#"map_bytes(address=CODE:0x0, file="fw.bin", file_offset=0x0, size=0x3)"#,
                "auto_disassemble(address=CODE:0x0)",
                r#"set_label(address=CODE:0x0, label="reset")"#,
            ],
            env,
        )
        .expect("build session")
    }

    #[test]
    fn raw_spans_chunk() {
        let mut bytes: Vec<u8> = (1..=8).collect();
        bytes.extend(std::iter::repeat_n(0u8, 64));
        bytes.extend(9..=16u8);
        let env = Box::new(MemoryEnvironment::new().with_file("fw.bin", bytes));
        let session = Session::from_commands(
            [
                r#"set_cpu(name="i8051")"#,
                r#"map_bytes(address=CODE:0x0, file="fw.bin", file_offset=0x0, size=0x50)"#,
            ],
            env,
        )
        .expect("build session");

        let listing = session.listing("CODE", 0, 100).expect("listing");
        let rows: Vec<&ListingRow> = listing.lines.iter().map(|l| &l.line).collect();
        assert!(rows.iter().any(|r| matches!(
            r,
            ListingRow::Fold(FoldRow::Region { addr: 0, kind: RegionKind::Unknown })
        )));
        for row in &rows {
            if let ListingRow::Line(Line::Raw { bytes, .. }) = row {
                assert!(bytes.len() <= 16, "row too wide: {} bytes", bytes.len());
            }
        }
        let fill = rows
            .iter()
            .find_map(|r| match r {
                ListingRow::Fold(FoldRow::Run { addr, value, len }) => Some((*addr, *value, *len)),
                _ => None,
            })
            .expect("a fill line");
        assert_eq!(fill, (0x8, 0x00, 64));

        let idx = session.locate("CODE:0x48").expect("locate").expect("found");
        let row = &listing.lines[idx];
        assert!(row.offset <= 0x48 && 0x48 < row.offset + 16, "row {:#x}", row.offset);
    }

    #[test]
    fn overview_bands_tile() {
        let env = Box::new(MemoryEnvironment::new().with_file("fw.bin", vec![0x00, 0x22, 0xAB, 0xCD]));
        let session = Session::from_commands(
            [
                r#"set_cpu(name="i8051")"#,
                r#"map_bytes(address=CODE:0x0, file="fw.bin", file_offset=0x0, size=0x4)"#,
                "auto_disassemble(address=CODE:0x0)",
            ],
            env,
        )
        .expect("build session");

        let total = session.listing("CODE", 0, 10_000).expect("listing").total;
        let bands = session.listing_overview("CODE").expect("overview");
        assert_eq!(bands.first().map(|b| b.kind), Some(RegionKind::Code));
        assert!(bands.iter().any(|b| b.kind == RegionKind::Unknown));
        let mut at = 0;
        for band in &bands {
            assert_eq!(band.start, at);
            at += band.len;
        }
        assert_eq!(at, total);
    }

    #[test]
    fn overview_reports_sizes() {
        let session = sample();
        let overview = session.disassembly_overview().expect("overview");
        assert!(overview.spaces.iter().any(|s| s.space == "CODE" && s.lines > 0));
        assert!(overview.symbols >= 1);
        assert!(overview.hint.contains("listing"));
    }

    #[test]
    fn listing_carries_addresses() {
        let session = sample();
        let listing = session.listing("CODE", 0, 100).expect("listing");
        assert_eq!(listing.space, "CODE");
        assert!(listing.total > 0);
        assert_eq!(listing.lines.len(), listing.total);
        assert!(listing.lines.iter().any(
            |l| matches!(&l.line, ListingRow::Line(Line::Label { name, .. }) if name == "reset")
        ));
        for (i, line) in listing.lines.iter().enumerate() {
            assert_eq!(line.index, i);
        }

        let tail = session.listing("CODE", 10_000, 10).expect("listing");
        assert!(tail.lines.is_empty());
        assert_eq!(tail.total, listing.total);
    }

    #[test]
    fn memory_map_counts_bytes() {
        let map = sample().memory_map();
        let code = map.iter().find(|u| u.space == "CODE").expect("CODE usage");
        assert_eq!(code.total, 3);
        assert_eq!(code.code, 3);
        assert_eq!(code.data, 0);
    }

    #[test]
    fn symbols_reports_the_label() {
        let symbols = sample().symbols(None).expect("symbols");
        let reset = symbols
            .iter()
            .find(|s| s.name == "reset")
            .expect("reset label");
        assert_eq!(reset.addr, "CODE:0x0");
        assert_eq!(reset.kind, "label");
    }

    #[test]
    fn unknown_space_rejected() {
        let session = sample();
        assert!(matches!(
            session.listing("NOPE", 0, 1),
            Err(ServiceError::Parse(_))
        ));
    }

    #[test]
    fn context_summarizes_address() {
        let cx = sample().context("CODE:0x0").expect("context");
        assert_eq!(cx.address, "CODE:0x0");
        assert_eq!(cx.label.as_deref(), Some("reset"));
        assert_eq!(cx.kind, "code");
        assert_eq!(cx.text.as_deref(), Some("NOP"));
    }

    #[test]
    fn peek_judges_code() {
        let good = sample().peek("CODE:0x0", None).expect("peek");
        assert_eq!(good.verdict, "likely_code");
        assert!(good.terminates);
        assert_eq!(good.out_of_range_targets, 0);
        assert_eq!(good.rows[0].bytes, vec![0x00]);
        assert!(good.text.contains("00"));

        let env = Box::new(MemoryEnvironment::new().with_file("f.bin", vec![0x02, 0xFF, 0xF0]));
        let session = Session::from_commands(
            [
                r#"set_cpu(name="i8051")"#,
                r#"map_bytes(address=CODE:0x0, file="f.bin", file_offset=0x0, size=0x3)"#,
            ],
            env,
        )
        .expect("build session");
        let suspect = session.peek("CODE:0x0", None).expect("peek");
        assert_eq!(suspect.verdict, "suspect");
        assert_eq!(suspect.out_of_range_targets, 1);
        assert!(suspect.text.contains("outside the loaded image"));
        assert_eq!(session.memory_map()[0].undefined, 3);
    }

    #[test]
    fn status_carries_next_action() {
        let env = Box::new(MemoryEnvironment::new().with_file("fw.bin", vec![0x00, 0x00, 0x22]));
        let session = Session::from_commands(
            [
                r#"set_cpu(name="i8051")"#,
                r#"map_bytes(address=CODE:0x0, file="fw.bin", file_offset=0x0, size=0x3)"#,
            ],
            env,
        )
        .expect("build session");

        let status = session.status(None).expect("status");
        assert!(!status.done);
        let next = status.next.as_ref().expect("unfinished work must name a next step");
        let head = session
            .worklist(None, None, None, Some(1), None)
            .expect("worklist")
            .items
            .remove(0);
        assert_eq!(next.item, head.id, "status and next must agree on the head");
        assert_eq!(next.run.as_deref(), head.suggested.first().map(String::as_str));

        assert!(sample().status(None).expect("status").next.is_none());
    }

    #[test]
    fn status_reports_done() {
        let status = sample().status(None).expect("status");
        assert!(status.done);
        assert!(status.phase.is_none());
        assert!(status.blocking.is_empty());
        assert_eq!(status.coverage.undefined, 0);
    }

    #[test]
    fn worklist_caps_at_default() {
        let env = Box::new(MemoryEnvironment::new().with_file("fw.bin", vec![0x00]));
        let maps: Vec<String> = (0..25u32)
            .map(|i| {
                format!(
                    r#"map_bytes(address=CODE:0x{:x}, file="fw.bin", file_offset=0x0, size=0x1)"#,
                    i * 4
                )
            })
            .collect();
        let mut commands = vec![r#"set_cpu(name="i8051")"#];
        commands.extend(maps.iter().map(String::as_str));
        let session = Session::from_commands(commands, env).expect("build session");

        let page = session.worklist(None, None, None, None, None).expect("worklist");
        assert_eq!(page.returned, DEFAULT_WORKLIST_LIMIT);
        assert!(page.remaining > DEFAULT_WORKLIST_LIMIT);
        assert!(page.cursor.is_some());
    }

    #[test]
    fn worklist_pages_and_filters() {
        let env =
            Box::new(MemoryEnvironment::new().with_file("fw.bin", vec![0x00, 0x22, 0xAA, 0xBB]));
        let session = Session::from_commands(
            [
                r#"set_cpu(name="i8051")"#,
                r#"map_bytes(address=CODE:0x0, file="fw.bin", file_offset=0x0, size=0x4)"#,
                "disassemble_range(range=CODE:0x0..0x2)",
            ],
            env,
        )
        .expect("build session");

        let status = session.status(None).expect("status");
        assert!(!status.done);
        assert_eq!(status.phase, Some(Phase::Decode));
        assert_eq!(status.coverage.undefined, 2);

        let page = session
            .worklist(None, Some("classify"), None, Some(1), None)
            .expect("worklist");
        assert_eq!(page.returned, 1);
        assert_eq!(page.items[0].kind, "undefined_bytes");
        assert_eq!(page.items[0].address, "CODE:0x2");

        assert!(matches!(
            session.worklist(Some("bogus"), None, None, None, None),
            Err(ServiceError::Parse(_))
        ));
    }
}
