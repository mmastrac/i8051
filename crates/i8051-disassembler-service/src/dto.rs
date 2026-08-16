use serde::Serialize;

use crate::{EditResult, Location, Session};

use i8051_disassembler::address::AddressValue;
use i8051_disassembler::analysis::completeness::{Coverage, Gate, Item, Phase};
use i8051_disassembler::render::Line;

/// A window of rendered listing lines.
#[derive(Serialize)]
/// A window of listing rows.
pub struct Listing {
    pub space: String,
    pub total: usize,
    /// Index of the first line in `lines`.
    pub start: usize,
    pub lines: Vec<LineInfo>,
}

#[derive(Serialize)]
/// Totals across every space.
pub struct DisassemblyOverview {
    pub hint: &'static str,
    pub spaces: Vec<SpaceOverview>,
    pub symbols: usize,
}

#[derive(Serialize)]
/// Totals for one space.
pub struct SpaceOverview {
    pub space: String,
    pub lines: usize,
    pub code: AddressValue,
    pub data: AddressValue,
    pub undefined: AddressValue,
    pub total: AddressValue,
}

#[derive(Serialize)]
/// One rendered listing line.
pub struct LineInfo {
    pub index: usize,
    /// The address in DSL form, e.g. `CODE:0x100`.
    pub addr: String,
    pub offset: AddressValue,
    pub line: ListingRow,
}

#[derive(Serialize)]
#[serde(untagged)]
/// A row: code, data, or fold.
pub enum ListingRow {
    Line(Line),
    Fold(FoldRow),
}

#[derive(Serialize)]
/// A collapsed run of rows.
pub enum FoldRow {
    /// `len` copies of one byte, shown `.ds`-style.
    Run {
        addr: AddressValue,
        value: u8,
        len: AddressValue,
    },
    /// `count` copies of a repeated multi-byte `unit`.
    Block {
        addr: AddressValue,
        unit: Vec<u8>,
        count: usize,
    },
    Region {
        addr: AddressValue,
        kind: RegionKind,
    },
}

impl ListingRow {
    /// The row's address.
    pub fn addr(&self) -> AddressValue {
        match self {
            Self::Line(line) => line.addr(),
            Self::Fold(
                FoldRow::Run { addr, .. }
                | FoldRow::Block { addr, .. }
                | FoldRow::Region { addr, .. },
            ) => *addr,
        }
    }

    /// Bytes the row covers.
    pub fn byte_len(&self) -> AddressValue {
        match self {
            Self::Line(
                Line::Instruction { bytes, .. }
                | Line::Data { bytes, .. }
                | Line::Raw { bytes, .. },
            ) => bytes.len() as AddressValue,
            Self::Line(_) | Self::Fold(FoldRow::Region { .. }) => 0,
            Self::Fold(FoldRow::Run { len, .. }) => *len,
            Self::Fold(FoldRow::Block { unit, count, .. }) => (unit.len() * count) as AddressValue,
        }
    }

    /// What the row holds.
    pub fn kind(&self) -> RegionKind {
        match self {
            Self::Line(Line::Data { .. }) => RegionKind::Data,
            Self::Line(Line::Raw { .. }) => RegionKind::Unknown,
            Self::Fold(FoldRow::Region { kind, .. }) => *kind,
            Self::Fold(_) => RegionKind::Unknown,
            Self::Line(_) => RegionKind::Code,
        }
    }
}

#[derive(Serialize, Debug, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
/// What a region holds.
pub enum RegionKind {
    Code,
    Data,
    Unknown,
}

#[derive(Serialize)]
/// One band of the scrollbar map.
pub struct OverviewBand {
    pub start: usize,
    pub len: usize,
    pub kind: RegionKind,
}

/// Byte usage for one address space.
#[derive(Serialize)]
/// Byte counts for one space.
pub struct SpaceUsageInfo {
    pub space: String,
    pub code: AddressValue,
    pub data: AddressValue,
    pub undefined: AddressValue,
    pub total: AddressValue,
}

#[derive(Serialize)]
/// A named address.
pub struct SymbolInfo {
    pub addr: String,
    pub space: String,
    pub name: String,
    /// `label` or `function`.
    pub kind: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub signature: Option<String>,
}

#[derive(Serialize)]
/// A note and where it sits.
pub struct NoteInfo {
    pub id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub location: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub distance: Option<u64>,
    pub content: String,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub tags: Vec<String>,
}

/// A cross-reference edge.
#[derive(Serialize)]
/// One cross-reference edge.
pub struct XrefInfo {
    pub from: String,
    pub to: String,
    /// `call`, `jump`, `read`, `write`, `rw`, or `pointer`.
    pub kind: &'static str,
}

/// A basic block.
#[derive(Serialize)]
/// One basic block.
pub struct BlockInfo {
    /// Inclusive start address in DSL form.
    pub start: String,
    /// Exclusive end address in DSL form.
    pub end: String,
    pub successors: Vec<String>,
}

#[derive(Serialize)]
/// Everything known about one address.
pub struct AddressContext {
    /// The address in DSL form, e.g. `CODE:0x26`.
    pub address: String,
    /// The label at this address, if any.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub label: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<String>,
    /// `code`, `data`, `raw`, or `empty`.
    pub kind: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub comment: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub routine: Option<RoutineContext>,
}

#[derive(Serialize)]
/// The routine containing an address.
pub struct RoutineContext {
    /// Entry address in DSL form.
    pub entry: String,
    /// Entry label, generated or given.
    pub name: String,
    /// Instruction count in the routine body.
    pub instructions: usize,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub callers: Vec<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub calls: Vec<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub touches: Vec<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub notes: Vec<String>,
}

#[derive(Serialize)]
/// An uncommitted decode and its verdict.
pub struct PeekInfo {
    pub address: String,
    /// `likely_code`, `suspect`, or `empty`.
    pub verdict: &'static str,
    /// Why, in one line.
    pub note: String,
    /// Flow reached a return or unconditional jump.
    pub terminates: bool,
    /// Decoded targets pointing outside the loaded image.
    pub out_of_range_targets: usize,
    /// Instructions decoded.
    pub lines: usize,
    pub text: String,
    pub rows: Vec<PeekLine>,
    pub committed: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub commit_with: Option<String>,
}

/// One scratch-decoded instruction in a [`PeekInfo`].
#[derive(Serialize)]
/// One line of an uncommitted decode.
pub struct PeekLine {
    /// DSL address of the instruction.
    pub addr: String,
    pub bytes: Vec<u8>,
    pub text: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub target: Option<String>,
    pub target_mapped: bool,
    pub target_misaligned: bool,
}

#[derive(Serialize)]
/// Coverage and what blocks done.
pub struct StatusInfo {
    pub gate: Gate,
    pub done: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub phase: Option<Phase>,
    /// The required phases still holding back `done`.
    pub blocking: Vec<Phase>,
    pub coverage: Coverage,
    /// How many outstanding items of each kind.
    pub counts: std::collections::BTreeMap<&'static str, usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub next: Option<NextStep>,
}

#[derive(Serialize)]
/// The next item worth doing.
pub struct NextStep {
    pub item: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub run: Option<String>,
    pub rest: &'static str,
}

/// A page of the completeness worklist.
#[derive(Serialize)]
/// A page of work items.
pub struct WorklistPage {
    pub done: bool,
    pub remaining: usize,
    /// Items in this page.
    pub returned: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cursor: Option<String>,
    pub items: Vec<Item>,
}
#[derive(Serialize)]
/// An edit's outcome and where it landed.
pub struct EditResponse {
    #[serde(flatten)]
    pub edit: EditResult,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context: Option<AddressContext>,
}

impl EditResponse {
    /// Bundle an edit with its context.
    pub fn new(session: &Session, edit: EditResult) -> Self {
        let context = focus_context(session, edit.address.as_deref());
        Self { edit, context }
    }
}

#[derive(Serialize)]
/// A move and what is there now.
pub struct NavResponse {
    #[serde(flatten)]
    pub location: Location,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context: Option<AddressContext>,
}

impl NavResponse {
    /// Bundle a move with its context.
    pub fn new(session: &Session, location: Location) -> Self {
        let context = focus_context(session, location.address.as_deref());
        Self { location, context }
    }
}

/// Best-effort context at an optional focus address.
fn focus_context(session: &Session, address: Option<&str>) -> Option<AddressContext> {
    session.context(address?).ok()
}
