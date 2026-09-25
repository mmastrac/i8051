use std::collections::BTreeMap;
use std::range::Range;

use serde::{Deserialize, Serialize};

use crate::address::{AddressSpace, AddressValue, PhysicalAddr, SpaceAddressValue, Xref};
use crate::commands::{Command, Environment, SetCpu, SetNote, boxed};
use crate::labels::{ImplicitLabels, LabelCollector};
pub use crate::note::{
    Note, NoteAddressIndex, NoteDb, NoteField, NoteGlobalIndex, NoteId, NotePath, Notes,
    ProximateNote,
};
use crate::platform::{Certainty, Platform, PlatformRef};
pub use crate::region::{
    Block, ByteRange, LabelAttrs, OperandType, Region, ScratchDecode, ScratchInsn,
};
use crate::render::Line;
use crate::render::sdas::SdasWriter;
use crate::store::dsl;

pub struct Db {
    regions: BTreeMap<AddressSpace, Region>,
    /// The processor driver: decodes bytes and declares the address regions.
    /// `None` until a `set_cpu` command (or [`with_platform`](Db::with_platform))
    /// selects one. Disassembly requires a CPU.
    platform: Option<PlatformRef>,
    pub notes: NoteDb,
}

impl Db {
    /// A database with no CPU selected. A `set_cpu` command must run before any
    /// disassembly.
    pub fn new() -> Self {
        Self {
            regions: BTreeMap::new(),
            platform: None,
            notes: NoteDb::default(),
        }
    }

    /// A database with `platform` already selected.
    pub fn with_platform(platform: PlatformRef) -> Self {
        let mut db = Self::new();
        db.set_platform(Some(platform));
        db
    }

    /// The selected processor driver, or `None` if no CPU is set.
    pub fn platform(&self) -> Option<&dyn Platform> {
        self.platform.as_deref()
    }

    /// Select the CPU (or clear it with `None`), propagating it to every region
    /// so their code re-derives. Returns the previous selection.
    pub fn set_platform(&mut self, platform: Option<PlatformRef>) -> Option<PlatformRef> {
        let prev = self.platform.take();
        self.platform = platform.clone();
        for region in self.regions.values_mut() {
            region.set_platform(platform.clone());
        }
        prev
    }

    /// The address spaces to render, in order: the driver's regions when a CPU
    /// is set, otherwise whatever spaces already hold mapped bytes.
    pub fn spaces(&self) -> Vec<AddressSpace> {
        match &self.platform {
            Some(p) => p.regions().iter().map(|r| r.space).collect(),
            None => self.regions.keys().copied().collect(),
        }
    }

    /// Space names are driver-defined.
    pub fn resolve_space(&self, name: &str) -> Result<AddressSpace, Error> {
        let spaces = self.spaces();
        match AddressSpace::from_dsl_name(name) {
            Some(space) if spaces.contains(&space) => Ok(space),
            _ => Err(ErrorKind::UnknownSpace {
                name: name.to_string(),
                suggestion: crate::commands::closest(name, spaces.iter().map(|s| s.dsl_name()))
                    .map(str::to_string),
            }
            .into()),
        }
    }

    /// The `.area` header for `space`: the driver's when a CPU is set, else a
    /// plain default built from the space name.
    fn area_header(&self, space: AddressSpace) -> String {
        self.platform
            .as_ref()
            .and_then(|p| p.area_header(space))
            .map(str::to_string)
            .unwrap_or_else(|| format!(".area {} (ABS)\n", space.dsl_name()))
    }

    pub fn region(&self, space: AddressSpace) -> Option<&Region> {
        self.regions.get(&space)
    }

    pub fn region_mut(&mut self, space: AddressSpace) -> &mut Region {
        let platform = self.platform.clone();
        self.regions
            .entry(space)
            .or_insert_with(|| Region::new(space, platform))
    }

    pub fn xrefs_to(&self, target: &PhysicalAddr) -> Vec<Xref> {
        // Each region indexes only its own instructions. Regions without code
        // carry an empty index and contribute nothing.
        self.regions
            .values()
            .flat_map(|region| region.xrefs_to(target))
            .filter(|x| self.pointer_candidate_survives(x))
            .collect()
    }

    /// Whether an inferred pointer candidate is still alive.
    fn pointer_candidate_survives(&self, xref: &Xref) -> bool {
        if xref.certainty != Certainty::Inferred {
            return true;
        }
        match self
            .regions
            .get(&xref.from.space)
            .and_then(|r| r.operand_type(xref.from.offset))
        {
            Some(OperandType::Pointer(space)) => xref.to.space == space,
            // A number references nothing, so no candidate survives.
            Some(OperandType::Value) => false,
            None => true,
        }
    }

    /// Instructions with ambiguous operands, as `(instruction, value, candidate
    /// spaces)`.
    pub fn undecided_operands(&self) -> Vec<(SpaceAddressValue, AddressValue, Vec<AddressSpace>)> {
        let mut by_site: BTreeMap<(AddressSpace, AddressValue, AddressValue), Vec<AddressSpace>> =
            BTreeMap::new();
        for (&space, region) in &self.regions {
            for (target, _, _) in region.inferred_pointer_candidates() {
                for from in region.pointer_sources(&target) {
                    if region.operand_type(from).is_some() {
                        continue;
                    }
                    let spaces = by_site.entry((space, from, target.offset)).or_default();
                    if !spaces.contains(&target.space) {
                        spaces.push(target.space);
                    }
                }
            }
        }
        by_site
            .into_iter()
            .filter(|(_, spaces)| spaces.len() > 1)
            .map(|((space, from, value), spaces)| ((space, from).into(), value, spaces))
            .collect()
    }

    pub fn xrefs_from(&self, source: &PhysicalAddr) -> Vec<Xref> {
        let Some(region) = self.regions.get(&source.space) else {
            return Vec::new();
        };
        region
            .xrefs_from(source)
            .into_iter()
            .filter(|x| self.pointer_candidate_survives(x))
            .collect()
    }

    /// The control-flow graph of the routine rooted at `entry` in `space`.
    pub fn basic_blocks(&self, space: AddressSpace, entry: AddressValue) -> Vec<Block> {
        self.region(space)
            .map(|region| region.basic_blocks(entry))
            .unwrap_or_default()
    }

    /// Decode bytes as code from `start` without committing, for a caller to
    /// judge whether a run is really code (see [`Region::scratch_decode`]).
    pub fn peek(
        &self,
        space: AddressSpace,
        start: AddressValue,
        max_lines: usize,
    ) -> ScratchDecode {
        self.region(space)
            .map(|region| region.scratch_decode(start, max_lines))
            .unwrap_or_default()
    }

    /// Test-decode `start..end` as one straight run without committing, for a
    /// caller about to commit the whole range as code (see
    /// [`Region::scratch_decode_linear`]).
    pub fn peek_linear(
        &self,
        space: AddressSpace,
        start: AddressValue,
        end: AddressValue,
    ) -> ScratchDecode {
        self.region(space)
            .map(|region| region.scratch_decode_linear(start, end))
            .unwrap_or_default()
    }

    pub fn undecoded_entry_points(&self) -> Vec<crate::platform::EntryPoint> {
        let Some(platform) = &self.platform else {
            return Vec::new();
        };
        platform
            .entry_points()
            .iter()
            .filter(|e| {
                self.region(e.space).is_some_and(|r| {
                    r.has_byte(e.offset)
                        && !r.platform_address_disabled(e.offset)
                        && !matches!(
                            r.get_equivalent_kind(e.offset),
                            Some(crate::db::EquivalentKind::Code)
                        )
                })
            })
            .copied()
            .collect()
    }

    /// Follow pure jump thunks from `addr` to the ultimate target. Rendering
    /// stays faithful to the bytes, so this is how a consumer asks where a call
    /// or jump really ends up. Returns `addr` when it is not a thunk.
    pub fn resolve_thunk(&self, space: AddressSpace, addr: AddressValue) -> AddressValue {
        self.region(space)
            .map_or(addr, |region| region.resolve_thunks(addr))
    }

    fn implicit_labels(&self) -> ImplicitLabels {
        let mut label_collector = LabelCollector::default();
        for region in self.regions.values() {
            region.collect_refs(&mut label_collector);
        }
        let mut labels = label_collector.into_implicit_labels();
        // Prefer platform names
        if let Some(platform) = &self.platform {
            for entry in platform.entry_points() {
                let decoded = self.region(entry.space).is_some_and(|r| {
                    matches!(
                        r.get_equivalent_kind(entry.offset),
                        Some(EquivalentKind::Code)
                    )
                });
                if decoded {
                    labels.insert_if_absent(entry.space, entry.offset, entry.name);
                }
            }
        }
        labels
    }

    pub fn render(&self, space: AddressSpace) -> Vec<Line> {
        let implicit_labels = self.implicit_labels();

        self.regions
            .get(&space)
            .map(|region| region.render(space, &implicit_labels))
            .unwrap_or_default()
    }

    pub fn render_range(
        &self,
        space: AddressSpace,
        start: AddressValue,
        end: AddressValue,
    ) -> Vec<Line> {
        self.render(space)
            .into_iter()
            .filter(|line| {
                let addr = line.addr();
                addr >= start && addr < end
            })
            .collect()
    }

    pub fn to_sdas(&self) -> String {
        let mut writer = SdasWriter::default();
        let implicit_labels = self.implicit_labels();

        for space in self.spaces() {
            let Some(region) = self.regions.get(&space) else {
                continue;
            };
            writer.write(&self.area_header(space));
            // Assembly names, not listing names
            let names =
                region.export_names(implicit_labels.get(&space).unwrap_or(&Default::default()));
            for line in region.render_named(space, &implicit_labels, Some(&names)) {
                writer.write_line(&line);
            }
        }

        writer.into_string()
    }

    pub fn to_commands(&self) -> Vec<Box<dyn Command>> {
        let mut commands = Vec::new();
        // The CPU comes first: the rest of the script decodes against it.
        if let Some(platform) = &self.platform {
            commands.push(boxed(SetCpu::new(platform.name().to_string())));
        }
        for (&space, region) in &self.regions {
            // Emit SetAddressBits first ...
            if let Some(bits) = region.address_bits() {
                commands.push(boxed(crate::commands::SetAddressBits {
                    space: space.dsl_name().to_string(),
                    bits: AddressValue::from(bits),
                }));
            }
            // ... then the region's own commands
            commands.extend(region.to_commands(space));
            // ... then any disabled platform addresses
            for (offset, reason) in region.disabled_platform_addresses() {
                commands.push(boxed(crate::commands::DisablePlatformAddress {
                    address: (space, offset).into(),
                    reason: reason.to_string(),
                }));
            }
            for (offset, kind) in region.operand_types() {
                commands.push(match kind {
                    OperandType::Pointer(target) => boxed(crate::commands::SetOperandPointer {
                        address: (space, offset).into(),
                        space: target.dsl_name().to_string(),
                    }),
                    OperandType::Value => boxed(crate::commands::SetOperandValue {
                        address: (space, offset).into(),
                    }),
                });
            }
        }

        for (id, note) in self.notes.notes.iter() {
            if let Some((space, range)) = self.notes.location(id) {
                commands.push(boxed(SetNote {
                    address: (space, range).into(),
                    note: note.clone(),
                }));
            }
        }
        commands
    }

    pub fn apply(
        &mut self,
        command: Box<dyn Command>,
        env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        self.check(command.as_ref())?;
        command.apply(self, env)
    }

    /// Judgment safe to run on replay.
    fn check(&self, command: &dyn Command) -> Result<(), CommandError> {
        self.check_cpu_still_needed(command)?;
        self.check_label(command)?;
        self.check_speculative_decode(command)
    }

    fn check_cpu_still_needed(&self, command: &dyn Command) -> Result<(), CommandError> {
        if command
            .as_any()
            .downcast_ref::<crate::commands::ClearCpu>()
            .is_none()
        {
            return Ok(());
        }
        let Some(cpu) = self.platform().map(|p| p.name().to_string()) else {
            return Ok(());
        };
        let decoded: u64 = self
            .regions
            .values()
            .map(|region| u64::from(region.coverage().code))
            .sum();
        if decoded == 0 {
            return Ok(());
        }
        Err(ErrorKind::CpuStillNeeded { cpu, decoded }.into())
    }

    fn check_label(&self, command: &dyn Command) -> Result<(), CommandError> {
        let Some(set) = command.as_any().downcast_ref::<crate::commands::SetLabel>() else {
            return Ok(());
        };
        let Ok(name) = crate::commands::normalize_label(&set.label) else {
            return Ok(());
        };
        let here = set.address.space.dsl_addr(set.address.offset);
        if crate::labels::is_provisional_name(&name) {
            return Err(
                CommandError::from(ErrorKind::GeneratedLabel { label: name }).suggest(vec![
                    dsl!(set_label(address = {here}, label = "...")
                        # "a name that says what the code does, e.g. uart_tx"),
                    dsl!(set_note(address = {here}, note = Note(content = "..."))
                        # "record what you know if you cannot tell yet"),
                ]),
            );
        }
        if set.local {
            return self.check_duplicate_local(set, &name);
        }
        for (&space, region) in &self.regions {
            let clash = region
                .labels()
                .map(|(offset, label)| (offset, label.to_string()))
                .chain(
                    region
                        .functions()
                        .map(|(offset, f)| (offset, f.name.clone())),
                )
                .find(|(offset, label)| {
                    *label == name && (space, *offset) != (set.address.space, set.address.offset)
                });
            let Some((offset, _)) = clash else { continue };
            let holder = space.dsl_addr(offset);
            return Err(CommandError::from(ErrorKind::LabelTaken {
                label: name.clone(),
                holder: holder.clone(),
            })
            .suggest(vec![
                dsl!(set_label(address = {here}, label = "...")
                    # "a name that distinguishes it from {holder}"),
                dsl!(set_label(address = {here}, label = "{name}", local = True)
                    # "if this is a spot inside a routine, not a routine of its own"),
                dsl!(set_label(address = {holder}, label = "...")
                    # "to free the name if this is its better home"),
            ]));
        }
        Ok(())
    }

    fn check_duplicate_local(
        &self,
        set: &crate::commands::SetLabel,
        name: &str,
    ) -> Result<(), CommandError> {
        let space = set.address.space;
        let Some(region) = self.region(space) else {
            return Ok(());
        };
        let Some(scope) = region.scope_of(set.address.offset) else {
            return Ok(());
        };
        let clash = region
            .labels()
            .find(|&(at, other)| {
                at != set.address.offset && other == name && region.scope_of(at) == Some(scope)
            })
            .map(|(at, _)| space.dsl_addr(at));
        let Some(clash) = clash else { return Ok(()) };
        let here = space.dsl_addr(set.address.offset);
        Err(CommandError::from(ErrorKind::LocalLabelTaken {
            label: name.to_string(),
            holder: clash,
        })
        .suggest(vec![
            dsl!(set_label(address = {here}, label = "...", local = True)
            # "a name unused in this routine"),
        ]))
    }

    fn check_speculative_decode(&self, command: &dyn Command) -> Result<(), CommandError> {
        let Some(range) = command
            .as_any()
            .downcast_ref::<crate::commands::DisassembleRange>()
        else {
            return Ok(());
        };
        if range.force {
            return Ok(());
        }
        let space = range.range.space;
        let decode = self.peek_linear(space, range.range.start, range.range.end);
        let mut reasons = Vec::new();
        if decode.out_of_range_targets > 0 {
            reasons.push(format!(
                "{} branch target(s) point outside the loaded image",
                decode.out_of_range_targets
            ));
        }
        if decode.self_misaligned_targets > 0 {
            reasons.push(format!(
                "{} branch target(s) land midway through another instruction in the same range",
                decode.self_misaligned_targets
            ));
        }
        if decode.misaligned_targets > 0 {
            reasons.push(format!(
                "{} branch target(s) land inside existing instructions",
                decode.misaligned_targets
            ));
        }
        if reasons.is_empty() {
            return Ok(());
        }
        Err(CommandError::from(ErrorKind::RangeDoesNotDecode {
            count: decode.lines.len(),
            reasons,
        })
        .suggest(vec![
            dsl!(auto_disassemble(address = {space.dsl_addr(range.range.start)})
                    # "follows flow and stops where it stops"),
            dsl!(mark_data(
                    range = {space.dsl_range(range.range.start, range.range.end)},
                    data_type = DataType::Byte
                ) # "if the whole range is data"),
            dsl!(disassemble_range(
                    range = {space.dsl_range(range.range.start, range.range.end)},
                    force = True
                ) # "to decode the bytes as-is"),
        ]))
    }

    /// Byte counts for mapped content classified by equivalent kind.
    pub fn space_usage(&self, space: AddressSpace) -> SpaceUsage {
        self.regions
            .get(&space)
            .map(Region::space_usage)
            .unwrap_or_default()
    }

    pub fn clear_note(
        &mut self,
        id: &NoteId,
    ) -> Option<(AddressSpace, crate::address::AddressRange, Note)> {
        self.notes.clear_address(id)
    }

    pub fn note_tip(&self) -> Option<NoteId> {
        self.notes.tip()
    }

    pub fn create_note(&mut self, content: impl Into<String>) -> Note {
        self.notes.create(content)
    }

    pub fn get_notes_overlapping(
        &self,
        space: AddressSpace,
        range: impl std::ops::RangeBounds<AddressValue>,
    ) -> Vec<&Note> {
        self.notes.get_notes_overlapping(space, range)
    }

    pub fn get_notes_inside(
        &self,
        space: AddressSpace,
        range: impl std::ops::RangeBounds<AddressValue>,
    ) -> Vec<&Note> {
        self.notes.get_notes_inside(space, range)
    }

    /// Notes within `window` bytes of `addr`, nearest first.
    pub fn notes_near(
        &self,
        space: AddressSpace,
        addr: AddressValue,
        window: AddressValue,
    ) -> Vec<crate::note::ProximateNote<'_>> {
        self.notes.notes_near(space, addr, window)
    }

    /// Notes matching `query` (case-insensitive over content, tags, fields).
    pub fn search_notes(&self, query: &str) -> Vec<&Note> {
        self.notes.search(query)
    }

    /// Where a note is attached, if anywhere.
    pub fn note_location(
        &self,
        id: &NoteId,
    ) -> Option<(AddressSpace, crate::address::AddressRange)> {
        self.notes.location(id)
    }
}

impl Default for Db {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Function {
    pub addr: PhysicalAddr,
    pub name: String,
    pub signature: Option<String>,
    pub length: AddressValue,
    pub noreturn: bool,
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum DataType {
    Byte,
    Word,
    Dword,
    Qword,
    Reference(Box<DataType>),
    Equivalent(Box<DataType>, String),
    Array(Box<DataType>, usize),
    String(usize),
    Struct(Vec<DataType>),
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum OperandOverride {
    Label(String),
    LabelOffset { label: String, offset: i32 },
    Text(String),
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum Equivalent {
    Code,
    Data(DataType, AddressValue),
    /// A barrier of the given byte length: renders raw, but blocks
    /// auto-disassembly (unlike undefined bytes, which it flows into).
    Unknown(AddressValue),
}

impl Equivalent {
    pub fn kind(&self) -> EquivalentKind {
        match self {
            Self::Code => EquivalentKind::Code,
            Self::Data(_, _) => EquivalentKind::Data,
            Self::Unknown(_) => EquivalentKind::Unknown,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum EquivalentKind {
    Code,
    Data,
    Unknown,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct EquivalentRange {
    pub end: AddressValue,
    pub equivalent: Equivalent,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum EquivalentAt<'a> {
    Undefined(Range<AddressValue>),
    Defined {
        start: AddressValue,
        range: &'a EquivalentRange,
    },
}

impl<'a> EquivalentAt<'a> {
    pub fn is_defined(&self) -> bool {
        matches!(self, Self::Defined { .. })
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct SpaceUsage {
    /// Bytes covered by a `Equivalent::Code` range.
    pub code: AddressValue,
    /// Bytes covered by a `Equivalent::Data` range.
    pub data: AddressValue,
    /// Mapped bytes with no equivalent (rendered as raw data).
    pub undefined: AddressValue,
}

impl SpaceUsage {
    pub fn total(&self) -> AddressValue {
        self.code
            .saturating_add(self.data)
            .saturating_add(self.undefined)
    }
}

/// A refused command: facts plus commands that resolve it.
#[derive(Debug)]
pub struct CommandError {
    pub what: ErrorKind,
    /// Commands that resolve it, runnable verbatim.
    pub suggested: Vec<String>,
}

/// Kept for existing signatures.
pub type Error = CommandError;

impl CommandError {
    /// Attach commands that resolve this error.
    pub fn suggest(mut self, suggested: Vec<String>) -> Self {
        #[cfg(debug_assertions)]
        for suggestion in &suggested {
            if let Err(e) = crate::store::parse_call(suggestion) {
                panic!("error suggestion does not parse: {suggestion:?}: {e}");
            }
        }
        self.suggested = suggested;
        self
    }
}

impl From<ErrorKind> for CommandError {
    fn from(what: ErrorKind) -> Self {
        Self {
            what,
            suggested: Vec::new(),
        }
    }
}

impl From<std::io::Error> for CommandError {
    fn from(error: std::io::Error) -> Self {
        ErrorKind::Io {
            message: error.to_string(),
        }
        .into()
    }
}

impl std::fmt::Display for CommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.what)?;
        for suggestion in &self.suggested {
            write!(f, "\n  {suggestion}")?;
        }
        Ok(())
    }
}

impl std::error::Error for CommandError {}

/// The facts of a refused command, rendered elsewhere.
#[derive(Debug, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ErrorKind {
    /// File access needed, none attached.
    NoEnvironment,
    /// A disassembly command with no CPU selected.
    NoCpu,
    /// `set_cpu` while a CPU is selected.
    CpuAlreadySet { current: String },
    /// A CPU with no built-in driver.
    UnknownCpu {
        name: String,
        suggestions: Vec<String>,
    },
    /// A range overlapping an existing classification.
    Overlap { at: String, marked: String },
    /// An unmap cutting a longer classification.
    PartialEquivalent { range: String, marked: String },
    /// An illegal assembler symbol.
    InvalidLabel { label: String, reason: &'static str },
    /// A value the command cannot take.
    InvalidArgument { value: String, reason: &'static str },
    /// An address space this database lacks.
    UnknownSpace {
        name: String,
        suggestion: Option<String>,
    },
    /// A classify target with no mapped byte.
    NothingMapped { at: String },
    /// A classification the bytes cannot take.
    InvalidEquivalent,
    /// A mark over already-classified bytes.
    AlreadyClassified {
        at: String,
        marked: String,
        covering: String,
        /// Bytes the unblocking clear takes.
        cleared: u64,
        /// Bytes the command asked about.
        asked: u64,
    },
    /// A filesystem failure.
    Io { message: String },
    /// A cleared CPU with decoded bytes.
    CpuStillNeeded { cpu: String, decoded: u64 },
    /// A label another address already holds.
    LabelTaken { label: String, holder: String },
    /// A local label reused inside one routine.
    LocalLabelTaken { label: String, holder: String },
    /// A label equal to the generated form.
    GeneratedLabel { label: String },
    /// A range that decodes badly.
    RangeDoesNotDecode { count: usize, reasons: Vec<String> },
    /// A classify range covering live vectors.
    RangeCoversVectors { vectors: Vec<String> },
    /// An auto-disassemble root under a barrier.
    BarrierStopsAuto {
        at: String,
        barrier: String,
        marked: String,
    },
    /// A classify range covering branch targets.
    RangeSwallowsTargets {
        /// `addr (from callers)` phrases, at most four.
        targets: Vec<String>,
        omitted: usize,
        first_target: String,
        first_source: String,
        sources: usize,
    },
}

/// The word for a classification in messages.
pub(crate) fn marked_word(kind: EquivalentKind) -> &'static str {
    match kind {
        EquivalentKind::Code => "code",
        EquivalentKind::Data => "data",
        EquivalentKind::Unknown => "barrier",
    }
}

/// A [`ErrorKind::NothingMapped`] with its fixes.
pub(crate) fn nothing_mapped(space: AddressSpace, offset: AddressValue) -> CommandError {
    let at = space.dsl_addr(offset);
    let range = space.dsl_range(offset, offset.saturating_add(1));
    CommandError::from(ErrorKind::NothingMapped { at: at.clone() }).suggest(vec![
        dsl!(map_bytes(address = {at}, file = "...", file_offset = 0x0, size = 0x0)
            # "bring bytes in from the image file"),
        dsl!(set_constant_bytes(range = {range}, value = 0x0)
            # "fill the gap with a value, then classify again"),
    ])
}

/// An [`ErrorKind::AlreadyClassified`] with its unblocking clear.
pub(crate) fn already_classified(
    at: SpaceAddressValue,
    existing: EquivalentKind,
    start: AddressValue,
    end: AddressValue,
    requested_end: AddressValue,
) -> CommandError {
    let space = at.space;
    let mut suggested = vec![dsl!(clear_equivalents(
        addresses = { space.dsl_set(start, end) }
    ))];
    if at.offset > start {
        suggested.push(dsl!(mark_data(
            range = {space.dsl_range(start, at.offset)},
            data_type = DataType::Byte
        ) # "restore this remainder after the clear"));
    }
    if requested_end < end {
        suggested.push(dsl!(mark_data(
            range = {space.dsl_range(requested_end, end)},
            data_type = DataType::Byte
        ) # "restore this remainder after the clear"));
    }
    CommandError::from(ErrorKind::AlreadyClassified {
        at: space.dsl_addr(at.offset),
        marked: marked_word(existing).to_string(),
        covering: space.dsl_range(start, end),
        cleared: u64::from(end.saturating_sub(start)),
        asked: u64::from(requested_end.saturating_sub(at.offset)),
    })
    .suggest(suggested)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::io;

    use super::*;
    use crate::address::SpaceAddressSet;
    use crate::address::XrefType;
    use crate::commands::{
        AutoDisassemble, ClearLabel, Command, MapBytes, SetConstantBytes, UnmapBytes, boxed,
    };
    use crate::platform::{Certainty, i8051::CODE};
    use pretty_assertions::assert_eq;

    static TEST_BINARY: [u8; 12] = [
        0x02, 0x00, 0x10, // LJMP 0x10
        0x74, 0x01, // MOV A, #0x1
        0xF0, // MOVX @DPTR, A
        0x90, 0x00, 0x10, // MOV DPTR, #0x10
        0x93, // MOVC A, @A+DPTR
        0x80, 0xF7, // SJMP 0x3 (rel = 3 - (10 + 2))
    ];

    struct TestEnvironment {
        files: HashMap<String, Vec<u8>>,
    }

    impl TestEnvironment {
        fn new() -> Self {
            Self {
                files: HashMap::new(),
            }
        }

        fn with_file(mut self, name: impl Into<String>, bytes: Vec<u8>) -> Self {
            self.files.insert(name.into(), bytes);
            self
        }
    }

    impl Environment for TestEnvironment {
        fn load_file_bytes(
            &self,
            file: &str,
            offset: usize,
            size: AddressValue,
        ) -> Result<Vec<u8>, io::Error> {
            let data = self.files.get(file).ok_or_else(|| {
                io::Error::new(io::ErrorKind::NotFound, format!("file not found: {file}"))
            })?;
            let end = offset.saturating_add(size as usize);
            if end > data.len() {
                return Err(io::Error::new(
                    io::ErrorKind::UnexpectedEof,
                    "read past end of file",
                ));
            }
            Ok(data[offset..end].to_vec())
        }
    }

    fn apply_all(db: &mut Db, commands: Vec<Box<dyn Command>>, env: &TestEnvironment) {
        for command in commands {
            db.apply(command, Some(env)).unwrap();
        }
    }

    #[test]
    fn decoded_vectors_named() {
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        let code = db.region_mut(CODE);
        code.set_bytes("test.bin", 0, 0, &[0x00; 0x30]);
        code.set_equivalent(0, Equivalent::Code).unwrap();
        code.set_equivalent(0x0B, Equivalent::Code).unwrap();

        let labels = db.implicit_labels();
        let named = &labels[&CODE];
        assert_eq!(named.get(&0).map(String::as_str), Some("INT_reset"));
        assert_eq!(named.get(&0x0B).map(String::as_str), Some("INT_timer0"));
        assert_eq!(named.get(&0x03), None, "filler must not be labelled");
    }

    fn make_test_db() -> Db {
        let mut db = Db::with_platform(crate::platform::i8051::platform());

        let code = db.region_mut(CODE);
        code.set_bytes("test.bin", 0, 0, &TEST_BINARY);

        code.set_label(0, "start", LabelAttrs::default());
        code.set_equivalent(0, Equivalent::Code).unwrap();

        code.set_comment(3, "Start of loop");
        code.set_label(3, "loop", LabelAttrs::default());
        code.set_equivalent(3, Equivalent::Code).unwrap();
        code.set_equivalent(5, Equivalent::Code).unwrap();
        code.set_equivalent(6, Equivalent::Code).unwrap();
        code.set_equivalent(9, Equivalent::Code).unwrap();
        code.set_equivalent(10, Equivalent::Code).unwrap();
        db
    }

    /// An i8051 DB with `bytes` mapped from `file` at CODE:0, plus its env.
    fn mapped(file: &str, bytes: &[u8]) -> (Db, TestEnvironment) {
        let env = TestEnvironment::new().with_file(file, bytes.to_vec());
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        let size = bytes.len() as AddressValue;
        db.apply(
            boxed(MapBytes::new((CODE, 0), file, 0usize, size)),
            Some(&env),
        )
        .unwrap();
        (db, env)
    }

    /// Export `db` to DSL and rebuild from scratch (resolving `map_bytes` files
    /// against `env`): the save/reload path.
    fn reload(db: &Db, env: &TestEnvironment) -> Db {
        use crate::store::{from_dsl_many, to_dsl_many};
        let mut reloaded = Db::new();
        for command in from_dsl_many(&to_dsl_many(&db.to_commands())).unwrap() {
            let env = (command.name() == "map_bytes").then_some(env as &dyn Environment);
            reloaded.apply(command, env).unwrap();
        }
        reloaded
    }

    #[test]
    fn test_db() {
        let db = make_test_db();
        assert_eq!(
            db.xrefs_to(&PhysicalAddr {
                space: CODE,
                offset: 3
            }),
            vec![Xref {
                xref_type: XrefType::Jump,
                from: PhysicalAddr {
                    space: CODE,
                    offset: 10
                },
                to: PhysicalAddr {
                    space: CODE,
                    offset: 3
                },
                certainty: Certainty::Definite,
            }]
        );

        assert_eq!(
            db.xrefs_from(&PhysicalAddr {
                space: CODE,
                offset: 10
            }),
            vec![Xref {
                xref_type: XrefType::Jump,
                from: PhysicalAddr {
                    space: CODE,
                    offset: 10
                },
                to: PhysicalAddr {
                    space: CODE,
                    offset: 3
                },
                certainty: Certainty::Definite,
            }]
        );

        let expected = r#"
.area CODE (CODE,ABS)
.org 0x0

start:
    LJMP    loc_0010
; Start of loop
loop:
    MOV     A,#01
    MOVX    @DPTR,A
    MOV     DPTR,#0x0010
    MOVC    A,@A+DPTR
    SJMP    loop
loc_0010:
        "#;
        assert_eq!(db.to_sdas().trim(), expected.trim());
    }

    // Also the general round-trip test: the full `make_test_db` listing (labels,
    // comments, strong code) survives export to DSL and reload.
    #[test]
    fn round_trips_with_notes() {
        use crate::address::AddressRange;
        use crate::commands::SetNote;

        let env = TestEnvironment::new().with_file("test.bin", TEST_BINARY.to_vec());
        let mut db = make_test_db();

        // Attach two notes (one tagged) to address ranges.
        let mut first = Note::new(None, "reset handler");
        first.tags.insert("entry".into());
        let second = Note::new(Some(&first.id), "jump table");
        db.apply(boxed(SetNote::new((CODE, 0x0..0x3), first.clone())), None)
            .unwrap();
        db.apply(boxed(SetNote::new((CODE, 0x3..0x5), second.clone())), None)
            .unwrap();

        assert!(
            crate::store::to_dsl_many(&db.to_commands()).contains("set_note("),
            "notes must be exported"
        );
        let reloaded = reload(&db, &env);

        // Both notes survived with their ids, content, tags, and locations.
        assert_eq!(reloaded.notes.notes.len(), 2);
        assert_eq!(reloaded.notes.get(&first.id), Some(&first));
        assert_eq!(reloaded.notes.get(&second.id), Some(&second));
        assert_eq!(
            reloaded.note_location(&first.id),
            Some((CODE, AddressRange::new(0x0, 0x3)))
        );
        assert_eq!(
            reloaded.note_location(&second.id),
            Some((CODE, AddressRange::new(0x3, 0x5)))
        );

        // The listing (which excludes notes) is unchanged.
        assert_eq!(reloaded.to_sdas(), db.to_sdas());
    }

    #[test]
    fn map_bytes_command_undo() {
        // Mapping over existing bytes undoes back to the originals.
        let env = TestEnvironment::new()
            .with_file("test.bin", vec![1, 2, 3])
            .with_file("other.bin", vec![4, 5]);
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(
            boxed(MapBytes::new((CODE, 0), "test.bin", 0usize, 3u32)),
            Some(&env),
        )
        .unwrap();

        let undo = db
            .apply(
                boxed(MapBytes::new((CODE, 0), "other.bin", 0usize, 2u32)),
                Some(&env),
            )
            .unwrap();
        assert_eq!(db.region(CODE).unwrap().bytes_at(0, 2), vec![4, 5]);

        apply_all(&mut db, undo, &env);
        assert_eq!(db.region(CODE).unwrap().bytes_at(0, 3), vec![1, 2, 3]);
    }

    #[test]
    fn unmap_bytes_command_undo() {
        let (mut db, env) = mapped("t.bin", &[1, 2, 3, 4, 5]);
        let undo = db
            .apply(boxed(UnmapBytes::new((CODE, 1..3))), None)
            .unwrap();
        assert_eq!(db.region(CODE).unwrap().bytes_at(0, 5), vec![1, 4, 5]);

        apply_all(&mut db, undo, &env);
        assert_eq!(db.region(CODE).unwrap().bytes_at(0, 5), vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn constant_bytes_undo() {
        let (mut db, env) = mapped("t.bin", &[1, 2, 3]);
        let undo = db
            .apply(boxed(SetConstantBytes::new((CODE, 0..2), 0xFF)), None)
            .unwrap();
        assert_eq!(db.region(CODE).unwrap().bytes_at(0, 3), vec![0xFF, 0xFF, 3]);

        apply_all(&mut db, undo, &env);
        assert_eq!(db.region(CODE).unwrap().bytes_at(0, 3), vec![1, 2, 3]);
    }

    #[test]
    fn sweep_undo_removes_code() {
        let (mut db, env) = mapped("t.bin", &TEST_BINARY);
        let undo = db
            .apply(boxed(AutoDisassemble::new((CODE, 0))), None)
            .unwrap();

        // Derived code, so the undo is just the root-clear. Nothing to un-set.
        assert!(db.space_usage(CODE).code > 0);
        assert!(db.region(CODE).unwrap().is_auto_root(0));
        assert_eq!(undo.len(), 1);
        assert_eq!(undo[0].name(), "clear_auto_disassemble_root");

        apply_all(&mut db, undo, &env);
        assert_eq!(db.space_usage(CODE).code, 0);
        assert!(!db.region(CODE).unwrap().is_auto_root(0));
    }

    #[test]
    fn sweep_exports_as_root() {
        // MOV A,#1 / INC A / SJMP back: a self-contained loop.
        let (mut db, env) = mapped("loop.bin", &[0x74, 0x01, 0x04, 0x80, 0xFB]);
        // The region method (which library callers use) must record a root.
        assert!(db.region_mut(CODE).auto_disassemble(0).is_success());
        assert!(db.region(CODE).unwrap().is_auto_root(0));

        let dsl = crate::store::to_dsl_many(&db.to_commands());
        assert!(dsl.contains("auto_disassemble(address=CODE:0x0)"), "{dsl}");
        assert!(!dsl.contains("disassemble_range"), "{dsl}");

        assert_eq!(reload(&db, &env).to_sdas(), db.to_sdas());
    }

    #[test]
    fn barrier_chops_sweep() {
        use crate::commands::MarkUnknown;

        // MOV A,#1 / INC A / NOP / RET: a straight-line run.
        let (mut db, env) = mapped("b.bin", &[0x74, 0x01, 0x04, 0x00, 0x22]);
        // Barrier at 0x3, set before disassembling.
        db.apply(boxed(MarkUnknown::new((CODE, 0x3u32..0x4u32))), None)
            .unwrap();
        db.apply(boxed(AutoDisassemble::new((CODE, 0u32))), None)
            .unwrap();

        // Flow stops at the barrier: 0x3 stays unknown, 0x4 is never reached.
        let region = db.region(CODE).unwrap();
        assert_eq!(region.get_equivalent_kind(0x0), Some(EquivalentKind::Code));
        assert_eq!(region.get_equivalent_kind(0x2), Some(EquivalentKind::Code));
        assert_eq!(
            region.get_equivalent_kind(0x3),
            Some(EquivalentKind::Unknown)
        );
        assert_eq!(region.get_equivalent_kind(0x4), None);

        assert!(
            crate::store::to_dsl_many(&db.to_commands())
                .contains("mark_unknown(range=CODE:0x3..0x4)"),
            "barrier is exported as a verb"
        );
        assert_eq!(reload(&db, &env).to_sdas(), db.to_sdas());
    }

    #[test]
    fn barrier_chops_retroactively() {
        use crate::commands::MarkUnknown;

        // MOV A,#1 / INC A / NOP / RET: a straight-line run.
        let (mut db, _env) = mapped("b.bin", &[0x74, 0x01, 0x04, 0x00, 0x22]);
        // Disassemble the whole run first.
        db.apply(boxed(AutoDisassemble::new((CODE, 0u32))), None)
            .unwrap();
        let region = db.region(CODE).unwrap();
        assert_eq!(region.get_equivalent_kind(0x3), Some(EquivalentKind::Code));
        assert_eq!(region.get_equivalent_kind(0x4), Some(EquivalentKind::Code));

        // Drop a barrier mid-flow. Derived code re-derives, so 0x3 onward vanish.
        db.apply(boxed(MarkUnknown::new((CODE, 0x3u32..0x4u32))), None)
            .unwrap();
        let region = db.region(CODE).unwrap();
        assert_eq!(region.get_equivalent_kind(0x0), Some(EquivalentKind::Code));
        assert_eq!(region.get_equivalent_kind(0x2), Some(EquivalentKind::Code));
        assert_eq!(
            region.get_equivalent_kind(0x3),
            Some(EquivalentKind::Unknown)
        );
        assert_eq!(region.get_equivalent_kind(0x4), None);
    }

    #[test]
    fn extents_coalesce_on_export() {
        use crate::commands::{DisassembleRange, MarkData};

        // MOV A,#1 / INC A (a 3-byte code block), then 2 data bytes.
        let (mut db, env) = mapped("m.bin", &[0x74, 0x01, 0x04, 0xAA, 0xBB]);
        db.apply(
            boxed(DisassembleRange::new((CODE, 0u32..3u32), false)),
            None,
        )
        .unwrap();
        db.apply(
            boxed(MarkData::new((CODE, 3u32..5u32), DataType::Byte)),
            None,
        )
        .unwrap();

        let dsl = crate::store::to_dsl_many(&db.to_commands());
        assert!(
            dsl.contains("disassemble_range(range=CODE:0x0..0x3, force=True)"),
            "code island coalesced: {dsl}"
        );
        assert!(
            dsl.contains("mark_data(range=CODE:0x3..0x5, data_type=DataType::Byte)"),
            "data as a verb: {dsl}"
        );
        assert!(
            !dsl.contains("set_equivalent"),
            "no low-level command: {dsl}"
        );

        assert_eq!(reload(&db, &env).to_sdas(), db.to_sdas());
    }

    #[test]
    fn override_operand_undoes() {
        use crate::commands::{DisassembleRange, OverrideOperand};
        use crate::db::OperandOverride;

        // CJNE A,0x20,rel: three operands. We override the third.
        let (mut db, env) = mapped("b.bin", &[0xB5, 0x20, 0x10]);
        db.apply(boxed(DisassembleRange::new((CODE, 0u32..3u32), true)), None)
            .unwrap();
        let undo = db
            .apply(
                boxed(OverrideOperand::new(
                    (CODE, 0u32),
                    2u8,
                    Some(OperandOverride::Text("HOT".into())),
                )),
                None,
            )
            .unwrap();
        assert!(db.to_sdas().contains("HOT"), "{}", db.to_sdas());

        assert!(crate::store::to_dsl_many(&db.to_commands()).contains("override_operand("));
        assert_eq!(reload(&db, &env).to_sdas(), db.to_sdas());

        // Undo clears the override.
        apply_all(&mut db, undo, &env);
        assert!(!db.to_sdas().contains("HOT"), "{}", db.to_sdas());
    }

    #[test]
    fn clear_labels_undo_restores() {
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        let code = db.region_mut(CODE);
        code.set_label(0x10, "a", LabelAttrs::default());
        code.set_label(0x14, "b", LabelAttrs::default());
        code.set_label(0x20, "c", LabelAttrs::default());

        // Clear a single range covering the first two labels in one command.
        let mut set = SpaceAddressSet::new(CODE);
        set.insert(0x10..0x18);
        let undo = db.apply(boxed(ClearLabel::new(set)), None).unwrap();

        let code = db.region(CODE).unwrap();
        assert_eq!(code.get_label(0x10), None);
        assert_eq!(code.get_label(0x14), None);
        assert_eq!(code.get_label(0x20), Some("c")); // outside the set, untouched

        // The undo restores both cleared labels.
        apply_all(&mut db, undo, &TestEnvironment::new());
        let code = db.region(CODE).unwrap();
        assert_eq!(code.get_label(0x10), Some("a"));
        assert_eq!(code.get_label(0x14), Some("b"));
    }

    #[test]
    fn refusal_names_restore_bytes() {
        let err = crate::db::already_classified(
            (CODE, 0xb6eu32).into(),
            EquivalentKind::Data,
            0xad5,
            0x1000,
            0xb72,
        );
        assert!(matches!(
            err.what,
            ErrorKind::AlreadyClassified { cleared: 0x52b, .. }
        ));
        let text = err.to_string();
        assert!(
            text.contains("clear_equivalents(addresses=CODE:{0xad5..0x1000})"),
            "the clear has to be named: {text}"
        );
        assert!(
            text.contains("mark_data(range=CODE:0xad5..0xb6e"),
            "the bytes before the request have to be restorable: {text}"
        );
        assert!(
            text.contains("mark_data(range=CODE:0xb72..0x1000"),
            "the bytes after the request have to be restorable: {text}"
        );
    }

    #[test]
    fn refusals_name_unblocking_command() {
        let occupied = crate::db::already_classified(
            (CODE, 0x8u32).into(),
            EquivalentKind::Code,
            0x8,
            0xA,
            0xA,
        );
        assert!(matches!(
            &occupied.what,
            ErrorKind::AlreadyClassified { marked, .. } if marked == "code"
        ));
        let text = occupied.to_string();
        assert!(
            text.contains("clear_equivalents(addresses=CODE:{0x8..0xa})"),
            "the message must carry a runnable command: {text}"
        );
        // Request and equivalent coincide, so there is no remainder to restore.
        assert!(!text.contains("mark_data"), "{text}");

        let unmapped = crate::db::nothing_mapped(CODE, 0x8);
        assert!(matches!(
            &unmapped.what,
            ErrorKind::NothingMapped { at } if at == "CODE:0x8"
        ));
    }
}
