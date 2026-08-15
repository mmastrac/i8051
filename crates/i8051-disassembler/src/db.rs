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
            _ => Err(Error::UnknownSpace {
                name: name.to_string(),
                suggestion: crate::commands::closest(name, spaces.iter().map(|s| s.dsl_name()))
                    .map(str::to_string),
            }),
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

    /// The CPU's entry points that are not decoded as code, with the name and
    /// reason for each.
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
                        && !matches!(r.get_equivalent_kind(e.offset), Some(EquivalentKind::Code))
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
            let names = region.export_names(
                implicit_labels.get(&space).unwrap_or(&Default::default()),
            );
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
            // ... then any disabled platform addresses
            for (offset, reason) in region.disabled_platform_addresses() {
                commands.push(boxed(crate::commands::DisablePlatformAddress {
                    address: (space, offset).into(),
                    reason: reason.to_string(),
                }));
            }
        }
        // Notes live outside the regions, so emit them separately or a DB would
        // not round-trip. Iterating by NoteId (Lamport order) is deterministic,
        // and SetNote carries the note's id, so a reload restores it unchanged.
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
        command.apply(self, env)
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

#[derive(Debug)]
pub enum Error {
    NoEnvironment,
    /// A disassembly command ran with no CPU selected (`set_cpu` must run first).
    NoCpu,
    /// `set_cpu` ran while a CPU was already selected.
    CpuAlreadySet {
        current: String,
    },
    /// `set_cpu` named a CPU with no built-in driver.
    UnknownCpu {
        name: String,
        suggestions: Vec<String>,
    },
    Overlap {
        at: SpaceAddressValue,
        existing: EquivalentKind,
    },
    /// Unmapping would have cut a classification that extends past the range.
    PartialEquivalent {
        at: SpaceAddressValue,
        existing: EquivalentKind,
        start: AddressValue,
        end: AddressValue,
    },
    /// A label was not a legal assembler symbol.
    InvalidLabel {
        label: String,
        reason: &'static str,
    },
    InvalidArgument {
        value: String,
        reason: &'static str,
    },
    /// An address space this database does not have.
    UnknownSpace {
        name: String,
        suggestion: Option<String>,
    },
    /// This address is not valid for this operation.
    InvalidAddress(SpaceAddressValue),
    InvalidEquivalent,
    /// This range is already classified.
    NotUndefined {
        at: SpaceAddressValue,
        existing: EquivalentKind,
        start: AddressValue,
        end: AddressValue,
        requested_end: AddressValue,
    },
    Io(std::io::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Overlap { at, existing } => {
                let kind = match existing {
                    EquivalentKind::Code => "code",
                    EquivalentKind::Data => "data",
                    EquivalentKind::Unknown => "barrier",
                };
                write!(
                    f,
                    "range overlaps existing {kind} at {}:0x{:x}",
                    at.space.dsl_name(),
                    at.offset
                )
            }
            Self::PartialEquivalent {
                at,
                existing,
                start,
                end,
            } => {
                let kind = match existing {
                    EquivalentKind::Code => "code",
                    EquivalentKind::Data => "data",
                    EquivalentKind::Unknown => "a barrier",
                };
                let space = at.space.dsl_name();
                write!(
                    f,
                    "unmapping would cut {kind} at {space}:0x{start:x}..0x{end:x}, which reaches \
                     past the bytes being unmapped: `clear_equivalents` first, or unmap a \
                     larger range"
                )
            }
            Self::InvalidLabel { label, reason } => {
                write!(f, "{label:?} is not a usable label: {reason}")
            }
            Self::InvalidArgument { value, reason } => {
                write!(f, "{value:?} is not usable here: {reason}")
            }
            Self::UnknownSpace { name, suggestion } => {
                write!(f, "unknown address space {name:?}")?;
                match suggestion {
                    Some(hint) => write!(f, " (did you mean `{hint}`?)"),
                    None => Ok(()),
                }
            }
            Self::CpuAlreadySet { current } => {
                write!(
                    f,
                    "CPU already set to {current:?} (use clear_cpu() before selecting another)"
                )
            }
            Self::UnknownCpu { name, suggestions } => {
                write!(f, "unknown CPU {name:?}")?;
                if !suggestions.is_empty() {
                    write!(f, ", did you mean: {}", suggestions.join(", "))?;
                }
                Ok(())
            }
            Self::NotUndefined { at, existing, start, end, requested_end } => {
                let kind = match existing {
                    EquivalentKind::Code => "code",
                    EquivalentKind::Data => "data",
                    EquivalentKind::Unknown => "a barrier",
                };
                let space = at.space.dsl_name();
                write!(
                    f,
                    "{space}:0x{:x} is already {kind}, covering {space}:0x{start:x}..0x{end:x}. \
                     Marking only applies to undefined bytes, so clear what is there first: \
                     clear_equivalents(addresses={space}:{{0x{start:x}..0x{end:x}}})",
                    at.offset
                )?;
                let covering = end.saturating_sub(*start);
                let asked = requested_end.saturating_sub(at.offset);
                if covering > asked {
                    write!(
                        f,
                        ". That clears all 0x{covering:x} byte(s), well past the 0x{asked:x} you \
                         asked about, so restore the remainder straight after"
                    )?;
                    if at.offset > *start {
                        write!(
                            f,
                            ": mark_data(range={space}:0x{start:x}..0x{:x}, \
                             data_type=DataType::Byte)",
                            at.offset
                        )?;
                    }
                    if requested_end < end {
                        write!(
                            f,
                            "{} mark_data(range={space}:0x{requested_end:x}..0x{end:x}, \
                             data_type=DataType::Byte)",
                            if at.offset > *start { " and" } else { ":" }
                        )?;
                    }
                }
                Ok(())
            }
            Self::InvalidAddress(at) => {
                let space = at.space.dsl_name();
                write!(
                    f,
                    "no byte is mapped at {space}:0x{:x}, so nothing there can be classified. \
                     Map it first: `map_bytes` to bring bytes in from the image file, or \
                     `set_constant_bytes` to fill the gap with a value and then classify again",
                    at.offset
                )
            }
            other => write!(f, "{other:?}"),
        }
    }
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
        db.apply(boxed(DisassembleRange::new((CODE, 0u32..3u32), false)), None)
            .unwrap();
        db.apply(
            boxed(MarkData::new((CODE, 3u32..5u32), DataType::Byte)),
            None,
        )
        .unwrap();

        let dsl = crate::store::to_dsl_many(&db.to_commands());
        assert!(
            dsl.contains("disassemble_range(force=False, range=CODE:0x0..0x3)"),
            "code island coalesced: {dsl}"
        );
        assert!(
            dsl.contains("mark_data(data_type=DataType::Byte, range=CODE:0x3..0x5)"),
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
        db.apply(boxed(DisassembleRange::new((CODE, 0u32..3u32), false)), None)
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
        let text = Error::NotUndefined {
            at: (CODE, 0xb6eu32).into(),
            existing: EquivalentKind::Data,
            start: 0xad5,
            end: 0x1000,
            requested_end: 0xb72,
        }
        .to_string();

        assert!(
            text.contains("clear_equivalents(addresses=CODE:{0xad5..0x1000})"),
            "the clear has to be named: {text}"
        );
        assert!(text.contains("0x52b"), "the real cost has to be stated: {text}");
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
        let occupied = Error::NotUndefined {
            at: (CODE, 0x8u32).into(),
            existing: EquivalentKind::Code,
            start: 0x8,
            end: 0xA,
            requested_end: 0xA,
        };
        let text = occupied.to_string();
        assert!(text.contains("already code"), "{text}");
        assert!(
            text.contains("clear_equivalents(addresses=CODE:{0x8..0xa})"),
            "the message must carry a runnable command: {text}"
        );
        // Request and equivalent coincide, so there is no remainder to mention.
        assert!(!text.contains("restore the remainder"), "{text}");

        let unmapped = Error::InvalidAddress((CODE, 0x8u32).into());
        let text = unmapped.to_string();
        assert!(text.contains("no byte is mapped at CODE:0x8"), "{text}");
        assert!(text.contains("map_bytes"), "{text}");
        assert!(text.contains("set_constant_bytes"), "the fill route is valid too: {text}");
    }
}
