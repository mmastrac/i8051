use std::collections::BTreeMap;
use std::range::Range;

use serde::{Deserialize, Serialize};

use crate::address::{AddressSpace, AddressValue, PhysicalAddr, Xref};
use crate::platform::{Platform, PlatformRef};
use crate::commands::{Command, Environment, SetCpu, SetNote, boxed};
use crate::labels::{ImplicitLabels, LabelCollector};
pub use crate::note::{
    Note, NoteAddressIndex, NoteDb, NoteField, NoteGlobalIndex, NoteId, NotePath, Notes,
    ProximateNote,
};
pub use crate::region::{Block, ByteRange, Region};
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
            .collect()
    }

    pub fn xrefs_from(&self, source: &PhysicalAddr) -> Vec<Xref> {
        let Some(region) = self.regions.get(&source.space) else {
            return Vec::new();
        };
        region.xrefs_from(source)
    }

    /// The control-flow graph of the routine rooted at `entry` in `space`.
    pub fn basic_blocks(&self, space: AddressSpace, entry: AddressValue) -> Vec<Block> {
        self.region(space)
            .map(|region| region.basic_blocks(entry))
            .unwrap_or_default()
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
        label_collector.into_implicit_labels()
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
            for line in region.render(space, &implicit_labels) {
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
            commands.extend(region.to_commands(space));
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
    /// `set_cpu` named a CPU with no built-in driver.
    UnknownCpu(String),
    Overlap(AddressValue),
    InvalidAddress(AddressValue),
    InvalidEquivalent,
    NotUndefined(AddressValue),
    Io(std::io::Error),
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::io;

    use super::*;
    use crate::address::SpaceAddressSet;
    use crate::address::XrefType;
    use crate::platform::i8051::CODE;
    use crate::commands::{
        AutoDisassemble, ClearBytes, ClearLabel, Command, MapBytes, SetConstantBytes, boxed,
    };
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

    fn make_test_db() -> Db {
        let mut db = Db::with_platform(crate::platform::i8051::platform());

        let code = db.region_mut(CODE);
        code.set_bytes("test.bin", 0, 0, &TEST_BINARY);

        code.set_label(0, "start");
        code.set_equivalent(0, Equivalent::Code).unwrap();

        code.set_comment(3, "Start of loop");
        code.set_label(3, "loop");
        code.set_equivalent(3, Equivalent::Code).unwrap();
        code.set_equivalent(5, Equivalent::Code).unwrap();
        code.set_equivalent(6, Equivalent::Code).unwrap();
        code.set_equivalent(9, Equivalent::Code).unwrap();
        code.set_equivalent(10, Equivalent::Code).unwrap();
        db
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

    #[test]
    fn test_db_to_commands() {
        let db = make_test_db();
        let commands = db.to_commands();
        let env = TestEnvironment::new().with_file("test.bin", TEST_BINARY.to_vec());
        let mut new_db = Db::new();
        for command in commands {
            let env = (command.name() == "map_bytes").then_some(&env as &dyn Environment);
            new_db.apply(command, env).expect("command should apply");
        }
        assert_eq!(new_db.to_sdas(), db.to_sdas());
    }

    #[test]
    fn to_commands_round_trips_notes_through_dsl() {
        use crate::commands::SetNote;
        use crate::store::{from_dsl_many, to_dsl_many};

        let env = TestEnvironment::new().with_file("test.bin", TEST_BINARY.to_vec());
        let mut db = make_test_db();

        // Attach two notes (one tagged) to address ranges.
        let mut first = Note::new(None, "reset handler");
        first.tags.insert("entry".into());
        let second = Note::new(Some(&first.id), "jump table");
        db.apply(
            boxed(SetNote::new((CODE, 0x0..0x3), first.clone())),
            None,
        )
        .unwrap();
        db.apply(
            boxed(SetNote::new((CODE, 0x3..0x5), second.clone())),
            None,
        )
        .unwrap();

        // Export the whole DB to canonical DSL, then reload from scratch.
        let dsl = to_dsl_many(&db.to_commands());
        assert!(dsl.contains("set_note("), "notes must be exported");
        let mut reloaded = Db::new();
        for command in from_dsl_many(&dsl).unwrap() {
            let env = (command.name() == "map_bytes").then_some(&env as &dyn Environment);
            reloaded.apply(command, env).expect("command should apply");
        }

        // Both notes survived with their ids, content, tags, and locations.
        assert_eq!(reloaded.notes.notes.len(), 2);
        assert_eq!(reloaded.notes.get(&first.id), Some(&first));
        assert_eq!(reloaded.notes.get(&second.id), Some(&second));
        assert_eq!(
            reloaded.note_location(&first.id),
            Some((
                CODE,
                crate::address::AddressRange::new(0x0, 0x3)
            ))
        );
        assert_eq!(
            reloaded.note_location(&second.id),
            Some((
                CODE,
                crate::address::AddressRange::new(0x3, 0x5)
            ))
        );

        // The listing (which excludes notes) is unchanged.
        assert_eq!(reloaded.to_sdas(), db.to_sdas());
    }

    #[test]
    fn test_db_space_usage() {
        let db = make_test_db();
        assert_eq!(
            db.space_usage(CODE),
            SpaceUsage {
                code: 12,
                data: 0,
                undefined: 0,
            }
        );
    }

    #[test]
    fn map_bytes_command_undo() {
        let env = TestEnvironment::new()
            .with_file("test.bin", vec![1, 2, 3])
            .with_file("other.bin", vec![4, 5]);
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(
            boxed(MapBytes::new(
                (CODE, 0),
                "test.bin",
                0usize,
                3u32,
            )),
            Some(&env),
        )
        .unwrap();

        let code = db.region(CODE).unwrap();
        assert_eq!(code.bytes_at(0, 3), vec![1, 2, 3]);

        let undo = db
            .apply(
                boxed(MapBytes::new(
                    (CODE, 0),
                    "other.bin",
                    0usize,
                    2u32,
                )),
                Some(&env),
            )
            .unwrap();
        assert_eq!(
            db.region(CODE).unwrap().bytes_at(0, 2),
            vec![4, 5]
        );

        apply_all(&mut db, undo, &env);
        assert_eq!(
            db.region(CODE).unwrap().bytes_at(0, 3),
            vec![1, 2, 3]
        );
    }

    #[test]
    fn clear_bytes_command_undo() {
        let env = TestEnvironment::new().with_file("test.bin", vec![1, 2, 3, 4, 5]);
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(
            boxed(MapBytes::new(
                (CODE, 0),
                "test.bin",
                0usize,
                5u32,
            )),
            Some(&env),
        )
        .unwrap();

        let undo = db
            .apply(boxed(ClearBytes::new((CODE, 1..3))), None)
            .unwrap();
        assert_eq!(
            db.region(CODE).unwrap().bytes_at(0, 5),
            vec![1, 4, 5]
        );

        apply_all(&mut db, undo, &env);
        assert_eq!(
            db.region(CODE).unwrap().bytes_at(0, 5),
            vec![1, 2, 3, 4, 5]
        );
    }

    #[test]
    fn set_constant_bytes_command_undo() {
        let env = TestEnvironment::new().with_file("test.bin", vec![1, 2, 3]);
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(
            boxed(MapBytes::new(
                (CODE, 0),
                "test.bin",
                0usize,
                3u32,
            )),
            Some(&env),
        )
        .unwrap();

        let undo = db
            .apply(
                boxed(SetConstantBytes::new((CODE, 0..2), 0xFF)),
                None,
            )
            .unwrap();
        assert_eq!(
            db.region(CODE).unwrap().bytes_at(0, 3),
            vec![0xFF, 0xFF, 3]
        );

        apply_all(&mut db, undo, &env);
        assert_eq!(
            db.region(CODE).unwrap().bytes_at(0, 3),
            vec![1, 2, 3]
        );
    }

    #[test]
    fn auto_disassemble_undo_removes_root_and_derived_code() {
        let env = TestEnvironment::new().with_file("test.bin", TEST_BINARY.to_vec());
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(
            boxed(MapBytes::new(
                (CODE, 0),
                "test.bin",
                0usize,
                TEST_BINARY.len() as AddressValue,
            )),
            Some(&env),
        )
        .unwrap();

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
    fn auto_disassemble_exports_as_root_and_round_trips() {
        use crate::store::{from_dsl_many, to_dsl_many};

        // MOV A,#1 / INC A / SJMP back: a self-contained loop.
        const BYTES: [u8; 5] = [0x74, 0x01, 0x04, 0x80, 0xFB];
        let env = TestEnvironment::new().with_file("loop.bin", BYTES.to_vec());
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(
            boxed(MapBytes::new(
                (CODE, 0),
                "loop.bin",
                0usize,
                5u32,
            )),
            Some(&env),
        )
        .unwrap();
        // The region method (which library callers use) must record a root.
        assert!(
            db.region_mut(CODE)
                .auto_disassemble(0)
                .is_success()
        );
        assert!(db.region(CODE).unwrap().is_auto_root(0));

        let dsl = to_dsl_many(&db.to_commands());
        assert!(dsl.contains("auto_disassemble(address=CODE:0x0)"), "{dsl}");
        assert!(!dsl.contains("disassemble_range"), "{dsl}");

        let mut reloaded = Db::new();
        for command in from_dsl_many(&dsl).unwrap() {
            let env = (command.name() == "map_bytes").then_some(&env as &dyn Environment);
            reloaded.apply(command, env).unwrap();
        }
        assert_eq!(reloaded.to_sdas(), db.to_sdas());
    }

    #[test]
    fn unknown_equivalent_chops_auto_disassembly_and_round_trips() {
        use crate::commands::MarkUnknown;
        use crate::store::{from_dsl_many, to_dsl_many};

        // MOV A,#1 / INC A / NOP / RET: a straight-line run.
        const BYTES: [u8; 5] = [0x74, 0x01, 0x04, 0x00, 0x22];
        let env = TestEnvironment::new().with_file("b.bin", BYTES.to_vec());
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(
            boxed(MapBytes::new(
                (CODE, 0),
                "b.bin",
                0usize,
                BYTES.len() as AddressValue,
            )),
            Some(&env),
        )
        .unwrap();
        // Barrier at 0x3, set before disassembling.
        db.apply(
            boxed(MarkUnknown::new((CODE, 0x3u32..0x4u32))),
            None,
        )
        .unwrap();
        db.apply(
            boxed(AutoDisassemble::new((CODE, 0u32))),
            None,
        )
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

        let dsl = to_dsl_many(&db.to_commands());
        assert!(
            dsl.contains("mark_unknown(range=CODE:0x3..0x4)"),
            "barrier is exported as a verb: {dsl}"
        );
        let mut reloaded = Db::new();
        for command in from_dsl_many(&dsl).unwrap() {
            let env = (command.name() == "map_bytes").then_some(&env as &dyn Environment);
            reloaded.apply(command, env).expect("command should apply");
        }
        assert_eq!(reloaded.to_sdas(), db.to_sdas());
    }

    #[test]
    fn unknown_barrier_retroactively_chops_weak_code() {
        use crate::commands::MarkUnknown;

        // MOV A,#1 / INC A / NOP / RET: a straight-line run.
        const BYTES: [u8; 5] = [0x74, 0x01, 0x04, 0x00, 0x22];
        let env = TestEnvironment::new().with_file("b.bin", BYTES.to_vec());
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(
            boxed(MapBytes::new(
                (CODE, 0),
                "b.bin",
                0usize,
                BYTES.len() as AddressValue,
            )),
            Some(&env),
        )
        .unwrap();
        // Disassemble the whole run first.
        db.apply(
            boxed(AutoDisassemble::new((CODE, 0u32))),
            None,
        )
        .unwrap();
        let region = db.region(CODE).unwrap();
        assert_eq!(region.get_equivalent_kind(0x3), Some(EquivalentKind::Code));
        assert_eq!(region.get_equivalent_kind(0x4), Some(EquivalentKind::Code));

        // Drop a barrier mid-flow. Derived code re-derives, so 0x3 onward vanish.
        db.apply(
            boxed(MarkUnknown::new((CODE, 0x3u32..0x4u32))),
            None,
        )
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
    fn extent_commands_round_trip_and_coalesce_on_export() {
        use crate::commands::{DisassembleRange, MarkData};
        use crate::store::{from_dsl_many, to_dsl_many};

        // MOV A,#1 / INC A (a 3-byte code block), then 2 data bytes.
        let env = TestEnvironment::new().with_file("m.bin", vec![0x74, 0x01, 0x04, 0xAA, 0xBB]);
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(
            boxed(MapBytes::new(
                (CODE, 0),
                "m.bin",
                0usize,
                5u32,
            )),
            Some(&env),
        )
        .unwrap();
        db.apply(
            boxed(DisassembleRange::new((CODE, 0u32..3u32))),
            None,
        )
        .unwrap();
        db.apply(
            boxed(MarkData::new(
                (CODE, 3u32..5u32),
                DataType::Byte,
            )),
            None,
        )
        .unwrap();

        let dsl = to_dsl_many(&db.to_commands());
        assert!(
            dsl.contains("disassemble_range(range=CODE:0x0..0x3)"),
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

        let mut reloaded = Db::new();
        for command in from_dsl_many(&dsl).unwrap() {
            let env = (command.name() == "map_bytes").then_some(&env as &dyn Environment);
            reloaded.apply(command, env).expect("command should apply");
        }
        assert_eq!(reloaded.to_sdas(), db.to_sdas());
    }

    #[test]
    fn override_operand_round_trips_and_undoes() {
        use crate::commands::{DisassembleRange, OverrideOperand};
        use crate::db::OperandOverride;
        use crate::store::{from_dsl_many, to_dsl_many};

        // CJNE A,0x20,rel: three operands. We override the third.
        let env = TestEnvironment::new().with_file("b.bin", vec![0xB5, 0x20, 0x10]);
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(
            boxed(MapBytes::new(
                (CODE, 0),
                "b.bin",
                0usize,
                3u32,
            )),
            Some(&env),
        )
        .unwrap();
        db.apply(
            boxed(DisassembleRange::new((CODE, 0u32..3u32))),
            None,
        )
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

        let dsl = to_dsl_many(&db.to_commands());
        assert!(dsl.contains("override_operand("), "{dsl}");
        let mut reloaded = Db::new();
        for command in from_dsl_many(&dsl).unwrap() {
            let env = (command.name() == "map_bytes").then_some(&env as &dyn Environment);
            reloaded.apply(command, env).expect("command should apply");
        }
        assert_eq!(reloaded.to_sdas(), db.to_sdas());

        // Undo clears the override.
        apply_all(&mut db, undo, &env);
        assert!(!db.to_sdas().contains("HOT"), "{}", db.to_sdas());
    }

    #[test]
    fn clear_label_set_clears_range_and_undo_restores() {
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        let code = db.region_mut(CODE);
        code.set_label(0x10, "a");
        code.set_label(0x14, "b");
        code.set_label(0x20, "c");

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
}
