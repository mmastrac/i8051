use std::collections::BTreeMap;
use std::range::Range;

use serde::{Deserialize, Serialize};

use crate::address::{AREA_ORDER, AddressSpace, AddressValue, PhysicalAddr, Xref};
use crate::command::{Command, Environment};
use crate::labels::{ImplicitLabels, LabelCollector};
pub use crate::region::{ByteRange, Region};

pub struct Db {
    regions: BTreeMap<AddressSpace, Region>,
}

impl Db {
    pub fn new() -> Self {
        Self {
            regions: BTreeMap::new(),
        }
    }

    pub fn region(&self, space: AddressSpace) -> Option<&Region> {
        self.regions.get(&space)
    }

    pub fn region_mut(&mut self, space: AddressSpace) -> &mut Region {
        self.regions.entry(space).or_insert_with(Region::new)
    }

    pub fn xrefs_to(&self, target: &PhysicalAddr) -> Vec<Xref> {
        let mut xrefs = Vec::new();
        for (&space, region) in &self.regions {
            xrefs.extend(region.xrefs_to(space, target));
        }
        xrefs
    }

    pub fn xrefs_from(&self, source: &PhysicalAddr) -> Vec<Xref> {
        let Some(region) = self.regions.get(&source.space) else {
            return Vec::new();
        };
        region.xrefs_from(source)
    }

    fn implicit_labels(&self) -> ImplicitLabels {
        let mut label_collector = LabelCollector::default();
        for (&space, region) in &self.regions {
            region.collect_refs(space, &mut label_collector);
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
        let mut s = String::new();
        let implicit_labels = self.implicit_labels();

        for &space in &AREA_ORDER {
            let Some(region) = self.regions.get(&space) else {
                continue;
            };
            s.push_str(space.area_header());
            for line in region.render(space, &implicit_labels) {
                s.push_str(&line.to_sdas());
            }
        }
        s
    }

    pub fn to_commands(&self) -> Vec<Command> {
        let mut commands = Vec::new();
        for (&space, region) in &self.regions {
            commands.extend(region.to_commands(space));
        }
        commands
    }

    pub fn apply(
        &mut self,
        command: Command,
        env: Option<&dyn Environment>,
    ) -> Result<Vec<Command>, Error> {
        command.apply(self, env)
    }

    /// Byte counts for mapped content classified by equivalent kind.
    pub fn space_usage(&self, space: AddressSpace) -> SpaceUsage {
        self.regions
            .get(&space)
            .map(Region::space_usage)
            .unwrap_or_default()
    }
}

impl Default for Db {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub enum Line {
    Org {
        addr: AddressValue,
    },
    Blank,
    Comment {
        addr: AddressValue,
        text: String,
    },
    Label {
        addr: AddressValue,
        name: String,
    },
    Instruction {
        addr: AddressValue,
        text: String,
        bytes: Vec<u8>,
    },
    Data {
        addr: AddressValue,
        data_type: DataType,
        bytes: Vec<u8>,
    },
    Raw {
        addr: AddressValue,
        bytes: Vec<u8>,
    },
    Function {
        addr: AddressValue,
        name: String,
        signature: Option<String>,
        length: AddressValue,
        noreturn: bool,
    },
}

impl Line {
    pub fn addr(&self) -> AddressValue {
        match self {
            Self::Org { addr, .. }
            | Self::Comment { addr, .. }
            | Self::Label { addr, .. }
            | Self::Function { addr, .. }
            | Self::Instruction { addr, .. }
            | Self::Data { addr, .. }
            | Self::Raw { addr, .. } => *addr,
            Self::Blank => 0,
        }
    }

    pub fn to_sdas(&self) -> String {
        crate::sdas::line_to_sdas(self)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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
    Code(Vec<Option<OperandOverride>>),
    Data(DataType, AddressValue),
}

impl Equivalent {
    pub fn kind(&self) -> EquivalentKind {
        match self {
            Self::Code(_) => EquivalentKind::Code,
            Self::Data(_, _) => EquivalentKind::Data,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum EquivalentKind {
    Code,
    Data,
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

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
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
    use crate::address::XrefType;
    use crate::command::Command;
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

    fn apply_all(db: &mut Db, commands: Vec<Command>, env: &TestEnvironment) {
        for command in commands {
            db.apply(command, Some(env)).unwrap();
        }
    }

    fn make_test_db() -> Db {
        let mut db = Db::new();

        let code = db.region_mut(AddressSpace::Code);
        code.set_bytes("test.bin", 0, 0, &TEST_BINARY);

        code.set_label(0, "start");
        code.set_equivalent(0, Equivalent::Code(vec![])).unwrap();

        code.set_comment(3, "Start of loop");
        code.set_label(3, "loop");
        code.set_equivalent(3, Equivalent::Code(vec![])).unwrap();
        code.set_equivalent(5, Equivalent::Code(vec![])).unwrap();
        code.set_equivalent(6, Equivalent::Code(vec![])).unwrap();
        code.set_equivalent(9, Equivalent::Code(vec![])).unwrap();
        code.set_equivalent(10, Equivalent::Code(vec![])).unwrap();
        db
    }

    #[test]
    fn test_db() {
        let db = make_test_db();
        assert_eq!(
            db.xrefs_to(&PhysicalAddr {
                space: AddressSpace::Code,
                offset: 3
            }),
            vec![Xref {
                xref_type: XrefType::Jump,
                from: PhysicalAddr {
                    space: AddressSpace::Code,
                    offset: 10
                },
                to: PhysicalAddr {
                    space: AddressSpace::Code,
                    offset: 3
                },
            }]
        );

        assert_eq!(
            db.xrefs_from(&PhysicalAddr {
                space: AddressSpace::Code,
                offset: 10
            }),
            vec![Xref {
                xref_type: XrefType::Jump,
                from: PhysicalAddr {
                    space: AddressSpace::Code,
                    offset: 10
                },
                to: PhysicalAddr {
                    space: AddressSpace::Code,
                    offset: 3
                },
            }]
        );

        let expected = concat!(
            ".area CODE (CODE,ABS)\n",
            ".org 0x0\n",
            "\n",
            "start:\n",
            "    LJMP    loc_0010\n",
            "; Start of loop\n",
            "loop:\n",
            "    MOV     A,#01\n",
            "    MOVX    @DPTR,A\n",
            "    MOV     DPTR,#0x0010\n",
            "    MOVC    A,@A+DPTR\n",
            "    SJMP    loop\n",
            "loc_0010:\n"
        );
        assert_eq!(db.to_sdas(), expected);
    }

    #[test]
    fn test_db_to_commands() {
        let db = make_test_db();
        let commands = db.to_commands();
        let env = TestEnvironment::new().with_file("test.bin", TEST_BINARY.to_vec());
        let mut new_db = Db::new();
        for command in commands {
            let env =
                matches!(command, Command::MapBytes { .. }).then_some(&env as &dyn Environment);
            new_db.apply(command, env).expect("command should apply");
        }
        assert_eq!(new_db.to_sdas(), db.to_sdas());
    }

    #[test]
    fn test_db_space_usage() {
        let db = make_test_db();
        assert_eq!(
            db.space_usage(AddressSpace::Code),
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
        let mut db = Db::new();
        db.apply(
            Command::map_bytes(AddressSpace::Code, 0, "test.bin", 0, 3),
            Some(&env),
        )
        .unwrap();

        let code = db.region(AddressSpace::Code).unwrap();
        assert_eq!(code.bytes_at(0, 3), vec![1, 2, 3]);

        let undo = db
            .apply(
                Command::map_bytes(AddressSpace::Code, 0, "other.bin", 0, 2),
                Some(&env),
            )
            .unwrap();
        assert_eq!(
            db.region(AddressSpace::Code).unwrap().bytes_at(0, 2),
            vec![4, 5]
        );

        apply_all(&mut db, undo, &env);
        assert_eq!(
            db.region(AddressSpace::Code).unwrap().bytes_at(0, 3),
            vec![1, 2, 3]
        );
    }

    #[test]
    fn clear_bytes_command_undo() {
        let env = TestEnvironment::new().with_file("test.bin", vec![1, 2, 3, 4, 5]);
        let mut db = Db::new();
        db.apply(
            Command::map_bytes(AddressSpace::Code, 0, "test.bin", 0, 5),
            Some(&env),
        )
        .unwrap();

        let undo = db
            .apply(Command::clear_bytes(AddressSpace::Code, 1, 2), None)
            .unwrap();
        assert_eq!(
            db.region(AddressSpace::Code).unwrap().bytes_at(0, 5),
            vec![1, 4, 5]
        );

        apply_all(&mut db, undo, &env);
        assert_eq!(
            db.region(AddressSpace::Code).unwrap().bytes_at(0, 5),
            vec![1, 2, 3, 4, 5]
        );
    }

    #[test]
    fn set_constant_bytes_command_undo() {
        let env = TestEnvironment::new().with_file("test.bin", vec![1, 2, 3]);
        let mut db = Db::new();
        db.apply(
            Command::map_bytes(AddressSpace::Code, 0, "test.bin", 0, 3),
            Some(&env),
        )
        .unwrap();

        let undo = db
            .apply(
                Command::set_constant_bytes(AddressSpace::Code, 0, 2, 0xFF),
                None,
            )
            .unwrap();
        assert_eq!(
            db.region(AddressSpace::Code).unwrap().bytes_at(0, 3),
            vec![0xFF, 0xFF, 3]
        );

        apply_all(&mut db, undo, &env);
        assert_eq!(
            db.region(AddressSpace::Code).unwrap().bytes_at(0, 3),
            vec![1, 2, 3]
        );
    }
}
