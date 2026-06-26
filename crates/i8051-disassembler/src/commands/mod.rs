/// Assert a command payload round-trips through the DSL in both directions:
/// that it renders to exactly `$dsl`, and that `$dsl` parses back to it. The
/// payload's struct name doubles as the [`Command`] variant. Emits a `#[test]`
/// named `$name`.
///
/// ```ignore
/// serialize_test!(
///     auto_disassemble,
///     "auto_disassemble(address=CODE:0x1234)",
///     AutoDisassemble { address: (AddressSpace::Code, 0x1234) }
/// );
/// ```
macro_rules! serialize_test {
    ($name:ident, $dsl:expr, $variant:ident $body:tt $(,)?) => {
        #[cfg(test)]
        #[test]
        fn $name() {
            let command = $crate::commands::Command::$variant($crate::commands::$variant $body);
            assert_eq!($crate::store::to_dsl(&command), $dsl, "Rust -> DSL");
            assert_eq!(
                $crate::store::from_dsl($dsl).expect("DSL -> Rust"),
                command,
                "DSL -> Rust",
            );
        }
    };
}

mod auto_disassemble;
mod bytes;
mod comment;
mod equivalent;
mod function;
mod label;
mod note;

use std::io;

pub use auto_disassemble::AutoDisassemble;
pub use bytes::{ClearBytes, MapBytes, SetConstantBytes};
pub use comment::{ClearComment, SetComment};
pub use equivalent::{ClearEquivalents, SetEquivalent};
pub use function::{ClearFunction, SetFunction};
pub use label::{ClearLabel, SetLabel};
pub use note::{ClearNote, SetNote};

use crate::address::{AddressRange, AddressSpace, AddressValue};
use crate::db::{Db, Equivalent, Error, Function, Note};

pub trait Environment {
    fn load_file_bytes(
        &self,
        file: &str,
        offset: usize,
        size: AddressValue,
    ) -> Result<Vec<u8>, io::Error>;
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Command {
    SetLabel(SetLabel),
    ClearLabel(ClearLabel),
    SetEquivalent(SetEquivalent),
    ClearEquivalents(ClearEquivalents),
    AutoDisassemble(AutoDisassemble),
    SetComment(SetComment),
    ClearComment(ClearComment),
    MapBytes(MapBytes),
    ClearBytes(ClearBytes),
    SetConstantBytes(SetConstantBytes),
    SetFunction(SetFunction),
    ClearFunction(ClearFunction),
    SetNote(SetNote),
    ClearNote(ClearNote),
}

impl Command {
    pub fn set_label(space: AddressSpace, offset: AddressValue, label: impl Into<String>) -> Self {
        Self::SetLabel(SetLabel {
            address: (space, offset),
            label: label.into(),
        })
    }

    pub fn clear_label(space: AddressSpace, offset: AddressValue) -> Self {
        Self::ClearLabel(ClearLabel {
            address: (space, offset),
        })
    }

    pub fn auto_disassemble(space: AddressSpace, start: AddressValue) -> Self {
        Self::AutoDisassemble(AutoDisassemble {
            address: (space, start),
        })
    }

    pub fn set_equivalent(
        space: AddressSpace,
        offset: AddressValue,
        equivalent: Equivalent,
    ) -> Self {
        Self::SetEquivalent(SetEquivalent {
            address: (space, offset),
            equivalent,
        })
    }

    pub fn clear_equivalents(
        space: AddressSpace,
        offset: AddressValue,
        size: AddressValue,
    ) -> Self {
        Self::ClearEquivalents(ClearEquivalents {
            range: (space, AddressRange::new(offset, offset + size)),
        })
    }

    pub fn set_comment(
        space: AddressSpace,
        offset: AddressValue,
        comment: impl Into<String>,
    ) -> Self {
        Self::SetComment(SetComment {
            address: (space, offset),
            comment: comment.into(),
        })
    }

    pub fn clear_comment(space: AddressSpace, offset: AddressValue) -> Self {
        Self::ClearComment(ClearComment {
            address: (space, offset),
        })
    }

    pub fn map_bytes(
        space: AddressSpace,
        offset: AddressValue,
        file: impl Into<String>,
        file_offset: usize,
        size: AddressValue,
    ) -> Self {
        Self::MapBytes(MapBytes {
            address: (space, offset),
            file: file.into(),
            file_offset,
            size,
        })
    }

    pub fn clear_bytes(space: AddressSpace, offset: AddressValue, size: AddressValue) -> Self {
        Self::ClearBytes(ClearBytes {
            range: (space, AddressRange::new(offset, offset + size)),
        })
    }

    pub fn set_constant_bytes(
        space: AddressSpace,
        offset: AddressValue,
        size: AddressValue,
        value: u8,
    ) -> Self {
        Self::SetConstantBytes(SetConstantBytes {
            range: (space, AddressRange::new(offset, offset + size)),
            value,
        })
    }

    pub fn set_function(space: AddressSpace, offset: AddressValue, function: Function) -> Self {
        Self::SetFunction(SetFunction {
            address: (space, offset),
            function,
        })
    }

    pub fn clear_function(space: AddressSpace, offset: AddressValue) -> Self {
        Self::ClearFunction(ClearFunction {
            address: (space, offset),
        })
    }

    pub fn set_note(space: AddressSpace, range: AddressRange, note: Note) -> Self {
        Self::SetNote(SetNote {
            address: (space, range),
            note,
        })
    }

    pub fn apply(
        self,
        db: &mut Db,
        env: Option<&dyn Environment>,
    ) -> Result<Vec<Command>, Error> {
        match self {
            Command::SetLabel(cmd) => cmd.apply(db, env),
            Command::ClearLabel(cmd) => cmd.apply(db, env),
            Command::SetEquivalent(cmd) => cmd.apply(db, env),
            Command::ClearEquivalents(cmd) => cmd.apply(db, env),
            Command::AutoDisassemble(cmd) => cmd.apply(db, env),
            Command::SetComment(cmd) => cmd.apply(db, env),
            Command::ClearComment(cmd) => cmd.apply(db, env),
            Command::MapBytes(cmd) => cmd.apply(db, env),
            Command::ClearBytes(cmd) => cmd.apply(db, env),
            Command::SetConstantBytes(cmd) => cmd.apply(db, env),
            Command::SetFunction(cmd) => cmd.apply(db, env),
            Command::ClearFunction(cmd) => cmd.apply(db, env),
            Command::SetNote(cmd) => cmd.apply(db, env),
            Command::ClearNote(cmd) => cmd.apply(db, env),
        }
    }
}

