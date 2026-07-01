/// Assert a command round-trips through the DSL in both directions: that it
/// renders to exactly `$dsl`, and that `$dsl` parses back to it. Emits a
/// `#[test]` named `$name`.
///
/// ```ignore
/// serialize_test!(
///     auto_disassemble,
///     "auto_disassemble(address=CODE:0x1234)",
///     AutoDisassemble { address: (AddressSpace::Code, 0x1234).into() }
/// );
/// ```
macro_rules! serialize_test {
    ($name:ident, $dsl:expr, $variant:ident $body:tt $(,)?) => {
        #[cfg(test)]
        #[test]
        fn $name() {
            let command: ::std::boxed::Box<dyn $crate::commands::Command> =
                ::std::boxed::Box::new($crate::commands::$variant $body);
            assert_eq!($crate::store::to_dsl(&*command), $dsl, "Rust -> DSL");
            let parsed = $crate::store::from_dsl($dsl).expect("DSL -> Rust");
            assert!(
                *parsed == *command,
                "DSL -> Rust: {parsed:?} != {command:?}",
            );
        }
    };
}

/// Register a command from its constructor's argument list.
///
/// Each `name: Type` becomes an `impl Into<Type>` parameter of a generated
/// `pub fn new(..) -> Self` (the struct is built field-by-field via `.into()`,
/// so argument names must match the payload's field names). This also:
/// - implements [`Command`] (forwarding `apply` to the [`Apply`] impl), and
/// - adds a [`CommandEntry`] — the constructor's doc, argument signature, and
///   DSL parser — to the link-time [`COMMANDS`] registry under the snake_case
///   form of the type name.
///
/// Each argument's `Type` is mapped to a semantic [`ArgKind`] via
/// [`CommandArgType`], so an MCP layer can enumerate commands and their
/// argument shapes without parsing raw Rust type strings.
///
/// ```ignore
/// register!(SetLabel(
///     /// Name the code `address` with `label`.
///     address: SpaceAddressValue,
///     label: String,
/// ));
/// ```
macro_rules! register {
    (
        $type:ident (
            $(#[doc = $doc:literal])*
            $($arg:ident : $argty:ty),* $(,)?
        )
    ) => {
        impl $type {
            const COMMAND_NAME: &'static str = {
                const SRC: &str = stringify!($type);
                const LEN: usize = $crate::commands::snake_len(SRC);
                const BYTES: [u8; LEN] = $crate::commands::snake_bytes::<LEN>(SRC);
                match ::core::str::from_utf8(&BYTES) {
                    ::core::result::Result::Ok(name) => name,
                    ::core::result::Result::Err(_) => ::core::panic!("non-ASCII command name"),
                }
            };

            $(#[doc = $doc])*
            pub fn new($($arg: impl ::core::convert::Into<$argty>),*) -> Self {
                Self { $($arg: $arg.into()),* }
            }
        }

        impl $crate::commands::Command for $type {
            fn apply(
                self: ::std::boxed::Box<Self>,
                db: &mut $crate::db::Db,
                env: ::core::option::Option<&dyn $crate::commands::Environment>,
            ) -> ::core::result::Result<
                ::std::vec::Vec<::std::boxed::Box<dyn $crate::commands::Command>>,
                $crate::db::Error,
            > {
                $crate::commands::Apply::apply(*self, db, env)
            }

            fn name(&self) -> &'static str {
                <$type>::COMMAND_NAME
            }

            fn to_value(&self) -> $crate::store::value::Value {
                $crate::store::ser::to_value(self).expect("command serialization is infallible")
            }

            fn as_any(&self) -> &dyn ::core::any::Any {
                self
            }
        }

        #[::scattered_collect::scatter($crate::commands::COMMANDS)]
        #[allow(non_upper_case_globals)]
        static $type: (&'static str, $crate::commands::CommandEntry) = (
            <$type>::COMMAND_NAME,
            $crate::commands::CommandEntry {
                doc: ::core::concat!($($doc, "\n",)*),
                args: &[$(
                    $crate::commands::CommandArg {
                        name: ::core::stringify!($arg),
                        ty: ::core::stringify!($argty),
                        kind: <$argty as $crate::commands::CommandArgType>::KIND,
                    }
                ),*],
                parse: $crate::commands::parse::<$type>,
            },
        );
    };
}

mod auto_disassemble;
mod bytes;
mod comment;
mod equivalent;
mod extent;
mod function;
mod label;
mod note;
mod operand;

use std::any::Any;
use std::collections::BTreeMap;
use std::io;

pub use auto_disassemble::{AutoDisassemble, ClearAutoDisassembleRoot};
pub use bytes::{ClearBytes, MapBytes, SetConstantBytes};
pub use comment::{ClearComment, SetComment};
pub use equivalent::ClearEquivalents;
pub use extent::{DisassembleRange, MarkData, MarkUnknown};
pub use function::{ClearFunction, SetFunction};
pub use label::{ClearLabel, SetLabel};
pub use note::{ClearNote, SetNote};
pub use operand::OverrideOperand;

use scattered_collect::{ScatteredMap, gather};
use serde::de::DeserializeOwned;

use crate::address::{AddressValue, SpaceAddressRange, SpaceAddressSet, SpaceAddressValue};
use crate::db::{DataType, Db, Error, Function, Note, NoteId, OperandOverride};
use crate::store::error::DslError;
use crate::store::value::Value;

pub trait Environment {
    fn load_file_bytes(
        &self,
        file: &str,
        offset: usize,
        size: AddressValue,
    ) -> Result<Vec<u8>, io::Error>;
}

/// A disassembly mutation.
pub trait Command: std::fmt::Debug {
    /// Apply the command, returning the inverse commands that undo it.
    fn apply(
        self: Box<Self>,
        db: &mut Db,
        env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error>;

    /// The snake_case name this command is spelled with in the DSL.
    fn name(&self) -> &'static str;

    /// The payload as a DSL [`Value`] (always a [`Value::Struct`]).
    fn to_value(&self) -> Value;

    fn as_any(&self) -> &dyn Any;
}

impl PartialEq for dyn Command {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name() && self.to_value() == other.to_value()
    }
}

/// The per-command logic behind [`Command::apply`], hand-written per payload.
pub trait Apply {
    fn apply(
        self,
        db: &mut Db,
        env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error>;
}

/// Box a command payload as a trait object (handy for building undo vectors).
pub fn boxed(command: impl Command + 'static) -> Box<dyn Command> {
    Box::new(command)
}

/// Parses a command's kwargs into a boxed payload of type `T`. The registry
/// stores one monomorphization of this per command.
pub type CommandParser = fn(BTreeMap<String, Value>) -> Result<Box<dyn Command>, DslError>;

/// The semantic kind of a command-constructor argument, derived from its type
/// via [`CommandArgType`]. Lets an MCP layer map each argument to a JSON shape
/// without parsing raw Rust type strings.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArgKind {
    /// A single address, e.g. `CODE:0x10`.
    Address,
    /// An address range, e.g. `CODE:0x10..0x20`.
    AddressRange,
    /// A set of addresses, e.g. `CODE:{0x10, 0x20..0x30}`.
    AddressSet,
    /// Free text.
    Text,
    /// A byte offset or length.
    Offset,
    /// A single byte value.
    Byte,
    /// A data type for `mark_data`.
    DataType,
    /// An optional operand override (text/label for one operand).
    Operand,
    /// A function definition.
    Function,
    /// A note.
    Note,
    /// A note id.
    NoteId,
}

/// Maps a constructor argument's target type to its [`ArgKind`]. Implemented
/// for every type accepted by a [`register!`] constructor; a missing impl is a
/// compile error, so new argument types must be classified here.
pub trait CommandArgType {
    const KIND: ArgKind;
}

impl CommandArgType for SpaceAddressValue {
    const KIND: ArgKind = ArgKind::Address;
}
impl CommandArgType for SpaceAddressRange {
    const KIND: ArgKind = ArgKind::AddressRange;
}
impl CommandArgType for SpaceAddressSet {
    const KIND: ArgKind = ArgKind::AddressSet;
}
impl CommandArgType for String {
    const KIND: ArgKind = ArgKind::Text;
}
impl CommandArgType for usize {
    const KIND: ArgKind = ArgKind::Offset;
}
impl CommandArgType for AddressValue {
    const KIND: ArgKind = ArgKind::Offset;
}
impl CommandArgType for u8 {
    const KIND: ArgKind = ArgKind::Byte;
}
impl CommandArgType for DataType {
    const KIND: ArgKind = ArgKind::DataType;
}
impl CommandArgType for Option<OperandOverride> {
    const KIND: ArgKind = ArgKind::Operand;
}
impl CommandArgType for Function {
    const KIND: ArgKind = ArgKind::Function;
}
impl CommandArgType for Note {
    const KIND: ArgKind = ArgKind::Note;
}
impl CommandArgType for NoteId {
    const KIND: ArgKind = ArgKind::NoteId;
}

/// One argument of a command's `new` constructor, captured for introspection
/// (e.g. an MCP server presenting commands to an LLM).
#[derive(Debug, Clone, Copy)]
pub struct CommandArg {
    pub name: &'static str,
    /// The raw constructor target type, e.g. `"SpaceAddressValue"`.
    pub ty: &'static str,
    /// The semantic kind, for schema generation.
    pub kind: ArgKind,
}

/// A command's registry entry: the constructor's doc comment and argument
/// signature (for introspection) plus the parser that builds it from DSL
/// kwargs. Built by [`register!`].
#[derive(Debug, Clone, Copy)]
pub struct CommandEntry {
    pub doc: &'static str,
    pub args: &'static [CommandArg],
    pub parse: CommandParser,
}

#[gather]
pub static COMMANDS: ScatteredMap<&'static str, CommandEntry>;

pub fn parse<T: Command + DeserializeOwned + 'static>(
    kwargs: BTreeMap<String, Value>,
) -> Result<Box<dyn Command>, DslError> {
    let value = Value::Struct {
        name: String::new(),
        fields: kwargs,
    };
    Ok(Box::new(crate::store::de::from_value::<T>(value)?))
}

/// Length of the snake_case form of an ASCII PascalCase identifier.
pub const fn snake_len(s: &str) -> usize {
    let bytes = s.as_bytes();
    let mut len = 0;
    let mut i = 0;
    while i < bytes.len() {
        if i > 0 && bytes[i].is_ascii_uppercase() {
            len += 1; // for the inserted '_'
        }
        len += 1;
        i += 1;
    }
    len
}

/// snake_case an ASCII PascalCase identifier into an `N`-byte buffer, where `N`
/// is [`snake_len`] of the same string.
pub const fn snake_bytes<const N: usize>(s: &str) -> [u8; N] {
    let bytes = s.as_bytes();
    let mut out = [0u8; N];
    let mut out_i = 0;
    let mut i = 0;
    while i < bytes.len() {
        let byte = bytes[i];
        if byte.is_ascii_uppercase() {
            if i > 0 {
                out[out_i] = b'_';
                out_i += 1;
            }
            out[out_i] = byte.to_ascii_lowercase();
        } else {
            out[out_i] = byte;
        }
        out_i += 1;
        i += 1;
    }
    out
}

#[cfg(test)]
mod tests {
    use super::COMMANDS;

    /// The registry exposes each command's constructor doc + typed signature,
    /// which an MCP server can enumerate without a hand-written schema.
    #[test]
    fn registry_captures_constructor_signature() {
        let entry = COMMANDS.get("set_label").expect("set_label is registered");
        assert!(entry.doc.contains("Name the code"), "doc: {:?}", entry.doc);
        let args: Vec<(&str, &str, super::ArgKind)> =
            entry.args.iter().map(|a| (a.name, a.ty, a.kind)).collect();
        assert_eq!(
            args,
            [
                ("address", "SpaceAddressValue", super::ArgKind::Address),
                ("label", "String", super::ArgKind::Text),
            ]
        );
    }

    #[test]
    fn arg_kinds_are_derived_from_types() {
        use super::ArgKind::*;
        let kinds = |name: &str| -> Vec<super::ArgKind> {
            COMMANDS
                .get(name)
                .unwrap()
                .args
                .iter()
                .map(|a| a.kind)
                .collect()
        };
        assert_eq!(kinds("map_bytes"), [Address, Text, Offset, Offset]);
        assert_eq!(kinds("set_constant_bytes"), [AddressRange, Byte]);
        assert_eq!(kinds("clear_bytes"), [AddressSet]);
        assert_eq!(kinds("disassemble_range"), [AddressRange]);
        assert_eq!(kinds("mark_data"), [AddressRange, DataType]);
        assert_eq!(kinds("mark_unknown"), [AddressRange]);
        assert_eq!(kinds("override_operand"), [Address, Byte, Operand]);
        assert_eq!(kinds("set_note"), [AddressRange, Note]);
        assert_eq!(kinds("clear_note"), [NoteId]);
    }
}
