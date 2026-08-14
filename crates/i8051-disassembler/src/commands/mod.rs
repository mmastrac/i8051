/// Assert a command round-trips.
///
/// ```ignore
/// serialize_test!(
///     auto_disassemble,
///     "auto_disassemble(address=CODE:0x1234)",
///     AutoDisassemble { address: (crate::platform::i8051::CODE, 0x1234).into() }
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
/// Each `name: Type` becomes an `impl Into<Type>` parameter of a generated `pub
/// fn new(..) -> Self`. Also implements [`Command`] and adds a [`CommandEntry`]
/// (doc, argument signature, DSL parser) to the link-time [`COMMANDS`]
/// registry, keyed by the type name (in `snake_case`).
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
                        kind: <$argty as $crate::commands::DslArg>::KIND,
                        slug: <$argty as $crate::commands::DslArg>::SLUG,
                        hint: <$argty as $crate::commands::DslArg>::HINT,
                        example: <$argty as $crate::commands::DslArg>::EXAMPLE,
                        check: |value: &$crate::store::value::Value| {
                            $crate::store::de::from_value::<$argty>(
                                ::core::clone::Clone::clone(value),
                            )
                            .map(|_| ())
                        },
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
mod cpu;
mod decoding;
mod equivalent;
mod extent;
mod function;
mod label;
mod note;
mod operand;
mod platform_addr;
mod pointer;

use std::any::Any;
use std::collections::BTreeMap;
use std::io;

pub use auto_disassemble::{AutoDisassemble, ClearAutoDisassembleRoot};
pub use bytes::{MapBytes, SetConstantBytes, UnmapBytes};
pub use comment::{ClearComment, SetComment};
pub use cpu::{ClearCpu, SetCpu};
pub use decoding::{ClearAddressBits, SetAddressBits};
pub use equivalent::ClearEquivalents;
pub use extent::{DisassembleRange, MarkData, MarkUnknown};
pub use function::{ClearFunction, SetFunction};
pub use label::{ClearLabel, SetLabel};
pub use note::{ClearNote, SetNote};
pub use operand::OverrideOperand;
pub use platform_addr::{DisablePlatformAddress, RestorePlatformAddress};
pub use pointer::{ClearOperandType, SetOperandPointer, SetOperandValue};

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

    /// The snake_case DSL name.
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

/// Box a command payload as a trait object.
pub fn boxed(command: impl Command + 'static) -> Box<dyn Command> {
    Box::new(command)
}

/// Parses a command's kwargs into a boxed payload of type `T`. The registry
/// stores one monomorphization of this per command.
pub type CommandParser = fn(BTreeMap<String, Value>) -> Result<Box<dyn Command>, DslError>;

/// Type-checks one DSL value.
pub type ArgCheck = fn(&Value) -> Result<(), DslError>;

/// The shape axis coercion and JSON typing switch on.
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
    /// A domain type (`Note`, `Function`, `DataType`, ...).
    Struct,
}

impl ArgKind {
    pub fn numeric(self) -> bool {
        matches!(self, ArgKind::Offset | ArgKind::Byte)
    }
}

/// The candidate closest to `target` by edit distance, if any is close enough
/// to plausibly be a typo.
pub fn closest<'a>(target: &str, candidates: impl IntoIterator<Item = &'a str>) -> Option<&'a str> {
    candidates
        .into_iter()
        .map(|c| (crate::strings::levenshtein(target, c), c))
        .filter(|(d, c)| *d <= 2.max(c.len() / 3))
        .min_by_key(|(d, _)| *d)
        .map(|(_, c)| c)
}

pub trait DslArg {
    /// How the generic machinery handles this argument's shape.
    const KIND: ArgKind;
    /// The stable wire slug a schema/UI switches on, finer than the JSON type.
    const SLUG: &'static str;
    /// A one-line spelling hint — the single string errors, schema hints, and
    /// help all share.
    const HINT: &'static str;
    /// A canonical example spelling, or `None` for free text (whose example
    /// depends on the argument's *name*, not its type).
    const EXAMPLE: Option<&'static str>;
}

macro_rules! dsl_arg {
    ($($ty:ty),+ => $kind:ident, $slug:literal, $hint:literal, $example:expr $(,)?) => {
        $(impl DslArg for $ty {
            const KIND: ArgKind = ArgKind::$kind;
            const SLUG: &'static str = $slug;
            const HINT: &'static str = $hint;
            const EXAMPLE: Option<&'static str> = $example;
        })+
    };
}

dsl_arg!(SpaceAddressValue => Address, "address", "a DSL address, e.g. CODE:0x100", Some("CODE:0x100"));
dsl_arg!(SpaceAddressRange => AddressRange, "address_range",
    "an address range (end exclusive), e.g. CODE:0x10..0x20, or a bare address for one byte",
    Some("CODE:0x10..0x20"));
dsl_arg!(SpaceAddressSet => AddressSet, "address_set",
    "an address set, e.g. CODE:{0x10..0x20, 0x30}, or a bare address/range", Some("CODE:{0x10..0x20}"));
dsl_arg!(String => Text, "text", "quoted text, e.g. \"...\"", None);
dsl_arg!(usize, AddressValue => Offset, "offset", "a byte offset or length", Some("0x0"));
dsl_arg!(u8 => Byte, "byte", "a byte value, 0-255", Some("0xFF"));
dsl_arg!(DataType => Struct, "data_type", "a data-type spelling, e.g. DataType::Byte", Some("DataType::Byte"));
dsl_arg!(Option<OperandOverride> => Struct, "operand",
    "an operand-override spelling, or None to clear", Some("None"));
dsl_arg!(Function => Struct, "function",
    "a function-definition spelling, e.g. Function(addr=..., name=\"...\", length=...)",
    Some("Function(addr=CODE:0x100, name=\"main\", signature=None, length=0x40, noreturn=False)"));
dsl_arg!(Note => Struct, "note",
    "a note, e.g. Note(content=\"...\", tags=[\"todo\"]), or bare \"text\"",
    Some("Note(content=\"...\", tags=[\"todo\"])"));
dsl_arg!(NoteId => Struct, "note_id", "a note id", Some("\"0000000000000YN222X7N2CE7T\""));

#[derive(Debug, Clone, Copy)]
pub struct CommandArg {
    pub name: &'static str,
    /// The raw constructor target type, e.g. `"SpaceAddressValue"`.
    pub ty: &'static str,
    pub kind: ArgKind,
    pub slug: &'static str,
    pub hint: &'static str,
    pub example: Option<&'static str>,
    pub check: ArgCheck,
}

/// A command's registry entry, built by [`register!`].
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
        assert_eq!(kinds("unmap_bytes"), [AddressSet]);
        assert_eq!(kinds("disassemble_range"), [AddressRange]);
        assert_eq!(kinds("mark_data"), [AddressRange, Struct]);
        assert_eq!(kinds("override_operand"), [Address, Byte, Struct]);
        assert_eq!(kinds("set_note"), [AddressRange, Struct]);
        assert_eq!(kinds("clear_note"), [Struct]);
    }

    #[test]
    fn struct_args_keep_a_per_type_slug() {
        // ...but stay distinguishable on the wire via the type's slug
        let arg = |cmd: &str, arg: &str| {
            let entry = COMMANDS.get(cmd).unwrap();
            *entry.args.iter().find(|a| a.name == arg).unwrap()
        };
        let note = arg("set_note", "note");
        assert_eq!(note.kind, super::ArgKind::Struct);
        assert_eq!(note.slug, "note");
        assert_eq!(arg("mark_data", "data_type").slug, "data_type");
    }
}
