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

/// Register a command from a `new(..)`-style constructor.
///
/// Given a doc-commented, typed argument list and a body, this:
/// - generates the `pub fn new(args) -> Self` constructor,
/// - implements [`Command`] (forwarding `apply` to the [`Apply`] impl), and
/// - adds a [`CommandEntry`] — the constructor's doc, argument signature, and
///   DSL parser — to the link-time [`COMMANDS`] registry under the snake_case
///   form of the type name.
///
/// The captured doc + signature let an MCP server enumerate the available
/// commands and their arguments without any hand-written schema.
///
/// ```ignore
/// register!(SetLabel(
///     /// Name the code address `offset`.
///     space: AddressSpace,
///     offset: AddressValue,
///     label: impl Into<String>,
/// ) {
///     Self { address: (space, offset).into(), label: label.into() }
/// });
/// ```
macro_rules! register {
    (
        $type:ident (
            $(#[doc = $doc:literal])*
            $($arg:ident : $argty:ty),* $(,)?
        ) $body:block
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
            pub fn new($($arg: $argty),*) -> Self $body
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
mod function;
mod label;
mod note;

use std::any::Any;
use std::collections::BTreeMap;
use std::io;

pub use auto_disassemble::AutoDisassemble;
pub use bytes::{ClearBytes, MapBytes, SetConstantBytes};
pub use comment::{ClearComment, SetComment};
pub use equivalent::{ClearEquivalents, SetEquivalent};
pub use function::{ClearFunction, SetFunction};
pub use label::{ClearLabel, SetLabel};
pub use note::{ClearNote, SetNote};

use scattered_collect::{ScatteredMap, gather};
use serde::de::DeserializeOwned;

use crate::address::AddressValue;
use crate::db::{Db, Error};
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

/// A disassembly mutation. Implemented by each payload type via [`register!`];
/// the per-command logic lives in its [`Apply`] impl.
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

/// One argument of a command's `new` constructor, captured for introspection
/// (e.g. an MCP server presenting commands to an LLM).
#[derive(Debug, Clone, Copy)]
pub struct CommandArg {
    pub name: &'static str,
    pub ty: &'static str,
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
        assert!(
            entry.doc.contains("Name the code address"),
            "doc: {:?}",
            entry.doc
        );
        let args: Vec<(&str, &str)> = entry.args.iter().map(|a| (a.name, a.ty)).collect();
        assert_eq!(
            args,
            [
                ("space", "AddressSpace"),
                ("offset", "AddressValue"),
                ("label", "impl Into<String>"),
            ]
        );
    }

    #[test]
    fn every_command_is_registered() {
        for name in [
            "set_label",
            "clear_label",
            "set_equivalent",
            "clear_equivalents",
            "auto_disassemble",
            "set_comment",
            "clear_comment",
            "map_bytes",
            "clear_bytes",
            "set_constant_bytes",
            "set_function",
            "clear_function",
            "set_note",
            "clear_note",
        ] {
            assert!(COMMANDS.get(name).is_some(), "{name} is not registered");
        }
    }
}
