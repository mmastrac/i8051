//! A textual DSL for [`Command`]s and the types they carry.
//!
//! The pipeline is fully generic — any `serde` type round-trips without
//! bespoke code:
//!
//! ```text
//!   T ──Serialize──▶ ser::to_value ──▶ Value ──Value::render──▶ "text"
//!   "text" ──parser──▶ Value ──de::from_value──▶ T
//! ```
//!
//! The only DSL-specific conventions live in [`fields`] (address syntax, note
//! ids) and in the top-level command shim below (`name(kwargs)` instead of the
//! generic `Command::Variant(...)`).

pub mod de;
pub mod error;
pub mod fields;
mod lexer;
mod parser;
pub mod ser;
pub mod value;

pub use error::DslError;

use crate::commands::Command;
use value::{EnumArgs, Value};

/// Render a command to DSL text, e.g. `set_label(address=CODE:0x100, label="x")`.
pub fn to_dsl(command: &Command) -> String {
    let value = ser::to_value(command).expect("command serialization is infallible");
    command_to_call(value).render()
}

/// Parse a single command from DSL text.
pub fn from_dsl(input: &str) -> Result<Command, DslError> {
    let value = parser::parse_command(input)?;
    de::from_value(call_to_command(value))
}

/// Render many commands, one per line.
pub fn to_dsl_many(commands: &[Command]) -> String {
    commands
        .iter()
        .map(to_dsl)
        .collect::<Vec<_>>()
        .join("\n")
}

/// Parse a document of newline-separated commands, skipping blank and `#` lines.
pub fn from_dsl_many(input: &str) -> Result<Vec<Command>, DslError> {
    input
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty() && !line.starts_with('#'))
        .map(from_dsl)
        .collect()
}

/// `Command::AutoDisassemble(AutoDisassemble { .. })` serializes generically to
/// an enum value; the DSL spells it as a bare call `auto_disassemble(..)`.
fn command_to_call(value: Value) -> Value {
    match value {
        Value::Enum {
            variant,
            args: EnumArgs::Positional(mut args),
            ..
        } if args.len() == 1 => match args.remove(0) {
            Value::Struct { fields, .. } => Value::Call {
                name: variant,
                kwargs: fields,
            },
            other => Value::Enum {
                type_name: String::new(),
                variant,
                args: EnumArgs::Positional(vec![other]),
            },
        },
        other => other,
    }
}

/// Inverse of [`command_to_call`]: re-wrap a parsed call as the enum value the
/// generic deserializer expects for `Command`.
fn call_to_command(value: Value) -> Value {
    match value {
        Value::Call { name, kwargs } => Value::Enum {
            type_name: "Command".to_owned(),
            variant: name,
            args: EnumArgs::Positional(vec![Value::Struct {
                name: String::new(),
                fields: kwargs,
            }]),
        },
        other => other,
    }
}

/// `#[serde(with = "store::as_dsl")]` for a single `Command` field.
pub mod as_dsl {
    use super::{from_dsl, to_dsl, Command};
    use serde::de::{Error, Visitor};
    use serde::{Deserializer, Serializer};

    pub fn serialize<S: Serializer>(command: &Command, s: S) -> Result<S::Ok, S::Error> {
        s.serialize_str(&to_dsl(command))
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<Command, D::Error> {
        struct V;
        impl Visitor<'_> for V {
            type Value = Command;
            fn expecting(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str("a command DSL string")
            }
            fn visit_str<E: Error>(self, value: &str) -> Result<Command, E> {
                from_dsl(value).map_err(E::custom)
            }
        }
        d.deserialize_str(V)
    }
}

/// `#[serde(with = "store::as_dsl_lines")]` for a `Vec<Command>` field.
pub mod as_dsl_lines {
    use super::{from_dsl_many, to_dsl_many, Command};
    use serde::de::{Error, Visitor};
    use serde::{Deserializer, Serializer};

    pub fn serialize<S: Serializer>(commands: &[Command], s: S) -> Result<S::Ok, S::Error> {
        s.serialize_str(&to_dsl_many(commands))
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<Vec<Command>, D::Error> {
        struct V;
        impl Visitor<'_> for V {
            type Value = Vec<Command>;
            fn expecting(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str("a newline-separated command DSL document")
            }
            fn visit_str<E: Error>(self, value: &str) -> Result<Vec<Command>, E> {
                from_dsl_many(value).map_err(E::custom)
            }
        }
        d.deserialize_str(V)
    }
}

#[cfg(test)]
mod tests {
    use crate::address::{AddressRange, AddressSpace};
    use crate::commands::Command;
    use crate::db::{DataType, Equivalent, Function};
    use crate::note::Note;

    use super::{from_dsl, to_dsl};

    #[test]
    fn round_trip_auto_disassemble() {
        let command = Command::auto_disassemble(AddressSpace::Code, 0x1234);
        let dsl = to_dsl(&command);
        assert_eq!(dsl, "auto_disassemble(address=CODE:0x1234)");
        assert_eq!(from_dsl(&dsl).unwrap(), command);
    }

    #[test]
    fn round_trip_clear_bytes_range() {
        let command = Command::clear_bytes(AddressSpace::Code, 0x10, 0x10);
        let dsl = to_dsl(&command);
        assert_eq!(dsl, "clear_bytes(range=CODE:0x10..0x20)");
        assert_eq!(from_dsl(&dsl).unwrap(), command);
    }

    #[test]
    fn round_trip_set_label() {
        let command = Command::set_label(AddressSpace::Code, 0x100, "reset_vector");
        let dsl = to_dsl(&command);
        assert_eq!(dsl, "set_label(address=CODE:0x100, label=\"reset_vector\")");
        assert_eq!(from_dsl(&dsl).unwrap(), command);
    }

    #[test]
    fn round_trip_multiline_string() {
        let command = Command::set_comment(AddressSpace::Code, 0x10, "line one\nline two");
        let dsl = to_dsl(&command);
        assert!(dsl.starts_with("set_comment(address=CODE:0x10, comment=r\""));
        assert_eq!(from_dsl(&dsl).unwrap(), command);
    }

    #[test]
    fn round_trip_raw_string_with_quotes() {
        let command = Command::set_comment(AddressSpace::Code, 0x10, "say \"hello\"");
        let dsl = to_dsl(&command);
        assert!(dsl.contains("r#\""));
        assert_eq!(from_dsl(&dsl).unwrap(), command);
    }

    #[test]
    fn round_trip_set_equivalent_code() {
        let command = Command::set_equivalent(AddressSpace::Code, 0, Equivalent::Code(vec![None]));
        let dsl = to_dsl(&command);
        assert_eq!(
            dsl,
            "set_equivalent(address=CODE:0x0, equivalent=Equivalent::Code([None]))"
        );
        assert_eq!(from_dsl(&dsl).unwrap(), command);
    }

    #[test]
    fn round_trip_set_equivalent_data() {
        let command =
            Command::set_equivalent(AddressSpace::Code, 0x20, Equivalent::Data(DataType::Byte, 4));
        let dsl = to_dsl(&command);
        assert_eq!(
            dsl,
            "set_equivalent(address=CODE:0x20, equivalent=Equivalent::Data(DataType::Byte, 0x4))"
        );
        assert_eq!(from_dsl(&dsl).unwrap(), command);
    }

    #[test]
    fn round_trip_set_function() {
        let command = Command::set_function(
            AddressSpace::Code,
            0,
            Function {
                addr: crate::address::PhysicalAddr {
                    space: AddressSpace::Code,
                    offset: 0,
                },
                name: "main".into(),
                signature: Some("void main(void)".into()),
                length: 0x40,
                noreturn: false,
            },
        );
        let dsl = to_dsl(&command);
        assert!(dsl.starts_with("set_function(address=CODE:0x0, function=Function("));
        assert_eq!(from_dsl(&dsl).unwrap(), command);
    }

    #[test]
    fn round_trip_set_note_minimal() {
        let note = Note::new(None, "interesting spot");
        let command = Command::set_note(
            AddressSpace::Code,
            AddressRange::new(0x100, 0x120),
            note.clone(),
        );
        let dsl = to_dsl(&command);
        assert!(dsl.contains("note=Note("));
        assert!(dsl.contains("content=\"interesting spot\""));
        let decoded = from_dsl(&dsl).unwrap();
        match decoded {
            Command::SetNote(cmd) => {
                assert_eq!(cmd.note.content, note.content);
                assert_eq!(cmd.note.id, note.id);
            }
            other => panic!("expected SetNote, got {other:?}"),
        }
    }

    #[test]
    fn deserialize_note_without_id_allocates() {
        let command =
            from_dsl("set_note(address=CODE:0x100..0x120, note=Note(content=\"hello\", tags={}))")
                .unwrap();
        match command {
            Command::SetNote(cmd) => {
                assert_eq!(cmd.note.content, "hello");
                assert!(cmd.note.tags.is_empty());
                assert!(!cmd.note.id.as_str().is_empty());
            }
            other => panic!("expected SetNote, got {other:?}"),
        }
    }

    #[test]
    fn round_trip_map_bytes() {
        let command = Command::map_bytes(AddressSpace::Code, 0, "test.bin", 0, 16);
        let dsl = to_dsl(&command);
        assert_eq!(from_dsl(&dsl).unwrap(), command);
    }

    #[test]
    fn serde_with_round_trip() {
        // Exercise the `#[serde(with = "as_dsl")]` adapter end to end.
        let command = Command::map_bytes(AddressSpace::Code, 0, "test.bin", 0, 16);
        let dsl = to_dsl(&command);
        let restored: Command = super::as_dsl::deserialize(
            serde::de::value::StrDeserializer::<serde::de::value::Error>::new(&dsl),
        )
        .unwrap();
        assert_eq!(restored, command);
    }
}
