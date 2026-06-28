//! A textual DSL for [`Command`]s and the types they carry.
//!
//! Serialization is fully generic — any `serde` type round-trips without
//! bespoke code:
//!
//! ```text
//!   T ──Serialize──▶ ser::to_value ──▶ Value ──Value::render──▶ "text"
//!   "text" ──parser──▶ Value ──de::from_value──▶ T
//! ```
//!
//! A command renders as `name(kwargs)`: the [`Command::name`] plus its payload
//! struct's fields. Parsing dispatches on that name through the link-time
//! [`COMMANDS`](crate::commands::COMMANDS) registry, so there is no central
//! list of commands to keep in sync. The remaining DSL-specific conventions
//! live in [`crate::address`] (address syntax) and [`fields`] (note ids).

pub mod de;
pub mod error;
pub mod fields;
mod lexer;
mod parser;
pub mod ser;
pub mod value;

pub use error::DslError;

use crate::commands::{COMMANDS, Command};
use value::Value;

/// Render a command to DSL text, e.g. `set_label(address=CODE:0x100, label="x")`.
pub fn to_dsl(command: &dyn Command) -> String {
    let Value::Struct { fields, .. } = command.to_value() else {
        unreachable!("command payloads serialize to a struct");
    };
    Value::Call {
        name: command.name().to_owned(),
        kwargs: fields,
    }
    .render()
}

/// Parse a single command from DSL text, dispatching on its name through the
/// registry.
pub fn from_dsl(input: &str) -> Result<Box<dyn Command>, DslError> {
    let Value::Call { name, kwargs } = parser::parse_command(input)? else {
        return Err(DslError::new("expected a command call"));
    };
    let entry = COMMANDS
        .get(name.as_str())
        .ok_or_else(|| DslError::new(format!("unknown command {name}")))?;
    (entry.parse)(kwargs)
}

/// Parse a single bare DSL value into any `Deserialize` type — the "layer on
/// top" that lets callers accept DSL spellings like `CODE:0x84` or
/// `CODE:0x10..0x20` as input (e.g. an MCP query argument), reusing the exact
/// same grammar the commands use.
pub fn from_dsl_value<T: serde::de::DeserializeOwned>(input: &str) -> Result<T, DslError> {
    de::from_value(parser::parse_value(input)?)
}

/// Render many commands, one per line.
pub fn to_dsl_many(commands: &[Box<dyn Command>]) -> String {
    commands
        .iter()
        .map(|command| to_dsl(&**command))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Parse a document of newline-separated commands, skipping blank and `#` lines.
pub fn from_dsl_many(input: &str) -> Result<Vec<Box<dyn Command>>, DslError> {
    input
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty() && !line.starts_with('#'))
        .map(from_dsl)
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::address::{AddressRange, AddressSpace};
    use crate::commands::{
        self, AutoDisassemble, ClearBytes, MapBytes, SetComment, SetEquivalent, SetFunction,
        SetLabel, SetNote,
    };
    use crate::db::{DataType, Equivalent, Function};
    use crate::note::Note;

    use super::{from_dsl, to_dsl};

    #[test]
    fn round_trip_auto_disassemble() {
        let command = commands::boxed(AutoDisassemble::new((AddressSpace::Code, 0x1234)));
        let dsl = to_dsl(&*command);
        assert_eq!(dsl, "auto_disassemble(address=CODE:0x1234)");
        assert_eq!(&*from_dsl(&dsl).unwrap(), &*command);
    }

    #[test]
    fn round_trip_clear_bytes_range() {
        let command = commands::boxed(ClearBytes::new((AddressSpace::Code, 0x10..0x20)));
        let dsl = to_dsl(&*command);
        assert_eq!(dsl, "clear_bytes(addresses=CODE:{0x10..0x20})");
        assert_eq!(&*from_dsl(&dsl).unwrap(), &*command);
    }

    #[test]
    fn round_trip_set_label() {
        let command = commands::boxed(SetLabel::new((AddressSpace::Code, 0x100), "reset_vector"));
        let dsl = to_dsl(&*command);
        assert_eq!(dsl, "set_label(address=CODE:0x100, label=\"reset_vector\")");
        assert_eq!(&*from_dsl(&dsl).unwrap(), &*command);
    }

    #[test]
    fn round_trip_multiline_string() {
        let command = commands::boxed(SetComment::new(
            (AddressSpace::Code, 0x10),
            "line one\nline two",
        ));
        let dsl = to_dsl(&*command);
        assert!(dsl.starts_with("set_comment(address=CODE:0x10, comment=r\""));
        assert_eq!(&*from_dsl(&dsl).unwrap(), &*command);
    }

    #[test]
    fn round_trip_raw_string_with_quotes() {
        let command = commands::boxed(SetComment::new((AddressSpace::Code, 0x10), "say \"hello\""));
        let dsl = to_dsl(&*command);
        assert!(dsl.contains("r#\""));
        assert_eq!(&*from_dsl(&dsl).unwrap(), &*command);
    }

    #[test]
    fn round_trip_set_equivalent_code() {
        let command = commands::boxed(SetEquivalent::new(
            (AddressSpace::Code, 0),
            Equivalent::Code(vec![None]),
        ));
        let dsl = to_dsl(&*command);
        assert_eq!(
            dsl,
            "set_equivalent(address=CODE:0x0, equivalent=Equivalent::Code([None]))"
        );
        assert_eq!(&*from_dsl(&dsl).unwrap(), &*command);
    }

    #[test]
    fn round_trip_set_equivalent_data() {
        let command = commands::boxed(SetEquivalent::new(
            (AddressSpace::Code, 0x20),
            Equivalent::Data(DataType::Byte, 4),
        ));
        let dsl = to_dsl(&*command);
        assert_eq!(
            dsl,
            "set_equivalent(address=CODE:0x20, equivalent=Equivalent::Data(DataType::Byte, 0x4))"
        );
        assert_eq!(&*from_dsl(&dsl).unwrap(), &*command);
    }

    #[test]
    fn round_trip_set_function() {
        let command = commands::boxed(SetFunction::new(
            (AddressSpace::Code, 0),
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
        ));
        let dsl = to_dsl(&*command);
        assert!(dsl.starts_with("set_function(address=CODE:0x0, function=Function("));
        assert_eq!(&*from_dsl(&dsl).unwrap(), &*command);
    }

    #[test]
    fn round_trip_set_note_minimal() {
        let note = Note::new(None, "interesting spot");
        let command = commands::boxed(SetNote::new(
            (AddressSpace::Code, AddressRange::new(0x100, 0x120)),
            note.clone(),
        ));
        let dsl = to_dsl(&*command);
        assert!(dsl.contains("note=Note("));
        assert!(dsl.contains("content=\"interesting spot\""));
        let decoded = from_dsl(&dsl).unwrap();
        let cmd = decoded
            .as_any()
            .downcast_ref::<SetNote>()
            .expect("expected SetNote");
        assert_eq!(cmd.note.content, note.content);
        assert_eq!(cmd.note.id, note.id);
    }

    #[test]
    fn deserialize_note_without_id_allocates() {
        let decoded =
            from_dsl("set_note(address=CODE:0x100..0x120, note=Note(content=\"hello\", tags={}))")
                .unwrap();
        let cmd = decoded
            .as_any()
            .downcast_ref::<SetNote>()
            .expect("expected SetNote");
        assert_eq!(cmd.note.content, "hello");
        assert!(cmd.note.tags.is_empty());
        assert!(!cmd.note.id.as_str().is_empty());
    }

    #[test]
    fn round_trip_map_bytes() {
        let command = commands::boxed(MapBytes::new(
            (AddressSpace::Code, 0),
            "test.bin",
            0usize,
            16u32,
        ));
        let dsl = to_dsl(&*command);
        assert_eq!(&*from_dsl(&dsl).unwrap(), &*command);
    }

    #[test]
    fn unknown_command_is_rejected() {
        let err = from_dsl("frobnicate(address=CODE:0x0)").unwrap_err();
        assert!(err.message.contains("unknown command frobnicate"));
    }

    #[test]
    fn from_dsl_value_parses_bare_address() {
        use crate::address::{SpaceAddressRange, SpaceAddressValue};
        use super::from_dsl_value;

        let addr: SpaceAddressValue = from_dsl_value("CODE:0x84").unwrap();
        assert_eq!(addr.space, AddressSpace::Code);
        assert_eq!(addr.offset, 0x84);

        let range: SpaceAddressRange = from_dsl_value("CODE:0x10..0x20").unwrap();
        assert_eq!(range.space, AddressSpace::Code);
        assert_eq!(range.range, AddressRange::new(0x10, 0x20));
    }

    #[test]
    fn missing_field_error_is_not_positional() {
        // A structural (missing-field) error has no byte offset, so it must not
        // claim "at byte 0".
        let err = from_dsl("set_label(address=CODE:0x0)").unwrap_err();
        assert_eq!(err.offset, None);
        let shown = err.to_string();
        assert!(!shown.contains("byte"), "got: {shown}");
        assert!(shown.contains("missing field"), "got: {shown}");
    }

    #[test]
    fn positional_error_keeps_its_offset() {
        // A lexer/parser failure is positional and keeps a byte offset.
        let err = from_dsl("set_label(address=CODE:0xZZ)").unwrap_err();
        assert!(err.offset.is_some());
        assert!(err.to_string().contains("byte"));
    }
}
