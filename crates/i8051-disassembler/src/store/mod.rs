//! A textual DSL for [`Command`]s and the types they carry.
//!
//! Serialization is fully generic — any `serde` type round-trips without
//! bespoke code.
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

use crate::commands::{self, COMMANDS, Command};
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
    let entry = COMMANDS.get(name.as_str()).ok_or_else(|| {
        let hint = commands::closest(&name, (&COMMANDS).into_iter().map(|(n, _)| *n))
            .map(|h| format!(" (did you mean `{h}`?)"))
            .unwrap_or_default();
        DslError::new(format!("unknown command `{name}`{hint}"))
    })?;

    // Reject stray arguments up front.
    for key in kwargs.keys() {
        if !entry.args.iter().any(|a| a.name == key) {
            let hint = commands::closest(key, entry.args.iter().map(|a| a.name))
                .map(|h| format!(" (did you mean `{h}`?)"))
                .unwrap_or_default();
            let expected = arg_names(entry);
            return Err(DslError::new(format!(
                "unknown argument `{key}` to `{name}`{hint}; expected {expected}"
            )));
        }
    }
    for arg in entry.args {
        if !kwargs.contains_key(arg.name) {
            return Err(DslError::new(format!(
                "`{name}` is missing argument `{}` — {}",
                arg.name, arg.hint
            )));
        }
    }

    match (entry.parse)(kwargs.clone()) {
        Ok(command) => Ok(command),
        Err(raw) => Err(diagnose_args(&name, entry, kwargs, raw)),
    }
}

/// A command's argument names as a backtick list for error text.
fn arg_names(entry: &crate::commands::CommandEntry) -> String {
    if entry.args.is_empty() {
        return "no arguments".to_string();
    }
    let names: Vec<String> = entry.args.iter().map(|a| format!("`{}`", a.name)).collect();
    names.join(", ")
}

fn diagnose_args(
    name: &str,
    entry: &crate::commands::CommandEntry,
    kwargs: std::collections::BTreeMap<String, Value>,
    raw: DslError,
) -> DslError {
    for arg in entry.args {
        let Some(value) = kwargs.get(arg.name) else {
            continue;
        };
        if let Err(inner) = (arg.check)(value) {
            let detail = if inner.message.contains("unknown variant")
                || inner.message.contains("missing field")
                || inner.message.contains("duplicate field")
            {
                inner.message
            } else {
                format!("expected {}, got {}", arg.hint, value.render())
            };
            return DslError::new(format!("argument `{}` of `{name}`: {detail}", arg.name));
        }
    }
    raw
}

/// Parse a single bare DSL value into any `Deserialize` type.
pub fn from_dsl_value<T: serde::de::DeserializeOwned>(input: &str) -> Result<T, DslError> {
    de::from_value(parser::parse_value(input)?)
}

/// Parse a single DSL value into its [`Value`] AST.
pub fn parse_value(input: &str) -> Result<Value, DslError> {
    parser::parse_value(input)
}

/// Parse a `verb(k=v, ...)` call into its name and keyword arguments.
pub fn parse_call(input: &str) -> Result<(String, std::collections::BTreeMap<String, Value>), DslError> {
    match parser::parse_command(input)? {
        Value::Call { name, kwargs } => Ok((name, kwargs)),
        _ => Err(DslError::new("expected a command call")),
    }
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
    use crate::address::AddressRange;
    use crate::commands::{
        self, AutoDisassemble, ClearBytes, MapBytes, SetComment, SetFunction, SetLabel, SetNote,
    };
    use crate::db::Function;
    use crate::note::Note;

    use super::{from_dsl, to_dsl};

    #[test]
    fn round_trip_auto_disassemble() {
        let command = commands::boxed(AutoDisassemble::new((crate::platform::i8051::CODE, 0x1234)));
        let dsl = to_dsl(&*command);
        assert_eq!(dsl, "auto_disassemble(address=CODE:0x1234)");
        assert_eq!(&*from_dsl(&dsl).unwrap(), &*command);
    }

    #[test]
    fn round_trip_clear_bytes_range() {
        let command = commands::boxed(ClearBytes::new((crate::platform::i8051::CODE, 0x10..0x20)));
        let dsl = to_dsl(&*command);
        assert_eq!(dsl, "clear_bytes(addresses=CODE:{0x10..0x20})");
        assert_eq!(&*from_dsl(&dsl).unwrap(), &*command);
    }

    #[test]
    fn round_trip_set_label() {
        let command = commands::boxed(SetLabel::new((crate::platform::i8051::CODE, 0x100), "reset_vector"));
        let dsl = to_dsl(&*command);
        assert_eq!(dsl, "set_label(address=CODE:0x100, label=\"reset_vector\")");
        assert_eq!(&*from_dsl(&dsl).unwrap(), &*command);
    }

    #[test]
    fn round_trip_multiline_string() {
        let command = commands::boxed(SetComment::new(
            (crate::platform::i8051::CODE, 0x10),
            "line one\nline two",
        ));
        let dsl = to_dsl(&*command);
        assert!(dsl.starts_with("set_comment(address=CODE:0x10, comment=r\""));
        assert_eq!(&*from_dsl(&dsl).unwrap(), &*command);
    }

    #[test]
    fn round_trip_raw_string_with_quotes() {
        let command = commands::boxed(SetComment::new((crate::platform::i8051::CODE, 0x10), "say \"hello\""));
        let dsl = to_dsl(&*command);
        assert!(dsl.contains("r#\""));
        assert_eq!(&*from_dsl(&dsl).unwrap(), &*command);
    }

    #[test]
    fn round_trip_set_function() {
        let command = commands::boxed(SetFunction::new(
            (crate::platform::i8051::CODE, 0),
            Function {
                addr: crate::address::PhysicalAddr {
                    space: crate::platform::i8051::CODE,
                    offset: 0,
                },
                name: "main".into(),
                signature: Some("void main(void)".into()),
                length: 0x40,
                noreturn: false,
            },
        ));
        let dsl = to_dsl(&*command);
        assert_eq!(
            dsl,
            "set_function(address=CODE:0x0, function=Function(addr=PhysicalAddr(offset=0x0, \
             space=\"CODE\"), length=0x40, name=\"main\", noreturn=False, signature=\"void main(void)\"))"
        );
        assert_eq!(&*from_dsl(&dsl).unwrap(), &*command);
    }

    #[test]
    fn round_trip_set_note_minimal() {
        let note = Note::new(None, "interesting spot");
        let command = commands::boxed(SetNote::new(
            (crate::platform::i8051::CODE, AddressRange::new(0x100, 0x120)),
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
    fn bare_address_coerces_to_a_one_byte_range() {
        let decoded = from_dsl("set_note(address=CODE:0x26, note=Note(content=\"hi\"))").unwrap();
        let cmd = decoded.as_any().downcast_ref::<SetNote>().expect("SetNote");
        assert_eq!(cmd.address.range, AddressRange::new(0x26, 0x27));
        assert!(to_dsl(&*decoded).contains("address=CODE:0x26..0x27"));
    }

    #[test]
    fn address_and_range_coerce_to_a_set() {
        let range = from_dsl("clear_bytes(addresses=CODE:0x10..0x20)").unwrap();
        assert_eq!(to_dsl(&*range), "clear_bytes(addresses=CODE:{0x10..0x20})");
        let single = from_dsl("clear_bytes(addresses=CODE:0x10)").unwrap();
        assert_eq!(to_dsl(&*single), "clear_bytes(addresses=CODE:{0x10})");
    }

    #[test]
    fn quoted_address_spellings_are_accepted() {
        let quoted = from_dsl(r#"set_label(address="CODE:0x100", label="reset_vector")"#).unwrap();
        let bare = from_dsl(r#"set_label(address=CODE:0x100, label="reset_vector")"#).unwrap();
        assert_eq!(&*quoted, &*bare);
        let range = from_dsl(r#"set_note(address="CODE:0x10..0x12", note=Note(content="x"))"#).unwrap();
        let cmd = range.as_any().downcast_ref::<SetNote>().expect("SetNote");
        assert_eq!(cmd.address.range, AddressRange::new(0x10, 0x12));
    }

    #[test]
    fn bare_identifiers_read_as_strings() {
        let bare = from_dsl("set_label(address=CODE:0x100, label=reset_vector)").unwrap();
        let quoted = from_dsl(r#"set_label(address=CODE:0x100, label="reset_vector")"#).unwrap();
        assert_eq!(&*bare, &*quoted);
    }

    #[test]
    fn bare_string_note_is_content_shorthand() {
        let decoded =
            from_dsl(r#"set_note(address=CODE:0x26..0x29, note="tighten this loop")"#).unwrap();
        let cmd = decoded.as_any().downcast_ref::<SetNote>().expect("SetNote");
        assert_eq!(cmd.note.content, "tighten this loop");
        assert!(!cmd.note.id.as_str().is_empty(), "id is allocated from content");
    }

    #[test]
    fn round_trip_map_bytes() {
        let command = commands::boxed(MapBytes::new(
            (crate::platform::i8051::CODE, 0),
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
        assert!(err.message.contains("unknown command `frobnicate`"), "{}", err.message);
        let typo = from_dsl(r#"set_lable(address=CODE:0x0, label="x")"#).unwrap_err();
        assert!(typo.message.contains("did you mean `set_label`?"), "{}", typo.message);
    }

    #[test]
    fn stray_and_missing_arguments_are_named() {
        let typo = from_dsl(r#"set_label(adress=CODE:0x0, label="x")"#).unwrap_err();
        assert!(typo.message.contains("unknown argument `adress`"), "{}", typo.message);
        assert!(typo.message.contains("did you mean `address`?"), "{}", typo.message);
        let missing = from_dsl(r#"set_note(address=CODE:0x0..0x1)"#).unwrap_err();
        assert!(missing.message.contains("missing argument `note`"), "{}", missing.message);
        assert!(missing.message.contains("Note(content="), "{}", missing.message);
    }

    #[test]
    fn shape_mismatches_name_the_argument_and_expected_spelling() {
        let err = from_dsl(r#"set_label(address=CODE:0x0..0x10, label="x")"#).unwrap_err();
        assert!(err.message.contains("argument `address` of `set_label`"), "{}", err.message);
        assert!(err.message.contains("a DSL address, e.g. CODE:0x100"), "{}", err.message);
        assert!(err.message.contains("got CODE:0x0..0x10"), "{}", err.message);
        let e = from_dsl("mark_data(range=CODE:0x0..0x4, data_type=DataType::Bogus)").unwrap_err();
        assert!(e.message.contains("argument `data_type` of `mark_data`"), "{}", e.message);
        assert!(e.message.contains("unknown variant `Bogus`"), "{}", e.message);
    }

    #[test]
    fn from_dsl_value_parses_bare_address() {
        use super::from_dsl_value;
        use crate::address::{SpaceAddressRange, SpaceAddressValue};

        let addr: SpaceAddressValue = from_dsl_value("CODE:0x84").unwrap();
        assert_eq!(addr.space, crate::platform::i8051::CODE);
        assert_eq!(addr.offset, 0x84);

        let range: SpaceAddressRange = from_dsl_value("CODE:0x10..0x20").unwrap();
        assert_eq!(range.space, crate::platform::i8051::CODE);
        assert_eq!(range.range, AddressRange::new(0x10, 0x20));
    }

    #[test]
    fn missing_field_error_is_not_positional() {
        let err = from_dsl("set_label(address=CODE:0x0)").unwrap_err();
        assert_eq!(err.offset, None);
        let shown = err.to_string();
        assert!(!shown.contains("at byte"), "got: {shown}");
        assert!(shown.contains("missing argument `label`"), "got: {shown}");
    }

    #[test]
    fn positional_error_keeps_its_offset() {
        let err = from_dsl("set_label(address=CODE:0xZZ)").unwrap_err();
        assert!(err.offset.is_some());
        assert!(err.to_string().contains("byte"));
    }
}
