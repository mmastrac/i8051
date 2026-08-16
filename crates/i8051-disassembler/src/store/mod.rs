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
pub use i8051_script as value;

pub use error::DslError;
pub use i8051_proc_macro::dsl;

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
    let Value::Call { name, mut kwargs } = parser::parse_command(input)? else {
        return Err(DslError::new("expected a command call"));
    };
    let entry = COMMANDS.get(name.as_str()).ok_or_else(|| {
        let hint = commands::closest(&name, (&COMMANDS).into_iter().map(|(n, _)| *n))
            .map(|h| format!(" (did you mean `{h}`?)"))
            .unwrap_or_default();
        DslError::new(format!("unknown command `{name}`{hint}"))
    })?;

    // Reject stray arguments up front.
    for (key, _) in &kwargs {
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
        if !kwargs.iter().any(|(key, _)| key == arg.name) {
            // Omitted flags default to false.
            if arg.kind == commands::ArgKind::Flag {
                kwargs.push((arg.name.to_string(), value::Value::Bool(false)));
                continue;
            }
            return Err(DslError::new(format!(
                "`{name}` is missing argument `{}`: {}",
                arg.name, arg.hint
            )));
        }
    }

    qualify_bare_variants(entry, &mut kwargs);
    match (entry.parse)(kwargs.clone()) {
        Ok(command) => Ok(command),
        Err(raw) => Err(diagnose_args(&name, entry, kwargs, raw)),
    }
}

/// Accept a bare enum variant.
pub fn qualify_bare_variants(entry: &crate::commands::CommandEntry, kwargs: &mut value::Fields) {
    for arg in entry.args {
        let Some(value) = kwargs.iter().find(|(key, _)| key == arg.name).map(|(_, v)| v) else {
            continue;
        };
        if (arg.check)(value).is_ok() {
            continue;
        }
        let bare = match value {
            Value::String(bare) if !bare.contains("::") => bare.clone(),
            other => {
                let rendered = other.render();
                if rendered.contains("::")
                    || !rendered.chars().all(|c| c.is_alphanumeric() || c == '_')
                {
                    continue;
                }
                rendered
            }
        };
        let Some(prefix) = arg.example.and_then(|e| e.split_once("::")).map(|(p, _)| p) else {
            continue;
        };
        let Ok(qualified) = parser::parse_value(&format!("{prefix}::{bare}")) else {
            continue;
        };
        if (arg.check)(&qualified).is_ok()
            && let Some(slot) = kwargs.iter_mut().find(|(key, _)| key == arg.name)
        {
            slot.1 = qualified;
        }
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

/// Explain which argument failed to deserialize.
pub fn diagnose_args(
    name: &str,
    entry: &crate::commands::CommandEntry,
    kwargs: value::Fields,
    raw: DslError,
) -> DslError {
    for arg in entry.args {
        let Some(value) = kwargs.iter().find(|(key, _)| key == arg.name).map(|(_, v)| v) else {
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
pub fn parse_call(input: &str) -> Result<(String, value::Fields), DslError> {
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

/// Parse a document of commands, skipping blank and `#` lines.
///
/// One command per line, except that a raw string may span lines.
pub fn from_dsl_many(input: &str) -> Result<Vec<Box<dyn Command>>, DslError> {
    split_commands(input).iter().map(|s| from_dsl(s)).collect()
}

/// Split a document at the newlines outside strings.
pub fn split_commands(input: &str) -> Vec<String> {
    fn flush(out: &mut Vec<String>, current: &mut String) {
        let line = current.trim();
        if !line.is_empty() && !line.starts_with('#') {
            out.push(line.to_string());
        }
        current.clear();
    }

    let mut out = Vec::new();
    let mut current = String::new();
    let mut rest = input;
    while let Some(ch) = rest.chars().next() {
        if ch == '\n' {
            flush(&mut out, &mut current);
            rest = &rest[1..];
            continue;
        }
        // The lexer decides where a string ends.
        if (ch == '"' || ch == 'r')
            && let Some(len) = lexer::leading_string_len(rest)
        {
            current.push_str(&rest[..len]);
            rest = &rest[len..];
            continue;
        }
        current.push(ch);
        rest = &rest[ch.len_utf8()..];
    }
    flush(&mut out, &mut current);
    out
}

#[cfg(test)]
mod tests {
    use crate::address::AddressRange;
    use crate::commands::{
        self, AutoDisassemble, MapBytes, SetComment, SetFunction, SetLabel, SetNote, UnmapBytes,
    };
    use crate::db::Function;
    use crate::note::Note;

    use super::{dsl, from_dsl, parse_call, to_dsl};

    #[test]
    fn dsl_macro_renders_calls() {
        let at = "CODE:0x4";
        let space = "CODE";
        let len = 2u32;
        let cases = [
            (dsl!(peek(address = {at}, lines = 4)), "peek(address=CODE:0x4, lines=4)"),
            (
                dsl!(set_label(address = {at}, label = ".loop", local = True)),
                "set_label(address=CODE:0x4, label=\".loop\", local=True)",
            ),
            (
                dsl!(mark_data(range = {format!("{at}..0x6")}, data_type = DataType::Byte)),
                "mark_data(range=CODE:0x4..0x6, data_type=DataType::Byte)",
            ),
            (
                dsl!(map_bytes(address = {at}, file = "...", file_offset = {len:#x}, size = 0x0)),
                "map_bytes(address=CODE:0x4, file=\"...\", file_offset=0x2, size=0x0)",
            ),
            (
                dsl!(set_note(address = {at}, note = Note(content = "..."))),
                "set_note(address=CODE:0x4, note=Note(content=\"...\"))",
            ),
            (
                dsl!(set_operand_pointer(address = {at}, space = "{space}")),
                "set_operand_pointer(address=CODE:0x4, space=\"CODE\")",
            ),
            (
                dsl!(auto_disassemble(address = {at}) # "after clearing {space}"),
                "auto_disassemble(address=CODE:0x4)  # after clearing CODE",
            ),
        ];
        for (rendered, expected) in cases {
            assert_eq!(rendered, expected);
            parse_call(&rendered).expect("parses");
        }
    }

    #[test]
    fn round_trip_auto_disassemble() {
        let command = commands::boxed(AutoDisassemble::new((crate::platform::i8051::CODE, 0x1234)));
        let dsl = to_dsl(&*command);
        assert_eq!(dsl, "auto_disassemble(address=CODE:0x1234)");
        assert_eq!(&*from_dsl(&dsl).unwrap(), &*command);
    }

    #[test]
    fn round_trip_unmap_range() {
        let command = commands::boxed(UnmapBytes::new((crate::platform::i8051::CODE, 0x10..0x20)));
        let dsl = to_dsl(&*command);
        assert_eq!(dsl, "unmap_bytes(addresses=CODE:{0x10..0x20})");
        assert_eq!(&*from_dsl(&dsl).unwrap(), &*command);
    }

    #[test]
    fn round_trip_set_label() {
        let command = commands::boxed(SetLabel::new((crate::platform::i8051::CODE, 0x100), "reset_vector", false, false));
        let dsl = to_dsl(&*command);
        assert_eq!(
            dsl,
            "set_label(address=CODE:0x100, label=\"reset_vector\", provisional=False, local=False)"
        );
        assert_eq!(&*from_dsl(&dsl).unwrap(), &*command);
    }

    #[test]
    fn round_trip_multiline_string() {
        let command = commands::boxed(SetComment::new(
            (crate::platform::i8051::CODE, 0x10),
            "line one\nline two",
        ));
        let dsl = to_dsl(&*command);
        assert!(dsl.starts_with("set_comment(address=CODE:0x10, comment=r#\""), "{dsl}");
        assert_eq!(&*from_dsl(&dsl).unwrap(), &*command);
    }

    #[test]
    fn raw_string_spans_lines() {
        let doc = "set_cpu(name=\"i8051\")\n\
                   set_comment(address=CODE:0x0, comment=r#\"first line\n\
                   second line\n\
                   # not a comment, it is inside the string\"#)\n\
                   # a real comment\n\
                   set_label(address=CODE:0x4, label=\"after\", provisional=False)\n";

        let commands = super::from_dsl_many(doc).expect("a multi-line note parses");
        assert_eq!(commands.len(), 3, "{:?}", commands.iter().map(|c| c.name()).collect::<Vec<_>>());
        assert_eq!(commands[1].name(), "set_comment");

        let text = to_dsl(&*commands[1]);
        assert!(text.contains("second line"), "{text}");
        assert!(text.contains("# not a comment"), "the `#` line is content: {text}");
        // And the whole document round-trips.
        assert_eq!(super::from_dsl_many(&super::to_dsl_many(&commands)).unwrap().len(), 3);
    }

    #[test]
    fn lightest_delimiters_chosen() {
        let render = |text: &str| {
            let command = commands::boxed(SetComment::new(
                (crate::platform::i8051::CODE, 0x10),
                text,
            ));
            let dsl = to_dsl(&*command);
            // Whatever form was chosen, it has to parse back to the same text.
            assert_eq!(&*from_dsl(&dsl).unwrap(), &*command, "{dsl}");
            dsl
        };
        assert!(render("plain words").contains("comment=\"plain words\""));
        // A backslash needs raw, but no padding.
        assert!(render("a\\\\b").contains("comment=r\"a\\\\b\""));
        // Multi-line and quoted both take the padded form.
        assert!(render("two\nlines").contains("comment=r#\""));
        assert!(render("say \"hi\"").contains("comment=r#\""));
    }

    #[test]
    fn round_trip_quoted_raw() {
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
            "set_function(address=CODE:0x0, function=Function(addr=PhysicalAddr(space=\"CODE\", \
             offset=0x0), name=\"main\", signature=\"void main(void)\", length=0x40, \
             noreturn=False))"
        );
        assert_eq!(&*from_dsl(&dsl).unwrap(), &*command);
    }

    #[test]
    fn round_trip_note_minimal() {
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
    fn note_without_id_allocates() {
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
    fn bare_address_coerces_range() {
        let decoded = from_dsl("set_note(address=CODE:0x26, note=Note(content=\"hi\"))").unwrap();
        let cmd = decoded.as_any().downcast_ref::<SetNote>().expect("SetNote");
        assert_eq!(cmd.address.range, AddressRange::new(0x26, 0x27));
        assert!(to_dsl(&*decoded).contains("address=CODE:0x26..0x27"));
    }

    #[test]
    fn address_coerces_to_set() {
        let range = from_dsl("unmap_bytes(addresses=CODE:0x10..0x20)").unwrap();
        assert_eq!(to_dsl(&*range), "unmap_bytes(addresses=CODE:{0x10..0x20})");
        let single = from_dsl("unmap_bytes(addresses=CODE:0x10)").unwrap();
        assert_eq!(to_dsl(&*single), "unmap_bytes(addresses=CODE:{0x10})");
    }

    #[test]
    fn quoted_addresses_accepted() {
        let quoted = from_dsl(r#"set_label(address="CODE:0x100", label="reset_vector")"#).unwrap();
        let bare = from_dsl(r#"set_label(address=CODE:0x100, label="reset_vector")"#).unwrap();
        assert_eq!(&*quoted, &*bare);
        let range = from_dsl(r#"set_note(address="CODE:0x10..0x12", note=Note(content="x"))"#).unwrap();
        let cmd = range.as_any().downcast_ref::<SetNote>().expect("SetNote");
        assert_eq!(cmd.address.range, AddressRange::new(0x10, 0x12));
    }

    #[test]
    fn bare_identifiers_are_strings() {
        let bare = from_dsl("set_label(address=CODE:0x100, label=reset_vector)").unwrap();
        let quoted = from_dsl(r#"set_label(address=CODE:0x100, label="reset_vector")"#).unwrap();
        assert_eq!(&*bare, &*quoted);
    }

    #[test]
    fn bare_note_is_content() {
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
    fn stray_arguments_named() {
        let typo = from_dsl(r#"set_label(adress=CODE:0x0, label="x")"#).unwrap_err();
        assert!(typo.message.contains("unknown argument `adress`"), "{}", typo.message);
        assert!(typo.message.contains("did you mean `address`?"), "{}", typo.message);
        let missing = from_dsl(r#"set_note(address=CODE:0x0..0x1)"#).unwrap_err();
        assert!(missing.message.contains("missing argument `note`"), "{}", missing.message);
        assert!(missing.message.contains("Note(content="), "{}", missing.message);
    }

    #[test]
    fn shape_mismatch_names_argument() {
        let err = from_dsl(r#"set_label(address=CODE:0x0..0x10, label="x")"#).unwrap_err();
        assert!(err.message.contains("argument `address` of `set_label`"), "{}", err.message);
        assert!(err.message.contains("a DSL address, e.g. CODE:0x100"), "{}", err.message);
        assert!(err.message.contains("got CODE:0x0..0x10"), "{}", err.message);
        let e = from_dsl("mark_data(range=CODE:0x0..0x4, data_type=DataType::Bogus)").unwrap_err();
        assert!(e.message.contains("argument `data_type` of `mark_data`"), "{}", e.message);
        assert!(e.message.contains("unknown variant `Bogus`"), "{}", e.message);
    }

    #[test]
    fn value_parses_bare_address() {
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
    fn missing_field_not_positional() {
        let err = from_dsl("set_label(address=CODE:0x0)").unwrap_err();
        assert_eq!(err.offset, None);
        let shown = err.to_string();
        assert!(!shown.contains("at byte"), "got: {shown}");
        assert!(shown.contains("missing argument `label`"), "got: {shown}");
    }

    #[test]
    fn positional_error_keeps_offset() {
        let err = from_dsl("set_label(address=CODE:0xZZ)").unwrap_err();
        assert!(err.offset.is_some());
        assert!(err.to_string().contains("byte"));
    }

    #[test]
    fn bare_variant_qualifies() {
        let bare = from_dsl("mark_data(range=CODE:0x10..0x12, data_type=Byte)")
            .expect("a bare variant is unambiguous here");
        let qualified = from_dsl("mark_data(range=CODE:0x10..0x12, data_type=DataType::Byte)")
            .expect("the full form still works");
        assert_eq!(to_dsl(bare.as_ref()), to_dsl(qualified.as_ref()));

        // Not a variant
        let err = from_dsl("mark_data(range=CODE:0x10..0x12, data_type=Sandwich)")
            .expect_err("an unknown variant fails");
        assert!(err.message.contains("DataType::Byte"), "{}", err.message);
    }
}
