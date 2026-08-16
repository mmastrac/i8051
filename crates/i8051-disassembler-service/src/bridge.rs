use serde_json::Value as Json;

use i8051_disassembler::commands::{ArgKind, COMMANDS};
use i8051_disassembler::store::value::Value;
use i8051_disassembler::store::{from_dsl, parse_value, to_dsl};

use crate::ServiceError;

pub(crate) fn command_focus(dsl: &str) -> Option<String> {
    let command = from_dsl(dsl).ok()?;
    let Value::Struct { fields, .. } = command.to_value() else {
        return None;
    };
    fields.values().find_map(focus_of_value)
}

pub(crate) fn build_command_dsl(
    name: &str,
    args: &serde_json::Map<String, Json>,
) -> Result<String, ServiceError> {
    let entry = COMMANDS
        .get(name)
        .ok_or_else(|| ServiceError::Parse(format!("unknown command `{name}`")))?;

    for key in args.keys() {
        if !entry.args.iter().any(|a| a.name == key) {
            let hint =
                i8051_disassembler::commands::closest(key, entry.args.iter().map(|a| a.name))
                    .map(|h| format!(" (did you mean `{h}`?)"))
                    .unwrap_or_default();
            return Err(ServiceError::Parse(format!(
                "unknown argument `{key}` to `{name}`{hint}"
            )));
        }
    }

    let mut kwargs = std::collections::BTreeMap::new();
    for arg in entry.args {
        let Some(json) = args.get(arg.name) else {
            if arg.kind == ArgKind::Flag {
                kwargs.insert(arg.name.to_string(), Value::Bool(false));
                continue;
            }
            return Err(ServiceError::Parse(format!(
                "missing argument `{}`: {}",
                arg.name, arg.hint
            )));
        };
        let value = json_to_value(arg.kind, json)
            .map_err(|e| ServiceError::Parse(format!("argument `{}`: {e}", arg.name)))?;
        kwargs.insert(arg.name.to_string(), value);
    }
    i8051_disassembler::store::qualify_bare_variants(entry, &mut kwargs);
    let command = (entry.parse)(kwargs.clone()).map_err(|raw| {
        ServiceError::Parse(
            i8051_disassembler::store::diagnose_args(name, entry, kwargs, raw).to_string(),
        )
    })?;
    Ok(to_dsl(command.as_ref()))
}

fn json_to_value(kind: ArgKind, json: &Json) -> Result<Value, String> {
    let parse = |s: &str| parse_value(s).map_err(|e| e.to_string());
    match kind {
        ArgKind::Text => json
            .as_str()
            .map(|s| Value::String(s.to_string()))
            .ok_or_else(|| "expected a string".to_string()),
        ArgKind::Flag => match json {
            Json::Bool(b) => Ok(Value::Bool(*b)),
            Json::String(s) => match s.as_str() {
                "True" | "true" => Ok(Value::Bool(true)),
                "False" | "false" => Ok(Value::Bool(false)),
                _ => Err("expected True or False".to_string()),
            },
            _ => Err("expected a boolean".to_string()),
        },
        ArgKind::Byte | ArgKind::Offset => match json {
            Json::Number(n) => n
                .as_u64()
                .map(Value::Int)
                .ok_or_else(|| "expected a non-negative integer".to_string()),
            Json::String(s) => parse(s),
            _ => Err("expected an integer".to_string()),
        },
        _ => {
            let s = json.as_str().ok_or("expected a DSL spelling string")?;
            Ok(parse(s).unwrap_or_else(|_| Value::String(s.to_string())))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn args(v: serde_json::Value) -> serde_json::Map<String, Json> {
        v.as_object().expect("object").clone()
    }

    #[test]
    fn bare_note_is_content() {
        let a = args(json!({
            "address": "CODE:0x26..0x47",
            "note": "System init. Sets SFRs for UART and Timers.",
        }));
        let dsl = build_command_dsl("set_note", &a).expect("bare-string note builds");
        assert!(dsl.contains("System init. Sets SFRs for UART and Timers."), "{dsl}");
        from_dsl(&dsl).expect("built DSL parses");
    }

    #[test]
    fn explicit_note_builds() {
        let a = args(json!({ "address": "CODE:0x26", "note": "Note(content=\"x\", tags=[\"todo\"])" }));
        let dsl = build_command_dsl("set_note", &a).expect("explicit Note builds");
        from_dsl(&dsl).expect("parses");
    }
}

fn focus_of_value(value: &Value) -> Option<String> {
    match value {
        Value::Address { space, offset } => Some(format!("{space}:{offset:#x}")),
        Value::AddressRange { space, start, .. } => Some(format!("{space}:{start:#x}")),
        Value::AddressSet { space, ranges } => {
            ranges.first().map(|&(start, _)| format!("{space}:{start:#x}"))
        }
        _ => None,
    }
}
