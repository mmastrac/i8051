use serde_json::Value;

/// A verb's result as text, when obvious.
pub fn render_human(verb: &str, value: &Value) -> Option<String> {
    match verb {
        "listing" => listing(value),
        "xrefs_to" | "xrefs_from" => xrefs(value),
        "symbols" => symbols(value),
        "memory_map" => memory_map(value),
        "peek" => peek(value),
        "help" => help(value),
        "save" => save(value),
        _ => edit_summary(value),
    }
}

fn edit_summary(value: &Value) -> Option<String> {
    let undo = value.get("undo")?.as_array()?;
    let depth = value.get("undo_depth").and_then(Value::as_u64)?;
    let mut out = String::from("ok");
    if let Some(addr) = value.get("address").and_then(Value::as_str) {
        out.push_str(&format!(" @ {addr}"));
    }
    if let Some(first) = undo.first().and_then(Value::as_str) {
        out.push_str(&format!("  (undo: {first})"));
    }
    out.push_str(&format!("  [{depth} on undo stack]"));
    Some(out)
}

fn listing(value: &Value) -> Option<String> {
    let lines = value.get("lines")?.as_array()?;
    let total = value.get("total").and_then(Value::as_u64).unwrap_or(0);
    let start = value.get("start").and_then(Value::as_u64).unwrap_or(0);
    let space = value.get("space").and_then(Value::as_str).unwrap_or("");
    let mut out = format!(
        "{space} lines {start}..{} of {total}\n",
        start + lines.len() as u64
    );
    for info in lines {
        let offset = info.get("offset").and_then(Value::as_u64).unwrap_or(0);
        let line = info.get("line")?;
        let text = line_text(line);
        if text.is_empty() {
            out.push('\n');
        } else {
            out.push_str(&format!("{offset:#06x}  {text}\n"));
        }
    }
    Some(out)
}

fn line_text(line: &Value) -> String {
    if line.as_str() == Some("Blank") {
        return String::new();
    }
    let obj = match line.as_object().and_then(|o| o.iter().next()) {
        Some(kv) => kv,
        None => return String::new(),
    };
    let (variant, body) = obj;
    let s = |key: &str| body.get(key).and_then(Value::as_str).unwrap_or("");
    match variant.as_str() {
        "Instruction" => s("text").trim_start().to_string(),
        "Label" => format!("{}:", s("name")),
        "Comment" => format!("; {}", s("text")),
        "Function" => {
            let sig = body.get("signature").and_then(Value::as_str);
            match sig {
                Some(sig) => format!("; function {} {}", s("name"), sig),
                None => format!("; function {}", s("name")),
            }
        }
        "Data" | "Raw" => format!(".db {}", hex_bytes(body.get("bytes"))),
        "Run" => {
            let n = |key: &str| body.get(key).and_then(Value::as_u64).unwrap_or(0);
            format!(".ds {:#x} ; 0x{:02x} x {}", n("len"), n("value"), n("len"))
        }
        "Block" => {
            let count = body.get("count").and_then(Value::as_u64).unwrap_or(0);
            format!(".db {} ; x {count}", hex_bytes(body.get("unit")))
        }
        "Region" => {
            let kind = body.get("kind").and_then(Value::as_str).unwrap_or("unknown");
            let label = if kind == "unknown" { "Unknown bytes" } else { kind };
            format!("; {label}")
        }
        "Org" => format!(
            ".org {:#06x}",
            body.get("addr").and_then(Value::as_u64).unwrap_or(0)
        ),
        _ => String::new(),
    }
}

fn hex_bytes(bytes: Option<&Value>) -> String {
    let Some(items) = bytes.and_then(Value::as_array) else {
        return String::new();
    };
    items
        .iter()
        .filter_map(Value::as_u64)
        .map(|b| format!("{b:#04x}"))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Xrefs as one aligned row per edge.
fn xrefs(value: &Value) -> Option<String> {
    let edges = value.as_array()?;
    if edges.is_empty() {
        return Some("none".to_string());
    }
    let rows: Vec<String> = edges
        .iter()
        .map(|x| {
            let s = |key: &str| x.get(key).and_then(Value::as_str).unwrap_or("?");
            format!("{:<8} {} -> {}", s("kind"), s("from"), s("to"))
        })
        .collect();
    Some(rows.join("\n"))
}

/// Symbols as `kind name  addr` rows.
fn symbols(value: &Value) -> Option<String> {
    let syms = value.as_array()?;
    if syms.is_empty() {
        return Some("none".to_string());
    }
    let rows: Vec<String> = syms
        .iter()
        .map(|s| {
            let get = |key: &str| s.get(key).and_then(Value::as_str).unwrap_or("?");
            let marker = if get("kind") == "function" { "fn" } else { "  " };
            format!("{marker} {:<24} {}", get("name"), get("addr"))
        })
        .collect();
    Some(rows.join("\n"))
}

fn memory_map(value: &Value) -> Option<String> {
    let spaces = value.as_array()?;
    let rows: Vec<String> = spaces
        .iter()
        .map(|u| {
            let n = |key: &str| u.get(key).and_then(Value::as_u64).unwrap_or(0);
            format!(
                "{:<8} code {:<6} data {:<6} undefined {:<6} of {}",
                u.get("space").and_then(Value::as_str).unwrap_or("?"),
                n("code"),
                n("data"),
                n("undefined"),
                n("total")
            )
        })
        .collect();
    Some(rows.join("\n"))
}

fn peek(value: &Value) -> Option<String> {
    let text = value.get("text")?.as_str()?;
    let verdict = value.get("verdict").and_then(Value::as_str).unwrap_or("?");
    let note = value.get("note").and_then(Value::as_str).unwrap_or("");
    let commit = value
        .get("commit_with")
        .and_then(Value::as_str)
        .map(|c| format!("\nnothing was committed — run `{c}` to decode these bytes for real"))
        .unwrap_or_else(|| "\nnothing was committed; these bytes are unchanged".to_string());
    Some(format!("[{verdict}] {note}\n{text}{commit}"))
}

fn help(value: &Value) -> Option<String> {
    if let (Some(name), Some(desc)) = (
        value.get("name").and_then(Value::as_str),
        value.get("description").and_then(Value::as_str),
    ) {
        let mut out = format!("{name} — {desc}\n");
        if let Some(args) = value.get("args").and_then(Value::as_array) {
            for a in args {
                let s = |key: &str| a.get(key).and_then(Value::as_str).unwrap_or("");
                let required = a.get("required").and_then(Value::as_bool).unwrap_or(true);
                let opt = if required { "" } else { " (optional)" };
                out.push_str(&format!("  {}{opt}: {}\n", s("name"), s("description")));
            }
        }
        if let Some(example) = value.get("example").and_then(Value::as_str) {
            out.push_str(&format!("e.g. {example}\n"));
        }
        return Some(out);
    }
    let verbs = value.get("verbs")?.as_array()?;
    let mut out = String::new();
    if let Some(usage) = value.get("usage").and_then(Value::as_str) {
        out.push_str(usage);
        out.push('\n');
    }
    for v in verbs {
        let s = |key: &str| v.get(key).and_then(Value::as_str).unwrap_or("");
        out.push_str(&format!("  {:<44} {}\n", s("example"), s("doc")));
    }
    Some(out)
}

fn save(value: &Value) -> Option<String> {
    let path = value.get("path")?.as_str()?;
    let commands = value.get("commands").and_then(Value::as_u64).unwrap_or(0);
    let diff = value.get("diff").and_then(Value::as_bool).unwrap_or(false);
    let form = if diff { "diff" } else { "full" };
    Some(format!("saved {commands} record(s) to {path} ({form})"))
}

/// Point at where a line failed.
pub fn caret_diagnostic(line: &str, error: &str) -> Option<String> {
    let at = error.find("at byte ")?;
    let digits: String = error[at + 8..]
        .chars()
        .take_while(char::is_ascii_digit)
        .collect();
    let offset: usize = digits.parse().ok()?;
    if offset > line.len() {
        return None;
    }
    Some(format!("{line}\n{}^", " ".repeat(offset)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn listing_renders_assembly() {
        let value = json!({
            "space": "CODE", "total": 3, "start": 0,
            "lines": [
                {"index": 0, "addr": "CODE:0x0", "offset": 0, "line": {"Label": {"addr": 0, "name": "reset"}}},
                {"index": 1, "addr": "CODE:0x0", "offset": 0, "line": {"Instruction": {"addr": 0, "text": "    NOP", "bytes": [0], "direct": null}}},
                {"index": 2, "addr": "CODE:0x1", "offset": 1, "line": "Blank"},
            ],
        });
        let text = render_human("listing", &value).expect("renders");
        assert!(text.contains("reset:"), "{text}");
        assert!(text.contains("0x0000  NOP"), "{text}");
        assert!(!text.contains("Instruction"), "no enum leak: {text}");
    }

    #[test]
    fn xrefs_render_per_row() {
        let value = json!([{ "from": "CODE:0x2", "to": "CODE:0x10", "kind": "call" }]);
        let text = render_human("xrefs_to", &value).expect("renders");
        assert_eq!(text, "call     CODE:0x2 -> CODE:0x10");
    }

    #[test]
    fn unknown_verb_falls_back() {
        assert!(render_human("status", &json!({})).is_none());
    }

    #[test]
    fn caret_lands_on_byte() {
        let line = "listing(space=CODE)";
        let err = "dsl parse error at byte 18: unexpected token after identifier CODE";
        let out = caret_diagnostic(line, err).expect("has offset");
        let rows: Vec<&str> = out.lines().collect();
        assert_eq!(rows[0], line);
        assert_eq!(rows[1].len(), 19);
        assert!(rows[1].ends_with('^'));
        assert!(caret_diagnostic(line, "unknown verb `x`").is_none());
    }
}
