use serde::Serialize;

use crate::{VerbArg, VerbInfo};

pub trait ValueSource {
    /// Address-space names, e.g. `["CODE", "XDATA"]`.
    fn spaces(&self) -> Vec<String>;
    fn symbols(&self) -> Vec<(String, String)>;
}

impl ValueSource for crate::Session {
    fn spaces(&self) -> Vec<String> {
        self.memory_map().into_iter().map(|u| u.space).collect()
    }
    fn symbols(&self) -> Vec<(String, String)> {
        self.symbols(None)
            .unwrap_or_default()
            .into_iter()
            .map(|s| (s.name, s.addr))
            .collect()
    }
}

#[derive(Debug, Default, Serialize, PartialEq, Eq)]
/// Candidates and the span they replace.
pub struct Completion {
    /// Byte offset where the replaced token begins.
    pub start: usize,
    pub candidates: Vec<Candidate>,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
/// One completion and how it was found.
pub struct Candidate {
    pub replacement: String,
    pub display: String,
}

/// Complete a partial line at a cursor.
pub fn complete(
    catalog: &[VerbInfo],
    line: &str,
    cursor: usize,
    values: Option<&dyn ValueSource>,
) -> Completion {
    let cursor = cursor.min(line.len());
    let head = &line[..cursor];

    let Some(open) = head.find('(') else {
        let start = head.len() - head.trim_start().len();
        let token = &head[start..];
        let candidates = catalog
            .iter()
            .filter(|v| v.name.starts_with(token))
            .map(|v| Candidate {
                replacement: v.name.clone(),
                display: format!("{}  {}", v.name, summary(&v.description)),
            })
            .collect();
        return Completion { start, candidates };
    };

    let verb_name = head[..open].trim();
    let Some(verb) = catalog.iter().find(|v| v.name == verb_name) else {
        return Completion::default();
    };

    let mut depth = 0i32;
    let mut seg_start = open + 1;
    let mut present: Vec<&str> = Vec::new();
    let mut name_start = open + 1; // start of the pending `ident` at depth 1
    let bytes = head.as_bytes();
    let mut i = open;
    while i < bytes.len() {
        match bytes[i] {
            b'(' | b'{' | b'[' => depth += 1,
            b')' | b'}' | b']' => depth -= 1,
            b',' if depth == 1 => {
                seg_start = i + 1;
                name_start = i + 1;
            }
            b'=' if depth == 1 => {
                let name = head[name_start..i].trim();
                if !name.is_empty() {
                    present.push(name);
                }
            }
            _ => {}
        }
        i += 1;
    }

    if depth != 1 {
        return Completion::default();
    }

    if let Some(eq) = head[seg_start..].find('=') {
        let arg_name = head[seg_start..seg_start + eq].trim();
        let Some(arg) = verb.args.iter().find(|a| a.name == arg_name) else {
            return Completion::default();
        };
        let partial = head[seg_start + eq + 1..].trim_start();
        let start = cursor - partial.len();
        let candidates = value_candidates(catalog, arg, partial, values);
        return Completion { start, candidates };
    }

    let token = head[seg_start..].trim_start();
    let start = cursor - token.len();
    let candidates = verb
        .args
        .iter()
        .filter(|a| a.name.starts_with(token) && !present.contains(&a.name.as_str()))
        .map(|a| Candidate {
            replacement: format!("{}=", a.name),
            display: format!("{}  {}", a.name, summary(&a.description)),
        })
        .collect();
    Completion { start, candidates }
}

fn summary(doc: &str) -> &str {
    doc.lines().next().unwrap_or("").trim()
}

const DATA_TYPES: &[&str] = &[
    "DataType::Byte",
    "DataType::Word",
    "DataType::Dword",
    "DataType::Qword",
    "DataType::Reference(DataType::Word)",
    "DataType::Array(DataType::Byte, 0x10)",
    "DataType::String(0x10)",
    "DataType::Struct([DataType::Byte, DataType::Word])",
];

fn value_candidates(
    catalog: &[VerbInfo],
    arg: &VerbArg,
    partial: &str,
    values: Option<&dyn ValueSource>,
) -> Vec<Candidate> {
    let plain = |replacement: String, hint: &str| Candidate {
        display: format!("{replacement}  {hint}"),
        replacement,
    };
    let mut out: Vec<Candidate> = match arg.kind.as_str() {
        "data_type" => DATA_TYPES
            .iter()
            .map(|v| plain(v.to_string(), ""))
            .collect(),
        "boolean" => ["True", "False"]
            .iter()
            .map(|v| plain(v.to_string(), ""))
            .collect(),
        "gate" => ["structural", "named", "documented"]
            .iter()
            .map(|v| plain(format!("\"{v}\""), "gate"))
            .collect(),
        "phase" => ["decode", "classify", "name", "document"]
            .iter()
            .map(|v| plain(format!("\"{v}\""), "phase"))
            .collect(),
        "space" => values
            .map(|v| v.spaces())
            .unwrap_or_default()
            .into_iter()
            .map(|s| plain(format!("\"{s}\""), "address space"))
            .collect(),
        "command" => catalog
            .iter()
            .map(|v| plain(format!("\"{}\"", v.name), summary(&v.description)))
            .collect(),
        "note" => vec![Candidate {
            replacement: "Note(content=\"\", tags=[\"todo\"])".to_string(),
            display: "Note(content=\"...\", tags=[...])  or bare \"text\"".to_string(),
        }],
        "operand" => vec![plain("None".to_string(), "clear the override")],
        "address" | "address_range" | "address_set" => values
            .map(|v| v.symbols())
            .unwrap_or_default()
            .into_iter()
            .map(|(name, addr)| Candidate {
                display: format!("{name}  {addr}"),
                replacement: addr,
            })
            .collect(),
        _ => Vec::new(),
    };
    out.retain(|c| {
        c.replacement.starts_with(partial)
            || c.display.starts_with(partial)
            || c.replacement
                .strip_prefix('"')
                .is_some_and(|r| r.starts_with(partial))
    });
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn catalog() -> Vec<VerbInfo> {
        crate::verbs::catalog()
    }

    fn names(c: &Completion) -> Vec<String> {
        c.candidates.iter().map(|x| x.replacement.clone()).collect()
    }

    #[test]
    fn empty_line_lists_verbs() {
        let cat = catalog();
        let c = complete(&cat, "", 0, None);
        assert_eq!(c.start, 0);
        assert!(names(&c).contains(&"set_label".to_string()));
        assert!(names(&c).contains(&"status".to_string()));
        assert!(names(&c).contains(&"navigate".to_string()));
    }

    #[test]
    fn verb_prefix_narrows() {
        let cat = catalog();
        let c = complete(&cat, "  set_l", 7, None);
        assert_eq!(c.start, 2, "replacement begins after the leading spaces");
        assert_eq!(names(&c), vec!["set_label".to_string()]);
    }

    #[test]
    fn parens_complete_args() {
        let cat = catalog();
        let c = complete(&cat, "set_label(", 10, None);
        assert_eq!(c.start, 10);
        assert!(names(&c).contains(&"label=".to_string()));
        assert!(names(&c).contains(&"address=".to_string()));
    }

    #[test]
    fn present_args_skipped() {
        let cat = catalog();
        let line = r#"set_label(address=CODE:0x0, "#;
        let c = complete(&cat, line, line.len(), None);
        let offered = names(&c);
        assert!(offered.iter().all(|r| !r.starts_with("address")));
        assert!(offered.contains(&"label=".to_string()));
    }

    #[test]
    fn partial_arg_narrows() {
        let cat = catalog();
        let line = "listing(sp";
        let c = complete(&cat, line, line.len(), None);
        assert_eq!(c.start, line.len() - 2);
        assert_eq!(names(&c), vec!["space=".to_string()]);
    }

    #[test]
    fn no_completion_inside_value() {
        let cat = catalog();
        let line = "navigate(address=CODE:0x1";
        let c = complete(&cat, line, line.len(), None);
        assert!(c.candidates.is_empty());
    }

    #[test]
    fn nested_commas_dont_leak() {
        let cat = catalog();
        let line = r#"set_note(address=CODE:0x0..0x10, note=Note(content="a, "#;
        let c = complete(&cat, line, line.len(), None);
        assert!(
            c.candidates.is_empty(),
            "no verb-arg names inside a nested value"
        );
    }

    #[test]
    fn unknown_verb_offers_nothing() {
        let cat = catalog();
        let c = complete(&cat, "frobnicate(", 11, None);
        assert!(c.candidates.is_empty());
    }

    struct FakeValues;
    impl ValueSource for FakeValues {
        fn spaces(&self) -> Vec<String> {
            vec!["CODE".into(), "XDATA".into()]
        }
        fn symbols(&self) -> Vec<(String, String)> {
            vec![
                ("reset".into(), "CODE:0x0".into()),
                ("main".into(), "CODE:0x100".into()),
            ]
        }
    }

    #[test]
    fn data_type_values_parse() {
        let cat = catalog();
        let line = "mark_data(range=CODE:0x0..0x4, data_type=DataType::B";
        let c = complete(&cat, line, line.len(), None);
        assert_eq!(names(&c), vec!["DataType::Byte".to_string()]);
        for v in DATA_TYPES {
            i8051_disassembler::store::from_dsl_value::<i8051_disassembler::db::DataType>(v)
                .unwrap_or_else(|e| panic!("{v}: {e}"));
        }
    }

    #[test]
    fn gate_and_bool_values() {
        let cat = catalog();
        let line = "status(gate=str";
        let c = complete(&cat, line, line.len(), None);
        assert_eq!(names(&c), vec!["\"structural\"".to_string()]);
        let line = "disassembly(full=T";
        let c = complete(&cat, line, line.len(), None);
        assert_eq!(names(&c), vec!["True".to_string()]);
    }

    #[test]
    fn values_from_source() {
        let cat = catalog();
        let line = r#"listing(space="X"#;
        let c = complete(&cat, line, line.len(), Some(&FakeValues));
        assert_eq!(names(&c), vec!["\"XDATA\"".to_string()]);
        let line = "navigate(address=res";
        let c = complete(&cat, line, line.len(), Some(&FakeValues));
        assert_eq!(names(&c), vec!["CODE:0x0".to_string()]);
        assert!(c.candidates[0].display.starts_with("reset"));
        let c = complete(&cat, line, line.len(), None);
        assert!(c.candidates.is_empty());
    }

    #[test]
    fn note_offers_template() {
        let cat = catalog();
        let line = "set_note(address=CODE:0x0..0x1, note=";
        let c = complete(&cat, line, line.len(), None);
        assert_eq!(
            names(&c),
            vec!["Note(content=\"\", tags=[\"todo\"])".to_string()]
        );
    }
}
