use serde::Serialize;
use serde_json::{Map, Value};

use i8051_disassembler::commands::{ArgKind, COMMANDS, CommandArg};

use crate::{Controller, EditResponse, NavResponse, ServiceError, Session};

/// Default listing window when `count` is omitted.
const DEFAULT_LISTING: usize = 40;

/// Which surface a verb belongs to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
/// What a verb does.
pub enum Category {
    Edit,
    /// Read-only.
    Query,
    Nav,
}

#[derive(Debug, Clone, Copy, Serialize)]
#[serde(rename_all = "snake_case")]
/// The shape of a verb argument.
pub enum ArgType {
    String,
    Integer,
    Boolean,
}

#[derive(Debug, Clone, Serialize)]
/// One argument's name and type.
pub struct VerbArg {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: ArgType,
    pub kind: String,
    pub required: bool,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub description: String,
}

#[derive(Debug, Clone, Serialize)]
/// A verb and its argument schema.
pub struct VerbInfo {
    pub name: String,
    pub category: Category,
    pub description: String,
    pub args: Vec<VerbArg>,
    pub example: String,
}

/// Every verb a frontend can invoke.
pub fn catalog() -> Vec<VerbInfo> {
    let mut out: Vec<VerbInfo> = (&COMMANDS)
        .into_iter()
        .map(|(name, entry)| {
            let args: Vec<VerbArg> = entry
                .args
                .iter()
                .map(|arg| VerbArg {
                    name: arg.name.to_string(),
                    ty: arg_type(arg.kind),
                    kind: arg.slug.to_string(),
                    required: true,
                    description: arg.hint.to_string(),
                })
                .collect();
            let example = call_example(
                name,
                entry.args.iter().map(|a| (a.name, edit_placeholder(a))),
            );
            VerbInfo {
                name: (*name).to_string(),
                category: Category::Edit,
                description: entry.doc.trim().to_string(),
                example,
                args,
            }
        })
        .collect();

    out.extend(VERBS.iter().map(|def| {
        let args: Vec<VerbArg> = def
            .args
            .iter()
            .map(|a| VerbArg {
                name: a.name.to_string(),
                ty: a.ty,
                kind: query_kind(a.name, a.ty).to_string(),
                required: a.required,
                description: name_hint(a.name).to_string(),
            })
            .collect();
        let example = call_example(
            def.name,
            def.args
                .iter()
                .filter(|a| a.required)
                .map(|a| (a.name, query_placeholder(a.name, a.ty))),
        );
        VerbInfo {
            name: def.name.to_string(),
            category: def.category(),
            description: def.doc.to_string(),
            example,
            args,
        }
    }));

    out.sort_by(|a, b| a.name.cmp(&b.name));
    out
}

fn call_example<'a>(name: &str, parts: impl Iterator<Item = (&'a str, String)>) -> String {
    let kwargs: Vec<(&str, i8051_script::Value)> = parts
        .map(|(name, text)| (name, i8051_script::Value::Verbatim(text)))
        .collect();
    i8051_script::render_call(name, &kwargs, None)
}

fn edit_placeholder(arg: &CommandArg) -> String {
    arg.example.map(str::to_string).unwrap_or_else(|| {
        match arg.name {
            "label" => "\"reset\"",
            "file" => "\"rom.bin\"",
            "name" => "\"i8051\"",
            _ => "\"...\"",
        }
        .to_string()
    })
}

fn query_placeholder(name: &str, ty: ArgType) -> String {
    match name {
        "address" => "CODE:0x100",
        "space" => "\"CODE\"",
        "gate" => "\"named\"",
        "phase" => "\"decode\"",
        "verb" => "\"set_note\"",
        _ => match ty {
            ArgType::Integer => "0",
            ArgType::Boolean => "True",
            ArgType::String => "\"...\"",
        },
    }
    .to_string()
}

/// Whether `name` is a registered database-mutating command.
pub(crate) fn is_edit(name: &str) -> bool {
    COMMANDS.get(name).is_some()
}

pub(crate) fn dispatch(
    controller: &mut Controller,
    name: &str,
    args: &Map<String, Value>,
) -> Option<Result<Value, ServiceError>> {
    let def = VERBS.iter().find(|d| d.name == name)?;
    for key in args.keys() {
        if !def.args.iter().any(|a| a.name == key) {
            let hint = i8051_disassembler::commands::closest(key, def.args.iter().map(|a| a.name))
                .map(|h| format!(" (did you mean `{h}`?)"))
                .unwrap_or_default();
            let expected = if def.args.is_empty() {
                format!("`{name}` takes no arguments")
            } else {
                let names: Vec<String> = def.args.iter().map(|a| format!("`{}`", a.name)).collect();
                format!("expected {}", names.join(", "))
            };
            return Some(Err(ServiceError::Parse(format!(
                "unknown argument `{key}` to `{name}`{hint}; {expected}"
            ))));
        }
    }
    let args = Args(args);
    Some(match def.handler {
        Handler::Query(run) => run(controller.session(), &args),
        Handler::Nav(run) => run(controller, &args),
    })
}

pub(crate) fn unknown_verb(name: &str) -> ServiceError {
    let catalog: Vec<String> = catalog().into_iter().map(|v| v.name).collect();
    let hint = i8051_disassembler::commands::closest(name, catalog.iter().map(String::as_str))
        .map(|h| format!(" (did you mean `{h}`?)"))
        .unwrap_or_default();
    ServiceError::Parse(format!("unknown command `{name}`{hint}"))
}

/// Serialize a verb result to JSON.
pub(crate) fn json<T: Serialize>(value: T) -> Result<Value, ServiceError> {
    serde_json::to_value(value).map_err(|e| ServiceError::Apply(e.to_string()))
}

pub(crate) fn kwargs_to_json(
    kwargs: i8051_disassembler::store::value::Fields,
) -> Map<String, Value> {
    use i8051_disassembler::store::value::Value as Dsl;
    kwargs
        .into_iter()
        .map(|(k, v)| {
            let json = match v {
                Dsl::Bool(b) => Value::Bool(b),
                Dsl::Int(n) => Value::from(n),
                Dsl::String(s) => Value::String(s),
                other => Value::String(other.render()),
            };
            (k, json)
        })
        .collect()
}

fn arg_type(kind: ArgKind) -> ArgType {
    match kind {
        ArgKind::Flag => ArgType::Boolean,
        k if k.numeric() => ArgType::Integer,
        _ => ArgType::String,
    }
}

fn query_kind(name: &str, ty: ArgType) -> &'static str {
    match name {
        "address" => "address",
        "space" => "space",
        "gate" => "gate",
        "phase" => "phase",
        "verb" => "verb",
        _ => match ty {
            ArgType::Integer => "integer",
            ArgType::Boolean => "boolean",
            ArgType::String => "text",
        },
    }
}

fn name_hint(name: &str) -> &'static str {
    match name {
        "address" => "a DSL address, e.g. CODE:0x100",
        "space" => "an address space, e.g. CODE",
        "gate" => "structural | named | documented",
        "phase" => "decode | classify | name | document",
        "kind" => "restrict to one worklist item kind",
        "limit" => "maximum items to return",
        "after" => "resume after this cursor id",
        "window" => "bytes on each side to include",
        "lines" => "how many instructions to decode",
        "full" => "return the full sdas dump",
        "start" => "first line index (0-based)",
        "count" => "number of lines",
        "query" => "case-insensitive text to match",
        "verb" => "a verb name from the catalog",
        _ => "",
    }
}

struct Args<'a>(&'a Map<String, Value>);

impl Args<'_> {
    fn present<'v>(&'v self, key: &str) -> Option<&'v Value> {
        self.0.get(key).filter(|v| !v.is_null())
    }

    fn req_str(&self, key: &str) -> Result<&str, ServiceError> {
        match self.present(key) {
            None => Err(ServiceError::Parse(format!("missing argument `{key}`"))),
            Some(Value::String(s)) => Ok(s),
            Some(other) => Err(ServiceError::Parse(format!(
                "argument `{key}` must be a string, got {other}"
            ))),
        }
    }

    fn opt_str(&self, key: &str) -> Option<&str> {
        self.present(key).and_then(Value::as_str)
    }

    fn opt_usize(&self, key: &str) -> Option<usize> {
        self.present(key)
            .and_then(Value::as_u64)
            .map(|n| n as usize)
    }

    fn opt_u64(&self, key: &str) -> Option<u64> {
        self.present(key).and_then(Value::as_u64)
    }

    fn opt_bool(&self, key: &str) -> Option<bool> {
        self.present(key).and_then(Value::as_bool)
    }
}

type QueryFn = fn(&Session, &Args) -> Result<Value, ServiceError>;
type NavFn = fn(&mut Controller, &Args) -> Result<Value, ServiceError>;

enum Handler {
    Query(QueryFn),
    Nav(NavFn),
}

struct ArgDecl {
    name: &'static str,
    ty: ArgType,
    required: bool,
}

impl ArgDecl {
    const fn req(name: &'static str, ty: ArgType) -> Self {
        Self {
            name,
            ty,
            required: true,
        }
    }
    const fn opt(name: &'static str, ty: ArgType) -> Self {
        Self {
            name,
            ty,
            required: false,
        }
    }
}

struct VerbDef {
    name: &'static str,
    doc: &'static str,
    args: &'static [ArgDecl],
    handler: Handler,
}

impl VerbDef {
    fn category(&self) -> Category {
        match self.handler {
            Handler::Query(_) => Category::Query,
            Handler::Nav(_) => Category::Nav,
        }
    }
}

use ArgType::{Boolean, Integer, String as Str};

static VERBS: &[VerbDef] = &[
    VerbDef {
        name: "help",
        doc: "The syntax of one command: its arguments, how to write them, and a filled \
              example. Without `verb`, every command with its example.",
        args: &[ArgDecl::opt("verb", Str)],
        handler: Handler::Query(|_, a| match a.opt_str("verb") {
            Some(name) => {
                let info = catalog()
                    .into_iter()
                    .find(|v| v.name == name)
                    .ok_or_else(|| unknown_verb(name))?;
                json(info)
            }
            None => {
                let verbs: Vec<Value> = catalog()
                    .into_iter()
                    .map(|v| {
                        serde_json::json!({ "verb": v.name, "example": v.example, "doc": v.description })
                    })
                    .collect();
                json(serde_json::json!({
                    "usage": format!(
                        "{} for one command's full syntax",
                        i8051_disassembler::store::dsl!(help(verb = "set_note"))
                    ),
                    "verbs": verbs,
                }))
            }
        }),
    },
    VerbDef {
        name: "status",
        doc: "How far the disassembly is from done at a gate (structural / named / documented).",
        args: &[ArgDecl::opt("gate", Str)],
        handler: Handler::Query(|s, a| json(s.status(a.opt_str("gate"))?)),
    },
    VerbDef {
        name: "next",
        doc: "The ordered worklist of outstanding items, paginated.",
        args: &[
            ArgDecl::opt("gate", Str),
            ArgDecl::opt("phase", Str),
            ArgDecl::opt("kind", Str),
            ArgDecl::opt("limit", Integer),
            ArgDecl::opt("after", Str),
        ],
        handler: Handler::Query(|s, a| {
            json(s.worklist(
                a.opt_str("gate"),
                a.opt_str("phase"),
                a.opt_str("kind"),
                a.opt_usize("limit"),
                a.opt_str("after"),
            )?)
        }),
    },
    VerbDef {
        name: "disassembly",
        doc: "A coverage overview by default; the full sdas dump with full=true (small ROMs).",
        args: &[ArgDecl::opt("full", Boolean)],
        handler: Handler::Query(|s, a| {
            if a.opt_bool("full").unwrap_or(false) {
                json(s.disassembly())
            } else {
                json(s.disassembly_overview()?)
            }
        }),
    },
    VerbDef {
        name: "listing",
        doc: "A window of rendered listing lines for one address space, each with \
              its address and structured content (label, instruction, data).",
        args: &[
            ArgDecl::req("space", Str),
            ArgDecl::opt("start", Integer),
            ArgDecl::opt("count", Integer),
        ],
        handler: Handler::Query(|s, a| {
            json(s.listing(
                a.req_str("space")?,
                a.opt_usize("start").unwrap_or(0),
                a.opt_usize("count").unwrap_or(DEFAULT_LISTING),
            )?)
        }),
    },
    VerbDef {
        name: "peek",
        doc: "Decode bytes as code from an address without committing, with a verdict.",
        args: &[ArgDecl::req("address", Str), ArgDecl::opt("lines", Integer)],
        handler: Handler::Query(|s, a| json(s.peek(a.req_str("address")?, a.opt_usize("lines"))?)),
    },
    VerbDef {
        name: "locate",
        doc: "The listing line index for an address.",
        args: &[ArgDecl::req("address", Str)],
        handler: Handler::Query(|s, a| json(s.locate(a.req_str("address")?)?)),
    },
    VerbDef {
        name: "memory_map",
        doc: "Mapped-byte coverage per address space.",
        args: &[],
        handler: Handler::Query(|s, _| json(s.memory_map())),
    },
    VerbDef {
        name: "listing_overview",
        doc: "Run-length region classification (code/data/unknown) over a space's \
              listing lines, for the scrollbar overview strip.",
        args: &[ArgDecl::req("space", Str)],
        handler: Handler::Query(|s, a| json(s.listing_overview(a.req_str("space")?)?)),
    },
    VerbDef {
        name: "symbols",
        doc: "Labels and functions, optionally restricted to one space.",
        args: &[ArgDecl::opt("space", Str)],
        handler: Handler::Query(|s, a| json(s.symbols(a.opt_str("space"))?)),
    },
    VerbDef {
        name: "xrefs_to",
        doc: "References that target an address.",
        args: &[ArgDecl::req("address", Str)],
        handler: Handler::Query(|s, a| json(s.xrefs_to(a.req_str("address")?)?)),
    },
    VerbDef {
        name: "xrefs_from",
        doc: "References made by the instruction at an address.",
        args: &[ArgDecl::req("address", Str)],
        handler: Handler::Query(|s, a| json(s.xrefs_from(a.req_str("address")?)?)),
    },
    VerbDef {
        name: "cfg",
        doc: "The basic blocks of the routine at an address.",
        args: &[ArgDecl::req("address", Str)],
        handler: Handler::Query(|s, a| json(s.cfg(a.req_str("address")?)?)),
    },
    VerbDef {
        name: "notes_near",
        doc: "Notes within a window of an address, nearest first.",
        args: &[
            ArgDecl::req("address", Str),
            ArgDecl::opt("window", Integer),
        ],
        handler: Handler::Query(|s, a| {
            json(s.notes_near(a.req_str("address")?, a.opt_u64("window"))?)
        }),
    },
    VerbDef {
        name: "notes_search",
        doc: "Notes matching a text query.",
        args: &[ArgDecl::req("query", Str)],
        handler: Handler::Query(|s, a| json(s.notes_search(a.req_str("query")?))),
    },
    VerbDef {
        name: "context",
        doc: "A one-line summary of what is at an address.",
        args: &[ArgDecl::req("address", Str)],
        handler: Handler::Query(|s, a| json(s.context(a.req_str("address")?)?)),
    },
    VerbDef {
        name: "navigate",
        doc: "Move the cursor to an address. A long jump pushes history; pass \
              local=true for an on-screen move that coalesces into one slot.",
        args: &[ArgDecl::req("address", Str), ArgDecl::opt("local", Boolean)],
        handler: Handler::Nav(|c, a| {
            let address = a.req_str("address")?;
            let location = if a.opt_bool("local").unwrap_or(false) {
                c.navigate_local(address)?
            } else {
                c.navigate(address)?
            };
            json(NavResponse::new(c.session(), location))
        }),
    },
    VerbDef {
        name: "back",
        doc: "Step back in the navigation history.",
        args: &[],
        handler: Handler::Nav(|c, _| {
            let location = c.back();
            json(NavResponse::new(c.session(), location))
        }),
    },
    VerbDef {
        name: "forward",
        doc: "Step forward in the navigation history.",
        args: &[],
        handler: Handler::Nav(|c, _| {
            let location = c.forward();
            json(NavResponse::new(c.session(), location))
        }),
    },
    VerbDef {
        name: "location",
        doc: "The current navigation location.",
        args: &[],
        handler: Handler::Nav(|c, _| json(NavResponse::new(c.session(), c.location()))),
    },
    VerbDef {
        name: "save",
        doc: "Persist the working state to the session's database file.",
        args: &[],
        handler: Handler::Nav(|c, _| {
            let report = c.save().map_err(|e| ServiceError::Apply(e.to_string()))?;
            json(report)
        }),
    },
    VerbDef {
        name: "undo",
        doc: "Undo the most recent edit.",
        args: &[],
        handler: Handler::Nav(|c, _| {
            let edit = c.undo()?;
            json(EditResponse::new(c.session(), edit))
        }),
    },
    VerbDef {
        name: "redo",
        doc: "Redo the most recently undone edit.",
        args: &[],
        handler: Handler::Nav(|c, _| {
            let edit = c.redo()?;
            json(EditResponse::new(c.session(), edit))
        }),
    },
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_catalog_example_parses() {
        for verb in catalog() {
            let (name, _) = i8051_disassembler::store::parse_call(&verb.example)
                .unwrap_or_else(|e| panic!("{}: {e}", verb.example));
            assert_eq!(name, verb.name);
            if verb.category == Category::Edit {
                i8051_disassembler::store::from_dsl(&verb.example)
                    .unwrap_or_else(|e| panic!("{}: {e}", verb.example));
            }
        }
    }
}
