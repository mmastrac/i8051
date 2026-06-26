use std::collections::BTreeMap;

/// The DSL abstract syntax tree. Every serializable value is lowered into this
/// shape, then rendered to text (or parsed back from it). It is the single
/// pivot between Rust types and the textual DSL — both the generic
/// [`Serializer`](crate::store::ser) and [`Deserializer`](crate::store::de)
/// speak only `Value`.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Int(u64),
    String(String),
    List(Vec<Value>),
    Set(Vec<Value>),
    Map(BTreeMap<String, Value>),
    Address {
        space: String,
        offset: u64,
    },
    AddressRange {
        space: String,
        start: u64,
        end: u64,
    },
    /// A set of addresses within one space: `CODE:{0x10..0x13, 0x20}`. Ranges
    /// are half-open `(start, end)`; a singleton has `end == start + 1`.
    AddressSet {
        space: String,
        ranges: Vec<(u64, u64)>,
    },
    /// A top-level command: `name(field=value, ...)`.
    Call {
        name: String,
        kwargs: BTreeMap<String, Value>,
    },
    /// A named struct: `Name(field=value, ...)`.
    Struct {
        name: String,
        fields: BTreeMap<String, Value>,
    },
    /// An enum variant: `Type::Variant`, `Type::Variant(a, b)`, or
    /// `Type::Variant(field=value)`.
    Enum {
        type_name: String,
        variant: String,
        args: EnumArgs,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumArgs {
    Unit,
    Positional(Vec<Value>),
    Named(BTreeMap<String, Value>),
}

impl Value {
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Self::Bool(v) => Some(*v),
            _ => None,
        }
    }

    pub fn as_u64(&self) -> Option<u64> {
        match self {
            Self::Int(v) => Some(*v),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Self::String(v) => Some(v),
            _ => None,
        }
    }

    pub fn field(&self, name: &str) -> Option<&Value> {
        match self {
            Self::Struct { fields, .. } | Self::Call { kwargs: fields, .. } => fields.get(name),
            _ => None,
        }
    }

    /// Render this value to DSL text.
    pub fn render(&self) -> String {
        match self {
            Self::Null => "None".into(),
            Self::Bool(true) => "True".into(),
            Self::Bool(false) => "False".into(),
            Self::Int(v) => format!("0x{v:X}"),
            Self::String(s) => render_string(s),
            Self::List(items) => format!("[{}]", render_seq(items)),
            Self::Set(items) => format!("{{{}}}", render_seq(items)),
            Self::Map(map) => format!("{{{}}}", render_map(map)),
            Self::Address { space, offset } => format!("{space}:0x{offset:X}"),
            Self::AddressRange { space, start, end } => format!("{space}:0x{start:X}..0x{end:X}"),
            Self::AddressSet { space, ranges } => {
                let body = ranges
                    .iter()
                    .map(|&(start, end)| {
                        if end == start + 1 {
                            format!("0x{start:X}")
                        } else {
                            format!("0x{start:X}..0x{end:X}")
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{space}:{{{body}}}")
            }
            Self::Call { name, kwargs } => format!("{name}({})", render_kwargs(kwargs)),
            Self::Struct { name, fields } => format!("{name}({})", render_kwargs(fields)),
            Self::Enum {
                type_name,
                variant,
                args,
            } => match args {
                EnumArgs::Unit => format!("{type_name}::{variant}"),
                EnumArgs::Positional(values) => {
                    format!("{type_name}::{variant}({})", render_seq(values))
                }
                EnumArgs::Named(fields) => {
                    format!("{type_name}::{variant}({})", render_kwargs(fields))
                }
            },
        }
    }
}

fn render_seq(items: &[Value]) -> String {
    items
        .iter()
        .map(Value::render)
        .collect::<Vec<_>>()
        .join(", ")
}

fn render_kwargs(fields: &BTreeMap<String, Value>) -> String {
    fields
        .iter()
        .map(|(key, value)| format!("{key}={}", value.render()))
        .collect::<Vec<_>>()
        .join(", ")
}

fn render_map(map: &BTreeMap<String, Value>) -> String {
    map.iter()
        .map(|(key, value)| format!("{}: {}", render_string(key), value.render()))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Render a string, choosing the most readable of `"..."`, `r"..."`, or
/// `r#"..."#` so the result re-lexes to the same contents.
fn render_string(value: &str) -> String {
    if !value.contains(['\n', '\r', '"', '\\']) {
        return format!("\"{value}\"");
    }
    if !value.contains('"') {
        return format!("r\"{value}\"");
    }
    let mut delim = String::from("#");
    while value.contains(&format!("\"{delim}")) {
        delim.push('#');
    }
    format!("r{delim}\"{value}\"{delim}")
}
