use crate::address::AddressValue;
use crate::data::{DataChunk, DataHeuristics};
use crate::db::{DataType, Line};

pub fn line_to_sdas(line: &Line) -> String {
    match line {
        Line::Org { addr } => format!(".org 0x{addr:X}\n"),
        Line::Blank => "\n".to_string(),
        Line::Comment { text, .. } => format!("; {text}\n"),
        Line::Label { name, .. } => format!("_{name}:\n"),
        Line::Instruction { text, .. } => format!("{text}\n"),
        Line::Data {
            addr,
            bytes,
            data_type,
            ..
        } => match data_type {
            DataType::Word => emit_words(bytes),
            DataType::Byte => emit_bytes(*addr, bytes),
            _ => emit_bytes(*addr, bytes),
        },
        Line::Raw { addr, bytes } => {
            let body = emit_unknown_bytes(*addr, bytes);
            if body.is_empty() {
                return String::new();
            }
            format!("; Unknown bytes at 0x{addr:04X}\n{body}")
        }
        Line::Function {
            addr,
            name,
            signature,
            length,
            noreturn,
        } => {
            let sig = signature.as_deref().unwrap_or("()");
            let mut line = format!("0x{addr:05X}: fn {name}{sig}");
            if *noreturn {
                line.push_str("; noreturn");
            } else {
                line.push_str(&format!("; len = 0x{length:X}"));
            }
            format!("{line}\n")
        }
    }
}

fn emit_bytes(address: AddressValue, bytes: &[u8]) -> String {
    let heuristics = DataHeuristics::default();
    emit_chunks(heuristics.iterate(address, None, bytes))
}

fn emit_unknown_bytes(base_addr: AddressValue, bytes: &[u8]) -> String {
    let heuristics = DataHeuristics::default();
    emit_chunks(heuristics.iterate(base_addr, None, bytes))
}

fn emit_chunks<'a>(chunks: impl IntoIterator<Item = DataChunk<'a, u8>>) -> String {
    let mut out = String::new();
    for chunk in chunks {
        match chunk {
            DataChunk::Literal(row) => out.push_str(&{
                let db = row
                    .iter()
                    .map(|b| format!("0x{b:02X}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("    .db {db}\n")
            }),
            DataChunk::Run(value, len) => match value {
                0 => {
                    let len = len;
                    out.push_str(&format!("    .ds {len}\n"));
                }
                value => {
                    out.push_str(&format!(
                        "    .rept {len}\n        .db 0x{value:02X}\n    .endm\n"
                    ));
                }
            },
            DataChunk::BlockRun(row, count) => {
                let db = row
                    .iter()
                    .map(|b| format!("0x{b:02X}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                out.push_str(&format!("    .rept {count}\n        .db {db}\n    .endm\n"));
            }
        }
    }
    out
}

fn emit_words(bytes: &[u8]) -> String {
    let (pairs, tail) = bytes.split_at(bytes.len() - bytes.len() % 2);
    let words: Vec<String> = pairs
        .chunks_exact(2)
        .map(|pair| format!("0x{:04X}", u16::from(pair[0]) | (u16::from(pair[1]) << 8)))
        .collect();
    let mut out = String::new();
    if !words.is_empty() {
        out.push_str(&format!("    .dw {}\n", words.join(", ")));
    }
    if !tail.is_empty() {
        // TODO: this probably needs some better handling, likely a generic feature instead
        // of just for words
        out.push_str("; WARNING: Leftover bytes\n");
    }
    out
}
