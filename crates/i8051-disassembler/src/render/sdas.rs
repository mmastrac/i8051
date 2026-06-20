use crate::address::AddressValue;
use crate::db::DataType;
use crate::render::Line;
use crate::render::data::{DataChunk, DataHeuristics};

pub struct SdasWriter {
    code: String,
    used_registers: [bool; 256],
}

impl Default for SdasWriter {
    fn default() -> Self {
        Self {
            code: String::new(),
            used_registers: [false; 256],
        }
    }
}

impl SdasWriter {
    pub fn write(&mut self, string: &str) {
        self.code.push_str(string);
    }

    pub fn write_line(&mut self, line: &Line) {
        line_to_sdas(self, line);
    }

    pub fn use_named_register(&mut self, register: u8) {
        self.used_registers[register as usize] = true;
    }

    pub fn into_string(self) -> String {
        let mut prefix = String::new();
        let mut register_count = 0;
        for i in 0..32 {
            if self.used_registers[i] {
                if register_count == 0 {
                    prefix.push_str("; Registers:\n");
                }
                register_count += 1;
                prefix.push_str(&format!("R{}_BANK{} = {}\n", i % 8, i / 8, i));
            }
        }
        if register_count > 0 {
            prefix.push_str("\n");
        }

        format!("{prefix}{}", self.code)
    }
}

fn line_to_sdas(writer: &mut SdasWriter, line: &Line) {
    let s = match line {
        Line::Org { addr } => format!(".org 0x{addr:X}\n"),
        Line::Blank => "\n".to_string(),
        Line::Comment { text, .. } => format!("; {text}\n"),
        Line::Label { name, .. } => format!("{name}:\n"),
        Line::Instruction { text, direct, .. } => {
            if let Some(direct) = direct {
                writer.use_named_register(*direct);
            }
            format!("{text}\n")
        }
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
                return;
            }
            format!("; Unknown bytes at 0x{addr:04X}\n{body}")
        }
        Line::Function {
            name,
            signature,
            length,
            noreturn,
            ..
        } => {
            let sig = signature.as_deref().unwrap_or("()");
            let mut line = format!("{name}: ; fn {name}{sig}");
            if *noreturn {
                line.push_str("; noreturn");
            } else {
                line.push_str(&format!("; len = 0x{length:X}"));
            }
            format!("{line}\n")
        }
    };
    writer.write(&s);
}

fn emit_bytes(address: AddressValue, bytes: &[u8]) -> String {
    let heuristics = DataHeuristics::default();
    emit_chunks(
        &heuristics,
        address,
        heuristics.iterate(address, None, bytes),
    )
}

fn emit_unknown_bytes(base_addr: AddressValue, bytes: &[u8]) -> String {
    let heuristics = DataHeuristics::default();
    emit_chunks(
        &heuristics,
        base_addr,
        heuristics.iterate(base_addr, None, bytes),
    )
}

fn emit_db_line(row: &[u8]) -> String {
    let db = row
        .iter()
        .map(|b| format!("0x{b:02X}"))
        .collect::<Vec<_>>()
        .join(", ");
    format!("    .db {db}\n")
}

fn emit_chunks<'a>(
    heuristics: &DataHeuristics,
    mut addr: AddressValue,
    chunks: impl IntoIterator<Item = DataChunk<'a, u8>>,
) -> String {
    let mut out = String::new();
    for chunk in chunks {
        match chunk {
            DataChunk::Literal(row) => {
                for sub in heuristics.literal_rows(addr, row) {
                    out.push_str(&emit_db_line(sub));
                }
                addr += row.len() as AddressValue;
            }
            DataChunk::Run(value, len) => {
                match value {
                    0 => out.push_str(&format!("    .ds {len}\n")),
                    value => out.push_str(&format!(
                        "    .rept {len}\n        .db 0x{value:02X}\n    .endm\n"
                    )),
                }
                addr += len as AddressValue;
            }
            DataChunk::BlockRun(row, count) => {
                let db = row
                    .iter()
                    .map(|b| format!("0x{b:02X}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                out.push_str(&format!("    .rept {count}\n        .db {db}\n    .endm\n"));
                addr += (row.len() * count) as AddressValue;
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

pub fn prefix() -> String {
    let mut s = String::new();
    for i in 0..32 {
        s.push_str(&format!("R{}BANK{} = {}\n", i % 8, i / 8, i));
    }
    s
}
