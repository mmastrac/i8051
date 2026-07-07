//! A tiny multi-CPU disassembler testbed.
//!
//! Loads a raw ROM image, auto-disassembles it from one or more entry points
//! using the chosen processor driver, and prints the `sdas` listing plus a
//! reachability summary.
//!
//! ```text
//! cargo run --example disasm -- <cpu> <rom> [--base HEX] [ENTRY...]
//!
//!   cpu      one of: i8051, mos6502, m6805
//!   rom      path to a raw binary image
//!   --base   address the image loads at (default 0x0)
//!   ENTRY    hex entry point(s) to trace from (default: the base address)
//! ```
//!
//! Examples:
//! ```text
//! cargo run --example disasm -- i8051 kbd.bin
//! cargo run --example disasm -- mos6502 6502_functional_test.bin 0x400
//! cargo run --example disasm -- m6805 23-001s9-00.bin 0x100
//! ```

use std::path::Path;
use std::process::ExitCode;

use i8051_disassembler::db::Db;
use i8051_disassembler::platform::{self, BUILTIN_PLATFORMS};
use i8051_disassembler::store::to_dsl_many;

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    match run(&args) {
        Ok(()) => ExitCode::SUCCESS,
        Err(msg) => {
            eprintln!("error: {msg}\n");
            eprintln!("usage: disasm <cpu> <rom> [--base HEX] [ENTRY...]");
            eprintln!("  cpu: {}", BUILTIN_PLATFORMS.join(", "));
            ExitCode::FAILURE
        }
    }
}

fn run(args: &[String]) -> Result<(), String> {
    let mut positional = Vec::new();
    let mut entries = Vec::new();
    let mut base = 0u32;
    let mut emit_dsl = false;

    let mut it = args.iter();
    while let Some(arg) = it.next() {
        match arg.as_str() {
            "--base" => {
                let v = it.next().ok_or("--base needs a value")?;
                base = parse_hex(v)?;
            }
            // Emit the reproducible command script instead of the listing.
            "--dsl" => emit_dsl = true,
            _ if positional.len() < 2 => positional.push(arg.clone()),
            // Anything after cpu + rom is an entry point.
            _ => entries.push(parse_hex(arg)?),
        }
    }

    let [cpu, rom] = positional.as_slice() else {
        return Err("expected <cpu> and <rom>".into());
    };
    let platform = platform::by_name(cpu).ok_or_else(|| format!("unknown cpu `{cpu}`"))?;
    let bytes = std::fs::read(rom).map_err(|e| format!("reading {rom}: {e}"))?;
    if entries.is_empty() {
        entries.push(base);
    }

    // One region per driver, the first is where code lives. The map records the
    // ROM by basename so an exported script reloads next to the sample image.
    let name = Path::new(rom)
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or(rom);
    let space = platform.regions()[0].space;
    let mut db = Db::with_platform(platform);
    let region = db.region_mut(space);
    region.set_bytes(name, 0, base, &bytes);

    let mut reached = 0usize;
    for &entry in &entries {
        reached += region.auto_disassemble(entry).success.len();
    }

    // The command script that reproduces this disassembly (byte-map + roots).
    if emit_dsl {
        print!("{}", to_dsl_many(&db.to_commands()));
        return Ok(());
    }

    let listing = db.to_sdas();
    print!("{listing}");

    let illegal = listing.matches("???").count();
    let entry_list = entries
        .iter()
        .map(|e| format!("{e:#06X}"))
        .collect::<Vec<_>>()
        .join(",");
    eprintln!(
        "\n; {cpu}: {} bytes @ {base:#06X}, entries [{entry_list}] \
         -> {reached} reachable instrs, {illegal} illegal",
        bytes.len(),
    );
    Ok(())
}

fn parse_hex(s: &str) -> Result<u32, String> {
    let t = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")).unwrap_or(s);
    u32::from_str_radix(t, 16).map_err(|_| format!("bad hex value `{s}`"))
}
