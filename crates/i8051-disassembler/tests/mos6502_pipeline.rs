//! End-to-end proof that the disassembler drives the 6502 platform: a synthetic
//! program is auto-disassembled and rendered, with branch targets resolved to
//! labels and data/control cross-references recovered, all through the generic
//! core, selecting the driver only at `Db` construction.

use i8051_disassembler::address::{PhysicalAddr, XrefType};
use i8051_disassembler::db::Db;
use i8051_disassembler::platform::mos6502::{self, CODE};

/// A self-contained copy loop at 0x1000:
///
/// ```text
/// 1000  A2 00        LDX #0x00
/// 1002  BD 00 20     LDA 0x2000,X    loop
/// 1005  F0 06        BEQ 0x100D      to done
/// 1007  9D 00 30     STA 0x3000,X
/// 100A  E8           INX
/// 100B  D0 F5        BNE 0x1002      to loop
/// 100D  60           RTS             done
/// ```
const PROGRAM: &[u8] = &[
    0xA2, 0x00, 0xBD, 0x00, 0x20, 0xF0, 0x06, 0x9D, 0x00, 0x30, 0xE8, 0xD0, 0xF5, 0x60,
];

fn loaded_db() -> Db {
    let mut db = Db::with_platform(mos6502::platform());
    let region = db.region_mut(CODE);
    region.set_bytes("kbd.bin", 0, 0x1000, PROGRAM);
    region.auto_disassemble(0x1000).unwrap_success();
    db
}

#[test]
fn auto_disassembles_and_labels_branches() {
    let db = loaded_db();
    let sdas = db.to_sdas();

    // The whole loop decoded as 6502, and both branch targets became labels.
    for expected in [
        ".area CODE (CODE,ABS)",
        "loc_1002:",
        "LDA     0x2000,X",
        "BEQ     loc_100D",
        "STA     0x3000,X",
        "INX",
        "BNE     loc_1002",
        "loc_100D:",
        "RTS",
    ] {
        assert!(sdas.contains(expected), "missing {expected:?} in:\n{sdas}");
    }
    // No byte decoded as an illegal opcode.
    assert!(!sdas.contains("???"), "unexpected illegal opcode:\n{sdas}");
}

#[test]
fn recovers_control_and_data_xrefs() {
    let db = loaded_db();

    // The loop label at 0x1002 has an inbound jump from the BNE at 0x100B.
    let loop_xrefs = db.xrefs_to(&PhysicalAddr {
        space: CODE,
        offset: 0x1002,
    });
    assert_eq!(loop_xrefs.len(), 1);
    assert_eq!(loop_xrefs[0].from.offset, 0x100B);
    assert_eq!(loop_xrefs[0].xref_type, XrefType::Jump);

    // The store at 0x1007 writes the data table base at 0x3000.
    let store_xrefs = db.xrefs_to(&PhysicalAddr {
        space: CODE,
        offset: 0x3000,
    });
    assert_eq!(store_xrefs.len(), 1);
    assert_eq!(store_xrefs[0].from.offset, 0x1007);
    assert_eq!(store_xrefs[0].xref_type, XrefType::Write);
}

#[test]
fn equivalents_cover_the_whole_program() {
    let db = loaded_db();
    let region = db.region(CODE).unwrap();
    // Every instruction start is derived code, no gaps, no data.
    for &addr in &[0x1000u32, 0x1002, 0x1005, 0x1007, 0x100A, 0x100B, 0x100D] {
        assert!(
            region.get_equivalent(addr).is_defined(),
            "0x{addr:04X} not covered"
        );
    }
    assert_eq!(db.space_usage(CODE).code, PROGRAM.len() as u32);
}

/// On-demand CPU fingerprint: decode a ROM linearly under a driver and report
/// the fraction of bytes that fall on an illegal opcode. A ROM's true ISA
/// decodes with far fewer illegals than a foreign one.
///
/// Run with e.g.:
/// ```text
/// DGQ_ROM=/path/to/keyboard.BIN cargo test -p i8051-disassembler \
///     --test mos6502_pipeline rom_fingerprint -- --ignored --nocapture
/// ```
#[test]
#[ignore]
fn rom_fingerprint() {
    let path = std::env::var("DGQ_ROM").expect("set DGQ_ROM to a ROM path");
    let bytes = std::fs::read(&path).expect("read ROM");

    for platform in [
        i8051_disassembler::platform::i8051::platform(),
        mos6502::platform(),
    ] {
        // Linear sweep: count instructions and how many are illegal opcodes.
        let mut addr = 0usize;
        let (mut total, mut illegal) = (0u32, 0u32);
        while addr < bytes.len() {
            let end = (addr + platform.max_insn_len()).min(bytes.len());
            let insn = platform.decode(addr as u32, &bytes[addr..end]);
            total += 1;
            if insn.as_string().starts_with("???") {
                illegal += 1;
            }
            addr += insn.len().max(1);
        }
        let pct = 100.0 * illegal as f64 / total as f64;
        eprintln!(
            "{:>8}: {illegal}/{total} illegal ({pct:.1}%)",
            platform.name()
        );
    }
}

/// Auto-disassemble a ROM from its entry point and report the illegal-opcode
/// density of the *reachable* code (the honest ISA signal, a blind linear
/// sweep hits data tables and misaligns). Prints the first instructions so the
/// disassembly can be eyeballed.
///
/// ```text
/// DGQ_ROM=/path/rom.bin DGQ_BASE=0x0 DGQ_START=0x400 cargo test \
///   -p i8051-disassembler --test mos6502_pipeline decode_window -- --ignored --nocapture
/// ```
#[test]
#[ignore]
fn decode_window() {
    let path = std::env::var("DGQ_ROM").expect("set DGQ_ROM");
    let cpu = std::env::var("DGQ_CPU").unwrap_or_else(|_| "mos6502".into());
    let platform = i8051_disassembler::platform::by_name(&cpu).expect("unknown DGQ_CPU");
    let base = env_u32("DGQ_BASE", 0);
    let start = env_u32("DGQ_START", base);
    let bytes = std::fs::read(&path).expect("read ROM");

    let mut db = Db::with_platform(platform);
    let region = db.region_mut(CODE);
    region.set_bytes("rom", 0, base, &bytes);
    let result = region.auto_disassemble(start);
    let region = db.region(CODE).unwrap();

    let disasm = |addr: u32| {
        let bytes = region.bytes_at(addr, 3);
        i8051_disassembler::platform::by_name(&cpu)
            .unwrap()
            .decode(addr, &bytes)
            .as_string()
            .to_string()
    };

    let mut addrs = result.success.clone();
    addrs.sort_unstable();
    let illegal = addrs
        .iter()
        .filter(|&&a| disasm(a).starts_with("???"))
        .count();
    eprintln!(
        "reachable: {} instrs, {illegal} illegal, {} dead-ends",
        addrs.len(),
        result.errors.len()
    );

    for &addr in addrs.iter().take(28) {
        eprintln!("  {addr:04X}: {}", disasm(addr));
    }
}

fn env_u32(key: &str, default: u32) -> u32 {
    std::env::var(key).ok().map_or(default, |v| {
        let v = v.trim_start_matches("0x");
        u32::from_str_radix(v, 16).unwrap()
    })
}
