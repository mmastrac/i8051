//! Verifies the committed disassembly scripts (`samples/*.dsl`). Each script is
//! the `to_commands` export of a real ROM's auto-disassembly: a `set_cpu`, a
//! byte-map, and `auto_disassemble` roots. It is the reproducible, binary-free
//! record of the testbed run. The ROM images are not committed (third-party
//! firmware), so a script whose image is absent is skipped: CI stays green, and
//! a checkout with `samples/` populated re-applies each script and confirms it
//! reconstructs the disassembly with zero illegal opcodes in reachable code.

use std::io;

use i8051_disassembler::address::AddressValue;
use i8051_disassembler::commands::Environment;
use i8051_disassembler::db::Db;
use i8051_disassembler::store::from_dsl_many;

const SAMPLES_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/samples/");

/// A committed script plus the ROM it maps (skip the case when it is absent).
const SAMPLES: &[(&str, &str)] = &[
    ("lk201-8051.dsl", "lk201-8051.bin"),
    ("6502_functional_test.dsl", "6502_functional_test.bin"),
    ("lk201-6805.dsl", "lk201-6805.bin"),
];

/// Resolves `map_bytes` file references against the `samples/` directory.
struct SamplesEnv;

impl Environment for SamplesEnv {
    fn load_file_bytes(
        &self,
        file: &str,
        offset: usize,
        size: AddressValue,
    ) -> Result<Vec<u8>, io::Error> {
        let data = std::fs::read(format!("{SAMPLES_DIR}{file}"))?;
        let end = offset.saturating_add(size as usize);
        data.get(offset..end)
            .map(<[u8]>::to_vec)
            .ok_or_else(|| io::Error::new(io::ErrorKind::UnexpectedEof, "read past end of file"))
    }
}

#[test]
fn committed_scripts_reconstruct_clean_disassembly() {
    let mut ran = 0;
    for (script, rom) in SAMPLES {
        // The script is committed and must parse.
        let dsl = std::fs::read_to_string(format!("{SAMPLES_DIR}{script}"))
            .unwrap_or_else(|e| panic!("reading {script}: {e}"));
        let commands = from_dsl_many(&dsl).expect("script parses");

        // The ROM it maps may be absent (not committed), skip if so.
        if !std::path::Path::new(&format!("{SAMPLES_DIR}{rom}")).exists() {
            eprintln!("skip {script}: {rom} not present");
            continue;
        }

        // A fresh DB has no CPU. The script's `set_cpu` selects one before any
        // `auto_disassemble` runs.
        let mut db = Db::new();
        for command in commands {
            db.apply(command, Some(&SamplesEnv)).expect("command applies");
        }
        assert!(db.platform().is_some(), "{script}: no set_cpu");

        let code: AddressValue = db.spaces().iter().map(|&s| db.space_usage(s).code).sum();
        assert!(code > 0, "{script}: reconstructed no code");
        // Illegal opcodes render as `???`; a clean listing means the ISA fits.
        let illegal = db.to_sdas().matches("???").count();
        assert_eq!(illegal, 0, "{script}: {illegal} illegal opcodes reachable");
        eprintln!("ok {script}: {code} code bytes, 0 illegal");
        ran += 1;
    }
    eprintln!("{ran}/{} sample ROMs present", SAMPLES.len());
}
