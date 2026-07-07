//! Assembler-verified reference sweep over every opcode.

use std::{
    fs,
    process::{Command, Stdio},
};

use m6805::Instruction;
use pretty_assertions::assert_eq;

#[test]
fn reference_sweep() {
    let mut actual = String::new();
    let expected = include_str!("reference_test.txt");

    let mut byte_mismatches = String::new();
    for testcase in expected.split('\n') {
        if testcase.is_empty() {
            continue;
        }
        if testcase.starts_with('#') {
            actual.push_str(testcase);
            actual.push('\n');
            continue;
        }
        let (bytes_str, output) = testcase.split_once(':').unwrap();
        let output = output.trim();
        let bytes = bytes_str
            .split_whitespace()
            .map(|x| u8::from_str_radix(x, 16).unwrap())
            .collect::<Vec<u8>>();

        // Illegal opcodes disassemble to "???", assemble them as raw data.
        let asm = if output == "???" {
            format!(".db 0x{:02X}", bytes[0])
        } else {
            output.to_string()
        };
        let reverse = sdas6808_assemble(&asm);
        if bytes != reverse {
            byte_mismatches.push_str(&format!(
                "{asm} -> bytes mismatch: {:02x?} != {:02x?}\n",
                bytes.as_slice(),
                reverse.as_slice()
            ));
        }

        let instruction = Instruction::decode_from_bytes(0x1000, &bytes);
        assert_eq!(instruction.len(), bytes.len());
        actual.push_str(bytes_str);
        actual.push_str(": ");
        actual.push_str(&instruction.as_string());
        actual.push('\n');
    }

    if byte_mismatches.is_empty() {
        eprintln!("All bytes match!");
    } else {
        eprintln!("Byte mismatches:\n{byte_mismatches}");
        panic!("Byte mismatches found");
    }

    assert_eq!(actual, expected);
}

/// Regenerate `reference_test.txt` by sweeping all 256 opcodes.
///
/// Run with `cargo test -p m6805 regenerate_reference -- --ignored --nocapture`.
#[test]
#[ignore]
fn regenerate_reference() {
    let mut out =
        String::from("# 6805 opcode decode sweep (pc=0x1000, operands filled 0x02,0x34)\n");
    for op in 0u16..256 {
        let bytes = [op as u8, 0x02, 0x34];
        let ins = Instruction::decode_from_bytes(0x1000, &bytes);
        let hex = ins.bytes()
            .iter()
            .map(|b| format!("{b:02X}"))
            .collect::<Vec<_>>()
            .join(" ");
        out.push_str(&format!("{hex}: {}\n", ins.as_string()));
    }
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/reference_test.txt");
    fs::write(path, &out).unwrap();
    eprintln!("wrote {path}");
}

/// Assemble one line of 6805 source with `sdas6808` (`.6805` mode) and return
/// the raw bytes.
fn sdas6808_assemble(s: &str) -> Vec<u8> {
    let script = r#"
#!/bin/bash
set -euo pipefail
sdas6808 -o "$1"/snippet.rel "$1"/snippet.input
sdld6808 -i "$1"/snippet.ihx "$1"/snippet.rel
sdobjcopy -I ihex -O binary "$1"/snippet.ihx "$1"/snippet.bin
    "#;

    let tempdir = tempfile::tempdir().unwrap();
    let script_path = tempdir.path().join("snippet.sh");
    let input = tempdir.path().join("snippet.input");
    let output = tempdir.path().join("snippet.bin");

    fs::write(&script_path, script).unwrap();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&script_path).unwrap().permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&script_path, perms).unwrap();
    }
    fs::write(
        &input,
        format!("\t.6805\n\t.area CODE (ABS)\n\t.org 0x1000\n\t{s}\n"),
    )
    .unwrap();

    let out = Command::new(&script_path)
        .arg(tempdir.path())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .unwrap();
    if !out.status.success() {
        panic!(
            "sdas6808 failed for {s:?}: {}",
            String::from_utf8_lossy(&out.stderr)
        );
    }

    fs::read(&output).unwrap()
}
