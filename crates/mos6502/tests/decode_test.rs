use std::{
    fs,
    process::{Command, Stdio},
};

use mos6502::Instruction;
use pretty_assertions::assert_eq;

/// Reference-file sweep over every opcode byte.
#[test]
fn decode_test() {
    let mut actual = String::new();
    let expected = include_str!("decode_test.txt");

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
        let reverse_bytes = sdas6500_compile_instruction(&asm);
        if bytes != reverse_bytes {
            byte_mismatches.push_str(&format!(
                "{asm} -> bytes mismatch: {:02x?} != {:02x?}\n",
                bytes.as_slice(),
                reverse_bytes.as_slice()
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

/// Regenerate `decode_test.txt` by sweeping all 256 opcodes.
///
/// Run with `cargo test -p mos6502 regenerate_reference -- --ignored --nocapture`.
#[test]
#[ignore]
fn regenerate_reference() {
    let mut out = String::from("# 6502 opcode decode sweep (pc=0x1000, operands filled 0x10,0x30)\n");
    for op in 0u16..256 {
        let bytes = [op as u8, 0x10, 0x30];
        let ins = Instruction::decode_from_bytes(0x1000, &bytes);
        let used = &bytes[..ins.len()];
        let hex = used
            .iter()
            .map(|b| format!("{b:02X}"))
            .collect::<Vec<_>>()
            .join(" ");
        out.push_str(&format!("{hex}: {}\n", ins.as_string()));
    }
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/decode_test.txt");
    fs::write(path, &out).unwrap();
    eprintln!("wrote {path}");
}

/// Hand-written cases, validate the table independently of the generator.
#[test]
fn spot_check() {
    let cases: &[(&[u8], &str)] = &[
        (&[0xEA], "NOP"),
        (&[0x00], "BRK"),
        (&[0xA9, 0x42], "LDA #0x42"),
        (&[0xA5, 0x10], "LDA 0x10"),
        (&[0xB5, 0x10], "LDA 0x10,X"),
        (&[0xAD, 0x34, 0x12], "LDA 0x1234"),
        (&[0xBD, 0x34, 0x12], "LDA 0x1234,X"),
        (&[0xB9, 0x34, 0x12], "LDA 0x1234,Y"),
        (&[0xA1, 0x10], "LDA [0x10,X]"),
        (&[0xB1, 0x10], "LDA [0x10],Y"),
        (&[0x0A], "ASL A"),
        (&[0x4C, 0x00, 0x20], "JMP 0x2000"),
        (&[0x6C, 0x00, 0x20], "JMP [0x2000]"),
        (&[0x20, 0x00, 0x20], "JSR 0x2000"),
        (&[0xB6, 0x10], "LDX 0x10,Y"),
        (&[0x96, 0x10], "STX 0x10,Y"),
        // Branch renders the absolute target: 0x1000 + 2 + 0x10 = 0x1012.
        (&[0xD0, 0x10], "BNE 0x1012"),
        // Backward branch: 0x1000 + 2 - 2 = 0x1000.
        (&[0xF0, 0xFE], "BEQ 0x1000"),
        // Illegal opcode.
        (&[0x02], "???"),
    ];
    for (bytes, text) in cases {
        let ins = Instruction::decode_from_bytes(0x1000, bytes);
        assert_eq!(&ins.as_string(), text, "decoding {bytes:02X?}");
    }
}

#[test]
fn control_flow() {
    use mos6502::ControlFlow::*;
    let cf = |bytes: &[u8]| Instruction::decode_from_bytes(0x1000, bytes).control_flow();
    assert_eq!(cf(&[0x4C, 0x00, 0x20]), Jump { target: 0x2000 });
    assert_eq!(cf(&[0x6C, 0x00, 0x20]), Diverge); // indirect jump
    assert_eq!(
        cf(&[0x20, 0x00, 0x20]),
        Call {
            target: 0x2000,
            return_pc: 0x1003
        }
    );
    assert_eq!(cf(&[0x60]), Diverge); // RTS
    assert_eq!(
        cf(&[0xD0, 0x10]),
        Choice {
            fall_through: 0x1002,
            branch_target: 0x1012
        }
    );
    assert_eq!(cf(&[0xEA]), Continue { next: 0x1001 }); // NOP
}

fn sdas6500_compile_instruction(s: &str) -> Vec<u8> {
    let sdas6500_script = r#"
#!/bin/bash
set -euo pipefail
sdas6500 -o "$1"/snippet.rel "$1"/snippet.input
sdld -i "$1"/snippet.ihx "$1"/snippet.rel
sdobjcopy -I ihex -O binary "$1"/snippet.ihx "$1"/snippet.bin
    "#;

    let tempdir = tempfile::tempdir().unwrap();
    let script = tempdir.path().join("snippet.sh");
    let input = tempdir.path().join("snippet.input");
    let output = tempdir.path().join("snippet.bin");

    fs::write(&script, sdas6500_script).unwrap();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut permissions = fs::metadata(&script).unwrap().permissions();
        permissions.set_mode(0o755);
        fs::set_permissions(&script, permissions).unwrap();
    }
    fs::write(&input, format!(".area CODE (CODE,ABS)\n.org 0x1000\n{s}\n")).unwrap();

    let mut process = Command::new(script);
    process.arg(tempdir.path());
    process.stdout(Stdio::piped());
    process.stderr(Stdio::piped());
    let process_output = process.output().unwrap();
    if !process_output.status.success() {
        panic!(
            "sdas6500 failed for {s:?}: {}",
            String::from_utf8_lossy(&process_output.stderr)
        );
    }

    fs::read(&output).unwrap()
}
