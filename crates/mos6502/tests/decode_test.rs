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

#[test]
fn a_backward_branch_sign_extends() {
    let at = |bytes: &[u8]| Instruction::decode_from_bytes(0x1000, bytes);
    assert_eq!(at(&[0xF0, 0xFE]).as_string(), "BEQ 0x1000");
    assert_eq!(at(&[0x10, 0x80]).as_string(), "BPL 0x0F82");
    assert_eq!(
        at(&[0xD0, 0xFE]).control_flow(),
        mos6502::ControlFlow::Choice { fall_through: 0x1002, branch_target: 0x1000 }
    );
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
