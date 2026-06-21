use std::{
    fs,
    process::{Command, Stdio},
};

use i8051::Instruction;
use pretty_assertions::assert_eq;

#[test]
fn decode_test() {
    let mut actual = String::new();
    let expected = include_str!("decode_test.txt");

    let mut byte_mismatches = String::new();
    for testcase in expected.split("\n") {
        if testcase.is_empty() {
            continue;
        }
        if testcase.starts_with("#") {
            actual.push_str(testcase);
            actual.push_str("\n");
            continue;
        }
        let (bytes_str, mut output) = testcase.split_once(":").unwrap();
        output = output.trim();
        if output == "???" {
            output = ".db 0xa5";
        }
        let reverse_bytes = sdas_compile_instruction(output);
        let bytes = bytes_str
            .split_whitespace()
            .map(|x| u8::from_str_radix(x, 16).unwrap())
            .collect::<Vec<u8>>();

        if bytes != reverse_bytes {
            byte_mismatches.push_str(
                format!(
                    "{output} -> bytes mismatch: {:02x?} != {:02x?}\n",
                    bytes.as_slice(),
                    reverse_bytes.as_slice()
                )
                .as_str(),
            );
        }

        let instruction = Instruction::decode_from_bytes(0x1000, &bytes);
        actual.push_str(bytes_str);
        actual.push_str(": ");
        actual.push_str(&instruction.as_string());
        actual.push_str("\n");
    }

    if byte_mismatches.is_empty() {
        eprintln!("All bytes match!");
    } else {
        eprintln!("Byte mismatches:\n{}", byte_mismatches);
        panic!("Byte mismatches found");
    }

    assert_eq!(actual, expected);
}

fn sdas_compile_instruction(s: &str) -> Vec<u8> {
    let sdas8051_script = r#"
#!/bin/bash
set -euo pipefail
echo "Compiling snippet..."
cat "$1"/snippet.input
sdas8051 -o "$1"/snippet.asm "$1"/snippet.input
echo "Linking snippet..."
sdld -i "$1"/snippet.ihx "$1"/snippet.asm
echo "Copying binary..."
sdobjcopy -I ihex -O binary "$1"/snippet.ihx "$1"/snippet.bin
echo "Done!"
    "#;

    let tempdir = tempfile::tempdir().unwrap();
    let script = tempdir.path().join("snippet.sh");
    let input = tempdir.path().join("snippet.input");
    let output = tempdir.path().join("snippet.bin");

    fs::write(&script, sdas8051_script).unwrap();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut permissions = fs::metadata(&script).unwrap().permissions();
        permissions.set_mode(0o755);
        fs::set_permissions(&script, permissions).unwrap();
    }
    fs::write(
        &input,
        &format!(".area CODE (CODE,ABS)\n.org 0x1000\n{s}\n"),
    )
    .unwrap();

    let mut process = Command::new(script);
    process.arg(tempdir.path());
    process.stdout(Stdio::piped());
    process.stderr(Stdio::piped());
    let process_output = process.output().unwrap();
    if !process_output.status.success() {
        panic!(
            "sdas8051 failed: {}",
            String::from_utf8_lossy(&process_output.stderr)
        );
    }

    eprintln!(
        "{}\n{}\n",
        String::from_utf8_lossy(&process_output.stdout),
        String::from_utf8_lossy(&process_output.stderr)
    );

    let bytes = fs::read(&output).unwrap();
    bytes
}
