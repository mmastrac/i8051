pub mod address;
#[cfg(feature = "analysis")]
pub mod analysis;
pub mod commands;
pub mod db;
pub mod labels;
pub mod note;
pub mod pattern;
pub mod platform;
pub mod region;
pub mod render;
pub mod store;

#[cfg(test)]
mod tests {
    use std::process::{Command, Stdio};

    use crate::db::Db;

    static MATH_BIN: &[u8] = &hex_literal::hex!(
        "
        02 00 4c 00 00 00 00 00  00 00 e2 fb ea f2 80 2c
        00 00 e0 fb ea f0 80 24  e6 b5 02 02 eb f6 22 00
        e2 b5 02 02 eb f2 22 00  e0 b5 02 02 eb f0 22 30
        f6 e0 a8 82 20 f5 d3 ea  c6 f5 82 22 8b 82 22 30
        f6 e6 a8 82 20 f5 d9 80  cf 02 00 a8 75 81 11 12
        02 04 e5 82 60 03 02 00  49 79 00 e9 44 00 60 1b
        7a 00 90 02 08 78 01 75  a0 00 e4 93 f2 a3 08 b8
        00 02 05 a0 d9 f4 da f2  75 a0 ff e4 78 ff f6 d8
        fd 78 00 e8 44 00 60 0a  79 01 75 a0 00 e4 f3 09
        d8 fc 78 00 e8 44 00 60  0c 79 00 90 00 01 e4 f0
        a3 d8 fc d9 fa 02 00 49  75 08 0a 75 09 00 85 08
        10 85 09 11 90 00 19 12  01 03 85 82 0a 85 83 0b
        75 10 0a 75 11 00 85 0a  82 85 0b 83 12 01 cc 85
        82 0c 85 83 0d 85 0c 82  85 0d 83 a3 75 10 0a 75
        11 00 12 01 96 85 82 0e  85 83 0f af 08 90 80 00
        ef f0 af 0a a3 ef f0 af  0c a3 ef f0 af 0e a3 ef
        f0 80 fe e5 82 85 10 f0  a4 c5 82 c0 f0 85 11 f0
        a4 d0 f0 25 f0 c5 83 85  10 f0 a4 25 83 f5 83 22
        e5 10 45 11 60 46 7a 01  e5 10 25 e0 f5 10 e5 11
        33 40 12 f5 11 e5 82 95  10 e5 83 95 11 40 03 0a
        80 e6 c3 e5 11 13 f5 11  e5 10 13 f5 10 c3 e5 82
        95 10 f5 f0 e5 83 95 11  40 05 f5 83 85 f0 82 c3
        e5 11 13 f5 11 e5 10 13  f5 10 da e1 22 7a 10 e4
        fb fc e5 82 25 e0 f5 82  e5 83 33 f5 83 eb 33 fb
        ec 33 fc eb 95 10 f5 f0  ec 95 11 40 06 fc ab f0
        43 82 01 da dd 22 c2 d5  e5 83 30 e7 0d d2 d5 e4
        c3 95 82 f5 82 e4 95 83  f5 83 e5 11 30 e7 0b e4
        c3 95 10 f5 10 e4 95 11  f5 11 12 01 20 30 d5 0b
        e4 c3 95 82 f5 82 e4 95  83 f5 83 22 c2 d5 e5 83
        30 e7 0d d2 d5 e4 c3 95  82 f5 82 e4 95 83 f5 83
        e5 11 30 e7 0d b2 d5 e4  c3 95 10 f5 10 e4 95 11
        f5 11 12 01 6d 30 d5 0b  e4 c3 95 82 f5 82 e4 95
        83 f5 83 22 75 82 00 22"
    );

    #[test]
    fn test_asm_examples() {
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.region_mut(crate::platform::i8051::CODE)
            .set_bytes("test.bin", 0, 0, MATH_BIN);

        db.region_mut(crate::platform::i8051::CODE)
            .auto_disassemble(0)
            .unwrap_success();
        db.region_mut(crate::platform::i8051::CODE)
            .auto_disassemble(3)
            .unwrap_success();
        db.region_mut(crate::platform::i8051::CODE)
            .auto_disassemble(0xa)
            .unwrap_success();
        db.region_mut(crate::platform::i8051::CODE)
            .auto_disassemble(0x10)
            .unwrap_success();
        db.region_mut(crate::platform::i8051::CODE)
            .auto_disassemble(0x18)
            .unwrap_success();
        db.region_mut(crate::platform::i8051::CODE)
            .auto_disassemble(0x1F)
            .unwrap_success();
        db.region_mut(crate::platform::i8051::CODE)
            .auto_disassemble(0x27)
            .unwrap_success();
        db.region_mut(crate::platform::i8051::CODE)
            .auto_disassemble(0x2F)
            .unwrap_success();
        db.region_mut(crate::platform::i8051::CODE)
            .auto_disassemble(0x3F)
            .unwrap_success();

        let code = db.to_sdas();
        for (i, line) in code.lines().enumerate() {
            eprintln!("{}: {line}", i + 1);
        }

        try_sdas_compile(&code).unwrap();
    }

    fn try_sdas_compile(code: &str) -> Result<(), String> {
        let tempdir = tempfile::tempdir().unwrap();
        let temp = tempfile::NamedTempFile::new().unwrap();
        std::fs::write(&temp, code).unwrap();

        let mut process = Command::new("sdas8051");
        process.arg("-l");
        process.arg(tempdir.path().join("snippet.lst"));
        // process.arg("-o");
        // process.arg(tempdir.path().join("snippet.rel"));
        // process.arg("-s");
        // process.arg(tempdir.path().join("snippet.sym"));
        process.arg(temp.path());
        process.stdout(Stdio::piped());
        process.stderr(Stdio::piped());
        match process.output() {
            Err(e) => {
                eprintln!("WARNING: sdas8051 did not run: {e}");
                return Ok(());
            }
            Ok(child) => {
                if !child.status.success() {
                    eprintln!("WARNING: sdas8051 failed: {:?}", child.status);
                    eprintln!("{}", String::from_utf8_lossy(&child.stdout));
                    eprintln!("{}", String::from_utf8_lossy(&child.stderr));
                    return Ok(());
                }
            }
        }
        Ok(())
    }
}
