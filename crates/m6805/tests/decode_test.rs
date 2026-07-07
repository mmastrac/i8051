use m6805::{ControlFlow, Instruction, Mnemonic, OPCODES};
use pretty_assertions::assert_eq;

fn text(bytes: &[u8]) -> String {
    Instruction::decode_from_bytes(0x1000, bytes).as_string()
}

#[test]
fn addressing_modes_render() {
    let cases: &[(&[u8], &str)] = &[
        // Inherent (implied), including the accumulator/index RMW forms and MUL.
        (&[0x9D], "NOP"),
        (&[0x4F], "CLRA"),
        (&[0x5F], "CLRX"),
        (&[0x42], "MUL"),
        (&[0x81], "RTS"),
        (&[0x97], "TAX"),
        // Immediate / direct / extended.
        (&[0xA6, 0x42], "LDA #0x42"),
        (&[0xB6, 0x50], "LDA 0x50"),
        (&[0xC6, 0x12, 0x34], "LDA 0x1234"), // big-endian: hi byte first
        // Indexed: no offset, 8-bit, 16-bit.
        (&[0xF6], "LDA ,X"),
        (&[0xE6, 0x10], "LDA 0x10,X"),
        (&[0xD6, 0x12, 0x34], "LDA 0x1234,X"),
        // Store forms (no immediate).
        (&[0xB7, 0x50], "STA 0x50"),
        (&[0xBF, 0x50], "STX 0x50"),
        // Read-modify-write on memory vs. registers.
        (&[0x3F, 0x50], "CLR 0x50"),
        (&[0x6A, 0x10], "DEC 0x10,X"),
        (&[0x70], "NEG ,X"),
    ];
    for (bytes, want) in cases {
        assert_eq!(&text(bytes), want, "decoding {bytes:02X?}");
    }
}

#[test]
fn branches_render_absolute_target() {
    // rel is added to pc + len. pc = 0x1000.
    assert_eq!(text(&[0x26, 0x10]), "BNE 0x1012"); // 0x1000 + 2 + 0x10
    assert_eq!(text(&[0x20, 0x7E]), "BRA 0x1080");
    assert_eq!(text(&[0xAD, 0x10]), "BSR 0x1012");
    // Backward branch: 0x1000 + 2 - 2 = 0x1000.
    assert_eq!(text(&[0x27, 0xFE]), "BEQ 0x1000");
}

#[test]
fn bit_instructions_render() {
    // BSET/BCLR: opcode carries the bit number, one direct operand follows.
    // The bit number renders as an immediate (sdas6808 `.6805` syntax).
    assert_eq!(text(&[0x16, 0x50]), "BSET #3,0x50"); // 0x10 + 3*2
    assert_eq!(text(&[0x11, 0x50]), "BCLR #0,0x50");
    assert_eq!(text(&[0x1E, 0x02]), "BSET #7,0x02");
    // BRSET/BRCLR: bit + direct + relative target (0x1000 + 3 + 0x10 = 0x1013).
    assert_eq!(text(&[0x0E, 0x50, 0x10]), "BRSET #7,0x50,0x1013");
    assert_eq!(text(&[0x01, 0x50, 0x10]), "BRCLR #0,0x50,0x1013");
}

#[test]
fn illegal_opcodes_are_unknown() {
    // 0x31 (RMW low-nibble 1) and 0xA7 (STA immediate) are not legal opcodes.
    assert_eq!(text(&[0x31]), "???");
    assert_eq!(text(&[0xA7]), "???");
    assert_eq!(text(&[0xAC]), "???"); // JMP immediate
}

#[test]
fn control_flow_is_classified() {
    use ControlFlow::*;
    let cf = |bytes: &[u8]| Instruction::decode_from_bytes(0x1000, bytes).control_flow();
    assert_eq!(cf(&[0xCC, 0x20, 0x00]), Jump { target: 0x2000 }); // JMP ext
    assert_eq!(cf(&[0xFC]), Diverge); // JMP ,X (indexed, dynamic)
    assert_eq!(
        cf(&[0xCD, 0x20, 0x00]),
        Call { target: 0x2000, return_pc: 0x1003 } // JSR ext
    );
    assert_eq!(
        cf(&[0xAD, 0x10]),
        Call { target: 0x1012, return_pc: 0x1002 } // BSR
    );
    assert_eq!(cf(&[0x81]), Diverge); // RTS
    assert_eq!(cf(&[0x20, 0x10]), Jump { target: 0x1012 }); // BRA
    assert_eq!(cf(&[0x21, 0x10]), Continue { next: 0x1002 }); // BRN (never)
    assert_eq!(
        cf(&[0x26, 0x10]),
        Choice { fall_through: 0x1002, branch_target: 0x1012 } // BNE
    );
    assert_eq!(
        cf(&[0x0E, 0x50, 0x10]),
        Choice { fall_through: 0x1003, branch_target: 0x1013 } // BRSET7
    );
    assert_eq!(cf(&[0x9D]), Continue { next: 0x1001 }); // NOP
}

#[test]
fn full_table_sweep_never_panics() {
    // Decoding every opcode with filler operand bytes must yield a length that
    // matches the table and consumes exactly that many bytes.
    let mut legal = 0;
    for op in 0u16..256 {
        let bytes = [op as u8, 0x12, 0x34];
        let insn = Instruction::decode_from_bytes(0x1000, &bytes);
        assert_eq!(insn.bytes().len(), insn.len());
        if insn.mnemonic() != Mnemonic::Unknown {
            legal += 1;
            assert_eq!(insn.len() as u8, OPCODES[op as usize].mode.length());
        }
    }
    // The base M68HC05 has 210 legal opcodes.
    assert_eq!(legal, 210);
}
