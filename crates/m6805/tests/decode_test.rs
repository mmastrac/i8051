use m6805::{ControlFlow, Instruction, Mnemonic, OPCODES};
use pretty_assertions::assert_eq;

#[test]
fn backward_branch_sign_extends() {
    let at = |bytes: &[u8]| Instruction::decode_from_bytes(0x1000, bytes);
    // 0x1000 + 2 - 2, and the largest step back.
    assert_eq!(at(&[0x27, 0xFE]).as_string(), "BEQ 0x1000");
    assert_eq!(at(&[0x20, 0x80]).as_string(), "BRA 0x0F82");
    // BRSET carries its rel in the third byte.
    assert_eq!(at(&[0x0E, 0x50, 0xFD]).as_string(), "BRSET #7,0x50,0x1000");
    assert_eq!(
        at(&[0x27, 0xFE]).control_flow(),
        ControlFlow::Choice {
            fall_through: 0x1002,
            branch_target: 0x1000
        }
    );
}

#[test]
fn control_flow_is_classified() {
    use ControlFlow::*;
    let cf = |bytes: &[u8]| Instruction::decode_from_bytes(0x1000, bytes).control_flow();
    assert_eq!(cf(&[0xCC, 0x20, 0x00]), Jump { target: 0x2000 }); // JMP ext
    assert_eq!(cf(&[0xFC]), Diverge); // JMP ,X (indexed, dynamic)
    assert_eq!(
        cf(&[0xCD, 0x20, 0x00]),
        Call {
            target: 0x2000,
            return_pc: 0x1003
        } // JSR ext
    );
    assert_eq!(
        cf(&[0xAD, 0x10]),
        Call {
            target: 0x1012,
            return_pc: 0x1002
        } // BSR
    );
    assert_eq!(cf(&[0x81]), Diverge); // RTS
    assert_eq!(cf(&[0x20, 0x10]), Jump { target: 0x1012 }); // BRA
    assert_eq!(cf(&[0x21, 0x10]), Continue { next: 0x1002 }); // BRN (never)
    assert_eq!(
        cf(&[0x26, 0x10]),
        Choice {
            fall_through: 0x1002,
            branch_target: 0x1012
        } // BNE
    );
    assert_eq!(
        cf(&[0x0E, 0x50, 0x10]),
        Choice {
            fall_through: 0x1003,
            branch_target: 0x1013
        } // BRSET7
    );
    assert_eq!(cf(&[0x9D]), Continue { next: 0x1001 }); // NOP
}

#[test]
fn table_sweep_never_panics() {
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
