//! # mos6502
//!
//! A decoder/disassembler for the MOS 6502, the 8-bit CPU behind the Apple II,
//! Commodore 64, NES, BBC Micro, and countless other 1980s machines.
//!
//! This crate is decode-only.
//!
//!  - A 16-bit program counter (`PC`).
//!  - An 8-bit accumulator (`A`) and index registers (`X`, `Y`).
//!  - A stack pointer (`SP`) into the fixed `0x0100`-`0x01FF` page.
//!  - A processor status register (`P`) of condition/control flags.
//!  - 13 addressing modes (see [`AddrMode`]) which alone fix each instruction's
//!    length and operand layout.
//!
//! Only the 151 legal NMOS opcodes are recognized, every other byte decodes as
//! [`Mnemonic::Unknown`]. Textual output matches `sdas6500` syntax (`0x` hex,
//! `#` immediates, `[...]` indirection) so it round-trips through the assembler.
//!
//! See [`Mnemonic`] for the list of supported instructions.

mod op;

pub use op::{
    AddrMode, ControlFlow, Instruction, Mnemonic, OpInfo, Operand, Operands, decode, decode_length,
    INSTRUCTION_LENGTHS, OPCODES,
};
