//! # m6805
//!
//! A decoder/disassembler for the Motorola 6805 (M68HC05) 8-bit microcontroller,
//! used in the "green LED" DEC LK201 keyboard, among many other embedded designs.
//!
//! Decode-only, sharing the [`Instruction`] / [`Mnemonic`] / [`Operand`] /
//! [`ControlFlow`] surface of the `i8051` and `mos6502` crates so one
//! disassembler can target all three. It models a base M68HC05:
//!
//!  - An 8-bit accumulator (`A`) and 8-bit index register (`X`).
//!  - A program counter and stack pointer into low memory.
//!  - A condition-code register (`H I N Z C`).
//!  - Ten addressing modes (see [`AddrMode`]), including the bit-manipulation
//!    and bit-test-and-branch forms unique to the 6805.
//!
//! Addresses are **big-endian**. Only legal opcodes are recognized, every other
//! byte decodes as [`Mnemonic::Unknown`].
//!
//! See [`Mnemonic`] for the list of supported instructions.

mod op;

pub use op::{
    AddrMode, ControlFlow, Instruction, Mnemonic, OpInfo, Operand, Operands, decode, decode_length,
    INSTRUCTION_LENGTHS, OPCODES,
};
