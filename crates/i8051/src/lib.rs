//! # i8051
//!
//! A fast, `unsafe`-free emulator for the i8051 microcontroller.
//!
//! The 8051 (a.k.a. the MCS-51) is a 8-bit microcontroller that was very common
//! in the 1980s and 1990s, and is still used in many modern devices.
//!
//! There are a number of different variants of the 8051, and this crate
//! supports a CPU like the original 8051, but with a zero-flag extension.
//!
//! The 8051/MCS-51 series has a complex architecture, consisting of:
//!
//!  - A program counter (`PC`), used to store the address of the next
//!    instruction to execute and stored in its own dedicated register on-chip.
//!  - Internal RAM (256 bytes), used for the stack, `R0`-`R7` registers, and
//!    general-purpose storage.
//!  - SFRs (Special Function Registers), used for internal peripherals and all
//!    registers other than `R0`-`R7` and `PC`. These include:
//!    - Ports (`P0`-`P3`), used for general-purpose I/O. Ports may be
//!      bidirectional and use an output latch for read/write control and
//!      special-function control.
//!    - Timers, for timing and counting.
//!    - Serial UART(s).
//!    - Interrupt control.
//!  - XDATA (External Memory), used for off-chip RAM and accessed via `MOVX`
//!    opcodes.
//!  - CODE (External Program Memory), used for off-chip ROM and accessed via
//!    `MOVC` opcodes.
//!
//! See the [`mod@ops`] module for a list of all supported instructions.

pub mod breakpoint;
mod cpu;
pub mod memory;
pub mod peripheral;
mod regs;
pub mod sfr;
mod traits;

pub use cpu::ops;
pub use cpu::{ControlFlow, Cpu, Instruction, Interrupt, Opcode, Register};
pub use traits::{
    CpuContext, CpuView, DefaultPortMapper, MemoryMapper, PortMapper, ReadOnlyMemoryMapper,
};
