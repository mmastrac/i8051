//! An emulator for the i8051 microcontroller.
//!
//! See the [`mod@ops`] module for a list of all supportedinstructions.

mod cpu;
pub mod memory;
mod regs;
pub mod sfr;

pub use cpu::Cpu;
pub use cpu::MemoryMapper;
pub use cpu::PortMapper;
pub use cpu::ops;
