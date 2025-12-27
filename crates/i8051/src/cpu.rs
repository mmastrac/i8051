use std::fmt;

use i8051_proc_macro::{op_def, unique};
use tracing::trace;
use type_mapper::map_types;

use crate::peripheral::{P3_INT0, P3_INT1, SCON_RI, SCON_TI, TCON_TF0, TCON_TF1};
use crate::regs::{Reg8, Reg16, U16Equivalent};
use crate::sfr::*;
use crate::{CpuContext, CpuView, MemoryMapper, PortMapper, ReadOnlyMemoryMapper};

/// Flags available in the PSW register.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Flag {
    C = PSW_C,
    AC = PSW_AC,
    F0 = PSW_F0,
    RS0 = PSW_RS0,
    RS1 = PSW_RS1,
    OV = PSW_OV,
    RES = PSW_RES,
    P = PSW_P,
}

impl Flag {
    pub fn all() -> [Flag; 8] {
        [
            Flag::C,
            Flag::AC,
            Flag::F0,
            Flag::RS0,
            Flag::RS1,
            Flag::OV,
            Flag::RES,
            Flag::P,
        ]
    }

    pub fn short_name(&self) -> &'static str {
        match self {
            Flag::C => "CY",
            Flag::AC => "AC",
            Flag::F0 => "F0",
            Flag::RS0 => "R0",
            Flag::RS1 => "R1",
            Flag::OV => "OV",
            Flag::RES => "RS",
            Flag::P => "PY",
        }
    }
}

/// A register in the 8051 CPU.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    A,
    B,
    DPL,
    DPH,
    DPTR,
    PSW,
    Flag(Flag),
    SP,
    IP,
    IE,
    /// Program Counter.
    PC,
    /// R0-R8
    R(u8),
    /// Internal CPU RAM.
    RAM(u8),
}

#[derive(Debug)]
pub enum Interrupt {
    External0,
    External1,
    Timer0,
    Timer1,
    Serial,
}

enum Direct {
    Ram(u8),
    Sfr(u8),
}

impl From<u8> for Direct {
    fn from(value: u8) -> Self {
        if value < 128 {
            Self::Ram(value)
        } else {
            Self::Sfr(value)
        }
    }
}

impl fmt::Display for Direct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ram(value @ 0..31) => write!(f, "R{}.BANK{}", value % 8, value / 8),
            Self::Ram(value) => write!(f, "RAM({})", value),
            Self::Sfr(SFR_P0) => write!(f, "P0"),
            Self::Sfr(SFR_A) => write!(f, "A"),
            Self::Sfr(SFR_B) => write!(f, "B"),
            Self::Sfr(SFR_PSW) => write!(f, "PSW"),
            Self::Sfr(SFR_SP) => write!(f, "SP"),
            Self::Sfr(SFR_DPL) => write!(f, "DPL"),
            Self::Sfr(SFR_DPH) => write!(f, "DPH"),
            Self::Sfr(SFR_PCON) => write!(f, "PCON"),
            Self::Sfr(SFR_TCON) => write!(f, "TCON"),
            Self::Sfr(SFR_TMOD) => write!(f, "TMOD"),
            Self::Sfr(SFR_TL0) => write!(f, "TL0"),
            Self::Sfr(SFR_TL1) => write!(f, "TL1"),
            Self::Sfr(SFR_TH0) => write!(f, "TH0"),
            Self::Sfr(SFR_TH1) => write!(f, "TH1"),
            Self::Sfr(SFR_P1) => write!(f, "P1"),
            Self::Sfr(SFR_SCON) => write!(f, "SCON"),
            Self::Sfr(SFR_SBUF) => write!(f, "SBUF"),
            Self::Sfr(SFR_P2) => write!(f, "P2"),
            Self::Sfr(SFR_IE) => write!(f, "IE"),
            Self::Sfr(SFR_P3) => write!(f, "P3"),
            Self::Sfr(SFR_IP) => write!(f, "IP"),
            Self::Sfr(SFR_T2CON) => write!(f, "T2CON"),
            Self::Sfr(SFR_T2MOD) => write!(f, "T2MOD"),
            Self::Sfr(SFR_RCAP2L) => write!(f, "RCAP2L"),
            Self::Sfr(SFR_RCAP2H) => write!(f, "RCAP2H"),
            Self::Sfr(SFR_TL2) => write!(f, "TL2"),
            Self::Sfr(SFR_TH2) => write!(f, "TH2"),
            Self::Sfr(sfr) => write!(f, "SFR({})", sfr),
        }
    }
}

enum Bit {
    Ram(u8),
    Sfr(u8),
}

impl From<u8> for Bit {
    fn from(value: u8) -> Self {
        if value < 128 {
            Self::Ram(value)
        } else {
            Self::Sfr(value)
        }
    }
}

impl fmt::Display for Bit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ram(value) => write!(f, "RAM(#{:02X}).{}", (value >> 3) + 0x20, value & 0x07),
            Self::Sfr(value) => {
                Direct::from(value & 0xF8).fmt(f)?;
                write!(f, ".{}", value & 0x07)
            }
        }
    }
}

impl<CTX: CpuContext> CpuView for (&Cpu, &CTX) {
    fn read_xdata(&self, addr: u16) -> u8 {
        self.1.xdata().read(self, addr as u32)
    }
    fn read_code(&self, addr: u32) -> u8 {
        self.1.code().read(self, addr)
    }
    fn pc(&self) -> u16 {
        self.0.pc
    }
    fn pc_ext(&self) -> u32 {
        self.0.pc_ext(self.1)
    }
    fn a(&self) -> u8 {
        self.0.a
    }
    fn b(&self) -> u8 {
        self.0.b
    }
    fn dptr(&self) -> u16 {
        self.0.dptr()
    }
    fn dpl(&self) -> u8 {
        self.0.dpl
    }
    fn dph(&self) -> u8 {
        self.0.dph
    }
    fn psw(&self, flag: Flag) -> bool {
        self.0.psw(flag)
    }
    fn sp(&self) -> u8 {
        self.0.sp
    }
    fn r(&self, x: u8) -> u8 {
        self.0.r(x)
    }
    fn sfr(&self, addr: u8) -> u8 {
        self.0.sfr(addr, self.1)
    }
}

impl<CTX: CpuContext> CpuView for (&mut Cpu, &mut CTX) {
    fn read_xdata(&self, addr: u16) -> u8 {
        self.1.xdata().read(self, addr as u32)
    }
    fn read_code(&self, addr: u32) -> u8 {
        self.1.code().read(self, addr)
    }
    fn pc(&self) -> u16 {
        self.0.pc
    }
    fn pc_ext(&self) -> u32 {
        self.0.pc_ext(self.1)
    }
    fn a(&self) -> u8 {
        self.0.a
    }
    fn b(&self) -> u8 {
        self.0.b
    }
    fn dptr(&self) -> u16 {
        self.0.dptr()
    }
    fn dpl(&self) -> u8 {
        self.0.dpl
    }
    fn dph(&self) -> u8 {
        self.0.dph
    }
    fn psw(&self, flag: Flag) -> bool {
        self.0.psw(flag)
    }
    fn sp(&self) -> u8 {
        self.0.sp
    }
    fn r(&self, x: u8) -> u8 {
        self.0.r(x)
    }
    fn sfr(&self, addr: u8) -> u8 {
        self.0.sfr(addr, self.1)
    }
}

pub enum ControlFlow {
    /// Continue execution from the given PC.
    Continue(u16),
    /// Call the given PC, eventually returning to the given PC.
    Call(u16, u16),
    /// Choose between two branches.
    Choice(u16, u16),
    /// Diverges, unknown control flow.
    Diverge,
}

pub struct Instruction {
    pc: u32,
    bytes: Vec<u8>,
}

#[allow(clippy::len_without_is_empty)]
impl Instruction {
    pub fn decode(&self) -> String {
        decode_string(&self.bytes, self.pc as u16)
    }

    pub fn pc(&self) -> u32 {
        self.pc
    }

    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    pub fn mnemonic(&self) -> Opcode {
        opcode(self.bytes[0])
    }

    pub fn addr(&self) -> Option<u16> {
        addr(&self.bytes)
    }

    pub fn control_flow(&self) -> ControlFlow {
        let pc = self.pc as u16;
        let next = pc.wrapping_add(self.bytes.len() as u16);
        match self.mnemonic() {
            Opcode::LJMP => ControlFlow::Continue(self.addr().unwrap()),
            Opcode::LCALL => ControlFlow::Call(next, self.addr().unwrap()),
            Opcode::AJMP => ControlFlow::Continue((pc & 0xF800) | self.addr().unwrap()),
            Opcode::ACALL => ControlFlow::Call(next, (pc & 0xF800) | self.addr().unwrap()),
            Opcode::JMP | Opcode::RET | Opcode::RETI => ControlFlow::Diverge,
            Opcode::SJMP => ControlFlow::Continue(
                pc.wrapping_add_signed(self.bytes[1] as i8 as i16) + self.bytes.len() as u16,
            ),
            _ => {
                if has_rel(self.bytes[0]) {
                    // `rel` instructions always have a relative offset in the last byte.
                    ControlFlow::Choice(
                        next,
                        pc.wrapping_add_signed(self.bytes[self.bytes.len() - 1] as i8 as i16)
                            + self.bytes.len() as u16,
                    )
                } else {
                    ControlFlow::Continue(next)
                }
            }
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:04X}: ", self.pc)?;
            for byte in &self.bytes {
                write!(f, "{:02X} ", byte)?;
            }
            for _ in 0..(3 - self.bytes.len()) {
                write!(f, "   ")?;
            }
            write!(f, "{}", self.decode())?;
            Ok(())
        } else {
            write!(f, "{}", self.decode())
        }
    }
}

/// The 8051 CPU.
pub struct Cpu {
    pub pc: u16,
    pub internal_ram: [u8; 256],
    a: u8,
    b: u8,
    dpl: u8,
    dph: u8,
    psw: u8,
    sp: u8,
    ip: u8,
    ie: u8,
    interrupt: Option<Interrupt>,
}

impl Default for Cpu {
    fn default() -> Self {
        Self::new()
    }
}

impl Cpu {
    /// Create a new 8051 CPU starting at PC 0x0000.
    pub fn new() -> Self {
        Self {
            pc: 0x0000,
            internal_ram: [0; 256],
            a: 0,
            b: 0,
            dpl: 0,
            dph: 0,
            psw: 0,
            ip: 0,
            ie: 0,
            sp: 7, // !
            interrupt: None,
        }
    }

    /// Step the CPU by one instruction.
    ///
    /// Provide the external memory (`XDATA` and `CODE`) and port mappings for the
    /// CPU to use, or use `()` for the default implementation.
    ///
    /// `XDATA` is external RAM, and `CODE` is external ROM.
    ///
    /// Returns `false` if the CPU has halted.
    pub fn step(&mut self, ctx: &mut impl CpuContext) -> bool {
        if self.ie & IE_EA != 0 {
            if self.ie & IE_ES != 0 {
                let scon = self.sfr(SFR_SCON, ctx);
                if scon & SCON_RI != 0 {
                    trace!("Serial interrupt triggered (RI)");
                    self.interrupt(Interrupt::Serial);
                } else if scon & SCON_TI != 0 {
                    trace!("Serial interrupt triggered (TI)");
                    self.interrupt(Interrupt::Serial);
                }
            }
            if self.ie & IE_ET0 != 0 {
                let tcon = self.sfr(SFR_TCON, ctx);
                if tcon & TCON_TF0 != 0 {
                    trace!("Timer 0 interrupt triggered (TF0)");
                    if self.interrupt(Interrupt::Timer0) {
                        self.sfr_set(SFR_TCON, tcon & !TCON_TF0, ctx);
                    }
                }
            }
            if self.ie & IE_ET1 != 0 {
                let tcon = self.sfr(SFR_TCON, ctx);
                if tcon & TCON_TF1 != 0 {
                    trace!("Timer 1 interrupt triggered (TF1)");
                    if self.interrupt(Interrupt::Timer1) {
                        self.sfr_set(SFR_TCON, tcon & !TCON_TF1, ctx);
                    }
                }
            }
            if self.ie & IE_EX0 != 0 {
                let ie0 = self.sfr(SFR_P3, ctx);
                if ie0 & P3_INT0 == 0 {
                    trace!("External 0 interrupt triggered (INT0)");
                    self.interrupt(Interrupt::External0);
                }
            }
            if self.ie & IE_EX1 != 0 {
                let ie1 = self.sfr(SFR_P3, ctx);
                if ie1 & P3_INT1 == 0 {
                    trace!("External 1 interrupt triggered (INT1)");
                    self.interrupt(Interrupt::External1);
                }
            }
        }

        let pc = self.pc_ext(ctx);
        let op = (&*self, &*ctx).read_code(pc);
        dispatch(self, ctx);
        if self.pc_ext(ctx) == pc {
            // SJMP, so we're in an infinite loop
            if op == 0b10000000 {
                return false;
            }
        }
        true
    }

    pub fn interrupt(&mut self, interrupt: Interrupt) -> bool {
        if self.ie & IE_EA == 0 || self.interrupt.is_some() {
            if let Some(current) = self.interrupt.as_ref() {
                trace!("Interrupt already in progress ({current:?}) while handling {interrupt:?}");
            }
            return false;
        }

        let (handler, ie) = match interrupt {
            Interrupt::Timer0 => (0x000B, IE_ET0),
            Interrupt::Timer1 => (0x001B, IE_ET1),
            Interrupt::Serial => (0x0023, IE_ES),
            Interrupt::External0 => (0x0003, IE_EX0),
            Interrupt::External1 => (0x0013, IE_EX1),
        };

        if self.ie & ie == 0 {
            return false;
        }

        trace!("Interrupt: {:?} (IE = {:02X})", interrupt, self.ie);
        self.interrupt = Some(interrupt);
        self.push_stack16(self.pc);
        self.pc = handler;

        true
    }

    pub fn clear_interrupt(&mut self) {
        self.interrupt = None;
    }

    /// Decode the instruction at the current PC.
    pub fn decode_pc(&self, ctx: &impl CpuContext) -> Instruction {
        let pc = self.pc_ext(ctx);
        Instruction {
            pc,
            bytes: decode(self, ctx, pc),
        }
    }

    /// Decode the instruction at the given PC.
    pub fn decode(&self, ctx: &impl CpuContext, pc: u32) -> Instruction {
        Instruction {
            pc,
            bytes: decode(self, ctx, pc),
        }
    }

    /// Decode an approximate range of instructions around the PC. The suggested
    /// start address may be adjusted to ensure the instruction stream decodes
    /// correctly based on the current PC.
    pub fn decode_range_pc(
        &self,
        ctx: &mut impl CpuContext,
        start: u32,
        lines: usize,
    ) -> Vec<Instruction> {
        let pc = self.pc_ext(ctx);
        self.decode_range(ctx, start, pc, lines)
    }

    /// Decode an approximate range of instructions. The suggested start address
    /// may be adjusted to ensure the instruction stream decodes correctly based on the
    /// current PC.
    pub fn decode_range(
        &self,
        ctx: &mut impl CpuContext,
        start: u32,
        focus: u32,
        lines: usize,
    ) -> Vec<Instruction> {
        let mut instructions = Vec::new();

        // Start with a naive approach of decoding the instructions at the given start address
        for offset in [0, -1, 1, -2, 2, 3, 0] {
            instructions.clear();
            let mut pc = start.saturating_add_signed(offset);
            let mut decoded_focus = false;
            for _ in 0..lines {
                if pc == focus {
                    decoded_focus = true;
                }
                let instruction = self.decode(ctx, pc);
                pc = pc.wrapping_add(instruction.len() as _);
                instructions.push(instruction);
            }
            let end = pc;

            // If we decoded the actual PC, return the instructions
            if decoded_focus {
                return instructions;
            }

            // If we aren't surrounding the current PC, just assume the decoding
            // is value.
            if !(start..end).contains(&focus) {
                return instructions;
            }
        }

        // If we hit this point, we failed to decode something that includes the PC,
        // so just do our best.
        let pc_stream = self.decode_range(ctx, focus, focus, lines);

        if let Some(index) = pc_stream
            .iter()
            .position(|instruction| instruction.pc() > focus)
        {
            instructions.truncate(index);
            instructions.extend(pc_stream);
            instructions.truncate(lines);
            instructions
        } else {
            pc_stream
        }
    }

    /// Retrieves the value of a CPU-managed register.
    pub fn register(&self, register: Register) -> u16 {
        match register {
            Register::A => self.a as _,
            Register::B => self.b as _,
            Register::DPL => self.dpl as _,
            Register::DPH => self.dph as _,
            Register::DPTR => self.dptr() as _,
            Register::PSW => self.psw as _,
            Register::SP => self.sp as _,
            Register::IP => self.ip as _,
            Register::IE => self.ie as _,
            Register::PC => self.pc as _,
            Register::R(n) => self.r(n) as _,
            Register::RAM(n) => self.internal_ram[n as usize] as _,
            Register::Flag(flag) => self.psw(flag) as _,
        }
    }

    /// Sets the value of a CPU-managed register.
    pub fn register_set(&mut self, register: Register, value: u16) {
        match register {
            Register::A => self.a = value as u8,
            Register::B => self.b = value as u8,
            Register::DPL => self.dpl = value as u8,
            Register::DPH => self.dph = value as u8,
            Register::DPTR => self.dptr_set(value),
            Register::PSW => self.psw = value as u8,
            Register::SP => self.sp = value as u8,
            Register::IP => self.ip = value as u8,
            Register::IE => self.ie = value as u8,
            Register::PC => self.pc = value,
            Register::R(n) => *self.r_mut(n) = value as u8,
            Register::RAM(n) => self.internal_ram[n as usize] = value as u8,
            Register::Flag(flag) => self.psw_set(flag, value != 0),
        }
    }

    /// Get the value of the A register.
    pub fn a(&self) -> u8 {
        self.a
    }

    /// Set the value of the A register.
    pub fn a_set(&mut self, value: u8) {
        self.a = value;
        self.psw_set(Flag::P, value.count_ones() % 2 == 1);
    }

    /// Get the value of the B register.
    pub fn b(&self) -> u8 {
        self.b
    }

    /// Set the value of the B register.
    pub fn b_set(&mut self, value: u8) {
        self.b = value;
    }

    /// Get the value of the DPTR register.
    pub fn dptr(&self) -> u16 {
        ((self.dph as u16) << 8) | self.dpl as u16
    }

    /// Set the value of the DPTR register.
    pub fn dptr_set(&mut self, value: u16) {
        self.dph = (value >> 8) as u8;
        self.dpl = (value & 0xFF) as u8;
    }

    /// Get the value of the indexed R register, offset using the current PSW's
    /// RS0/RS1 bits.
    pub fn r(&self, x: u8) -> u8 {
        let rs0 = (self.psw & (1 << PSW_RS0)) != 0;
        let rs1 = (self.psw & (1 << PSW_RS1)) != 0;
        let offset = ((rs0 as u8 | ((rs1 as u8) << 1)) * 8) as usize;
        self.internal_ram[x as usize + offset]
    }

    /// Set the value of the indexed R register, offset using the current PSW's
    /// RS0/RS1 bits.
    pub fn r_mut(&mut self, x: u8) -> &mut u8 {
        let rs0 = (self.psw & (1 << PSW_RS0)) != 0;
        let rs1 = (self.psw & (1 << PSW_RS1)) != 0;
        let offset = ((rs0 as u8 | ((rs1 as u8) << 1)) * 8) as usize;
        &mut self.internal_ram[x as usize + offset]
    }

    /// Read from an SFR.
    pub fn sfr(&self, addr: u8, ctx: &impl CpuContext) -> u8 {
        match addr {
            SFR_A => self.a,
            SFR_B => self.b,
            SFR_DPH => self.dph,
            SFR_DPL => self.dpl,
            SFR_PSW => self.psw,
            SFR_SP => self.sp,
            SFR_IP => self.ip,
            SFR_IE => self.ie,
            _ => ctx.ports().read(&(self, ctx), addr),
        }
    }

    /// Write to an SFR.
    pub fn sfr_set(&mut self, addr: u8, value: u8, ctx: &mut impl CpuContext) {
        match addr {
            SFR_A => self.a = value,
            SFR_B => self.b = value,
            SFR_DPH => self.dph = value,
            SFR_DPL => self.dpl = value,
            SFR_PSW => self.psw = value,
            SFR_SP => self.sp = value,
            SFR_IP => self.ip = value,
            SFR_IE => {
                trace!("IE set to {:02X} @ {:X}", value, self.pc_ext(ctx));
                self.ie = value;
            }
            _ => {
                let write = ctx.ports().prepare_write(&(&*self, &*ctx), addr, value);
                ctx.ports_mut().write(write);
            }
        }
    }

    /// Get the value of a PSW flag.
    pub fn psw(&self, flag: Flag) -> bool {
        self.psw & (1 << flag as u8) != 0
    }

    /// Set the value of a PSW flag.
    pub fn psw_set(&mut self, flag: Flag, value: bool) {
        if value {
            self.psw |= 1 << flag as u8;
        } else {
            self.psw &= !(1 << flag as u8);
        }
    }

    /// Get the value of the IP register.
    pub fn ip(&self) -> u8 {
        self.ip
    }

    /// Set the value of the IP register.
    pub fn ip_set(&mut self, value: u8) {
        self.ip = value;
    }

    /// Get the value of the IE register.
    pub fn ie(&self) -> u8 {
        self.ie
    }

    /// Set the value of the IE register.
    pub fn ie_set(&mut self, value: u8) {
        self.ie = value;
    }

    /// Get the value of the SP register.
    pub fn sp(&self) -> u8 {
        self.sp
    }

    /// Set the value of the SP register.
    pub fn sp_set(&mut self, value: u8) {
        self.sp = value;
    }

    pub fn pc_ext(&self, ctx: &impl CpuContext) -> u32 {
        (self.pc as u32) | (ctx.ports().pc_extension(&(self, ctx)) as u32) << 16
    }

    pub fn pc_ext_addr(&self, ctx: &impl CpuContext, addr: u16) -> u32 {
        (addr as u32) | (ctx.ports().pc_extension(&(self, ctx)) as u32) << 16
    }

    pub fn internal_ram(&self, addr: u8) -> u8 {
        self.internal_ram[addr as usize]
    }

    pub fn internal_ram_write(&mut self, addr: u8, value: u8) {
        self.internal_ram[addr as usize] = value;
    }

    /// Push a value to the stack. The stack is stored in internal RAM and
    /// indexed by the SP register.
    pub fn push_stack(&mut self, value: u8) {
        self.sp = self.sp.wrapping_add(1);
        self.internal_ram[self.sp as usize] = value;
    }

    /// Push a 16-bit value to the stack, low byte first, matching CALL
    /// semantics. The stack is stored in internal RAM and indexed by the SP
    /// register.
    pub fn push_stack16(&mut self, value: u16) {
        self.push_stack((value & 0xFF) as u8);
        self.push_stack((value >> 8) as u8);
    }

    /// Pop a value from the stack. The stack is stored in internal RAM and
    /// indexed by the SP register.
    pub fn pop_stack(&mut self) -> u8 {
        let value = self.internal_ram[self.sp as usize];
        self.sp = self.sp.wrapping_sub(1);
        value
    }

    /// Pop a 16-bit value from the stack, high byte first, matching CALL
    /// semantics. The stack is stored in internal RAM and indexed by the SP
    /// register.
    pub fn pop_stack16(&mut self) -> u16 {
        let a = self.pop_stack();
        let b = self.pop_stack();
        ((a as u16) << 8) | b as u16
    }

    /// Read a value from internal RAM or an SFR, matching the semantics of
    /// `direct`-loading opcodes.
    fn read(&self, addr: u8, ctx: &impl CpuContext) -> u8 {
        if addr < 128 {
            self.internal_ram[addr as usize]
        } else {
            self.sfr(addr, ctx)
        }
    }

    /// Read a value from internal RAM, matching the semantics of `@Rn` opcodes.
    fn read_indirect(&self, addr: u8) -> u8 {
        self.internal_ram[addr as usize]
    }

    /// Write a value to internal RAM or an SFR, matching the semantics of
    /// `direct`-storing opcodes.
    fn write(&mut self, addr: u8, value: u8, ctx: &mut impl CpuContext) {
        if addr < 128 {
            self.internal_ram[addr as usize] = value;
        } else {
            self.sfr_set(addr, value, ctx);
        }
    }

    /// Write a value to internal RAM, matching the semantics of `@Rn` opcodes.
    fn write_indirect(&mut self, addr: u8, value: u8) {
        self.internal_ram[addr as usize] = value;
    }

    /// Read a bit from the internal RAM or SFR. Bit addresses are 0-127 for
    /// internal RAM (mapped to 0x20-0x2F), 128-255 for SFRs (mapped to 0x80,
    /// 0x90, ... 0xf0).
    fn read_bit(&self, bit_addr: u8, ctx: &impl CpuContext) -> bool {
        let bit_pos = bit_addr & 0x07;
        if bit_addr < 0x80 {
            let byte_index = 0x20 + (bit_addr >> 3);
            self.internal_ram[byte_index as usize] & (1 << bit_pos) != 0
        } else {
            let sfr_addr = bit_addr & 0xF8; // base byte of SFR
            self.sfr(sfr_addr, ctx) & (1 << bit_pos) != 0
        }
    }

    /// Write a bit to the internal RAM or SFR. Bit addresses are 0-127 for
    /// internal RAM (mapped to 0x20-0x2F), 128-255 for SFRs (mapped to 0x80,
    /// 0x90, ... 0xf0).
    fn write_bit(&mut self, bit_addr: u8, value: bool, ctx: &mut impl CpuContext) {
        let bit_pos = bit_addr & 0x07;
        if bit_addr < 0x80 {
            let byte_index = 0x20 + (bit_addr >> 3);
            let byte = &mut self.internal_ram[byte_index as usize];
            if value {
                *byte |= 1 << bit_pos;
            } else {
                *byte &= !(1 << bit_pos);
            }
        } else {
            let sfr_addr = bit_addr & 0xF8;
            let byte = self.sfr(sfr_addr, ctx);
            let byte = if value {
                byte | 1 << bit_pos
            } else {
                byte & !(1 << bit_pos)
            };
            self.sfr_set(sfr_addr, byte, ctx);
        }
    }

    fn write_bit_latch(&mut self, bit_addr: u8, value: bool, ctx: &mut impl CpuContext) {
        match bit_addr & 0xF8 {
            sfr @ (SFR_P0 | SFR_P1 | SFR_P2 | SFR_P3) => {
                let byte = ctx.ports().read_latch(&(&*self, &*ctx), sfr);
                let byte = if value {
                    byte | 1 << (bit_addr & 0x07)
                } else {
                    byte & !(1 << (bit_addr & 0x07))
                };
                self.sfr_set(sfr, byte, ctx);
            }
            _ => {
                self.write_bit(bit_addr, value, ctx);
            }
        }
    }
}

// Helper macros for the proc macro to use
macro_rules! op_def_read {
    ($ctx:ident, A) => {
        Reg8($ctx.a())
    };
    ($ctx:ident, B) => {
        Reg8($ctx.b())
    };
    ($ctx:ident, PC) => {
        Reg16($ctx.pc())
    };
    ($ctx:ident, DPTR) => {{ Reg16($ctx.dptr()) }};
    ($ctx:ident, C) => {
        $ctx.psw(Flag::C)
    };
    ($ctx:ident, OV) => {
        $ctx.psw(Flag::OV)
    };
    ($ctx:ident, AC) => {
        $ctx.psw(Flag::AC)
    };
    ($ctx:ident, BIT, $index:expr) => {
        $ctx.0.read_bit($index.to_u8().0, $ctx.1)
    };
    ($ctx:ident, PBIT, $index:expr) => {{
        let port = $index.to_u8().0;
        let sfr = port & 0xF8;
        let mask = match sfr {
            SFR_P0 | SFR_P1 | SFR_P2 | SFR_P3 => {
                ($ctx.1.ports().read_latch(&$ctx, sfr) & (1 << ($index.to_u8().0 & 0x07))) != 0
            }
            _ => true,
        };
        mask & $ctx.0.read_bit(port, $ctx.1)
    }};
    ($ctx:ident, DATA, $index:expr) => {
        Reg8($ctx.0.read($index.to_u8().0, $ctx.1))
    };
    ($ctx:ident, PDATA, $index:expr) => {{
        let port = $index.to_u8().0;
        match port {
            SFR_P0 | SFR_P1 | SFR_P2 | SFR_P3 => {
                $ctx.1.ports().read_latch(&$ctx, port) & $ctx.0.read(port, $ctx.1)
            }
            _ => $ctx.0.read(port, $ctx.1),
        }
    }};
    ($ctx:ident, IDATA, $index:expr) => {
        Reg8($ctx.0.read_indirect($index.to_u8().0))
    };
    ($ctx:ident, R, $index:expr) => {
        Reg8($ctx.0.r($index.to_u8().0))
    };
    ($ctx:ident, CODE, $index:expr) => {
        Reg8($ctx.read_code(
            (($ctx.1.ports().pc_extension(&$ctx) as u32) << 16) as u32 | $index.to_u16() as u32,
        ))
    };
    ($ctx:ident, XDATA, $index:expr) => {
        if std::mem::size_of_val(&$index) == 2 {
            $ctx.1.xdata().read(&$ctx, $index.to_u16() as _)
        } else {
            let high = ($ctx.1.ports().read_latch(&$ctx, SFR_P2) as u16) << 8;
            $ctx.1.xdata().read(&$ctx, ($index.to_u16() | high) as _)
        }
    };
}

macro_rules! op_def_write {
    ($ctx:ident, A, $value:expr) => {{
        $ctx.0.a_set($value.to_u8().0);
    }};
    ($ctx:ident, B, $value:expr) => {{
        $ctx.0.b_set($value.to_u8().0);
    }};
    ($ctx:ident, PC, $value:expr) => {{
        $ctx.0.pc = $value.to_u16();
    }};
    ($ctx:ident, DPTR, $value:expr) => {{
        $ctx.0.dptr_set($value.to_u16());
    }};
    ($ctx:ident, C, $value:expr) => {{
        $ctx.0.psw_set(Flag::C, $value);
    }};
    ($ctx:ident, OV, $value:expr) => {{
        $ctx.0.psw_set(Flag::OV, $value);
    }};
    ($ctx:ident, AC, $value:expr) => {{
        $ctx.0.psw_set(Flag::AC, $value);
    }};
    ($ctx:ident, BIT, $index:expr, $value:expr) => {
        $ctx.0.write_bit_latch($index.to_u8().0, $value, $ctx.1)
    };
    ($ctx:ident, PBIT, $index:expr, $value:expr) => {
        $ctx.0.write_bit($index.to_u8().0, $value, $ctx.1)
    };
    ($ctx:ident, DATA, $index:expr, $value:expr) => {
        $ctx.0.write($index.to_u8().0, $value.to_u8().0, $ctx.1)
    };
    ($ctx:ident, PDATA, $index:expr, $value:expr) => {
        $ctx.0.write($index.to_u8().0, $value.to_u8().0, $ctx.1)
    };
    ($ctx:ident, IDATA, $index:expr, $value:expr) => {
        $ctx.0.write_indirect($index.to_u8().0, $value.to_u8().0)
    };
    ($ctx:ident, R, $index:expr, $value:expr) => {
        *$ctx.0.r_mut($index.to_u8().0) = $value.to_u8().0;
    };
    ($ctx:ident, CODE, $index:expr, $value:expr) => {
        "CODE cannot be written"
    };
    ($ctx:ident, XDATA, $index:expr, $value:expr) => {
        if std::mem::size_of_val(&$index) == 2 {
            let write = $ctx
                .1
                .xdata()
                .prepare_write(&$ctx, $index.to_u16() as _, $value.to_u8().0);
            $ctx.1.xdata_mut().write(write)
        } else {
            let high = ($ctx.1.ports().read_latch(&$ctx, SFR_P2) as u16) << 8;
            let write = $ctx.1.xdata().prepare_write(
                &$ctx,
                ($index.to_u16() | high) as _,
                $value.to_u8().0,
            );
            $ctx.1.xdata_mut().write(write)
        }
    };
}

macro_rules! op_def_call {
    ($ctx:ident, POP()) => {
        Reg8($ctx.0.pop_stack())
    };
    ($ctx:ident, POP16()) => {
        Reg16($ctx.0.pop_stack16())
    };
    ($ctx:ident, PUSH($value:expr)) => {
        $ctx.0.push_stack(U16Equivalent::to_u16($value).to_u8().0)
    };
    ($ctx:ident, PUSH16($value:expr)) => {
        $ctx.0.push_stack16(U16Equivalent::to_u16($value))
    };
    ($ctx:ident, CLEAR_INT()) => {
        $ctx.0.clear_interrupt();
    };
    ($ctx:ident, SEXT($value:expr)) => {
        U16Equivalent::sext($value)
    };
}

macro_rules! opcodes {
    ($($opcode:ident)*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum Opcode {
            Unknown,
        $(
            $opcode,
        )*
        }
    };
}

macro_rules! op {
    (
        docs
        $([
            $opcode:ident
            $name:literal
            { $($stmt:stmt);+ $(;)? }
        ])*
    ) => {
        /// All instructions for the i8051 microcontroller.
        ///
        /// Instructions are defined in pseudo-Rust code.
        ///
        $(
        #[doc = concat!(" → `", stringify!($opcode), " ", $name, "`\n\n```nocompile\n")]
        $(
            #[doc = concat!(stringify!($stmt), ";")]
        )*
        #[doc = "```\n\n"]
        )*
        pub mod ops {}
    };

    (
        $(
            OP $opcode:ident $name:literal $start:literal $(- $mask:ident $mask_pattern:literal)? $($arg:ident)*  $(, $arg_mask:ident = $arg_mask_expr:tt)? => $stmt:tt ;
        )*
    ) => {
        op!( docs $( [$opcode $name $stmt] )* );

        unique!(opcodes $( $opcode )*);

        pub fn has_rel(op: u8) -> bool {
            $(
                if (op $(& !$mask_pattern)? == $start) {
                    $(
                        if stringify!($arg) == "rel" {
                            return true;
                        }
                    )*
                }
            )*
            false
        }

        /// Decode the absolute address of the instruction at the given PC, if one exists.
        pub fn addr(code: &[u8]) -> Option<u16> {
            #![allow(unused)]
            let op = Reg8(code[0]);
            $(
                if (op $(& !$mask_pattern)? == $start) {
                    $(let $mask = op & $mask_pattern;)?
                    let mut next_read = 1;
                    $(
                        let b = code[next_read];
                        let $arg: Reg8 = b.into();
                        next_read = next_read.wrapping_add(1);
                    )*
                    $(
                        op_def!(__no_cpu {
                            let $arg_mask = $arg_mask_expr;
                        });
                        if stringify!($arg_mask) == "addr" {
                            return Some($arg_mask.to_u16());
                        }
                        return None;
                    )?
                }
            )*
            None
        }

        pub fn opcode(op: u8) -> Opcode {
            $(
                if (op $(& !$mask_pattern)? == $start) {
                    return Opcode::$opcode;
                }
            )*
            Opcode::Unknown
        }

        pub fn decode(cpu: &Cpu, ctx: &impl CpuContext, pc: u32) -> Vec<u8> {
            #![allow(unused)]
            #![allow(non_snake_case)]

            let ctx = (&*cpu, &*ctx);
            let op = Reg8(ctx.read_code(pc));
            let mut bytes = vec![op.0];

            $(
                if (op $(& !$mask_pattern)? == $start) {
                    $(let $mask = op & $mask_pattern;)?
                    let mut next_read = pc.wrapping_add(1);
                    $(
                        let b = ctx.read_code(next_read);
                        let $arg: map_types!(match $arg {
                            direct => Direct,
                            bit => Bit,
                            dst => Direct,
                            src => Direct,
                            _ => Reg8,
                        }) = b.into();
                        next_read = next_read.wrapping_add(1);
                        bytes.push(b);
                    )*
                    $(op_def!(__no_cpu {let $arg_mask = $arg_mask_expr;});)?
                    bytes
                } else
            )*
            {
                // Unimplemented instruction
                bytes
            }
        }

        pub fn decode_string(code: &[u8], pc: u16) -> String {
            #![allow(unused)]
            #![allow(non_snake_case)]

            let op = Reg8(code[0]);

            $(
                if (op $(& !$mask_pattern)? == $start) {
                    $(let $mask = op & $mask_pattern;)?
                    let mut next_read = 1;
                    $(
                        let b = code[next_read];
                        let $arg: map_types!(match $arg {
                            direct => Direct,
                            bit => Bit,
                            dst => Direct,
                            src => Direct,
                            _ => Reg8,
                        }) = b.into();
                        next_read = next_read.wrapping_add(1);
                    )*
                    $(op_def!(__no_cpu {let $arg_mask = $arg_mask_expr;});)?
                   format!("{} {}", stringify!($opcode), format!($name))
                } else
            )*
            {
                // Unimplemented instruction
                "???".to_string()
            }
        }

        pub fn dispatch(cpu: &mut Cpu, ctx: &mut impl CpuContext) {
            #![allow(non_snake_case)]
            #![allow(redundant_semicolons)]
            #![allow(unused_assignments)]

            let pc_ext = cpu.pc_ext(ctx);
            let ctx = (cpu, ctx);
            let op = Reg8(ctx.read_code(pc_ext));

            $(
                if (op $(& !$mask_pattern)? == $start) {
                    $(let $mask = op & $mask_pattern;)?
                    #[allow(unused)]
                    let mut next_read = pc_ext.wrapping_add(1);
                    $(
                        let $arg = Reg8(ctx.read_code(next_read));
                        next_read = next_read.wrapping_add(1);
                    )*
                    $(op_def!(__no_cpu {let $arg_mask = $arg_mask_expr;});)?
                    op_def!(ctx $stmt);
                } else
            )*
            {
                // Unimplemented instruction
                ctx.0.pc += 1;
            }
        }
    };
}

op! {
    OP AJMP "#{addr:X}" 0b00000001 - mask 0b11100000 imm8, addr = (mask<<3|imm8) => { PC=(PC&0xF800)|addr; };
    OP ACALL "#{addr:X}" 0b00010001 - mask 0b11100000 imm8, addr = (mask<<3|imm8) => { PUSH16(PC+2); PC=(PC&0xF800)|addr; };

    // Control flow
    OP NOP "" 0b00000000 => {PC+=1};
    OP LJMP "#{addr:04X}" 0b00000010 imm_hi imm_lo, addr = (imm_hi<<8|imm_lo) => {PC=addr};
    OP LCALL "#{addr:04X}" 0b00010010 imm_hi imm_lo, addr = (imm_hi<<8|imm_lo) => {PUSH16(PC+3); PC=addr};
    OP JC "{rel:+}" 0b01000000 rel, rel=(SEXT(rel)) => {if (C) {PC=PC+2+rel} else {PC+=2}};
    OP JNC "{rel:+}" 0b01010000 rel, rel=(SEXT(rel)) => {if (!C) {PC=PC+2+rel} else {PC+=2}};
    OP JZ "{rel:+}" 0b01100000 rel, rel=(SEXT(rel)) => {if (A==0) {PC=PC+2+rel} else {PC+=2}};
    OP JNZ "{rel:+}" 0b01110000 rel, rel=(SEXT(rel)) => {if (A!=0) {PC=PC+2+rel} else {PC+=2}};
    OP SJMP "{rel:+}" 0b10000000 rel, rel=(SEXT(rel)) => {PC=PC+2+rel};
    OP RET "" 0b00100010 => {PC=POP16()};
    OP RETI "" 0b00110010 => {PC=POP16(); CLEAR_INT()};
    OP JMP "@A+DPTR"       0b01110011 => {PC=DPTR+A};
    OP JB "{bit},{rel:+}"  0b00100000 bit rel, rel=(SEXT(rel)) => {if (PBIT[bit]) {PC=PC+3+rel} else {PC+=3}};
    OP JNB "{bit},{rel:+}" 0b00110000 bit rel, rel=(SEXT(rel)) => {if (!PBIT[bit]) {PC=PC+3+rel} else {PC+=3}};
    OP JBC "{bit},{rel:+}" 0b00010000 bit rel, rel=(SEXT(rel)) => {if (PBIT[bit]) {BIT[bit]=false; PC=PC+3+rel} else {PC+=3}};

    // Conditional branches and compare/jump
    OP DJNZ "{direct},{rel:+}" 0b11010101 direct rel, rel=(SEXT(rel)) => {DATA[direct]-=1; if (DATA[direct]!=0) {PC=PC+3+rel} else {PC+=3}};
    OP DJNZ "R{x},{rel:+}" 0b11011000 -x 0b111 rel, rel=(SEXT(rel)) => {R[x]-=1; if (R[x]!=0) {PC=PC+2+rel} else {PC+=2}};
    OP CJNE "A,{direct},{rel:+}" 0b10110101 direct rel, rel=(SEXT(rel)) => {let tmp=DATA[direct]; C=A<tmp; if (A!=tmp) {PC=PC+3+rel} else {PC+=3}};
    OP CJNE "@R{x},#{imm8:02X},{rel:+}" 0b10110110 -x 0b1 imm8 rel, rel=(SEXT(rel)) => {C=IDATA[R[x]]<imm8; if (IDATA[R[x]]!=imm8) {PC=PC+3+rel} else {PC+=3}};
    OP CJNE "R{x},#{imm8:02X},{rel:+}" 0b10111000 -x 0b111 imm8 rel, rel=(SEXT(rel)) => {C=R[x]<imm8; if (R[x]!=imm8) {PC=PC+3+rel} else {PC+=3}};
    OP CJNE "A,#{imm8:02X},{rel:+}" 0b10110100 imm8 rel, rel=(SEXT(rel)) => {C=A<imm8; if (A!=imm8) { PC=PC+3+rel } else { PC+=3 }};

    // DPTR / MOVX / MOVC
    OP MOV "DPTR,#{imm16:04X}" 0b10010000 imm_hi imm_lo, imm16 = (imm_hi<<8|imm_lo) => {DPTR=imm16; PC+=3};
    OP INC "DPTR" 0b10100011 => {DPTR+=1; PC+=1};
    OP MOVX "@DPTR,A" 0b11110000 => {XDATA[DPTR]=A; PC+=1};
    OP MOVX "A,@DPTR" 0b11100000 => {A=XDATA[DPTR]; PC+=1};
    OP MOVX "A,@R{x}" 0b11100010 -x 0b1 => {A=XDATA[R[x]]; PC+=1};
    OP MOVX "@R{x},A" 0b11110010 -x 0b1 => {XDATA[R[x]]=A; PC+=1};
    OP MOVC "A,@A+DPTR" 0b10010011 => {A=CODE[DPTR+A]; PC+=1};
    OP MOVC "A,@A+PC" 0b10000011 => {A=CODE[PC+1+A]; PC+=1};

    // Accumulator and arithmetic
    OP CLR "A" 0b11100100 => {A=0; PC+=1};

    OP INC "A" 0b00000100 => {A=A+1; PC+=1};
    OP INC "{direct}" 0b00000101 direct => {DATA[direct]+=1; PC+=2};
    OP INC "@R{x}" 0b00000110 -x 0b1 => {IDATA[R[x]]+=1; PC+=1};
    OP INC "R{x}" 0b00001000 -x 0b111 => {R[x]+=1; PC+=1};

    OP DEC "A" 0b00010100 => {A=A-1; PC+=1};
    OP DEC "{direct}" 0b00010101 direct => {DATA[direct]-=1; PC+=2};
    OP DEC "@R{x}" 0b00010110 -x 0b1 => {IDATA[R[x]]-=1; PC+=1};
    OP DEC "R{x}" 0b00011000 -x 0b111 => {R[x]-=1; PC+=1};

    OP CPL "A" 0b11110100 => {A=!A; PC+=1};

    OP MUL "AB" 0b10100100 => {(A, B, C, OV) = mul(A, B); PC+=1};
    OP DIV "AB" 0b10000100 => {(A, B, C, OV) = div(A, B); PC+=1};

    OP ADD "A,#{imm8:02X}" 0b00100100 imm8 => {(A, C, OV, AC) = add_with_carry(A, imm8, false); PC+=2};
    OP ADD "A,{direct}" 0b00100101 direct => {(A, C, OV, AC) = add_with_carry(A, DATA[direct], false); PC+=2};
    OP ADD "A,@R{x}" 0b00100110 -x 0b1 => {(A, C, OV, AC) = add_with_carry(A, IDATA[R[x]], false); PC+=1};
    OP ADD "A,R{x}" 0b00101000 -x 0b111 => {(A, C, OV, AC) = add_with_carry(A, R[x], false); PC+=1};

    OP ADDC "A,#{imm8:02X}" 0b00110100 imm8 => {(A, C, OV, AC) = add_with_carry(A, imm8, C); PC+=2};
    OP ADDC "A,{direct}" 0b00110101 direct => {(A, C, OV, AC) = add_with_carry(A, DATA[direct], C); PC+=2};
    OP ADDC "A,@R{x}" 0b00110110 -x 0b1 => {(A, C, OV, AC) = add_with_carry(A, IDATA[R[x]], C); PC+=1};
    OP ADDC "A,R{x}" 0b00111000 -x 0b111 => {(A, C, OV, AC) = add_with_carry(A, R[x], C); PC+=1};

    OP SUBB "A,#{imm8:02X}" 0b10010100 imm8 => {(A, C) = sub_with_borrow(A, imm8, C); PC+=2};
    OP SUBB "A,{direct}" 0b10010101 direct => {(A, C) = sub_with_borrow(A, DATA[direct], C); PC+=2};
    OP SUBB "A,@R{x}" 0b10010110 -x 0b1 => {(A, C) = sub_with_borrow(A, IDATA[R[x]], C); PC+=1};
    OP SUBB "A,R{x}" 0b10011000 -x 0b111 => {(A, C) = sub_with_borrow(A, R[x], C); PC+=1};

    OP RLC "A" 0b00110011 => { (A, C) = rlc(A, C); PC+=1 };
    OP RRC "A" 0b00010011 => { (A, C) = rrc(A, C); PC+=1 };
    OP RL "A" 0b00100011 => { A = rl(A); PC+=1 };
    OP RR "A" 0b00000011 => { A = rr(A); PC+=1 };

    OP ANL "A,#{imm8:02X}"    0b01010100 imm8 => {A=A&imm8; PC+=2};
    OP ANL "A,{direct}"       0b01010101 direct => {A=A&PDATA[direct]; PC+=2};
    OP ANL "A,@R{x}"          0b01010110 -x 0b1 => {A=A&IDATA[R[x]]; PC+=1};
    OP ANL "A,R{x}"           0b01011000 -x 0b111 => {A=A&R[x]; PC+=1};
    OP ANL "{direct},A"       0b01010010 direct => {DATA[direct]&=A; PC+=2};
    OP ANL "{direct},#{imm8}" 0b01010011 direct imm8 => {DATA[direct]&=imm8; PC+=3};

    OP ORL "A,#{imm8:02X}"    0b01000100 imm8 => {A=A|imm8; PC+=2};
    OP ORL "A,{direct}"       0b01000101 direct => {A=A|PDATA[direct]; PC+=2};
    OP ORL "A,@R{x}"          0b01000110 -x 0b1 => {A=A|IDATA[R[x]]; PC+=1};
    OP ORL "A,R{x}"           0b01001000 -x 0b111 => {A=A|R[x]; PC+=1};
    OP ORL "{direct},A"       0b01000010 direct => {DATA[direct]|=A; PC+=2};
    OP ORL "{direct},#{imm8}" 0b01000011 direct imm8 => {DATA[direct]|=imm8; PC+=3};

    OP XRL "A,#{imm8:02X}"    0b01100100 imm8 => {A=A^imm8; PC+=2};
    OP XRL "A,{direct}"       0b01100101 direct => {A=A^PDATA[direct]; PC+=2};
    OP XRL "A,@R{x}"          0b01100110 -x 0b1 => {let tmp=IDATA[R[x]]; A=A^tmp; PC+=1};
    OP XRL "A,R{x}"           0b01101000 -x 0b111 => {A=A^R[x]; PC+=1};
    OP XRL "{direct},A"       0b01100010 direct => {DATA[direct]^=A; PC+=2};
    OP XRL "{direct},#{imm8}" 0b01100011 direct imm8 => {DATA[direct]^=imm8; PC+=3};

    OP DA "A" 0b11010100 => {(A,C,AC)=decimal_adjust(A,C,AC); PC+=1};

    // MOV families
    OP MOV "A,#{imm8:02X}"         0b01110100 imm8 => {A=imm8; PC+=2};
    OP MOV "R{x},#{imm8:02X}"      0b01111000 -x 0b111 imm8 => {R[x]=imm8; PC+=2};
    OP MOV "@R{x},A"               0b11110110 -x 0b1 => {IDATA[R[x]]=A; PC+=1};
    OP MOV "A,@R{x}"               0b11100110 -x 0b1 => {A=IDATA[R[x]]; PC+=1};
    OP MOV "R{x},A"                0b11111000 -x 0b111 => {R[x]=A; PC+=1};
    OP MOV "A,R{x}"                0b11101000 -x 0b111 => {A=R[x]; PC+=1};
    OP MOV "{direct},A"            0b11110101 direct => {DATA[direct]=A; PC+=2};
    OP MOV "{direct},#{imm8:02X}"  0b01110101 direct imm8 => { DATA[direct]=imm8; PC+=3 };
    OP MOV "@R{x},#{imm8:02X}"     0b01110110 -x 0b1 imm8 => { IDATA[R[x]]=imm8; PC+=2 };
    OP MOV "{dst},{src}"           0b10000101 src dst => { DATA[dst]=PDATA[src]; PC+=3 };
    OP MOV "{direct},@R{x}"        0b10000110 -x 0b1 direct => { DATA[direct]=IDATA[R[x]]; PC+=2 };
    OP MOV "{direct},R{x}"         0b10001000 -x 0b111 direct => { DATA[direct]=R[x]; PC+=2 };

    OP MOV "A,{direct}"            0b11100101 direct => {A=PDATA[direct]; PC+=2};
    OP MOV "@R{x},{direct}"        0b10100110 -x 0b1 direct => { IDATA[R[x]]=PDATA[direct]; PC+=2 };
    OP MOV "R{x},{direct}"         0b10101000 -x 0b111 direct => { R[x]=PDATA[direct]; PC+=2 };

    // Stack
    OP PUSH "{direct}" 0b11000000 direct => {PUSH(DATA[direct]); PC+=2};
    OP POP "{direct}" 0b11010000 direct => {DATA[direct]=POP(); PC+=2};

    // Carry/bit operations
    OP CLR "C" 0b11000011 => {C=false; PC+=1};
    OP SETB "C" 0b11010011 => {C=true; PC+=1};
    OP CPL "C" 0b10110011 => {C=!C; PC+=1};
    OP CLR "#{bit}" 0b11000010 bit => {BIT[bit]=false; PC+=2};
    OP SETB "#{bit}" 0b11010010 bit => {BIT[bit]=true; PC+=2};
    OP CPL "#{bit}" 0b10110010 bit => {BIT[bit]=!BIT[bit]; PC+=2};
    OP MOV "C,#{bit}" 0b10100010 bit => {C=PBIT[bit]; PC+=2};
    OP MOV "#{bit},C" 0b10010010 bit => {PBIT[bit]=C; PC+=2};
    OP ANL "C,#{bit}" 0b10000010 bit => {C&=PBIT[bit]; PC+=2};
    OP ANL "C,/#{bit}" 0b10110000 bit => {C&=!PBIT[bit]; PC+=2};
    OP ORL "C,#{bit}" 0b01110010 bit => {C|=PBIT[bit]; PC+=2};
    OP ORL "C,/#{bit}" 0b10100000 bit => {C|=!PBIT[bit]; PC+=2};

    // Exchange
    OP SWAP "A" 0b11000100 => {A=swap_nibbles(A); PC+=1};
    OP XCH "A,{direct}" 0b11000101 direct => {(A, DATA[direct])=(DATA[direct], A);PC+=2};
    OP XCH "A,@R{x}" 0b11000110 -x 0b1 => {(A, IDATA[R[x]])=(IDATA[R[x]], A); PC+=1};
    OP XCH "A,R{x}" 0b11001000 -x 0b111 => {(A, R[x])=(R[x], A);PC+=1};
    OP XCHD "A,@R{x}" 0b11010110 -x 0b1 => {let tmp=IDATA[R[x]]; let tmp2=A&0x0F; A=(A&0xF0)|(tmp&0x0F); IDATA[R[x]]=tmp&0xF0|tmp2; PC+=1};
}
pub(crate) use op;

fn swap_nibbles(a: Reg8) -> Reg8 {
    Reg8(((a.0 & 0x0F) << 4) | ((a.0 & 0xF0) >> 4))
}

fn decimal_adjust(a: Reg8, c: bool, ac: bool) -> (Reg8, bool, bool) {
    let mut result = a.0;
    let mut carry = c;
    let aux_carry = ac;

    // Check if low nibble needs adjustment
    if (result & 0x0F) > 9 || aux_carry {
        result = result.wrapping_add(6);
        // Check if adding 6 to low nibble caused carry to high nibble
        if result < a.0 {
            carry = true;
        }
    }

    // Check if high nibble needs adjustment
    if ((result & 0xF0) >> 4) > 9 || carry {
        result = result.wrapping_add(0x60);
        // Check if adding 0x60 caused overflow
        if result < (result.wrapping_sub(0x60)) {
            carry = true;
        }
    }

    // AC is set if there was carry from bit 3 to bit 4 during the low nibble adjustment
    let new_ac = if (a.0 & 0x0F) > 9 || aux_carry {
        ((a.0 & 0x0F) + 6) > 0x0F
    } else {
        aux_carry
    };

    (Reg8(result), carry, new_ac)
}

#[inline(always)]
fn add_with_carry(a: Reg8, b: Reg8, c: bool) -> (Reg8, bool, bool, bool) {
    // Aux carry is set if the sum of the low nibbles is greater than 0x0F
    let ac = ((a.0 & 0x0F) + (b.0 & 0x0F) + c as u8) > 0x0F;
    let sum = a.to_u16() + b.to_u16() + c as u16;

    // Treat as signed 16-bit to detect overflow in signed 8-bit
    let sum_s = a.0 as i16 + b.0 as i16 + c as i16;
    let ov = sum_s < i8::MIN as i16 || sum_s > i8::MAX as i16;

    (Reg8(sum as u8), (sum >> 8) != 0, ov, ac)
}

#[inline(always)]
fn sub_with_borrow(a: Reg8, b: Reg8, c: bool) -> (Reg8, bool) {
    let sum = a.to_u16().wrapping_sub(b.to_u16()).wrapping_sub(c as u16);
    (Reg8(sum as u8), (sum >> 8) != 0)
}

#[inline(always)]
fn mul(a: Reg8, b: Reg8) -> (Reg8, Reg8, bool, bool) {
    let mul = a.0 as u16 * b.0 as u16;
    (Reg8(mul as u8), Reg8((mul >> 8) as u8), false, mul > 0xFF)
}

#[inline(always)]
fn div(a: Reg8, b: Reg8) -> (Reg8, Reg8, bool, bool) {
    if b == 0 {
        return (Reg8(0), Reg8(0), false, true);
    }
    (Reg8(a.0 / b.0), Reg8(a.0 % b.0), false, false)
}

#[inline(always)]
fn rlc(a: Reg8, c: bool) -> (Reg8, bool) {
    let new_carry = (a.0 & 0x80) != 0;
    (Reg8(a.0 << 1) | (c as u8), new_carry)
}

#[inline(always)]
fn rrc(a: Reg8, c: bool) -> (Reg8, bool) {
    let new_carry = (a.0 & 0x01) != 0;
    (Reg8((a.0 >> 1) | (if c { 0x80 } else { 0x00 })), new_carry)
}

#[inline(always)]
fn rl(a: Reg8) -> Reg8 {
    let b = (a.0 & 0x80) >> 7;
    Reg8((a.0 << 1) | b)
}

#[inline(always)]
fn rr(a: Reg8) -> Reg8 {
    let b = (a.0 & 0x01) << 7;
    Reg8((a.0 >> 1) | b)
}

#[test]
fn op_def_compile_test() {
    #![allow(redundant_semicolons, non_snake_case)]

    let mut cpu = Cpu::new();
    let bit = 0;
    let FUNC = |a| a;

    let ctx = (&mut cpu, &mut ());

    op_def!(ctx {
        BIT[bit] = true;
        PC+=2;
        DPTR=10;
        A=DPTR;
        A=SEXT(A);
        let _tmp = SEXT(A);
        PC=POP16();
        A=FUNC(A);
        PC=(PC+1)+1;
        PC += 2;
        (A, B) = (A, B);
        if C {
            PC += 1;
        } else {
            PC += 2;
        }
    });
}
