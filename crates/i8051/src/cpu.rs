use std::fmt;

use tracing::trace;

use crate::peripheral::{P3_INT0, P3_INT1, SCON_RI, SCON_TI, TCON_TF0, TCON_TF1};
use crate::{
    CpuContext, CpuView, MemoryMapper, Mnemonic, PortMapper, ReadOnlyMemoryMapper, sfr::*,
};

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

impl<CTX: CpuContext> CpuView for (&Cpu, &CTX) {
    #[inline]
    fn read_xdata(&self, addr: u16) -> u8 {
        self.1.xdata().read(self, addr as u32)
    }
    #[inline]
    fn read_code(&self, addr: u32) -> u8 {
        self.1.code().read(self, addr)
    }
    #[inline]
    fn pc(&self) -> u16 {
        self.0.pc
    }
    #[inline]
    fn pc_ext(&self) -> u32 {
        self.0.pc_ext(self.1)
    }
    #[inline]
    fn a(&self) -> u8 {
        self.0.a
    }
    #[inline]
    fn b(&self) -> u8 {
        self.0.b
    }
    #[inline]
    fn dptr(&self) -> u16 {
        self.0.dptr()
    }
    #[inline]
    fn dpl(&self) -> u8 {
        self.0.dpl
    }
    #[inline]
    fn dph(&self) -> u8 {
        self.0.dph
    }
    #[inline]
    fn psw(&self, flag: Flag) -> bool {
        self.0.psw(flag)
    }
    #[inline]
    fn sp(&self) -> u8 {
        self.0.sp
    }
    #[inline]
    fn r(&self, x: u8) -> u8 {
        self.0.r(x)
    }
    #[inline]
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
    Continue(u32),
    /// Call the given PC, eventually returning to the given PC.
    Call(u32, u32),
    /// Choose between two branches.
    Choice(u32, u32),
    /// Diverges, unknown control flow.
    Diverge,
}

pub struct Instruction {
    pc: u32,
    len: u8,
    bytes: [u8; Self::MAX_LENGTH],
}

#[allow(clippy::len_without_is_empty)]
impl Instruction {
    pub const MAX_LENGTH: usize = 3;

    pub fn decode_from_bytes(pc: u32, bytes: &[u8]) -> Self {
        let len = crate::op::decode_length(bytes);
        let mut ins_bytes = [0; Self::MAX_LENGTH];
        for i in 0..len {
            ins_bytes[i as usize] = bytes.get(i as usize).copied().unwrap_or(0);
        }
        Self {
            pc,
            len,
            bytes: ins_bytes,
        }
    }

    pub fn as_string(&self) -> String {
        crate::op::decode_string(&self.bytes, self.pc)
    }

    #[inline(always)]
    pub fn pc(&self) -> u32 {
        self.pc
    }

    #[inline(always)]
    pub fn bytes(&self) -> &[u8] {
        &self.bytes[..self.len as usize]
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.len as usize
    }

    pub fn mnemonic(&self) -> Mnemonic {
        crate::op::opcode(self.bytes[0])
    }

    /// Get the absolute target address of this instruction, if one exists.
    pub fn addr(&self) -> Option<u32> {
        crate::op::addr(&self.bytes, self.pc)
    }

    /// If this instruction references an internal memory address, return it.
    ///
    /// This will miss an instruction that uses two direct operands.
    pub fn direct(&self) -> Option<u8> {
        crate::op::direct(&self.bytes, self.pc)
    }

    pub fn control_flow(&self) -> ControlFlow {
        let pc = self.pc as u16;
        let len = self.len as u16;
        let next = pc.wrapping_add(len) as u32 | (self.pc & !0xffff);
        match self.mnemonic() {
            Mnemonic::LJMP | Mnemonic::AJMP | Mnemonic::SJMP => {
                ControlFlow::Continue(self.addr().unwrap())
            }
            Mnemonic::LCALL | Mnemonic::ACALL => ControlFlow::Call(next, self.addr().unwrap()),
            Mnemonic::JMP | Mnemonic::RET | Mnemonic::RETI => ControlFlow::Diverge,
            _ => {
                if crate::op::has_rel(self.bytes[0]) {
                    // `rel` instructions always have a relative offset in the last byte.
                    ControlFlow::Choice(next, self.addr().unwrap())
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
            for _ in 0..(3 - self.len as usize) {
                write!(f, "   ")?;
            }
            write!(f, "{}", self.as_string())?;
            Ok(())
        } else {
            write!(f, "{}", self.as_string())
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
        crate::op::dispatch(self, ctx);
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
        let (len, bytes) = crate::op::decode_fetch(self, ctx, pc);
        Instruction { pc, len, bytes }
    }

    /// Decode the instruction at the given PC.
    pub fn decode(&self, ctx: &impl CpuContext, pc: u32) -> Instruction {
        let (len, bytes) = crate::op::decode_fetch(self, ctx, pc);
        Instruction { pc, len, bytes }
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

    #[inline]
    pub fn pc_ext(&self, ctx: &impl CpuContext) -> u32 {
        (self.pc as u32) | (ctx.ports().pc_extension(&(self, ctx)) as u32) << 16
    }

    #[inline]
    pub fn pc_ext_addr(&self, ctx: &impl CpuContext, addr: u16) -> u32 {
        (addr as u32) | (ctx.ports().pc_extension(&(self, ctx)) as u32) << 16
    }

    #[inline]
    pub fn internal_ram(&self, addr: u8) -> u8 {
        self.internal_ram[addr as usize]
    }

    #[inline]
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
    pub(crate) fn read(&self, addr: u8, ctx: &impl CpuContext) -> u8 {
        if addr < 128 {
            self.internal_ram[addr as usize]
        } else {
            self.sfr(addr, ctx)
        }
    }

    /// Read a value from internal RAM, matching the semantics of `@Rn` opcodes.
    pub(crate) fn read_indirect(&self, addr: u8) -> u8 {
        self.internal_ram[addr as usize]
    }

    /// Write a value to internal RAM or an SFR, matching the semantics of
    /// `direct`-storing opcodes.
    pub(crate) fn write(&mut self, addr: u8, value: u8, ctx: &mut impl CpuContext) {
        if addr < 128 {
            self.internal_ram[addr as usize] = value;
        } else {
            self.sfr_set(addr, value, ctx);
        }
    }

    /// Write a value to internal RAM, matching the semantics of `@Rn` opcodes.
    pub(crate) fn write_indirect(&mut self, addr: u8, value: u8) {
        self.internal_ram[addr as usize] = value;
    }

    /// Read a bit from the internal RAM or SFR. Bit addresses are 0-127 for
    /// internal RAM (mapped to 0x20-0x2F), 128-255 for SFRs (mapped to 0x80,
    /// 0x90, ... 0xf0).
    pub(crate) fn read_bit(&self, bit_addr: u8, ctx: &impl CpuContext) -> bool {
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
    pub(crate) fn write_bit(&mut self, bit_addr: u8, value: bool, ctx: &mut impl CpuContext) {
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

    pub(crate) fn write_bit_latch(&mut self, bit_addr: u8, value: bool, ctx: &mut impl CpuContext) {
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
