use std::cmp::PartialEq;
use std::fmt;
use std::ops::{
    Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not, Shl, Shr,
    Sub, SubAssign,
};

use bytemuck::TransparentWrapper;
use i8051_proc_macro::op_def;
use type_mapper::map_types;

pub const SFR_BASE: u8 = 0x80;

pub const SFR_P0: u8 = 0x80;
pub const SFR_SP: u8 = 0x81;
pub const SFR_DPL: u8 = 0x82;
pub const SFR_DPH: u8 = 0x83;
pub const SFR_PCON: u8 = 0x87;
pub const SFR_TCON: u8 = 0x88;
pub const SFR_TMOD: u8 = 0x89;
pub const SFR_TL0: u8 = 0x8A;
pub const SFR_TL1: u8 = 0x8B;
pub const SFR_TH0: u8 = 0x8C;
pub const SFR_TH1: u8 = 0x8D;
pub const SFR_P1: u8 = 0x90;
pub const SFR_SCON: u8 = 0x98;
pub const SFR_SBUF: u8 = 0x99;
pub const SFR_P2: u8 = 0xA0;
pub const SFR_IE: u8 = 0xA8;
pub const SFR_P3: u8 = 0xB0;
pub const SFR_IP: u8 = 0xB8;
pub const SFR_T2CON: u8 = 0xC8;
pub const SFR_T2MOD: u8 = 0xC9;
pub const SFR_RCAP2L: u8 = 0xCA;
pub const SFR_RCAP2H: u8 = 0xCB;
pub const SFR_TL2: u8 = 0xCC;
pub const SFR_TH2: u8 = 0xCD;
pub const SFR_PSW: u8 = 0xD0;
pub const SFR_A: u8 = 0xE0;
pub const SFR_B: u8 = 0xF0;

pub const PSW_C: u8 = 0x00;
pub const PSW_AC: u8 = 0x01;
pub const PSW_Z: u8 = 0x02;
pub const PSW_RS0: u8 = 0x03;
pub const PSW_RS1: u8 = 0x04;
pub const PSW_OV: u8 = 0x06;
pub const PSW_P: u8 = 0x07;

pub trait MemoryMapper {
    fn read(&self, addr: u16) -> u8;
    fn write(&mut self, addr: u16, value: u8);
}

impl MemoryMapper for () {
    fn read(&self, addr: u16) -> u8 {
        0
    }
    fn write(&mut self, addr: u16, value: u8) {}
}

pub trait PortMapper {
    fn read(&self, addr: u8) -> u8;
    fn write(&mut self, addr: u8, value: u8);
}

impl PortMapper for () {
    fn read(&self, addr: u8) -> u8 {
        0
    }
    fn write(&mut self, addr: u8, value: u8) {}
}

trait U16Equivalent: Copy + Clone {
    fn to_u16(self) -> u16;
    fn from_u16(value: u16) -> Self;
    fn to_u8(self) -> Reg8 {
        Reg8(self.to_u16() as u8)
    }
    fn sext(self) -> RegI16 {
        RegI16(self.to_u16() as i8 as i16)
    }
}

enum Direct {
    RAM(u8),
    SFR(u8),
}

impl From<u8> for Direct {
    fn from(value: u8) -> Self {
        if (value < 128) {
            Self::RAM(value)
        } else {
            Self::SFR(value)
        }
    }
}

impl fmt::Display for Direct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::RAM(value @ 0..31) => write!(f, "R{}.BANK{}", value % 8, value / 8),
            Self::RAM(value) => write!(f, "RAM({})", value),
            Self::SFR(SFR_P0) => write!(f, "P0"),
            Self::SFR(SFR_A) => write!(f, "A"),
            Self::SFR(SFR_B) => write!(f, "B"),
            Self::SFR(SFR_PSW) => write!(f, "PSW"),
            Self::SFR(SFR_SP) => write!(f, "SP"),
            Self::SFR(SFR_DPL) => write!(f, "DPL"),
            Self::SFR(SFR_DPH) => write!(f, "DPH"),
            Self::SFR(SFR_PCON) => write!(f, "PCON"),
            Self::SFR(SFR_TCON) => write!(f, "TCON"),
            Self::SFR(SFR_TMOD) => write!(f, "TMOD"),
            Self::SFR(SFR_TL0) => write!(f, "TL0"),
            Self::SFR(SFR_TL1) => write!(f, "TL1"),
            Self::SFR(SFR_TH0) => write!(f, "TH0"),
            Self::SFR(SFR_TH1) => write!(f, "TH1"),
            Self::SFR(SFR_P1) => write!(f, "P1"),
            Self::SFR(SFR_SCON) => write!(f, "SCON"),
            Self::SFR(SFR_SBUF) => write!(f, "SBUF"),
            Self::SFR(SFR_P2) => write!(f, "P2"),
            Self::SFR(SFR_IE) => write!(f, "IE"),
            Self::SFR(SFR_P3) => write!(f, "P3"),
            Self::SFR(SFR_IP) => write!(f, "IP"),
            Self::SFR(SFR_T2CON) => write!(f, "T2CON"),
            Self::SFR(SFR_T2MOD) => write!(f, "T2MOD"),
            Self::SFR(SFR_RCAP2L) => write!(f, "RCAP2L"),
            Self::SFR(SFR_RCAP2H) => write!(f, "RCAP2H"),
            Self::SFR(SFR_TL2) => write!(f, "TL2"),
            Self::SFR(SFR_TH2) => write!(f, "TH2"),
            Self::SFR(sfr) => write!(f, "SFR({})", sfr),
        }
    }
}

enum Bit {
    RAM(u8),
    SFR(u8),
}

impl From<u8> for Bit {
    fn from(value: u8) -> Self {
        if (value < 128) {
            Self::RAM(value)
        } else {
            Self::SFR(value)
        }
    }
}

impl fmt::Display for Bit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::RAM(value) => write!(f, "RAM(#{:02X}).{}", (value >> 3) + 0x20, value & 0x07),
            Self::SFR(value) => {
                Direct::from(value & 0xF8).fmt(f)?;
                write!(f, ".{}", value & 0x07)
            }
        }
    }
}

#[derive(Copy, Clone, TransparentWrapper)]
#[repr(transparent)]
struct Reg8(u8);

impl PartialOrd for Reg8 {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.0.cmp(&other.0))
    }
}

impl Ord for Reg8 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl From<u8> for Reg8 {
    fn from(value: u8) -> Self {
        Self(value)
    }
}

impl U16Equivalent for Reg8 {
    fn to_u16(self) -> u16 {
        self.0 as u16
    }
    fn from_u16(value: u16) -> Self {
        Self(value as u8)
    }
}

#[derive(Copy, Clone, TransparentWrapper)]
#[repr(transparent)]
struct RegI16(i16);

impl U16Equivalent for RegI16 {
    fn to_u16(self) -> u16 {
        self.0 as u16
    }
    fn from_u16(value: u16) -> Self {
        Self(value as i16)
    }
}

#[derive(Copy, Clone, TransparentWrapper)]
#[repr(transparent)]
struct Reg16(u16);

impl U16Equivalent for Reg16 {
    fn to_u16(self) -> u16 {
        self.0
    }
    fn from_u16(value: u16) -> Self {
        Self(value)
    }
}

impl U16Equivalent for u16 {
    fn to_u16(self) -> u16 {
        self
    }
    fn from_u16(value: u16) -> Self {
        value
    }
}

impl U16Equivalent for u8 {
    fn to_u16(self) -> u16 {
        self as u16
    }
    fn from_u16(value: u16) -> Self {
        value as u8
    }
}

impl U16Equivalent for i32 {
    fn to_u16(self) -> u16 {
        self as u16
    }
    fn from_u16(value: u16) -> Self {
        value as i32
    }
}

macro_rules! derive_ops {
    ($type:ident) => {
        impl<T> Add<T> for $type
        where
            T: U16Equivalent,
        {
            type Output = $type;
            fn add(self, other: T) -> Self::Output {
                $type::from_u16(self.to_u16().wrapping_add(other.to_u16()))
            }
        }
        impl<T> AddAssign<T> for $type
        where
            T: U16Equivalent,
        {
            fn add_assign(&mut self, other: T) {
                *self = $type::from_u16(self.to_u16().wrapping_add(other.to_u16()))
            }
        }
        impl<T> Sub<T> for $type
        where
            T: U16Equivalent,
        {
            type Output = $type;
            fn sub(self, other: T) -> Self::Output {
                $type::from_u16(self.to_u16().wrapping_sub(other.to_u16()))
            }
        }
        impl<T> SubAssign<T> for $type
        where
            T: U16Equivalent,
        {
            fn sub_assign(&mut self, other: T) {
                *self = $type::from_u16(self.to_u16().wrapping_sub(other.to_u16()))
            }
        }
        impl<T> Shl<T> for $type
        where
            T: U16Equivalent,
        {
            type Output = Reg16;
            fn shl(self, other: T) -> Self::Output {
                Reg16(self.to_u16() << other.to_u16())
            }
        }
        impl<T> Shr<T> for $type
        where
            T: U16Equivalent,
        {
            type Output = $type;
            fn shr(self, other: T) -> Self::Output {
                $type::from_u16(self.to_u16() >> other.to_u16())
            }
        }
        impl<T> BitAnd<T> for $type
        where
            T: U16Equivalent,
        {
            type Output = $type;
            fn bitand(self, other: T) -> Self::Output {
                $type::from_u16(self.to_u16() & other.to_u16())
            }
        }
        impl<T> BitAndAssign<T> for $type
        where
            T: U16Equivalent,
        {
            fn bitand_assign(&mut self, other: T) {
                *self = $type::from_u16(self.to_u16() & other.to_u16())
            }
        }
        impl<T> BitOr<T> for $type
        where
            T: U16Equivalent,
        {
            type Output = $type;
            fn bitor(self, other: T) -> Self::Output {
                $type::from_u16(self.to_u16() | other.to_u16())
            }
        }
        impl<T> BitOrAssign<T> for $type
        where
            T: U16Equivalent,
        {
            fn bitor_assign(&mut self, other: T) {
                *self = $type::from_u16(self.to_u16() | other.to_u16())
            }
        }
        impl<T> BitXor<T> for $type
        where
            T: U16Equivalent,
        {
            type Output = $type;
            fn bitxor(self, other: T) -> Self::Output {
                $type::from_u16(self.to_u16() ^ other.to_u16())
            }
        }
        impl<T> BitXorAssign<T> for $type
        where
            T: U16Equivalent,
        {
            fn bitxor_assign(&mut self, other: T) {
                *self = $type::from_u16(self.to_u16() ^ other.to_u16())
            }
        }
        impl<T> PartialEq<T> for $type
        where
            T: U16Equivalent,
        {
            fn eq(&self, other: &T) -> bool {
                self.to_u16() == other.to_u16()
            }
        }
        impl Eq for $type {}
        impl Not for $type {
            type Output = $type;
            fn not(self) -> Self::Output {
                $type::from_u16(!self.to_u16())
            }
        }
        impl fmt::Display for $type {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.fmt(f)
            }
        }
        impl fmt::UpperHex for $type {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.fmt(f)
            }
        }
    };
}

derive_ops!(Reg8);
derive_ops!(Reg16);
derive_ops!(RegI16);

struct ExternalMemory<'a, T>
where
    T: MemoryMapper,
{
    memory: &'a mut T,
}

impl<T> ExternalMemory<'_, T>
where
    T: MemoryMapper,
{
    #[inline(always)]
    fn read(&self, addr: impl U16Equivalent) -> Reg8 {
        Reg8(self.memory.read(addr.to_u16()))
    }
    #[inline(always)]
    fn write(&mut self, addr: impl U16Equivalent, value: impl U16Equivalent) {
        self.memory.write(addr.to_u16(), value.to_u16() as u8)
    }
}

struct Context<'a, XDATA: MemoryMapper, CODE: MemoryMapper, PORTS: PortMapper> {
    pub cpu: &'a mut Cpu,
    pub xdata: ExternalMemory<'a, XDATA>,
    pub code: ExternalMemory<'a, CODE>,
    pub ports: &'a mut PORTS,
}

pub struct Cpu {
    pub pc: u16,
    pub internal_ram: [u8; 256],
    pub sfr: [u8; 128],
    pub ports: [u8; 4],
    pub interrupt: bool,
}

impl Cpu {
    pub fn new() -> Self {
        let mut cpu = Self {
            pc: 0x0000,
            internal_ram: [0; 256],
            sfr: [0; 128],
            interrupt: false,
            ports: [0; 4],
        };
        cpu.sfr_set(SFR_SP, 7);
        cpu
    }

    pub fn a(&self) -> u8 {
        self.sfr(SFR_A)
    }

    pub fn b(&self) -> u8 {
        self.sfr(SFR_B)
    }

    pub fn dptr(&self) -> u16 {
        (((self.sfr(SFR_DPH) as u16) << 8) as u16) | self.sfr(SFR_DPL) as u16
    }

    pub fn dptr_set(&mut self, value: u16) {
        *self.sfr_mut(SFR_DPH) = (value >> 8) as u8;
        *self.sfr_mut(SFR_DPL) = (value & 0xFF) as u8;
    }

    pub fn r(&self, x: u8) -> u8 {
        let rs0 = (self.sfr(SFR_PSW) & (1 << PSW_RS0)) != 0;
        let rs1 = (self.sfr(SFR_PSW) & (1 << PSW_RS1)) != 0;
        let offset = ((rs0 as u8 | ((rs1 as u8) << 1)) * 8) as usize;
        self.internal_ram[x as usize + offset]
    }

    pub fn r_mut(&mut self, x: u8) -> &mut u8 {
        let rs0 = (self.sfr(SFR_PSW) & (1 << PSW_RS0)) != 0;
        let rs1 = (self.sfr(SFR_PSW) & (1 << PSW_RS1)) != 0;
        let offset = ((rs0 as u8 | ((rs1 as u8) << 1)) * 8) as usize;
        &mut self.internal_ram[x as usize + offset]
    }

    pub fn sfr(&self, addr: u8) -> u8 {
        self.sfr[addr.wrapping_sub(SFR_BASE) as usize]
    }

    pub fn sfr_mut(&mut self, addr: u8) -> &mut u8 {
        debug_assert!(addr != SFR_P0 && addr != SFR_P1 && addr != SFR_P2 && addr != SFR_P3);
        &mut self.sfr[addr.wrapping_sub(SFR_BASE) as usize]
    }

    pub fn sfr_set(&mut self, addr: u8, value: u8) {
        if addr == SFR_A {
            self.psw_set(PSW_Z, value == 0);
        }
        if addr == SFR_P0 || addr == SFR_P1 || addr == SFR_P2 || addr == SFR_P3 {
            println!("PORT write {:02X} = {:02X}", addr, value);
        }
        if addr == SFR_SBUF {
            println!("PORT SBUF write {:02X}", value);
            return;
        }
        self.sfr[addr.wrapping_sub(SFR_BASE) as usize] = value;
    }

    pub fn psw(&self, flag: u8) -> bool {
        self.sfr(SFR_PSW) & (1 << flag) != 0
    }

    pub fn psw_set(&mut self, flag: u8, value: bool) {
        if value {
            *self.sfr_mut(SFR_PSW) |= 1 << flag;
        } else {
            *self.sfr_mut(SFR_PSW) &= !(1 << flag);
        }
    }

    pub fn push_stack(&mut self, value: u8) {
        *self.sfr_mut(SFR_SP) = self.sfr(SFR_SP).wrapping_add(1);
        self.internal_ram[self.sfr(SFR_SP) as usize] = value;
    }

    pub fn push_stack16(&mut self, value: u16) {
        self.push_stack((value & 0xFF) as u8);
        self.push_stack((value >> 8) as u8);
    }

    pub fn pop_stack(&mut self) -> u8 {
        let value = self.internal_ram[self.sfr(SFR_SP) as usize];
        *self.sfr_mut(SFR_SP) = self.sfr(SFR_SP).wrapping_sub(1);
        value
    }

    pub fn pop_stack16(&mut self) -> u16 {
        let a = self.pop_stack();
        let b = self.pop_stack();
        ((a as u16) << 8) as u16 | b as u16
    }

    fn read(&self, addr: u8) -> u8 {
        if addr < 128 {
            self.internal_ram[addr as usize]
        } else {
            self.sfr(addr)
        }
    }

    fn read_indirect(&self, addr: u8) -> u8 {
        self.internal_ram[addr as usize]
    }

    fn write(&mut self, addr: u8, value: u8) {
        if addr < 128 {
            self.internal_ram[addr as usize] = value;
        } else {
            self.sfr_set(addr, value);
        }
    }

    fn write_indirect(&mut self, addr: u8, value: u8) {
        self.internal_ram[addr as usize] = value;
    }

    /// Read a bit from the internal RAM or SFR. Bit addresses are 0-127 for
    /// internal RAM (mapped to 0x20-0x2F), 128-255 for SFRs (mapped to 0x80,
    /// 0x90, ... 0xf0).
    fn read_bit(&self, bit_addr: u8) -> bool {
        let bit_pos = bit_addr & 0x07;
        if bit_addr < 0x80 {
            let byte_index = 0x20 + (bit_addr >> 3);
            self.internal_ram[byte_index as usize] & (1 << bit_pos) != 0
        } else {
            let sfr_addr = bit_addr & 0xF8; // base byte of SFR
            self.sfr(sfr_addr) & (1 << bit_pos) != 0
        }
    }

    /// Write a bit to the internal RAM or SFR. Bit addresses are 0-127 for
    /// internal RAM (mapped to 0x20-0x2F), 128-255 for SFRs (mapped to 0x80,
    /// 0x90, ... 0xf0).
    fn write_bit(&mut self, bit_addr: u8, value: bool) {
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
            let byte = self.sfr(sfr_addr);
            let byte = if value {
                byte | 1 << bit_pos
            } else {
                byte & !(1 << bit_pos)
            };
            self.sfr_set(sfr_addr, byte);
        }
    }
}

// Helper macros for the proc macro to use
macro_rules! op_def_read {
    ($ctx:ident, A) => {
        Reg8($ctx.cpu.sfr(SFR_A))
    };
    ($ctx:ident, B) => {
        Reg8($ctx.cpu.sfr(SFR_B))
    };
    ($ctx:ident, PC) => {
        Reg16($ctx.cpu.pc)
    };
    ($ctx:ident, DPTR) => {{ Reg16((($ctx.cpu.sfr(SFR_DPH) as u16) << 8) | $ctx.cpu.sfr(SFR_DPL) as u16) }};
    ($ctx:ident, C) => {
        $ctx.cpu.psw(PSW_C)
    };
    ($ctx:ident, OV) => {
        $ctx.cpu.psw(PSW_OV)
    };
    ($ctx:ident, AC) => {
        $ctx.cpu.psw(PSW_AC)
    };
    ($ctx:ident, Z) => {
        $ctx.cpu.psw(PSW_Z)
    };
    ($ctx:ident, BIT, $index:expr) => {
        $ctx.cpu.read_bit($index.to_u8().0)
    };
    ($ctx:ident, PBIT, $index:expr) => {{
        let port = $index.to_u8().0;
        let mut bit = $ctx.cpu.read_bit(port);
        if bit {
            let sfr = port & 0xF8;
            let bit_pos = $index.to_u8().0 & 0x07;
            if sfr == SFR_P0 || sfr == SFR_P1 || sfr == SFR_P2 || sfr == SFR_P3 {
                println!("PORT read bit {:02X}.{}", sfr, bit_pos);
            }
            bit = match sfr {
                SFR_P0 => $ctx.cpu.ports[0] & (1 << bit_pos) != 0,
                SFR_P1 => $ctx.cpu.ports[1] & (1 << bit_pos) != 0,
                SFR_P2 => $ctx.cpu.ports[2] & (1 << bit_pos) != 0,
                SFR_P3 => $ctx.cpu.ports[3] & (1 << bit_pos) != 0,
                _ => bit,
            };
        }
        bit
    }};
    ($ctx:ident, DATA, $index:expr) => {
        Reg8($ctx.cpu.read($index.to_u8().0))
    };
    ($ctx:ident, PDATA, $index:expr) => {{
        let port = $index.to_u8().0;
        let value = $ctx.cpu.read(port);
        if port == SFR_P0 || port == SFR_P1 || port == SFR_P2 || port == SFR_P3 {
            println!("PORT read {:02X}", port);
        }

        Reg8(match port {
            SFR_P0 => $ctx.cpu.ports[0] & value,
            SFR_P1 => $ctx.cpu.ports[1] & value,
            SFR_P2 => $ctx.cpu.ports[2] & value,
            SFR_P3 => $ctx.cpu.ports[3] & value,
            _ => value,
        })
    }};
    ($ctx:ident, IDATA, $index:expr) => {
        Reg8($ctx.cpu.read_indirect($index.to_u8().0))
    };
    ($ctx:ident, R, $index:expr) => {
        Reg8($ctx.cpu.r($index.to_u8().0))
    };
    ($ctx:ident, CODE, $index:expr) => {
        $ctx.code.read($index.to_u8().0)
    };
    ($ctx:ident, XDATA, $index:expr) => {
        $ctx.xdata.read($index.to_u8().0)
    };
}

macro_rules! op_def_write {
    ($ctx:ident, A, $value:expr) => {{
        $ctx.cpu.sfr_set(SFR_A, $value.to_u16() as u8);
    }};
    ($ctx:ident, B, $value:expr) => {{
        *$ctx.cpu.sfr_mut(SFR_B) = $value.to_u16() as u8;
    }};
    ($ctx:ident, PC, $value:expr) => {{
        $ctx.cpu.pc = $value.to_u16();
    }};
    ($ctx:ident, DPTR, $value:expr) => {{
        $ctx.cpu.dptr_set($value.to_u16());
    }};
    ($ctx:ident, C, $value:expr) => {{
        $ctx.cpu.psw_set(PSW_C, $value);
    }};
    ($ctx:ident, OV, $value:expr) => {{
        $ctx.cpu.psw_set(PSW_OV, $value);
    }};
    ($ctx:ident, AC, $value:expr) => {{
        $ctx.cpu.psw_set(PSW_AC, $value);
    }};
    ($ctx:ident, Z, $value:expr) => {{
        $ctx.cpu.psw_set(PSW_Z, $value);
    }};
    ($ctx:ident, BIT, $index:expr, $value:expr) => {
        $ctx.cpu.write_bit($index.to_u8().0, $value)
    };
    ($ctx:ident, PBIT, $index:expr, $value:expr) => {
        $ctx.cpu.write_bit($index.to_u8().0, $value)
    };
    ($ctx:ident, DATA, $index:expr, $value:expr) => {
        $ctx.cpu.write($index.to_u8().0, $value.to_u8().0)
    };
    ($ctx:ident, PDATA, $index:expr, $value:expr) => {
        $ctx.cpu.write($index.to_u8().0, $value.to_u8().0)
    };
    ($ctx:ident, IDATA, $index:expr, $value:expr) => {
        $ctx.cpu.write_indirect($index.to_u8().0, $value.to_u8().0)
    };
    ($ctx:ident, R, $index:expr, $value:expr) => {
        *$ctx.cpu.r_mut($index.to_u8().0) = $value.to_u8().0;
    };
    ($ctx:ident, CODE, $index:expr, $value:expr) => {
        "CODE cannot be written"
    };
    ($ctx:ident, XDATA, $index:expr, $value:expr) => {
        $ctx.xdata.write($index.to_u8().0, $value.to_u8().0)
    };
}

macro_rules! op_def_call {
    ($ctx:ident, POP()) => {
        Reg8($ctx.cpu.pop_stack())
    };
    ($ctx:ident, POP16()) => {
        Reg16($ctx.cpu.pop_stack16())
    };
    ($ctx:ident, PUSH($value:expr)) => {
        $ctx.cpu.push_stack(U16Equivalent::to_u16($value).to_u8().0)
    };
    ($ctx:ident, PUSH16($value:expr)) => {
        $ctx.cpu.push_stack16(U16Equivalent::to_u16($value))
    };
    ($ctx:ident, CLEAR_INT()) => {
        $ctx.cpu.interrupt = false
    };
    ($ctx:ident, SEXT($value:expr)) => {
        U16Equivalent::sext($value)
    };
}

macro_rules! op {
    (
        $XDATA:ident, $CODE:ident,
        $(
            OP $name:literal $start:literal $(- $mask:ident $mask_pattern:literal)? $($arg:ident)*  $(, $arg_mask:ident = $arg_mask_expr:tt)? => $stmt:tt ;
        )*
    ) => {
        pub fn decode(code: &mut impl MemoryMapper, pc: u16) -> (Vec<u8>,String) {
            #![allow(unused)]
            #![allow(non_snake_case)]

            let op = Reg8(code.read(pc));
            let mut bytes = vec![op.0];

            $(
                if (op $(& !$mask_pattern)? == $start) {
                    $(let $mask = op & $mask_pattern;)?
                    let mut next_read = pc + 1;
                    $(
                        let b = code.read(next_read);
                        let $arg: map_types!(match $arg {
                            direct => Direct,
                            bit => Bit,
                            dst => Direct,
                            src => Direct,
                            _ => Reg8,
                        }) = b.into();
                        next_read += 1;
                        bytes.push(b);
                    )*
                    $(op_def!(__no_cpu {let $arg_mask = $arg_mask_expr;});)?
                    (bytes, format!($name))
                } else
            )*
            {
                // Unimplemented instruction
                (bytes, "???".to_string())
            }
        }

        pub fn dispatch(cpu: &mut Cpu, xdata: &mut impl MemoryMapper, code: &mut impl MemoryMapper) {
            #![allow(non_snake_case)]
            #![allow(redundant_semicolons)]
            #![allow(unused_assignments)]

            let $XDATA = ExternalMemory { memory: xdata };
            let $CODE = ExternalMemory { memory: code };

            let mut ctx = Context {
                cpu,
                xdata: $XDATA,
                code: $CODE,
                ports: &mut (),
            };

            let op = ctx.code.read(ctx.cpu.pc);

            $(
                if (op.0 $(& !$mask_pattern)? == $start) {
                    $(let $mask = op & $mask_pattern;)?
                    #[allow(unused)]
                    let mut next_read = ctx.cpu.pc + 1;
                    $(
                        let $arg = ctx.code.read(next_read);
                        next_read += 1;
                    )*
                    $(op_def!(__no_cpu {let $arg_mask = $arg_mask_expr;});)?
                    op_def!(ctx $stmt);
                } else
            )*
            {
                // Unimplemented instruction
                cpu.pc += 1;
            }

        }
    };
}

op! {
    XDATA, CODE,
    OP "AJMP #{imm11:X}" 0b00000001 - mask 0b11100000 imm8, imm11 = (mask<<3|imm8) => { PC=(PC&0xF800)|imm11; };
    OP "ACALL #{imm11:X}" 0b00010001 - mask 0b11100000 imm8, imm11 = (mask<<3|imm8) => { PUSH16(PC+2); PC=(PC&0xF800)|imm11; };

    // Control flow
    OP "NOP" 0b00000000 => {PC+=1};
    OP "LJMP #{addr16:04X}" 0b00000010 imm_hi imm_lo, addr16 = (imm_hi<<8|imm_lo) => {PC=addr16};
    OP "LCALL #{addr16:04X}" 0b00010010 imm_hi imm_lo, addr16 = (imm_hi<<8|imm_lo) => {PUSH16(PC+3); PC=addr16};
    OP "JC {rel:+}" 0b01000000 rel, rel=(SEXT(rel)) => {if (C) {PC=PC+2+rel} else {PC+=2}};
    OP "JNC {rel:+}" 0b01010000 rel, rel=(SEXT(rel)) => {if (!C) {PC=PC+2+rel} else {PC+=2}};
    OP "JZ {rel:+}" 0b01100000 rel, rel=(SEXT(rel)) => {if (A==0) {PC=PC+2+rel} else {PC+=2}};
    OP "JNZ {rel:+}" 0b01110000 rel, rel=(SEXT(rel)) => {if (A!=0) {PC=PC+2+rel} else {PC+=2}};
    OP "SJMP {rel:+}" 0b10000000 rel, rel=(SEXT(rel)) => {PC=PC+2+rel};
    OP "RET" 0b00100010 => {PC=POP16()};
    OP "RETI" 0b00110010 => {PC=POP16(); CLEAR_INT()};
    OP "JMP @A+DPTR"       0b01110011 => {PC=DPTR+A};
    OP "JB {bit},{rel:+}"  0b00100000 bit rel, rel=(SEXT(rel)) => {if (PBIT[bit]) {PC=PC+3+rel} else {PC+=3}};
    OP "JNB {bit},{rel:+}" 0b00110000 bit rel, rel=(SEXT(rel)) => {if (!PBIT[bit]) {PC=PC+3+rel} else {PC+=3}};
    OP "JBC {bit},{rel:+}" 0b00010000 bit rel, rel=(SEXT(rel)) => {if (PBIT[bit]) {BIT[bit]=false; PC=PC+3+rel} else {PC+=3}};

    // Conditional branches and compare/jump
    OP "DJNZ {direct},{rel:+}" 0b11010101 direct rel, rel=(SEXT(rel)) => {DATA[direct]-=1; if (DATA[direct]!=0) {PC=PC+3+rel} else {PC+=3}};
    OP "DJNZ R{x},{rel:+}" 0b11011000 -x 0b111 rel, rel=(SEXT(rel)) => {R[x]-=1; if (R[x]!=0) {PC=PC+2+rel} else {PC+=2}};
    OP "CJNE A,{direct},{rel:+}" 0b10110101 direct rel, rel=(SEXT(rel)) => {let tmp=DATA[direct]; C=A<tmp; if (A!=tmp) {PC=PC+3+rel} else {PC+=3}};
    OP "CJNE @R{x},#{imm8:02X},{rel:+}" 0b10110110 -x 0b1 imm8 rel, rel=(SEXT(rel)) => {C=IDATA[R[x]]<imm8; if (IDATA[R[x]]!=imm8) {PC=PC+3+rel} else {PC+=3}};
    OP "CJNE R{x},#{imm8:02X},{rel:+}" 0b10111000 -x 0b111 imm8 rel, rel=(SEXT(rel)) => {C=R[x]<imm8; if (R[x]!=imm8) {PC=PC+3+rel} else {PC+=3}};
    OP "CJNE A,#{imm8:02X},{rel:+}" 0b10110100 imm8 rel, rel=(SEXT(rel)) => {C=A<imm8; if (A!=imm8) { PC=PC+3+rel } else { PC+=3 }};

    // DPTR / MOVX / MOVC
    OP "MOV DPTR,#{imm16:04X}" 0b10010000 imm_hi imm_lo, imm16 = (imm_hi<<8|imm_lo) => {DPTR=imm16; PC+=3};
    OP "INC DPTR" 0b10100011 => {DPTR+=1; PC+=1};
    OP "MOVX @DPTR,A" 0b11110000 => {XDATA[DPTR]=A; PC+=1};
    OP "MOVX A,@DPTR" 0b11100000 => {A=XDATA[DPTR]; PC+=1};
    OP "MOVX A,@R{x}" 0b11100010 -x 0b1 => {A=XDATA[R[x]]; PC+=1};
    OP "MOVX @R{x},A" 0b11110010 -x 0b1 => {XDATA[R[x]]=A; PC+=1};
    OP "MOVC A,@A+DPTR" 0b10010011 => {A=CODE[DPTR+A]; PC+=1};
    OP "MOVC A,@A+PC" 0b10000011 => {A=CODE[PC+1+A]; PC+=1};

    // Accumulator and arithmetic
    OP "CLR A" 0b11100100 => {A=0; PC+=1};

    OP "INC A" 0b00000100 => {A=A+1; PC+=1};
    OP "INC {direct}" 0b00000101 direct => {DATA[direct]+=1; PC+=2};
    OP "INC @R{x}" 0b00000110 -x 0b1 => {IDATA[R[x]]+=1; PC+=1};
    OP "INC R{x}" 0b00001000 -x 0b111 => {R[x]+=1; PC+=1};

    OP "DEC A" 0b00010100 => {A=A-1; PC+=1};
    OP "DEC {direct}" 0b00010101 direct => {DATA[direct]-=1; PC+=2};
    OP "DEC @R{x}" 0b00010110 -x 0b1 => {IDATA[R[x]]-=1; PC+=1};
    OP "DEC R{x}" 0b00011000 -x 0b111 => {R[x]-=1; PC+=1};

    OP "CPL A" 0b11110100 => {A=!A; PC+=1};

    OP "MUL AB" 0b10100100 => {(A, B, C, OV) = mul(A, B); PC+=1};
    OP "DIV AB" 0b10000100 => {(A, B, C, OV) = div(A, B); PC+=1};

    OP "ADD A,#{imm8:02X}" 0b00100100 imm8 => {(A, C, OV, AC) = add_with_carry(A, imm8, false); PC+=2};
    OP "ADD A,{direct}" 0b00100101 direct => {(A, C, OV, AC) = add_with_carry(A, DATA[direct], false); PC+=2};
    OP "ADD A,@R{x}" 0b00100110 -x 0b1 => {(A, C, OV, AC) = add_with_carry(A, IDATA[R[x]], false); PC+=1};
    OP "ADD A,R{x}" 0b00101000 -x 0b111 => {(A, C, OV, AC) = add_with_carry(A, R[x], false); PC+=1};

    OP "ADDC A,#{imm8:02X}" 0b00110100 imm8 => {(A, C, OV, AC) = add_with_carry(A, imm8, C); PC+=2};
    OP "ADDC A,{direct}" 0b00110101 direct => {(A, C, OV, AC) = add_with_carry(A, DATA[direct], C); PC+=2};
    OP "ADDC A,@R{x}" 0b00110110 -x 0b1 => {(A, C, OV, AC) = add_with_carry(A, IDATA[R[x]], C); PC+=1};
    OP "ADDC A,R{x}" 0b00111000 -x 0b111 => {(A, C, OV, AC) = add_with_carry(A, R[x], C); PC+=1};

    OP "SUBB A,#{imm8:02X}" 0b10010100 imm8 => {(A, C) = sub_with_borrow(A, imm8, C); PC+=2};
    OP "SUBB A,{direct}" 0b10010101 direct => {(A, C) = sub_with_borrow(A, DATA[direct], C); PC+=2};
    OP "SUBB A,@R{x}" 0b10010110 -x 0b1 => {(A, C) = sub_with_borrow(A, IDATA[R[x]], C); PC+=1};
    OP "SUBB A,R{x}" 0b10011000 -x 0b111 => {(A, C) = sub_with_borrow(A, R[x], C); PC+=1};

    OP "RLC A" 0b00110011 => { (A, C) = rlc(A, C); PC+=1 };
    OP "RRC A" 0b00010011 => { (A, C) = rrc(A, C); PC+=1 };
    OP "RL A" 0b00100011 => { A = rl(A); PC+=1 };
    OP "RR A" 0b00000011 => { A = rr(A); PC+=1 };

    OP "ANL A,#{imm8:02X}"    0b01010100 imm8 => {A=A&imm8; PC+=2};
    OP "ANL A,{direct}"       0b01010101 direct => {A=A&DATA[direct]; PC+=2};
    OP "ANL A,@R{x}"          0b01010110 -x 0b1 => {A=A&IDATA[R[x]]; PC+=1};
    OP "ANL A,R{x}"           0b01011000 -x 0b111 => {A=A&R[x]; PC+=1};
    OP "ANL {direct},A"       0b01010010 direct => {DATA[direct]&=A; PC+=2};
    OP "ANL {direct},#{imm8}" 0b01010011 direct imm8 => {DATA[direct]&=imm8; PC+=3};

    OP "ORL A,#{imm8:02X}"    0b01000100 imm8 => {A=A|imm8; PC+=2};
    OP "ORL A,{direct}"       0b01000101 direct => {A=A|DATA[direct]; PC+=2};
    OP "ORL A,@R{x}"          0b01000110 -x 0b1 => {A=A|IDATA[R[x]]; PC+=1};
    OP "ORL A,R{x}"           0b01001000 -x 0b111 => {A=A|R[x]; PC+=1};
    OP "ORL {direct},A"       0b01000010 direct => {DATA[direct]|=A; PC+=2};
    OP "ORL {direct},#{imm8}" 0b01000011 direct imm8 => {DATA[direct]|=imm8; PC+=3};

    OP "XRL A,#{imm8:02X}"    0b01100100 imm8 => {A=A^imm8; PC+=2};
    OP "XRL A,{direct}"       0b01100101 direct => {A=A^DATA[direct]; PC+=2};
    OP "XRL A,@R{x}"          0b01100110 -x 0b1 => {let tmp=IDATA[R[x]]; A=A^tmp; PC+=1};
    OP "XRL A,R{x}"           0b01101000 -x 0b111 => {A=A^R[x]; PC+=1};
    OP "XRL {direct},A"       0b01100010 direct => {DATA[direct]^=A; PC+=2};
    OP "XRL {direct},#{imm8}" 0b01100011 direct imm8 => {DATA[direct]^=imm8; PC+=3};

    OP "DA A" 0b11010100 => {(A,C,AC)=decimal_adjust(A,C,AC); PC+=1};

    // MOV families
    OP "MOV A,#{imm8:02X}"         0b01110100 imm8 => {A=imm8; PC+=2};
    OP "MOV R{x},#{imm8:02X}"      0b01111000 -x 0b111 imm8 => {R[x]=imm8; PC+=2};
    OP "MOV @R{x},A"               0b11110110 -x 0b1 => {IDATA[R[x]]=A; PC+=1};
    OP "MOV A,@R{x}"               0b11100110 -x 0b1 => {A=IDATA[R[x]]; PC+=1};
    OP "MOV R{x},A"                0b11111000 -x 0b111 => {R[x]=A; PC+=1};
    OP "MOV A,R{x}"                0b11101000 -x 0b111 => {A=R[x]; PC+=1};
    OP "MOV {direct},A"            0b11110101 direct => {DATA[direct]=A; PC+=2};
    OP "MOV {direct},#{imm8:02X}"  0b01110101 direct imm8 => { DATA[direct]=imm8; PC+=3 };
    OP "MOV @R{x},#{imm8:02X}"     0b01110110 -x 0b1 imm8 => { IDATA[R[x]]=imm8; PC+=2 };
    OP "MOV {dst},{src}"           0b10000101 src dst => { DATA[dst]=DATA[src]; PC+=3 };
    OP "MOV {direct},@R{x}"        0b10000110 -x 0b1 direct => { DATA[direct]=IDATA[R[x]]; PC+=2 };
    OP "MOV {direct},R{x}"         0b10001000 -x 0b111 direct => { DATA[direct]=R[x]; PC+=2 };

    OP "MOV A,{direct}"            0b11100101 direct => {A=PDATA[direct]; PC+=2};
    OP "MOV @R{x},{direct}"        0b10100110 -x 0b1 direct => { IDATA[R[x]]=PDATA[direct]; PC+=2 };
    OP "MOV R{x},{direct}"         0b10101000 -x 0b111 direct => { R[x]=PDATA[direct]; PC+=2 };

    // Stack
    OP "PUSH {direct}" 0b11000000 direct => {PUSH(DATA[direct]); PC+=2};
    OP "POP {direct}" 0b11010000 direct => {DATA[direct]=POP(); PC+=2};

    // Carry/bit operations
    OP "CLR C" 0b11000011 => {C=false; PC+=1};
    OP "SETB C" 0b11010011 => {C=true; PC+=1};
    OP "CPL C" 0b10110011 => {C=!C; PC+=1};
    OP "CLR #{bit}" 0b11000010 bit => {BIT[bit]=false; PC+=2};
    OP "SETB #{bit}" 0b11010010 bit => {BIT[bit]=true; PC+=2};
    OP "CPL #{bit}" 0b10110010 bit => {BIT[bit]=!BIT[bit]; PC+=2};
    OP "MOV C,#{bit}" 0b10100010 bit => {C=PBIT[bit]; PC+=2};
    OP "MOV #{bit},C" 0b10010010 bit => {PBIT[bit]=C; PC+=2};
    OP "ANL C,#{bit}" 0b10000010 bit => {C&=PBIT[bit]; PC+=2};
    OP "ANL C,/#{bit}" 0b10110000 bit => {C&=!PBIT[bit]; PC+=2};
    OP "ORL C,#{bit}" 0b01110010 bit => {C|=PBIT[bit]; PC+=2};
    OP "ORL C,/#{bit}" 0b10100000 bit => {C|=!PBIT[bit]; PC+=2};

    // Exchange
    OP "SWAP A" 0b11000100 => {A=swap_nibbles(A); PC+=1};
    OP "XCH A,{direct}" 0b11000101 direct => {(A, DATA[direct])=(DATA[direct], A);PC+=2};
    OP "XCH A,@R{x}" 0b11000110 -x 0b1 => {(A, IDATA[R[x]])=(IDATA[R[x]], A); PC+=1};
    OP "XCH A,R{x}" 0b11001000 -x 0b111 => {(A, R[x])=(R[x], A);PC+=1};
    OP "XCHD A,@R{x}" 0b11010110 -x 0b1 => {let tmp=IDATA[R[x]]; let tmp2=A&0x0F; A=(A&0xF0)|(tmp&0x0F); IDATA[R[x]]=tmp&0xF0|tmp2; PC+=1};
}

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
    let ac = ((a.0 & 0x0F) + (b.0 & 0x0F) + c as u8) > 0x0F;
    let sum = a.to_u16() + b.to_u16() + c as u16;
    let ov = ((a.0 ^ b.0) & 0x80) != 0 && ((a.0 ^ sum as u8) & 0x80) != 0;
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
    (Reg8((a.0 << 1) & 0xFF) | (c as u8), new_carry)
}

#[inline(always)]
fn rrc(a: Reg8, c: bool) -> (Reg8, bool) {
    let new_carry = (a.0 & 0x01) != 0;
    (Reg8((a.0 >> 1) | (if c { 0x80 } else { 0x00 })), new_carry)
}

#[inline(always)]
fn rl(a: Reg8) -> Reg8 {
    let b = (a.0 & 0x80) >> 7;
    Reg8((a.0 << 1) & 0xFF | b)
}

#[inline(always)]
fn rr(a: Reg8) -> Reg8 {
    let b = (a.0 & 0x01) << 7;
    Reg8((a.0 >> 1) | b)
}

#[test]
fn foo() {
    #![allow(redundant_semicolons, non_snake_case)]

    let mut cpu = Cpu::new();
    let bit = 0;
    let FUNC = |a| a;

    let ctx = Context {
        cpu: &mut cpu,
        xdata: ExternalMemory { memory: &mut () },
        code: ExternalMemory { memory: &mut () },
        ports: &mut (),
    };

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
