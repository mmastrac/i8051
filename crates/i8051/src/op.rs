use std::fmt::Write as _;

use i8051_proc_macro::op_def;

use crate::cpu::{Cpu, Flag};
use crate::regs::{Reg8, Reg16, Reg32, U16Equivalent};
use crate::sfr::*;
use crate::traits::{CpuContext, CpuView, MemoryMapper, PortMapper};

#[inline(always)]
fn swap_nibbles(a: Reg8) -> Reg8 {
    Reg8(((a.0 & 0x0F) << 4) | ((a.0 & 0xF0) >> 4))
}

#[inline(always)]
fn decimal_adjust(a: Reg8, c: bool, ac: bool) -> (Reg8, bool, bool) {
    let mut result = a.0;
    let mut carry = c;
    let aux_carry = ac;

    if (result & 0x0F) > 9 || aux_carry {
        result = result.wrapping_add(6);
        if result < a.0 {
            carry = true;
        }
    }

    if ((result & 0xF0) >> 4) > 9 || carry {
        result = result.wrapping_add(0x60);
        if result < (result.wrapping_sub(0x60)) {
            carry = true;
        }
    }

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

macro_rules! op_def_read {
    ($ctx:ident, A) => {
        Reg8($ctx.a())
    };
    ($ctx:ident, B) => {
        Reg8($ctx.b())
    };
    ($ctx:ident, PC) => {
        Reg32($ctx.pc_ext())
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Operand {
    A,
    C,
    AB,
    Dptr,
    IndirectDptr,
    IndirectADptr,
    IndirectAPc,
    Reg(u8),
    IndirectReg(u8),
    Direct(u8),
    Bit(u8),
    BitNot(u8),
    Imm8(u8),
    Imm16(u16),
    Rel(i8),
    Addr11(u16),
    Addr16(u16),
}

/// Stack-allocated operand list (no deps, no heap). 8051 max arity is 3.
#[derive(Debug, Clone, Copy)]
pub struct Operands {
    buf: [Operand; 3],
    len: u8,
}

impl Operands {
    pub(crate) fn new() -> Self {
        Self {
            buf: [Operand::A; 3],
            len: 0,
        }
    }
    pub(crate) fn push(&mut self, o: Operand) {
        self.buf[self.len as usize] = o;
        self.len += 1;
    }
    pub fn as_slice(&self) -> &[Operand] {
        &self.buf[..self.len as usize]
    }
}

/// How execution proceeds after an instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ControlFlow {
    /// Fall through to `next`.
    Continue { next: u32 },
    /// Unconditional jump to `target`.
    Jump { target: u32 },
    /// Call `target`, returning to `return_pc`.
    Call { target: u32, return_pc: u32 },
    /// Branch to `branch_target` or fall through to `fall_through`.
    Choice {
        fall_through: u32,
        branch_target: u32,
    },
    /// Unknown or data-dependent control flow.
    Diverge,
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pc: u32,
    len: u8,
    bytes: [u8; Self::MAX_LENGTH],
    mnemonic: Mnemonic,
    operands: Operands,
}

impl Instruction {
    pub const MAX_LENGTH: usize = 3;

    pub fn decode_from_bytes(pc: u32, bytes: &[u8]) -> Self {
        let len = decode_length(bytes);
        let mut ins_bytes = [0u8; Self::MAX_LENGTH];
        for (i, &byte) in bytes.iter().take(len as usize).enumerate() {
            ins_bytes[i] = byte;
        }
        if let Some(decoded) = decode(&ins_bytes[..len as usize], pc) {
            decoded
        } else {
            Self {
                pc,
                len,
                bytes: ins_bytes,
                mnemonic: Mnemonic::Unknown,
                operands: Operands::new(),
            }
        }
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
        self.mnemonic
    }

    pub fn operands(&self) -> &Operands {
        &self.operands
    }

    /// If this instruction references an internal memory address, return it.
    ///
    /// Technically we're missing dst+src for one direct-to-direct move.
    pub fn direct_addr(&self) -> Option<u8> {
        self.operands.as_slice().iter().find_map(|o| match o {
            Operand::Direct(d) | Operand::Bit(d) | Operand::BitNot(d) => Some(*d),
            _ => None,
        })
    }

    pub fn has_rel(&self) -> bool {
        self.operands
            .as_slice()
            .iter()
            .any(|o| matches!(o, Operand::Rel(_)))
    }

    /// Get the absolute target address of this instruction, if one exists.
    pub fn target(&self) -> Option<u32> {
        let page = self.pc & !0xFFFF;
        self.operands.as_slice().iter().find_map(|o| match o {
            Operand::Rel(r) => {
                Some(((self.pc as i64 + self.len as i64 + *r as i64) as u32 & 0xFFFF) | page)
            }
            Operand::Addr11(a) => Some((self.pc & !0x7FF) | *a as u32),
            Operand::Addr16(a) => Some(*a as u32 | page),
            _ => None,
        })
    }

    /// Render the instruction in the canonical textual form (SFR names, `0x`
    /// directs, `#` immediates, and absolute branch targets).
    pub fn as_string(&self) -> String {
        let mut s = String::from(self.mnemonic.as_str());
        for (i, op) in self.operands.as_slice().iter().enumerate() {
            s.push_str(if i == 0 { " " } else { "," });
            self.push_operand(&mut s, op);
        }
        s
    }

    pub fn control_flow(&self) -> ControlFlow {
        let fall_through = self.pc + self.len as u32;
        match self.mnemonic {
            Mnemonic::LJMP | Mnemonic::AJMP | Mnemonic::SJMP => ControlFlow::Jump {
                target: self.target().unwrap(),
            },
            Mnemonic::LCALL | Mnemonic::ACALL => ControlFlow::Call {
                target: self.target().unwrap(),
                return_pc: fall_through,
            },
            Mnemonic::JMP | Mnemonic::RET | Mnemonic::RETI => ControlFlow::Diverge,
            _ if self.has_rel() => ControlFlow::Choice {
                fall_through,
                branch_target: self.target().unwrap(),
            },
            _ => ControlFlow::Continue { next: fall_through },
        }
    }

    fn push_operand(&self, s: &mut String, op: &Operand) {
        use Operand::*;
        match op {
            A => s.push('A'),
            C => s.push('C'),
            AB => s.push_str("AB"),
            Dptr => s.push_str("DPTR"),
            IndirectDptr => s.push_str("@DPTR"),
            IndirectADptr => s.push_str("@A+DPTR"),
            IndirectAPc => s.push_str("@A+PC"),
            Reg(n) => {
                let _ = write!(s, "R{n}");
            }
            IndirectReg(i) => {
                let _ = write!(s, "@R{i}");
            }
            Direct(d) => s.push_str(&format_direct(*d)),
            Bit(b) => s.push_str(&format_bit(*b)),
            BitNot(b) => {
                s.push('/');
                s.push_str(&format_bit(*b));
            }
            Imm8(v) => {
                let prefix = if *v > 9 { "0x" } else { "" };
                let _ = write!(s, "#{prefix}{v:02X}");
            }
            Imm16(v) => {
                let prefix = if *v > 9 { "0x" } else { "" };
                let _ = write!(s, "#{prefix}{v:04X}");
            }
            Rel(_) | Addr11(_) | Addr16(_) => {
                let _ = write!(s, "#0x{:04X}", self.target().unwrap_or(0));
            }
        }
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "{:04X}: ", self.pc)?;
            for byte in self.bytes() {
                write!(f, "{:02X} ", byte)?;
            }
            for _ in 0..(Self::MAX_LENGTH - self.len()) {
                write!(f, "   ")?;
            }
            write!(f, "{}", self.as_string())?;
            Ok(())
        } else {
            write!(f, "{}", self.as_string())
        }
    }
}

/// Format a direct address, using SFR names where applicable.
fn format_direct(value: u8) -> String {
    if value < 128 {
        return format!("0x{:02X}", value);
    }
    match value {
        SFR_P0 => "P0".to_string(),
        SFR_A => "ACC".to_string(),
        SFR_B => "B".to_string(),
        SFR_PSW => "PSW".to_string(),
        SFR_SP => "SP".to_string(),
        SFR_DPL => "DPL".to_string(),
        SFR_DPH => "DPH".to_string(),
        SFR_PCON => "PCON".to_string(),
        SFR_TCON => "TCON".to_string(),
        SFR_TMOD => "TMOD".to_string(),
        SFR_TL0 => "TL0".to_string(),
        SFR_TL1 => "TL1".to_string(),
        SFR_TH0 => "TH0".to_string(),
        SFR_TH1 => "TH1".to_string(),
        SFR_P1 => "P1".to_string(),
        SFR_SCON => "SCON".to_string(),
        SFR_SBUF => "SBUF".to_string(),
        SFR_P2 => "P2".to_string(),
        SFR_IE => "IE".to_string(),
        SFR_P3 => "P3".to_string(),
        SFR_IP => "IP".to_string(),
        SFR_T2CON => "T2CON".to_string(),
        SFR_T2MOD => "T2MOD".to_string(),
        SFR_RCAP2L => "RCAP2L".to_string(),
        SFR_RCAP2H => "RCAP2H".to_string(),
        SFR_TL2 => "TL2".to_string(),
        SFR_TH2 => "TH2".to_string(),
        sfr => format!("SFR_{:02X}", sfr),
    }
}

/// Format a bit address. RAM bits print as `0xNN`; SFR bits print as `NAME.bit`.
fn format_bit(value: u8) -> String {
    if value < 128 {
        format!("0x{:02X}", value)
    } else {
        format!("{}.{}", format_direct(value & 0xF8), value & 0x07)
    }
}

/// Generate per-mnemonic rustdoc from one `isa!` mnemonic group.
macro_rules! isa_doc {
    (@mnem $mnem:ident $( @rule $base:literal $( [ $($op:tt)* ] )* => $sem:tt ; )*) => {
        concat!(
            stringify!($mnem),
            " instruction.",
            $( isa_doc!(@rule $mnem $base $( [ $($op)* ] )* => $sem) ),*
        )
    };
    (@rule $mnem:ident $base:literal $( [ $($op:tt)* ] )* => $sem:tt) => {
        concat!(
            "\n\n- `",
            stringify!($mnem),
            " ",
            concat!("" $( , isa_doc!(@op $($op)*),  )", "*),
            "`\n\n    \n```# #[cfg(false)]\n",
            stringify!($sem),
            "\n```",
        )
    };
    (@op $a:ident $b:ident = direct2) => { stringify!($a, $b) };
    (@op $i:ident = Rn $m:literal) => { "Rn" };
    (@op $i:ident = @Ri $m:literal) => { "@Ri" };
    (@op $i:ident = $($op:tt)*) => { stringify!($( $op )* ) };
    (@op $($op:tt)*) => { stringify!($( $op )* ) };
}

macro_rules! isa {
    (
        $(
            $mnem:ident {
                $( $base:literal $([ $($op:tt)* ])* => $sem:tt ; )*
            }
        )*
    ) => {
        /// Maps a raw op start byte to its instruction length.
        pub const INSTRUCTION_LENGTHS: [u8; 256] = {
            let mut lengths = [1; 256];
            let mut op: usize = 0;
            while op < 256 {
                $( $(
                    {
                        const EMB: u8 = 0u8 $(| isa!(@embmask [$($op)*]))*;
                        if op as u8 & !EMB == $base {
                            lengths[op] = 1 $( + isa!(@count [$($op)*]) )*;
                        }
                    }
                )* )*
                op += 1;
            }
            lengths
        };

        /// Maps a raw op start byte to its base instruction.
        pub const INSTRUCTION_BASES: [u8; 256] = {
            let mut bases = [0; 256];
            let mut op: usize = 0;
            while op < 256 {
                $( $(
                    {
                        const EMB: u8 = 0u8 $(| isa!(@embmask [$($op)*]))*;
                        if op as u8 & !EMB == $base {
                            bases[op] = op as u8 & !EMB;
                        }
                    }
                )* )*
                op += 1;
            }
            bases
        };

        /// 8051 instruction mnemonics.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum Mnemonic {
            /// Unrecognized or unimplemented opcode byte.
            Unknown,
            $(
                #[doc = isa_doc!(@mnem $mnem $( @rule $base $( [ $($op)* ] )* => $sem ; )*)]
                $mnem,
            )*
        }
        impl Mnemonic {
            pub fn as_str(self) -> &'static str {
                match self { Mnemonic::Unknown => "???", $(Mnemonic::$mnem => stringify!($mnem),)* }
            }
        }

        #[allow(unused_mut, unused_variables, unused_assignments, non_snake_case)]
        pub fn decode(code: &[u8], pc: u32) -> Option<Instruction> {
            let code = [
                code.first().copied().unwrap_or(0),
                code.get(1).copied().unwrap_or(0),
                code.get(2).copied().unwrap_or(0),
            ];
            let len = INSTRUCTION_LENGTHS[code[0] as usize];
            let op = code[0];
            $( $(
                {
                    const EMB: u8 = 0u8 $(| isa!(@embmask [$($op)*]))*;
                    if op & !EMB == $base {
                        let mut n = 1usize;
                        let mut operands = Operands::new();
                        $( isa!(@bind op, code, n, operands, pc, [$($op)*]); )*
                        return Some(Instruction {
                            pc,
                            len,
                            bytes: code,
                            mnemonic: Mnemonic::$mnem,
                            operands,
                        });
                    }
                }
            )* )*
            None
        }

        #[allow(unused_mut, unused_variables, unused_assignments, non_snake_case, redundant_semicolons, unconditional_panic)]
        #[inline]
        pub fn dispatch(cpu: &mut Cpu, ctx: &mut impl CpuContext) {
            let mut pc = cpu.pc_ext(ctx);
            let view = (&*cpu, &*ctx);
            let op = view.read_code(pc);
            match INSTRUCTION_BASES[op as usize] {
            $( $(
                $base => {
                    let mut code = [0; INSTRUCTION_LENGTHS[$base] as usize - 1];
                    if code.len() > 0 {
                        code[0] = view.read_code(pc.wrapping_add(1));
                    }
                    if code.len() > 1 {
                        code[1] = view.read_code(pc.wrapping_add(2));
                    }
                    let ctx = (cpu, ctx);
                    let mut n = 0usize;
                    let mut operands = Operands::new();
                    $( isa!(@bind op, code, n, operands, pc, [$($op)*]); )*
                    op_def!(ctx $sem);
                    return;
                }
            )* )*
                _ => {
                    // Unknown instruction, just skip 1 byte.
                    let ctx = (cpu, ctx);
                    ctx.0.pc += 1;
                    return;
                }
            }
        }
    };

    (@embmask [$b:ident = Rn $m:literal])  => { $m };
    (@embmask [$b:ident = @Ri $m:literal]) => { $m };
    (@embmask [$b:ident = addr11])         => { 0b1110_0000u8 };
    (@embmask [$($x:tt)*])                 => { 0u8 };

    (@count [A])                           => { 0 };
    (@count [C])                           => { 0 };
    (@count [AB])                          => { 0 };
    (@count [DPTR])                        => { 0 };
    (@count [@DPTR])                       => { 0 };
    (@count [@A+DPTR])                     => { 0 };
    (@count [@A+PC])                       => { 0 };
    (@count [$b:ident])                    => { isa!(@count [$b = $b]) };
    (@count [$b:ident = Rn $m:literal])    => { 0 };
    (@count [$b:ident = @Ri $m:literal])   => { 0 };
    (@count [$a:ident $b:ident = direct2]) => { 2 };
    (@count [$id:ident = addr16])          => { 2 };
    (@count [$id:ident = imm16])           => { 2 };
    (@count [$id:ident = rel])             => { 1 };
    (@count [$id:ident = direct])          => { 1 };
    (@count [$id:ident = imm8])            => { 1 };
    (@count [$id:ident = addr11])          => { 1 };
    (@count [$id:ident = bit])             => { 1 };
    (@count [$id:ident = /bit])            => { 1 };

    (@bind $op:ident,$c:ident,$n:ident,$o:ident,$pc:ident,[A])        => { $o.push(Operand::A); };
    (@bind $op:ident,$c:ident,$n:ident,$o:ident,$pc:ident,[C])        => { $o.push(Operand::C); };
    (@bind $op:ident,$c:ident,$n:ident,$o:ident,$pc:ident,[AB])       => { $o.push(Operand::AB); };
    (@bind $op:ident,$c:ident,$n:ident,$o:ident,$pc:ident,[DPTR])     => { $o.push(Operand::Dptr); };
    (@bind $op:ident,$c:ident,$n:ident,$o:ident,$pc:ident,[@DPTR])    => { $o.push(Operand::IndirectDptr); };
    (@bind $op:ident,$c:ident,$n:ident,$o:ident,$pc:ident,[@A+DPTR])  => { $o.push(Operand::IndirectADptr); };
    (@bind $op:ident,$c:ident,$n:ident,$o:ident,$pc:ident,[@A+PC])    => { $o.push(Operand::IndirectAPc); };
    (@bind $op:ident,$c:ident,$n:ident,$o:ident,$pc:ident,[$b:ident]) => { isa!(@bind $op,$c,$n,$o,$pc,[$b = $b]); };

    (@bind $op:ident,$c:ident,$n:ident,$o:ident,$pc:ident,[$b:ident = Rn $m:literal])  => {
        let $b = $op & $m; $o.push(Operand::Reg($b));
    };
    (@bind $op:ident,$c:ident,$n:ident,$o:ident,$pc:ident,[$b:ident = @Ri $m:literal]) => {
        let $b = $op & $m; $o.push(Operand::IndirectReg($b));
    };
    (@bind $op:ident,$c:ident,$n:ident,$o:ident,$pc:ident,[$b:ident = direct]) => {
        let $b = $c[$n]; $n += 1; $o.push(Operand::Direct($b));
    };
    (@bind $op:ident,$c:ident,$n:ident,$o:ident,$pc:ident,[$b:ident = bit]) => {
        let $b = $c[$n]; $n += 1; $o.push(Operand::Bit($b));
    };
    (@bind $op:ident,$c:ident,$n:ident,$o:ident,$pc:ident,[$b:ident = /bit]) => {
        let $b = $c[$n]; $n += 1; $o.push(Operand::BitNot($b));
    };
    (@bind $op:ident,$c:ident,$n:ident,$o:ident,$pc:ident,[$b:ident = imm8]) => {
        let $b = Reg8($c[$n]); $n += 1; $o.push(Operand::Imm8($b.0));
    };
    (@bind $op:ident,$c:ident,$n:ident,$o:ident,$pc:ident,[$b:ident = imm16]) => {
        let $b = Reg16((($c[$n] as u16) << 8) | $c[$n + 1] as u16); $n += 2;
        $o.push(Operand::Imm16($b.0));
    };
    (@bind $op:ident,$c:ident,$n:ident,$o:ident,$pc:ident,[$b:ident = rel]) => {
        let raw = $c[$n] as i8; $n += 1;
        let $b = Reg32(
            (($pc & 0xFFFF).wrapping_add($n as u32 + 1).wrapping_add(raw as u32) & 0xFFFF)
                | ($pc & !0xFFFF),
        );
        $o.push(Operand::Rel(raw));
    };
    (@bind $op:ident,$c:ident,$n:ident,$o:ident,$pc:ident,[$b:ident = addr16]) => {
        let raw = (($c[$n] as u16) << 8) | $c[$n + 1] as u16; $n += 2;
        let $b = Reg32(raw as u32 | ($pc & !0xFFFF));
        $o.push(Operand::Addr16(raw));
    };
    (@bind $op:ident,$c:ident,$n:ident,$o:ident,$pc:ident,[$b:ident = addr11]) => {
        let raw = ((($op >> 5) & 0x7) as u16) << 8 | $c[$n] as u16; $n += 1;
        let $b = Reg32(($pc & !0x7FF) | raw as u32);
        $o.push(Operand::Addr11(raw));
    };
    // direct,direct move: object bytes are [src][dst]; display is dst,src.
    (@bind $op:ident,$c:ident,$n:ident,$o:ident,$pc:ident,[$d:ident $s:ident = direct2]) => {
        let $s = $c[$n]; let $d = $c[$n + 1]; $n += 2;
        $o.push(Operand::Direct($d)); $o.push(Operand::Direct($s));
    };
}

pub const fn decode_length(bytes: &[u8]) -> u8 {
    if bytes.len() == 0 {
        return 1;
    }
    INSTRUCTION_LENGTHS[bytes[0] as usize]
}

pub fn decode_string(code: &[u8], pc: u32) -> String {
    decode(code, pc)
        .map(|i| i.as_string())
        .unwrap_or_else(|| "???".to_string())
}

/// Decode the absolute address of the instruction at the given PC, if one exists.
pub fn addr(code: &[u8], pc: u32) -> Option<u32> {
    decode(code, pc).and_then(|i| i.target())
}

/// If this instruction references an internal memory address, return it.
pub fn direct(code: &[u8], pc: u32) -> Option<u8> {
    decode(code, pc).and_then(|i| i.direct_addr())
}

pub fn has_rel(op: u8) -> bool {
    decode(&[op], 0).map(|i| i.has_rel()).unwrap_or(false)
}

pub fn opcode(op: u8) -> Mnemonic {
    decode(&[op], 0)
        .map(|i| i.mnemonic())
        .unwrap_or(Mnemonic::Unknown)
}

/// Fetch and length-decode the instruction at `pc` from CPU code memory.
pub fn decode_fetch(cpu: &Cpu, ctx: &impl CpuContext, pc: u32) -> (u8, [u8; 3]) {
    let view = (cpu, ctx);
    let op = view.read_code(pc);
    let len = decode_length(&[op]);
    let mut bytes = [op, 0, 0];
    let mut i = 1u32;
    while i < len as u32 {
        bytes[i as usize] = view.read_code(pc.wrapping_add(i));
        i += 1;
    }
    (len, bytes)
}

#[rustfmt::skip]
isa! {
    NOP   { 0x00                 => { PC+=1 }; }
    AJMP  { 0x01 [addr = addr11] => { PC=addr }; }
    ACALL { 0x11 [addr = addr11] => { PUSH16(PC+2); PC=addr }; }
    LJMP  { 0x02 [addr = addr16] => { PC=addr }; }
    LCALL { 0x12 [addr = addr16] => { PUSH16(PC+3); PC=addr }; }
    SJMP  { 0x80 [rel]           => { PC=rel }; }
    RET   { 0x22                 => { PC=POP16() }; }
    RETI  { 0x32                 => { PC=POP16(); CLEAR_INT() }; }
    JMP   { 0x73 [@A+DPTR]       => { PC=DPTR+A }; }

    JC  { 0x40 [rel]      => { if C {PC=rel} else {PC+=2} }; }
    JNC { 0x50 [rel]      => { if !C {PC=rel} else {PC+=2} }; }
    JZ  { 0x60 [rel]      => { if A==0 {PC=rel} else {PC+=2} }; }
    JNZ { 0x70 [rel]      => { if A!=0 {PC=rel} else {PC+=2} }; }
    JB  { 0x20 [bit][rel] => { if PBIT[bit] {PC=rel} else {PC+=3} }; }
    JNB { 0x30 [bit][rel] => { if !PBIT[bit] {PC=rel} else {PC+=3} }; }
    JBC { 0x10 [bit][rel] => { if PBIT[bit] {BIT[bit]=false; PC=rel} else {PC+=3} }; }

    DJNZ {
        0xD5 [direct][rel]       => { DATA[direct]-=1; if DATA[direct]!=0 {PC=rel} else {PC+=3} };
        0xD8 [x = Rn 0b111][rel] => { R[x]-=1; if R[x]!=0 {PC=rel} else {PC+=2} };
    }
    CJNE {
        0xB5 [A][direct][rel]          => { let t=DATA[direct]; C=A<t; if A!=t {PC=rel} else {PC+=3} };
        0xB6 [x = @Ri 0b1][imm8][rel]  => { C=IDATA[R[x]]<imm8; if IDATA[R[x]]!=imm8 {PC=rel} else {PC+=3} };
        0xB8 [x = Rn 0b111][imm8][rel] => { C=R[x]<imm8; if R[x]!=imm8 {PC=rel} else {PC+=3} };
        0xB4 [A][imm8][rel]            => { C=A<imm8; if A!=imm8 {PC=rel} else {PC+=3} };
    }

    MOV {
        0x90 [DPTR][imm16]          => { DPTR=imm16; PC+=3 };
        0x74 [A][imm8]              => { A=imm8; PC+=2 };
        0x78 [x = Rn 0b111][imm8]   => { R[x]=imm8; PC+=2 };
        0xF6 [x = @Ri 0b1][A]       => { IDATA[R[x]]=A; PC+=1 };
        0xE6 [A][x = @Ri 0b1]       => { A=IDATA[R[x]]; PC+=1 };
        0xF8 [x = Rn 0b111][A]      => { R[x]=A; PC+=1 };
        0xE8 [A][x = Rn 0b111]      => { A=R[x]; PC+=1 };
        0xF5 [direct][A]            => { DATA[direct]=A; PC+=2 };
        0x75 [direct][imm8]         => { DATA[direct]=imm8; PC+=3 };
        0x76 [x = @Ri 0b1][imm8]    => { IDATA[R[x]]=imm8; PC+=2 };
        0x85 [dst src = direct2]    => { DATA[dst]=PDATA[src]; PC+=3 };
        0x86 [direct][x = @Ri 0b1]  => { DATA[direct]=IDATA[R[x]]; PC+=2 };
        0x88 [direct][x = Rn 0b111] => { DATA[direct]=R[x]; PC+=2 };
        0xE5 [A][direct]            => { A=PDATA[direct]; PC+=2 };
        0xA6 [x = @Ri 0b1][direct]  => { IDATA[R[x]]=PDATA[direct]; PC+=2 };
        0xA8 [x = Rn 0b111][direct] => { R[x]=PDATA[direct]; PC+=2 };
        0xA2 [C][bit]               => { C=PBIT[bit]; PC+=2 };
        0x92 [bit][C]               => { PBIT[bit]=C; PC+=2 };
    }
    MOVX {
        0xF0 [@DPTR][A]       => { XDATA[DPTR]=A; PC+=1 };
        0xE0 [A][@DPTR]       => { A=XDATA[DPTR]; PC+=1 };
        0xE2 [A][x = @Ri 0b1] => { A=XDATA[R[x]]; PC+=1 };
        0xF2 [x = @Ri 0b1][A] => { XDATA[R[x]]=A; PC+=1 };
    }
    MOVC {
        0x93 [A][@A+DPTR] => { A=CODE[DPTR+A]; PC+=1 };
        0x83 [A][@A+PC]   => { A=CODE[PC+1+A]; PC+=1 };
    }

    INC {
        0x04 [A]              => { A=A+1; PC+=1 };
        0x05 [direct]         => { DATA[direct]+=1; PC+=2 };
        0x06 [x = @Ri 0b1]    => { IDATA[R[x]]+=1; PC+=1 };
        0x08 [x = Rn 0b111]   => { R[x]+=1; PC+=1 };
        0xA3 [DPTR]           => { DPTR+=1; PC+=1 };
    }
    DEC {
        0x14 [A]              => { A=A-1; PC+=1 };
        0x15 [direct]         => { DATA[direct]-=1; PC+=2 };
        0x16 [x = @Ri 0b1]    => { IDATA[R[x]]-=1; PC+=1 };
        0x18 [x = Rn 0b111]   => { R[x]-=1; PC+=1 };
    }
    MUL { 0xA4 [AB] => { (A,B,C,OV)=mul(A,B); PC+=1 }; }
    DIV { 0x84 [AB] => { (A,B,C,OV)=div(A,B); PC+=1 }; }
    DA  { 0xD4 [A]  => { (A,C,AC)=decimal_adjust(A,C,AC); PC+=1 }; }

    ADD {
        0x24 [A][imm8]           => { (A,C,OV,AC)=add_with_carry(A,imm8,false); PC+=2 };
        0x25 [A][direct]         => { (A,C,OV,AC)=add_with_carry(A,DATA[direct],false); PC+=2 };
        0x26 [A][x = @Ri 0b1]    => { (A,C,OV,AC)=add_with_carry(A,IDATA[R[x]],false); PC+=1 };
        0x28 [A][x = Rn 0b111]   => { (A,C,OV,AC)=add_with_carry(A,R[x],false); PC+=1 };
    }
    ADDC {
        0x34 [A][imm8]           => { (A,C,OV,AC)=add_with_carry(A,imm8,C); PC+=2 };
        0x35 [A][direct]         => { (A,C,OV,AC)=add_with_carry(A,DATA[direct],C); PC+=2 };
        0x36 [A][x = @Ri 0b1]    => { (A,C,OV,AC)=add_with_carry(A,IDATA[R[x]],C); PC+=1 };
        0x38 [A][x = Rn 0b111]   => { (A,C,OV,AC)=add_with_carry(A,R[x],C); PC+=1 };
    }
    SUBB {
        0x94 [A][imm8]         => { (A,C)=sub_with_borrow(A,imm8,C); PC+=2 };
        0x95 [A][direct]       => { (A,C)=sub_with_borrow(A,DATA[direct],C); PC+=2 };
        0x96 [A][x = @Ri 0b1]  => { (A,C)=sub_with_borrow(A,IDATA[R[x]],C); PC+=1 };
        0x98 [A][x = Rn 0b111] => { (A,C)=sub_with_borrow(A,R[x],C); PC+=1 };
    }

    ANL {
        0x54 [A][imm8]         => { A=A&imm8; PC+=2 };
        0x55 [A][direct]       => { A=A&PDATA[direct]; PC+=2 };
        0x56 [A][x = @Ri 0b1]  => { A=A&IDATA[R[x]]; PC+=1 };
        0x58 [A][x = Rn 0b111] => { A=A&R[x]; PC+=1 };
        0x52 [direct][A]       => { DATA[direct]&=A; PC+=2 };
        0x53 [direct][imm8]    => { DATA[direct]&=imm8; PC+=3 };
        0x82 [C][bit]          => { C&=PBIT[bit]; PC+=2 };
        0xB0 [C][bit = /bit]   => { C&=!PBIT[bit]; PC+=2 };
    }
    ORL {
        0x44 [A][imm8]         => { A=A|imm8; PC+=2 };
        0x45 [A][direct]       => { A=A|PDATA[direct]; PC+=2 };
        0x46 [A][x = @Ri 0b1]  => { A=A|IDATA[R[x]]; PC+=1 };
        0x48 [A][x = Rn 0b111] => { A=A|R[x]; PC+=1 };
        0x42 [direct][A]       => { DATA[direct]|=A; PC+=2 };
        0x43 [direct][imm8]    => { DATA[direct]|=imm8; PC+=3 };
        0x72 [C][bit]          => { C|=PBIT[bit]; PC+=2 };
        0xA0 [C][bit = /bit]   => { C|=!PBIT[bit]; PC+=2 };
    }
    XRL {
        0x64 [A][imm8]         => { A=A^imm8; PC+=2 };
        0x65 [A][direct]       => { A=A^PDATA[direct]; PC+=2 };
        0x66 [A][x = @Ri 0b1]  => { let t=IDATA[R[x]]; A=A^t; PC+=1 };
        0x68 [A][x = Rn 0b111] => { A=A^R[x]; PC+=1 };
        0x62 [direct][A]       => { DATA[direct]^=A; PC+=2 };
        0x63 [direct][imm8]    => { DATA[direct]^=imm8; PC+=3 };
    }

    RLC { 0x33 [A] => { (A,C)=rlc(A,C); PC+=1 }; }
    RRC { 0x13 [A] => { (A,C)=rrc(A,C); PC+=1 }; }
    RL  { 0x23 [A] => { A=rl(A); PC+=1 }; }
    RR  { 0x03 [A] => { A=rr(A); PC+=1 }; }
    SWAP{ 0xC4 [A] => { A=swap_nibbles(A); PC+=1 }; }

    CLR  { 0xE4 [A] => { A=0; PC+=1 }; 0xC3 [C] => { C=false; PC+=1 }; 0xC2 [bit] => { BIT[bit]=false; PC+=2 }; }
    SETB { 0xD3 [C] => { C=true; PC+=1 }; 0xD2 [bit] => { BIT[bit]=true; PC+=2 }; }
    CPL  { 0xF4 [A] => { A=!A; PC+=1 }; 0xB3 [C] => { C=!C; PC+=1 }; 0xB2 [bit] => { BIT[bit]=!BIT[bit]; PC+=2 }; }

    PUSH { 0xC0 [direct] => { PUSH(DATA[direct]); PC+=2 }; }
    POP  { 0xD0 [direct] => { DATA[direct]=POP(); PC+=2 }; }

    XCH {
        0xC5 [A][direct]       => { (A,DATA[direct])=(DATA[direct],A); PC+=2 };
        0xC6 [A][x = @Ri 0b1]  => { (A,IDATA[R[x]])=(IDATA[R[x]],A); PC+=1 };
        0xC8 [A][x = Rn 0b111] => { (A,R[x])=(R[x],A); PC+=1 };
    }
    XCHD { 0xD6 [A][x = @Ri 0b1] => { let t=IDATA[R[x]]; let t2=A&0x0F; A=(A&0xF0)|(t&0x0F); IDATA[R[x]]=t&0xF0|t2; PC+=1 }; }
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
