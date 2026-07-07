//! Instruction definitions and decoding for the NMOS 6502.
//!
//! Decode-only: the [`isa!`] table maps each of the 256 opcode bytes to a
//! ([`Mnemonic`], [`AddrMode`]) pair, from which lengths, operands, and
//! control-flow are derived. There is no execution engine (yet).

use std::fmt::Write as _;

/// The addressing mode of an instruction, this alone fixes its byte length and
/// how the trailing operand bytes are interpreted.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AddrMode {
    /// No operand (`NOP`, `TAX`, `BRK`, ...).
    Implied,
    /// Operates on the accumulator (`ASL A`).
    Accumulator,
    /// `#0xNN`.
    Immediate,
    /// `0xNN` (page 0).
    ZeroPage,
    /// `0xNN,X`.
    ZeroPageX,
    /// `0xNN,Y`.
    ZeroPageY,
    /// Signed PC-relative branch displacement.
    Relative,
    /// `0xNNNN`.
    Absolute,
    /// `0xNNNN,X`.
    AbsoluteX,
    /// `0xNNNN,Y`.
    AbsoluteY,
    /// `[0xNNNN]`, indirect, `JMP` only.
    Indirect,
    /// `[0xNN,X]`, indexed indirect.
    IndexedIndirect,
    /// `[0xNN],Y`, indirect indexed.
    IndirectIndexed,
}

impl AddrMode {
    /// Total instruction length in bytes, opcode included.
    pub const fn length(self) -> u8 {
        use AddrMode::*;
        match self {
            Implied | Accumulator => 1,
            Immediate | ZeroPage | ZeroPageX | ZeroPageY | Relative | IndexedIndirect
            | IndirectIndexed => 2,
            Absolute | AbsoluteX | AbsoluteY | Indirect => 3,
        }
    }
}

/// A single entry in the opcode table.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OpInfo {
    pub mnemonic: Mnemonic,
    pub mode: AddrMode,
}

/// A decoded operand. At most one is present per instruction, the addressing
/// mode carries any indexing, so there is no second operand as on the 8051.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Operand {
    /// Placeholder / default (never emitted for a real operand).
    Implied,
    /// The accumulator, `A`.
    Accumulator,
    Imm8(u8),
    ZeroPage(u8),
    ZeroPageX(u8),
    ZeroPageY(u8),
    Absolute(u16),
    AbsoluteX(u16),
    AbsoluteY(u16),
    Indirect(u16),
    IndexedIndirect(u8),
    IndirectIndexed(u8),
    /// Branch displacement (rendered as its absolute target).
    Relative(i8),
}

/// Stack-allocated operand list, mirrors the 8051 API, holds at most one.
#[derive(Debug, Clone, Copy)]
pub struct Operands {
    buf: [Operand; Instruction::MAX_OPERANDS],
    len: u8,
}

impl Operands {
    pub(crate) fn new() -> Self {
        Self {
            buf: [Operand::Implied; Instruction::MAX_OPERANDS],
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
    /// Unknown or data-dependent control flow (`RTS`, `RTI`, `JMP [ind]`, ...).
    Diverge,
}

/// An instruction decoded from a sequence of bytes.
#[derive(Debug, Clone)]
pub struct Instruction {
    pc: u32,
    len: u8,
    bytes: [u8; Self::MAX_LENGTH],
    mnemonic: Mnemonic,
    mode: AddrMode,
    operands: Operands,
}

#[allow(clippy::len_without_is_empty)]
impl Instruction {
    /// The maximum byte length of an instruction.
    pub const MAX_LENGTH: usize = 3;
    /// The maximum number of operands an instruction can have.
    pub const MAX_OPERANDS: usize = 1;

    pub fn decode_from_bytes(pc: u32, bytes: &[u8]) -> Self {
        if let Some(decoded) = decode(bytes, pc) {
            return decoded;
        }
        // Unknown opcode: consume a single byte and mark it as such.
        let mut ins_bytes = [0u8; Self::MAX_LENGTH];
        if let Some(&b) = bytes.first() {
            ins_bytes[0] = b;
        }
        Self {
            pc,
            len: 1,
            bytes: ins_bytes,
            mnemonic: Mnemonic::Unknown,
            mode: AddrMode::Implied,
            operands: Operands::new(),
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

    pub fn mode(&self) -> AddrMode {
        self.mode
    }

    pub fn operands(&self) -> &Operands {
        &self.operands
    }

    /// If this instruction references a memory address, return it.
    pub fn direct_addr(&self) -> Option<u16> {
        self.operands.as_slice().iter().find_map(|o| match o {
            Operand::ZeroPage(a)
            | Operand::ZeroPageX(a)
            | Operand::ZeroPageY(a)
            | Operand::IndexedIndirect(a)
            | Operand::IndirectIndexed(a) => Some(*a as u16),
            Operand::Absolute(a)
            | Operand::AbsoluteX(a)
            | Operand::AbsoluteY(a)
            | Operand::Indirect(a) => Some(*a),
            _ => None,
        })
    }

    pub fn has_rel(&self) -> bool {
        self.operands
            .as_slice()
            .iter()
            .any(|o| matches!(o, Operand::Relative(_)))
    }

    /// The absolute branch/jump target of this instruction, if statically known.
    pub fn target(&self) -> Option<u32> {
        let page = self.pc & !0xFFFF;
        match self.operands.as_slice().first()? {
            Operand::Relative(r) => {
                Some(((self.pc as i64 + self.len as i64 + *r as i64) as u32 & 0xFFFF) | page)
            }
            Operand::Absolute(a) if matches!(self.mnemonic, Mnemonic::JMP | Mnemonic::JSR) => {
                Some(*a as u32 | page)
            }
            _ => None,
        }
    }

    /// Render the instruction in canonical `sdas6500` textual form.
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
        use Mnemonic::*;
        match self.mnemonic {
            JMP => match self.mode {
                AddrMode::Absolute => ControlFlow::Jump {
                    target: self.target().unwrap(),
                },
                _ => ControlFlow::Diverge, // JMP [ind]
            },
            JSR => ControlFlow::Call {
                target: self.target().unwrap(),
                return_pc: fall_through,
            },
            RTS | RTI | BRK => ControlFlow::Diverge,
            BCC | BCS | BEQ | BMI | BNE | BPL | BVC | BVS => ControlFlow::Choice {
                fall_through,
                branch_target: self.target().unwrap(),
            },
            _ => ControlFlow::Continue { next: fall_through },
        }
    }

    fn push_operand(&self, s: &mut String, op: &Operand) {
        use Operand::*;
        match op {
            Implied => {}
            Accumulator => s.push('A'),
            Imm8(v) => {
                let prefix = if *v > 9 { "0x" } else { "" };
                let _ = write!(s, "#{prefix}{v:02X}");
            }
            ZeroPage(v) => {
                let _ = write!(s, "0x{v:02X}");
            }
            ZeroPageX(v) => {
                let _ = write!(s, "0x{v:02X},X");
            }
            ZeroPageY(v) => {
                let _ = write!(s, "0x{v:02X},Y");
            }
            Absolute(v) => {
                let _ = write!(s, "0x{v:04X}");
            }
            AbsoluteX(v) => {
                let _ = write!(s, "0x{v:04X},X");
            }
            AbsoluteY(v) => {
                let _ = write!(s, "0x{v:04X},Y");
            }
            Indirect(v) => {
                let _ = write!(s, "[0x{v:04X}]");
            }
            IndexedIndirect(v) => {
                let _ = write!(s, "[0x{v:02X},X]");
            }
            IndirectIndexed(v) => {
                let _ = write!(s, "[0x{v:02X}],Y");
            }
            Relative(_) => {
                let _ = write!(s, "0x{:04X}", self.target().unwrap_or(0));
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
            write!(f, "{}", self.as_string())
        } else {
            write!(f, "{}", self.as_string())
        }
    }
}

/// Total instruction length for the opcode starting `bytes`.
pub const fn decode_length(bytes: &[u8]) -> u8 {
    if bytes.is_empty() {
        return 1;
    }
    INSTRUCTION_LENGTHS[bytes[0] as usize]
}

/// Build the [`Operand`] for `mode` from the (already length-trimmed) `bytes`.
fn read_operand(mode: AddrMode, bytes: &[u8]) -> Option<Operand> {
    let lo = bytes.get(1).copied().unwrap_or(0);
    let hi = bytes.get(2).copied().unwrap_or(0);
    let word = (lo as u16) | ((hi as u16) << 8);
    use AddrMode::*;
    Some(match mode {
        Implied => return None,
        Accumulator => Operand::Accumulator,
        Immediate => Operand::Imm8(lo),
        ZeroPage => Operand::ZeroPage(lo),
        ZeroPageX => Operand::ZeroPageX(lo),
        ZeroPageY => Operand::ZeroPageY(lo),
        Relative => Operand::Relative(lo as i8),
        Absolute => Operand::Absolute(word),
        AbsoluteX => Operand::AbsoluteX(word),
        AbsoluteY => Operand::AbsoluteY(word),
        Indirect => Operand::Indirect(word),
        IndexedIndirect => Operand::IndexedIndirect(lo),
        IndirectIndexed => Operand::IndirectIndexed(lo),
    })
}

/// Decode a single instruction at `pc`. Returns `None` for illegal opcodes.
pub fn decode(bytes: &[u8], pc: u32) -> Option<Instruction> {
    let op = *bytes.first()?;
    let info = OPCODES[op as usize];
    if info.mnemonic == Mnemonic::Unknown {
        return None;
    }
    let len = info.mode.length();
    let mut ins_bytes = [0u8; Instruction::MAX_LENGTH];
    for (i, &b) in bytes.iter().take(len as usize).enumerate() {
        ins_bytes[i] = b;
    }
    let mut operands = Operands::new();
    if let Some(o) = read_operand(info.mode, &ins_bytes[..len as usize]) {
        operands.push(o);
    }
    Some(Instruction {
        pc,
        len,
        bytes: ins_bytes,
        mnemonic: info.mnemonic,
        mode: info.mode,
        operands,
    })
}

/// Map an ISA-table addressing-mode shorthand to an [`AddrMode`].
macro_rules! mode {
    (imp) => { AddrMode::Implied };
    (acc) => { AddrMode::Accumulator };
    (imm) => { AddrMode::Immediate };
    (zp)  => { AddrMode::ZeroPage };
    (zpx) => { AddrMode::ZeroPageX };
    (zpy) => { AddrMode::ZeroPageY };
    (rel) => { AddrMode::Relative };
    (abs) => { AddrMode::Absolute };
    (abx) => { AddrMode::AbsoluteX };
    (aby) => { AddrMode::AbsoluteY };
    (ind) => { AddrMode::Indirect };
    (izx) => { AddrMode::IndexedIndirect };
    (izy) => { AddrMode::IndirectIndexed };
}

/// The 6502 ISA, grouped by mnemonic. Each `0xNN mode` line binds one opcode
/// byte. Every byte not listed decodes as [`Mnemonic::Unknown`] (illegal).
macro_rules! isa {
    (
        $( $mnem:ident { $( $opcode:literal $m:ident ; )* } )*
    ) => {
        /// 6502 instruction mnemonics.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum Mnemonic {
            /// Unrecognized / illegal opcode byte.
            Unknown,
            $( $mnem, )*
        }

        impl Mnemonic {
            pub fn as_str(self) -> &'static str {
                match self {
                    Mnemonic::Unknown => "???",
                    $( Mnemonic::$mnem => stringify!($mnem), )*
                }
            }
        }

        /// Maps a raw opcode byte to its ([`Mnemonic`], [`AddrMode`]).
        pub const OPCODES: [OpInfo; 256] = {
            let mut t = [OpInfo { mnemonic: Mnemonic::Unknown, mode: AddrMode::Implied }; 256];
            $( $(
                t[$opcode] = OpInfo { mnemonic: Mnemonic::$mnem, mode: mode!($m) };
            )* )*
            t
        };
    };
}

/// Maps a raw opcode byte to its instruction length (1 for illegal opcodes).
pub const INSTRUCTION_LENGTHS: [u8; 256] = {
    let mut lengths = [1u8; 256];
    let mut op = 0usize;
    while op < 256 {
        lengths[op] = OPCODES[op].mode.length();
        op += 1;
    }
    lengths
};

#[rustfmt::skip]
isa! {
    ADC { 0x69 imm; 0x65 zp; 0x75 zpx; 0x6D abs; 0x7D abx; 0x79 aby; 0x61 izx; 0x71 izy; }
    AND { 0x29 imm; 0x25 zp; 0x35 zpx; 0x2D abs; 0x3D abx; 0x39 aby; 0x21 izx; 0x31 izy; }
    ASL { 0x0A acc; 0x06 zp; 0x16 zpx; 0x0E abs; 0x1E abx; }
    BCC { 0x90 rel; }
    BCS { 0xB0 rel; }
    BEQ { 0xF0 rel; }
    BIT { 0x24 zp; 0x2C abs; }
    BMI { 0x30 rel; }
    BNE { 0xD0 rel; }
    BPL { 0x10 rel; }
    BRK { 0x00 imp; }
    BVC { 0x50 rel; }
    BVS { 0x70 rel; }
    CLC { 0x18 imp; }
    CLD { 0xD8 imp; }
    CLI { 0x58 imp; }
    CLV { 0xB8 imp; }
    CMP { 0xC9 imm; 0xC5 zp; 0xD5 zpx; 0xCD abs; 0xDD abx; 0xD9 aby; 0xC1 izx; 0xD1 izy; }
    CPX { 0xE0 imm; 0xE4 zp; 0xEC abs; }
    CPY { 0xC0 imm; 0xC4 zp; 0xCC abs; }
    DEC { 0xC6 zp; 0xD6 zpx; 0xCE abs; 0xDE abx; }
    DEX { 0xCA imp; }
    DEY { 0x88 imp; }
    EOR { 0x49 imm; 0x45 zp; 0x55 zpx; 0x4D abs; 0x5D abx; 0x59 aby; 0x41 izx; 0x51 izy; }
    INC { 0xE6 zp; 0xF6 zpx; 0xEE abs; 0xFE abx; }
    INX { 0xE8 imp; }
    INY { 0xC8 imp; }
    JMP { 0x4C abs; 0x6C ind; }
    JSR { 0x20 abs; }
    LDA { 0xA9 imm; 0xA5 zp; 0xB5 zpx; 0xAD abs; 0xBD abx; 0xB9 aby; 0xA1 izx; 0xB1 izy; }
    LDX { 0xA2 imm; 0xA6 zp; 0xB6 zpy; 0xAE abs; 0xBE aby; }
    LDY { 0xA0 imm; 0xA4 zp; 0xB4 zpx; 0xAC abs; 0xBC abx; }
    LSR { 0x4A acc; 0x46 zp; 0x56 zpx; 0x4E abs; 0x5E abx; }
    NOP { 0xEA imp; }
    ORA { 0x09 imm; 0x05 zp; 0x15 zpx; 0x0D abs; 0x1D abx; 0x19 aby; 0x01 izx; 0x11 izy; }
    PHA { 0x48 imp; }
    PHP { 0x08 imp; }
    PLA { 0x68 imp; }
    PLP { 0x28 imp; }
    ROL { 0x2A acc; 0x26 zp; 0x36 zpx; 0x2E abs; 0x3E abx; }
    ROR { 0x6A acc; 0x66 zp; 0x76 zpx; 0x6E abs; 0x7E abx; }
    RTI { 0x40 imp; }
    RTS { 0x60 imp; }
    SBC { 0xE9 imm; 0xE5 zp; 0xF5 zpx; 0xED abs; 0xFD abx; 0xF9 aby; 0xE1 izx; 0xF1 izy; }
    SEC { 0x38 imp; }
    SED { 0xF8 imp; }
    SEI { 0x78 imp; }
    STA { 0x85 zp; 0x95 zpx; 0x8D abs; 0x9D abx; 0x99 aby; 0x81 izx; 0x91 izy; }
    STX { 0x86 zp; 0x96 zpy; 0x8E abs; }
    STY { 0x84 zp; 0x94 zpx; 0x8C abs; }
    TAX { 0xAA imp; }
    TAY { 0xA8 imp; }
    TSX { 0xBA imp; }
    TXA { 0x8A imp; }
    TXS { 0x9A imp; }
    TYA { 0x98 imp; }
}
