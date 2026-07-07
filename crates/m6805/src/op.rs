//! Instruction definitions and decoding for the Motorola 6805 (M68HC05).
//!
//! Decode-only. The [`isa!`] table maps each opcode byte to a ([`Mnemonic`],
//! [`AddrMode`]) pair, operands are then read from the trailing bytes. Note the
//! 6805 is big-endian: 16-bit addresses store the high byte first.

use std::fmt::Write as _;

/// The addressing mode of an instruction, fixes its length and operand layout.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AddrMode {
    /// No operand (`NOP`, `CLRA`, `RTS`, ...).
    Inherent,
    /// `#0xNN`.
    Immediate,
    /// `0xNN` (direct / zero page).
    Direct,
    /// `0xNNNN` (extended, 16-bit).
    Extended,
    /// `,X` (indexed, no offset).
    Indexed0,
    /// `0xNN,X` (indexed, 8-bit offset).
    Indexed1,
    /// `0xNNNN,X` (indexed, 16-bit offset).
    Indexed2,
    /// Signed PC-relative branch displacement.
    Relative,
    /// `n,0xNN`, bit number + direct address (`BSET`/`BCLR`).
    BitDirect,
    /// `n,0xNN,rel`, bit + direct + branch (`BRSET`/`BRCLR`).
    BitRelative,
}

impl AddrMode {
    /// Total instruction length in bytes, opcode included.
    pub const fn length(self) -> u8 {
        use AddrMode::*;
        match self {
            Inherent | Indexed0 => 1,
            Immediate | Direct | Indexed1 | Relative | BitDirect => 2,
            Extended | Indexed2 | BitRelative => 3,
        }
    }
}

/// A single entry in the opcode table.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OpInfo {
    pub mnemonic: Mnemonic,
    pub mode: AddrMode,
}

/// A decoded operand.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Operand {
    Imm8(u8),
    Direct(u8),
    Extended(u16),
    /// `,X`, indexed, no offset.
    IndexedNo,
    /// `0xNN,X`.
    Indexed8(u8),
    /// `0xNNNN,X`.
    Indexed16(u16),
    /// Branch displacement (rendered as its absolute target).
    Relative(i8),
    /// A bit number, 0-7 (`BSET`/`BRCLR`/...).
    Bit(u8),
}

/// Stack-allocated operand list (at most three: bit, direct, relative).
#[derive(Debug, Clone, Copy)]
pub struct Operands {
    buf: [Operand; Instruction::MAX_OPERANDS],
    len: u8,
}

impl Operands {
    pub(crate) fn new() -> Self {
        Self {
            buf: [Operand::IndexedNo; Instruction::MAX_OPERANDS],
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
    Continue { next: u32 },
    Jump { target: u32 },
    Call { target: u32, return_pc: u32 },
    Choice { fall_through: u32, branch_target: u32 },
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
    pub const MAX_OPERANDS: usize = 3;

    pub fn decode_from_bytes(pc: u32, bytes: &[u8]) -> Self {
        if let Some(decoded) = decode(bytes, pc) {
            return decoded;
        }
        let mut ins_bytes = [0u8; Self::MAX_LENGTH];
        if let Some(&b) = bytes.first() {
            ins_bytes[0] = b;
        }
        Self {
            pc,
            len: 1,
            bytes: ins_bytes,
            mnemonic: Mnemonic::Unknown,
            mode: AddrMode::Inherent,
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

    /// If this instruction references a fixed memory address, return it.
    pub fn direct_addr(&self) -> Option<u16> {
        self.operands.as_slice().iter().find_map(|o| match o {
            Operand::Direct(a) => Some(u16::from(*a)),
            Operand::Extended(a) => Some(*a),
            _ => None,
        })
    }

    pub fn has_rel(&self) -> bool {
        self.operands
            .as_slice()
            .iter()
            .any(|o| matches!(o, Operand::Relative(_)))
    }

    /// The absolute branch/jump/call target of this instruction, if known.
    pub fn target(&self) -> Option<u32> {
        let page = self.pc & !0xFFFF;
        // A relative operand names a PC-relative branch target.
        for op in self.operands.as_slice() {
            if let Operand::Relative(r) = op {
                return Some(((self.pc as i64 + self.len as i64 + *r as i64) as u32 & 0xFFFF) | page);
            }
        }
        // A direct/extended JMP or JSR names an absolute target.
        if matches!(self.mnemonic, Mnemonic::JMP | Mnemonic::JSR) {
            for op in self.operands.as_slice() {
                match op {
                    Operand::Extended(a) => return Some(*a as u32 | page),
                    Operand::Direct(a) => return Some(u32::from(*a) | page),
                    _ => {}
                }
            }
        }
        None
    }

    /// Render the instruction in canonical textual form.
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
                AddrMode::Direct | AddrMode::Extended => ControlFlow::Jump {
                    target: self.target().unwrap(),
                },
                _ => ControlFlow::Diverge, // indexed jump
            },
            JSR => match self.mode {
                AddrMode::Direct | AddrMode::Extended => ControlFlow::Call {
                    target: self.target().unwrap(),
                    return_pc: fall_through,
                },
                _ => ControlFlow::Diverge, // indexed call
            },
            BSR => ControlFlow::Call {
                target: self.target().unwrap(),
                return_pc: fall_through,
            },
            BRA => ControlFlow::Jump {
                target: self.target().unwrap(),
            },
            // Branch-never is a two-byte no-op that always falls through.
            BRN => ControlFlow::Continue { next: fall_through },
            RTS | RTI | SWI => ControlFlow::Diverge,
            BHI | BLS | BCC | BCS | BNE | BEQ | BHCC | BHCS | BPL | BMI | BMC | BMS | BIL | BIH
            | BRSET | BRCLR => ControlFlow::Choice {
                fall_through,
                branch_target: self.target().unwrap(),
            },
            _ => ControlFlow::Continue { next: fall_through },
        }
    }

    fn push_operand(&self, s: &mut String, op: &Operand) {
        use Operand::*;
        match op {
            Imm8(v) => {
                let _ = write!(s, "#0x{v:02X}");
            }
            Direct(v) => {
                let _ = write!(s, "0x{v:02X}");
            }
            Extended(v) => {
                let _ = write!(s, "0x{v:04X}");
            }
            IndexedNo => s.push_str(",X"),
            Indexed8(v) => {
                let _ = write!(s, "0x{v:02X},X");
            }
            Indexed16(v) => {
                let _ = write!(s, "0x{v:04X},X");
            }
            Relative(_) => {
                let _ = write!(s, "0x{:04X}", self.target().unwrap_or(0));
            }
            // The bit number is an immediate in sdas6808 (`.6805`) syntax.
            Bit(n) => {
                let _ = write!(s, "#{n}");
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

/// Big-endian 16-bit read (high byte first, the 6805 convention).
#[inline]
fn be16(hi: u8, lo: u8) -> u16 {
    (u16::from(hi) << 8) | u16::from(lo)
}

/// Build the operands for `op`/`mode` from the (length-trimmed) `bytes`.
fn read_operands(op: u8, mode: AddrMode, bytes: &[u8]) -> Operands {
    let b1 = bytes.get(1).copied().unwrap_or(0);
    let b2 = bytes.get(2).copied().unwrap_or(0);
    // BSET/BCLR/BRSET/BRCLR encode the bit number in the low nibble.
    let bit = (op & 0x0E) >> 1;
    let mut operands = Operands::new();
    use AddrMode::*;
    match mode {
        Inherent => {}
        Immediate => operands.push(Operand::Imm8(b1)),
        Direct => operands.push(Operand::Direct(b1)),
        Extended => operands.push(Operand::Extended(be16(b1, b2))),
        Indexed0 => operands.push(Operand::IndexedNo),
        Indexed1 => operands.push(Operand::Indexed8(b1)),
        Indexed2 => operands.push(Operand::Indexed16(be16(b1, b2))),
        Relative => operands.push(Operand::Relative(b1 as i8)),
        BitDirect => {
            operands.push(Operand::Bit(bit));
            operands.push(Operand::Direct(b1));
        }
        BitRelative => {
            operands.push(Operand::Bit(bit));
            operands.push(Operand::Direct(b1));
            operands.push(Operand::Relative(b2 as i8));
        }
    }
    operands
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
    let operands = read_operands(op, info.mode, &ins_bytes[..len as usize]);
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
    (inh)  => { AddrMode::Inherent };
    (imm)  => { AddrMode::Immediate };
    (dir)  => { AddrMode::Direct };
    (ext)  => { AddrMode::Extended };
    (ix0)  => { AddrMode::Indexed0 };
    (ix1)  => { AddrMode::Indexed1 };
    (ix2)  => { AddrMode::Indexed2 };
    (rel)  => { AddrMode::Relative };
    (bdir) => { AddrMode::BitDirect };
    (brel) => { AddrMode::BitRelative };
}

/// The 6805 ISA, grouped by mnemonic. Each `0xNN mode` line binds one opcode
/// byte. Every byte not listed decodes as [`Mnemonic::Unknown`] (illegal).
macro_rules! isa {
    (
        $( $mnem:ident { $( $opcode:literal $m:ident ; )* } )*
    ) => {
        /// 6805 instruction mnemonics.
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
            let mut t = [OpInfo { mnemonic: Mnemonic::Unknown, mode: AddrMode::Inherent }; 256];
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
    // Bit test and branch (0x00-0x0F): bit number is in the low nibble.
    BRSET { 0x00 brel; 0x02 brel; 0x04 brel; 0x06 brel; 0x08 brel; 0x0A brel; 0x0C brel; 0x0E brel; }
    BRCLR { 0x01 brel; 0x03 brel; 0x05 brel; 0x07 brel; 0x09 brel; 0x0B brel; 0x0D brel; 0x0F brel; }
    // Bit set/clear (0x10-0x1F).
    BSET  { 0x10 bdir; 0x12 bdir; 0x14 bdir; 0x16 bdir; 0x18 bdir; 0x1A bdir; 0x1C bdir; 0x1E bdir; }
    BCLR  { 0x11 bdir; 0x13 bdir; 0x15 bdir; 0x17 bdir; 0x19 bdir; 0x1B bdir; 0x1D bdir; 0x1F bdir; }

    // Relative branches (0x20-0x2F).
    BRA  { 0x20 rel; }  BRN  { 0x21 rel; }  BHI  { 0x22 rel; }  BLS  { 0x23 rel; }
    BCC  { 0x24 rel; }  BCS  { 0x25 rel; }  BNE  { 0x26 rel; }  BEQ  { 0x27 rel; }
    BHCC { 0x28 rel; }  BHCS { 0x29 rel; }  BPL  { 0x2A rel; }  BMI  { 0x2B rel; }
    BMC  { 0x2C rel; }  BMS  { 0x2D rel; }  BIL  { 0x2E rel; }  BIH  { 0x2F rel; }

    // Read-modify-write on memory (direct 0x3x, indexed8 0x6x, indexed0 0x7x).
    NEG { 0x30 dir; 0x60 ix1; 0x70 ix0; }
    COM { 0x33 dir; 0x63 ix1; 0x73 ix0; }
    LSR { 0x34 dir; 0x64 ix1; 0x74 ix0; }
    ROR { 0x36 dir; 0x66 ix1; 0x76 ix0; }
    ASR { 0x37 dir; 0x67 ix1; 0x77 ix0; }
    LSL { 0x38 dir; 0x68 ix1; 0x78 ix0; }
    ROL { 0x39 dir; 0x69 ix1; 0x79 ix0; }
    DEC { 0x3A dir; 0x6A ix1; 0x7A ix0; }
    INC { 0x3C dir; 0x6C ix1; 0x7C ix0; }
    TST { 0x3D dir; 0x6D ix1; 0x7D ix0; }
    CLR { 0x3F dir; 0x6F ix1; 0x7F ix0; }

    // Read-modify-write on the accumulator (0x4x) and X (0x5x), MUL at 0x42.
    NEGA { 0x40 inh; }  MUL  { 0x42 inh; }  COMA { 0x43 inh; }  LSRA { 0x44 inh; }
    RORA { 0x46 inh; }  ASRA { 0x47 inh; }  LSLA { 0x48 inh; }  ROLA { 0x49 inh; }
    DECA { 0x4A inh; }  INCA { 0x4C inh; }  TSTA { 0x4D inh; }  CLRA { 0x4F inh; }
    NEGX { 0x50 inh; }  COMX { 0x53 inh; }  LSRX { 0x54 inh; }  RORX { 0x56 inh; }
    ASRX { 0x57 inh; }  LSLX { 0x58 inh; }  ROLX { 0x59 inh; }  DECX { 0x5A inh; }
    INCX { 0x5C inh; }  TSTX { 0x5D inh; }  CLRX { 0x5F inh; }

    // Control (0x80-0x9F).
    RTI  { 0x80 inh; }  RTS  { 0x81 inh; }  SWI  { 0x83 inh; }  STOP { 0x8E inh; }
    WAIT { 0x8F inh; }  TAX  { 0x97 inh; }  CLC  { 0x98 inh; }  SEC  { 0x99 inh; }
    CLI  { 0x9A inh; }  SEI  { 0x9B inh; }  RSP  { 0x9C inh; }  NOP  { 0x9D inh; }
    TXA  { 0x9F inh; }

    // ALU / load / store (imm 0xAx, dir 0xBx, ext 0xCx, ix2 0xDx, ix1 0xEx, ix0 0xFx).
    SUB { 0xA0 imm; 0xB0 dir; 0xC0 ext; 0xD0 ix2; 0xE0 ix1; 0xF0 ix0; }
    CMP { 0xA1 imm; 0xB1 dir; 0xC1 ext; 0xD1 ix2; 0xE1 ix1; 0xF1 ix0; }
    SBC { 0xA2 imm; 0xB2 dir; 0xC2 ext; 0xD2 ix2; 0xE2 ix1; 0xF2 ix0; }
    CPX { 0xA3 imm; 0xB3 dir; 0xC3 ext; 0xD3 ix2; 0xE3 ix1; 0xF3 ix0; }
    AND { 0xA4 imm; 0xB4 dir; 0xC4 ext; 0xD4 ix2; 0xE4 ix1; 0xF4 ix0; }
    BIT { 0xA5 imm; 0xB5 dir; 0xC5 ext; 0xD5 ix2; 0xE5 ix1; 0xF5 ix0; }
    LDA { 0xA6 imm; 0xB6 dir; 0xC6 ext; 0xD6 ix2; 0xE6 ix1; 0xF6 ix0; }
    STA {           0xB7 dir; 0xC7 ext; 0xD7 ix2; 0xE7 ix1; 0xF7 ix0; }
    EOR { 0xA8 imm; 0xB8 dir; 0xC8 ext; 0xD8 ix2; 0xE8 ix1; 0xF8 ix0; }
    ADC { 0xA9 imm; 0xB9 dir; 0xC9 ext; 0xD9 ix2; 0xE9 ix1; 0xF9 ix0; }
    ORA { 0xAA imm; 0xBA dir; 0xCA ext; 0xDA ix2; 0xEA ix1; 0xFA ix0; }
    ADD { 0xAB imm; 0xBB dir; 0xCB ext; 0xDB ix2; 0xEB ix1; 0xFB ix0; }
    JMP {           0xBC dir; 0xCC ext; 0xDC ix2; 0xEC ix1; 0xFC ix0; }
    JSR {           0xBD dir; 0xCD ext; 0xDD ix2; 0xED ix1; 0xFD ix0; }
    LDX { 0xAE imm; 0xBE dir; 0xCE ext; 0xDE ix2; 0xEE ix1; 0xFE ix0; }
    STX {           0xBF dir; 0xCF ext; 0xDF ix2; 0xEF ix1; 0xFF ix0; }
    BSR { 0xAD rel; }
}
