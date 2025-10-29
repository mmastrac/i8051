//! SFR (special-function register) addresses for the i8051 microcontroller.

/// Base address for SFRs: 0x80
pub const SFR_BASE: u8 = 0x80;
/// Port 0 register: 8-bit bidirectional I/O port
pub const SFR_P0: u8 = 0x80;
/// Stack pointer register: points to the top of the stack in internal RAM
pub const SFR_SP: u8 = 0x81;
/// Data pointer low byte: low byte of the 16-bit data pointer (DPTR)
pub const SFR_DPL: u8 = 0x82;
/// Data pointer high byte: high byte of the 16-bit data pointer (DPTR)
pub const SFR_DPH: u8 = 0x83;
/// Power control register: controls power management features
pub const SFR_PCON: u8 = 0x87;
/// Timer/counter control register: controls timer 0 and timer 1 operation
pub const SFR_TCON: u8 = 0x88;
/// Timer/counter mode register: sets operating modes for timer 0 and timer 1
pub const SFR_TMOD: u8 = 0x89;
/// Timer 0 low byte: low byte of the 16-bit timer 0 counter
pub const SFR_TL0: u8 = 0x8A;
/// Timer 1 low byte: low byte of the 16-bit timer 1 counter
pub const SFR_TL1: u8 = 0x8B;
/// Timer 0 high byte: high byte of the 16-bit timer 0 counter
pub const SFR_TH0: u8 = 0x8C;
/// Timer 1 high byte: high byte of the 16-bit timer 1 counter
pub const SFR_TH1: u8 = 0x8D;
/// Port 1 register: 8-bit bidirectional I/O port
pub const SFR_P1: u8 = 0x90;
/// Serial control register: controls serial port operation
pub const SFR_SCON: u8 = 0x98;
/// Serial buffer register: holds data for serial transmission/reception
pub const SFR_SBUF: u8 = 0x99;
/// Port 2 register: 8-bit bidirectional I/O port
pub const SFR_P2: u8 = 0xA0;
/// Interrupt enable register: enables/disables individual interrupt sources
pub const SFR_IE: u8 = 0xA8;
/// Port 3 register: 8-bit bidirectional I/O port
pub const SFR_P3: u8 = 0xB0;
/// Interrupt priority register: sets priority levels for interrupt sources
pub const SFR_IP: u8 = 0xB8;
/// Timer 2 control register: controls timer 2 operation
pub const SFR_T2CON: u8 = 0xC8;
/// Timer 2 mode register: sets operating mode for timer 2
pub const SFR_T2MOD: u8 = 0xC9;
/// Timer 2 capture/reload low byte: low byte for capture/reload operations
pub const SFR_RCAP2L: u8 = 0xCA;
/// Timer 2 capture/reload high byte: high byte for capture/reload operations
pub const SFR_RCAP2H: u8 = 0xCB;
/// Timer 2 low byte: low byte of the 16-bit timer 2 counter
pub const SFR_TL2: u8 = 0xCC;
/// Timer 2 high byte: high byte of the 16-bit timer 2 counter
pub const SFR_TH2: u8 = 0xCD;
/// Program status word: contains processor status flags
pub const SFR_PSW: u8 = 0xD0;
/// Accumulator register: primary working register for arithmetic operations
pub const SFR_A: u8 = 0xE0;
/// B register: secondary working register used in multiplication/division
pub const SFR_B: u8 = 0xF0;

/// Carry flag: indicates carry from/to the most significant bit
pub const PSW_C: u8 = 0x07;
/// Auxiliary carry flag: indicates carry from/to the fourth bit
pub const PSW_AC: u8 = 0x06;
/// User-defined flag 0
pub const PSW_F0: u8 = 0x05;
/// Register bank select 1: selects between four 8-bit register banks
pub const PSW_RS1: u8 = 0x04;
/// Register bank select 0: selects between four 8-bit register banks
pub const PSW_RS0: u8 = 0x03;
/// Overflow flag: indicates overflow from/to the most significant bit
pub const PSW_OV: u8 = 0x02;
/// Reserved bit: reserved for future use
pub const PSW_RES: u8 = 0x01;
/// Parity flag: indicates even/odd number of 1s in the accumulator
pub const PSW_P: u8 = 0x00;
