use std::sync::mpsc;

use tracing::{trace, warn};

use crate::{Cpu, CpuContext, sfr::*};
use crate::{CpuView, PortMapper};

/// A serial port peripheral using `SCON` and `SBUF` that emulates UART
/// communication with configurable baud rate timing.
///
/// SCON (Serial Control Register):
///
/// `[SM0, SM1, SM2, REN, TB8, RB8, TI, RI]`
///
///  - SM0: Serial mode bit 0
///  - SM1: Serial mode bit 1
///  - SM2: Serial mode 2 (multiprocessor communication)
///  - REN: Receive enable
///  - TB8: Transmit bit 8 (9th data bit)
///  - RB8: Receive bit 8 (9th data bit)
///  - TI: Transmit interrupt flag
///  - RI: Receive interrupt flag
///
/// Modes:
///  - Mode 0 (SM0=0, SM1=0): Shift register, fixed baud rate
///  - Mode 1 (SM0=0, SM1=1): 8-bit UART, variable baud rate (most common)
///  - Mode 2 (SM0=1, SM1=0): 9-bit UART, fixed baud rate
///  - Mode 3 (SM0=1, SM1=1): 9-bit UART, variable baud rate
pub struct Serial {
    input_queue: mpsc::Receiver<u8>,
    send_queue: mpsc::Sender<u8>,

    /// The byte in the process of being latched
    sbuf_pending_read: u8,
    /// The tick count before we present it in SBUF
    recv_tick_count: u16,
    /// The byte that can be read when RI is set
    sbuf_read: u8,

    /// The byte in the process of being sent
    sbuf_pending_write: u8,
    /// The tick count before we set TI
    send_tick_count: u16,
    /// The send double-buffer
    sbuf_send_double_buffer: Option<u8>,

    /// The number of CPU ticks per byte send/recv
    baud_rate_ticks: u16,

    /// The serial control register
    scon: u8,
}

pub const SCON_SM0: u8 = 1 << 7;
pub const SCON_SM1: u8 = 1 << 6;
pub const SCON_SM2: u8 = 1 << 5;
pub const SCON_REN: u8 = 1 << 4;
pub const SCON_TB8: u8 = 1 << 3;
pub const SCON_RB8: u8 = 1 << 2;
pub const SCON_TI: u8 = 1 << 1;
pub const SCON_RI: u8 = 1 << 0;

impl Serial {
    /// Creates a new `Serial` peripheral with the specified baud rate in CPU
    /// ticks.
    ///
    /// `baud_rate_ticks`  is the number of CPU ticks per bit. For example, with
    /// an 11.0592 MHz crystal and 9600 baud, this would be approximately
    /// `(11059200 / 12 / 9600) ≈ 96` ticks.
    pub fn new(baud_rate_ticks: u16) -> (Self, mpsc::Sender<u8>, mpsc::Receiver<u8>) {
        let (in_tx, in_rx) = mpsc::channel();
        let (out_tx, out_rx) = mpsc::channel();
        (
            Self {
                input_queue: in_rx,
                send_queue: out_tx,
                sbuf_pending_read: 0,
                sbuf_pending_write: 0,
                sbuf_read: 0,
                scon: 0,
                recv_tick_count: 0,
                send_tick_count: 0,
                baud_rate_ticks,
                sbuf_send_double_buffer: None,
            },
            in_tx,
            out_rx,
        )
    }

    pub fn tick(&mut self, cpu: &mut Cpu) {
        // Handle transmission
        if self.send_tick_count > 0 {
            self.send_tick_count -= 1;
            if self.send_tick_count == 0 {
                _ = self.send_queue.send(self.sbuf_pending_write);
                trace!(
                    "Serial: TX complete {:02X}, set TI",
                    self.sbuf_pending_write
                );
                self.sbuf_pending_write = 0;
                self.scon |= SCON_TI;

                // If there's a double-buffered value, start transmission again
                if let Some(value) = self.sbuf_send_double_buffer.take() {
                    trace!("Serial: TX sending buffered value {:02X}", value);
                    self.sbuf_pending_write = value;
                    self.send_tick_count = self.baud_rate_ticks * bits_per_frame(self.scon);
                }
            }
        }

        // Handle reception (only if REN is set)
        if self.recv_tick_count > 0 {
            self.recv_tick_count -= 1;
            if self.recv_tick_count == 0 {
                if (self.scon & SCON_REN) != 0 {
                    if self.scon & SCON_RI == 0 {
                        self.sbuf_read = self.sbuf_pending_read;
                        self.scon |= SCON_RI;
                        trace!("Serial: RX complete {:02X}, set RI", self.sbuf_read);
                    } else {
                        trace!("Serial: RX ignored, RI is already set");
                    }
                } else {
                    trace!("Serial: RX ignored, REN is not set");
                }
            }
        }

        if self.recv_tick_count == 0
            && let Ok(value) = self.input_queue.try_recv()
        {
            trace!("Serial: RX started {:02X}", value);
            self.sbuf_pending_read = value;
            self.recv_tick_count = self.baud_rate_ticks * bits_per_frame(self.scon);
        }
    }
}

fn bits_per_frame(scon: u8) -> u16 {
    let sm0 = (scon & SCON_SM0) != 0;
    let sm1 = (scon & SCON_SM1) != 0;
    match (sm0, sm1) {
        (false, false) => 8, // Mode 0: shift 8 bits (no start/stop) — we still model as 8 bits
        (false, true) => 10, // Mode 1: start + 8 + stop
        (true, false) => 9, // Mode 2: 9 data (incl TB8/RB8) + fixed rate (we emulate timing anyway)
        (true, true) => 11, // Mode 3: start + 9 + stop
    }
}

impl PortMapper for Serial {
    type WriteValue = (u8, u8);

    fn interest<C: CpuView>(&self, _cpu: &C, addr: u8) -> bool {
        addr == SFR_SCON || addr == SFR_SBUF
    }

    fn read<C: CpuView>(&self, _cpu: &C, addr: u8) -> u8 {
        match addr {
            SFR_SCON => self.scon,
            SFR_SBUF => {
                trace!("Serial: SBUF read by CPU: {:02X}", self.sbuf_read);
                self.sbuf_read
            }
            _ => unreachable!(),
        }
    }

    fn prepare_write<C: CpuView>(&self, _cpu: &C, addr: u8, value: u8) -> Self::WriteValue {
        (addr, value)
    }

    fn write(&mut self, (addr, value): Self::WriteValue) {
        match addr {
            SFR_SCON => {
                trace!("Serial: SCON write {:02X}", value);
                self.scon = value;
            }
            SFR_SBUF => {
                if self.send_tick_count == 0 {
                    // Writing to SBUF starts transmission
                    let ticks = self.baud_rate_ticks * bits_per_frame(self.scon);
                    trace!(
                        "Serial: SBUF write, TX started {:02X} ({} ticks)",
                        value, ticks
                    );
                    self.send_tick_count = ticks;
                    self.sbuf_pending_write = value;
                } else {
                    if self.sbuf_send_double_buffer.is_some() {
                        trace!("Serial: SBUF write, TX double-buffer lost! {:02X}", value);
                    } else {
                        trace!("Serial: SBUF write, TX double-buffered {:02X}", value);
                    }
                    self.sbuf_send_double_buffer = Some(value);
                }
            }
            _ => unreachable!(),
        }
    }
}

pub const P3_RXD: u8 = 1 << 0;
pub const P3_TXD: u8 = 1 << 1;
pub const P3_INT0: u8 = 1 << 2;
pub const P3_INT1: u8 = 1 << 3;
pub const P3_T0: u8 = 1 << 4;
pub const P3_T1: u8 = 1 << 5;
pub const P3_WR: u8 = 1 << 6;
pub const P3_RD: u8 = 1 << 7;

pub const TCON_TF1: u8 = 1 << 7;
pub const TCON_TR1: u8 = 1 << 6;
pub const TCON_TF0: u8 = 1 << 5;
pub const TCON_TR0: u8 = 1 << 4;
pub const TCON_IE1: u8 = 1 << 3;
pub const TCON_IT1: u8 = 1 << 2;
pub const TCON_IE0: u8 = 1 << 1;
pub const TCON_IT0: u8 = 1 << 0;

pub const TMOD_GATE1: u8 = 1 << 7;
pub const TMOD_C_T1: u8 = 1 << 6;
pub const TMOD_M11: u8 = 1 << 5;
pub const TMOD_M10: u8 = 1 << 4;
pub const TMOD_GATE0: u8 = 1 << 3;
pub const TMOD_C_T0: u8 = 1 << 2;
pub const TMOD_M01: u8 = 1 << 1;
pub const TMOD_M00: u8 = 1 << 0;

/// A timer peripheral that emulates the timer 0 and timer 1 registers and
/// interrupts.
///
/// TCON:
///
/// `[TF0, TR0, TF1, TR1, IE0, IT0, IE1, IT1]`
///
///  - `TF0`: Timer 0 overflow flag
///  - `TR0`: Timer 0 run control
///  - `TF1`: Timer 1 overflow flag
///  - `TR1`: Timer 1 run control
///  - `IE0`: External interrupt 0 enable
///  - `IT0`: External interrupt 0 type
///  - `IE1`: External interrupt 1 enable
///  - `IT1`: External interrupt 1 type
///
/// TMOD:
///
/// `[GATE0, C/T0, M1, M0, GATE1, C/T1, M3, M2]`
///
///  - `GATE0`: Timer 0 gate
///  - `C`/`T0`: Timer 0 counter/timer mode
///  - `M1`: Timer 0 mode 1
///  - `M0`: Timer 0 mode 0
///  - `GATE1`: Timer 1 gate
///  - `C`/`T1`: Timer 1 counter/timer mode
///  - `M3`: Timer 1 mode 3
///  - `M2`: Timer 1 mode 2
#[derive(Debug, Default)]
pub struct Timer {
    tcon: u8,
    tmod: u8,
    th0: u8,
    tl0: u8,
    th1: u8,
    tl1: u8,

    prev_p3: u8,
    warned_timer: bool,
}

pub struct TimerTick {
    tick_t0: bool,
    tick_t1: bool,
    p3: u8,
}

impl Timer {
    pub fn prepare_tick(&self, cpu: &mut Cpu, ctx: &impl CpuContext) -> TimerTick {
        let tr0 = (self.tcon & TCON_TR0) != 0;
        let tc0 = (self.tmod & TMOD_C_T0) != 0;
        let gate0 = (self.tmod & TMOD_GATE0) != 0;

        let tr1 = (self.tcon & TCON_TR1) != 0;
        let tc1 = (self.tmod & TMOD_C_T1) != 0;
        let gate1 = (self.tmod & TMOD_GATE1) != 0;

        // Read P3 iff a timer is enabled AND is in counter mode OR is gated by INT0/1,
        // otherwise use historical pin history.
        let needs_p3 = (tr0 && (tc0 || gate0)) || (tr1 && (tc1 || gate1));
        let p3 = if needs_p3 {
            cpu.sfr(SFR_P3, ctx)
        } else {
            self.prev_p3
        };
        let int0 = (p3 & P3_INT0) != 0;
        let int1 = (p3 & P3_INT1) != 0;
        let t0 = (p3 & P3_T0) != 0;
        let t1 = (p3 & P3_T1) != 0;

        let mut res = TimerTick {
            tick_t0: false,
            tick_t1: false,
            p3,
        };

        if tr0 && (!gate0 || int0) {
            if tc0 {
                res.tick_t0 = self.prev_p3 & P3_T0 != 0 && !t0; // falling edge
            } else {
                res.tick_t0 = true;
            }
        }

        if tr1 && (!gate1 || int1) {
            if tc1 {
                res.tick_t1 = self.prev_p3 & P3_T1 != 0 && !t1; // falling edge
            } else {
                res.tick_t1 = true;
            }
        }

        res
    }

    pub fn tick(&mut self, cpu: &mut Cpu, tick: TimerTick) {
        self.prev_p3 = tick.p3;

        if tick.tick_t0 {
            match self.tmod & 0x03 {
                1 => {
                    self.tl0 = self.tl0.wrapping_add(1);
                    if self.tl0 == 0 {
                        self.th0 = self.th0.wrapping_add(1);
                    }
                    if self.th0 == 0 && self.tl0 == 0 {
                        self.tcon |= TCON_TF0;
                    }
                }
                2 => {
                    // 8-bit auto-reload
                    self.tl0 = self.tl0.wrapping_add(1);
                    if self.tl0 == 0 {
                        self.tl0 = self.th0;
                        self.tcon |= TCON_TF0;
                    }
                }
                mode => {
                    if !self.warned_timer {
                        warn!("Timer 0: Timer mode {mode} not supported");
                        self.warned_timer = true;
                    }
                }
            }
        }

        if tick.tick_t1 {
            match (self.tmod & 0x30) >> 4 {
                1 => {
                    self.tl1 = self.tl1.wrapping_add(1);
                    if self.tl1 == 0 {
                        self.th1 = self.th1.wrapping_add(1);
                    }
                    if self.th1 == 0 && self.tl1 == 0 {
                        self.tcon |= TCON_TF1;
                    }
                }
                2 => {
                    // 8-bit auto-reload
                    self.tl1 = self.tl1.wrapping_add(1);
                    if self.tl1 == 0 {
                        self.tl1 = self.th1;
                        self.tcon |= TCON_TF1;
                    }
                }
                mode => {
                    if !self.warned_timer {
                        warn!("Timer 1: Timer mode {mode} not supported");
                        self.warned_timer = true;
                    }
                }
            }
        }
    }
}

impl PortMapper for Timer {
    type WriteValue = (u8, u8);
    fn interest<C: CpuView>(&self, _cpu: &C, addr: u8) -> bool {
        addr == SFR_TCON
            || addr == SFR_TMOD
            || addr == SFR_TH0
            || addr == SFR_TL0
            || addr == SFR_TH1
            || addr == SFR_TL1
    }
    fn read<C: CpuView>(&self, _cpu: &C, addr: u8) -> u8 {
        match addr {
            SFR_TCON => self.tcon,
            SFR_TMOD => self.tmod,
            SFR_TH0 => self.th0,
            SFR_TL0 => self.tl0,
            SFR_TH1 => self.th1,
            SFR_TL1 => self.tl1,
            _ => {
                unreachable!()
            }
        }
    }
    fn prepare_write<C: CpuView>(&self, _cpu: &C, addr: u8, value: u8) -> Self::WriteValue {
        (addr, value)
    }
    fn write(&mut self, (addr, value): Self::WriteValue) {
        match addr {
            SFR_TCON => {
                self.tcon = value;
            }
            SFR_TMOD => {
                if self.tmod != value {
                    trace!("Timer mode changed: {:02X} -> {:02X}", self.tmod, value);
                }
                self.tmod = value;
            }
            SFR_TH0 => {
                self.th0 = value;
            }
            SFR_TL0 => {
                self.tl0 = value;
            }
            SFR_TH1 => {
                self.th1 = value;
            }
            SFR_TL1 => {
                self.tl1 = value;
            }
            _ => {
                unreachable!()
            }
        }
    }
}
