use std::cell::RefCell;
use std::sync::mpsc;

use tracing::{trace, warn};

use crate::{Cpu, CpuContext, Interrupt, sfr::*};
use crate::{CpuView, PortMapper};

/// A basic serial port peripheral using `SCON` and `SBUF` that emulates a
/// delayed send/receive.
///
/// SCON:
///
/// `[SM0, SM1, SM2, REN, TB8, RB8, TI, RI]`
///
///  - SM0: Serial mode 0
///  - SM1: Serial mode 1
///  - SM2: Serial mode 2
///  - REN: Receive enable
///  - TB8: Transmit bit 8
///  - RB8: Receive bit 8
///  - TI: Transmit interrupt
///  - RI: Receive interrupt
pub struct Serial {
    input_queue: mpsc::Receiver<u8>,
    send_queue: mpsc::Sender<u8>,
    sbuf_read: RefCell<Option<u8>>,
    sbuf_write: Option<u8>,
    recv_tick_delay: RefCell<u16>,
    send_tick_delay: u16,
    scon: u8,
}

impl Serial {
    pub fn new() -> (Self, mpsc::Sender<u8>, mpsc::Receiver<u8>) {
        let (in_tx, in_rx) = mpsc::channel();
        let (out_tx, out_rx) = mpsc::channel();
        (
            Self {
                input_queue: in_rx,
                send_queue: out_tx,
                sbuf_read: RefCell::new(None),
                sbuf_write: None,
                scon: 1 << 4,
                recv_tick_delay: RefCell::new(0),
                send_tick_delay: 0,
            },
            in_tx,
            out_rx,
        )
    }

    pub fn tick(&mut self) {
        // This may delay loopback by one tick
        if let Some(value) = self.sbuf_write {
            self.send_tick_delay = self.send_tick_delay.wrapping_add(1);
            if self.send_tick_delay >= 10 {
                self.send_tick_delay = 0;
                if let Ok(()) = self.send_queue.send(value) {
                    self.sbuf_write = None;
                    self.scon |= 1 << 1;
                    // println!("Serial: Wrote {:02X}, set TI SCON={:02X}", value, self.scon);
                }
            }
        }
        if self.scon & (1 << 4) != 0 {
            let mut delay = self.recv_tick_delay.borrow_mut();
            *delay = delay.wrapping_add(1);
            if *delay >= 20 {
                *delay = 0;
                if let Ok(value) = self.input_queue.try_recv() {
                    *self.sbuf_read.borrow_mut() = Some(value);
                    self.scon |= 1 << 0;
                    // println!(
                    // "Serial: Got value {:02X}, set RI SCON={:02X}",
                    // value, self.scon
                    // );
                }
            }
        }
    }
}

impl PortMapper for Serial {
    type WriteValue = (u8, u8);
    fn interest<C: CpuView>(&self, cpu: &C, addr: u8) -> bool {
        addr == SFR_SCON || addr == SFR_SBUF
    }
    fn read<C: CpuView>(&self, cpu: &C, addr: u8) -> u8 {
        match addr {
            SFR_SCON => {
                return self.scon;
            }
            SFR_SBUF => {
                if let Some(value) = self.sbuf_read.borrow_mut().take() {
                    *self.recv_tick_delay.borrow_mut() = 0;
                    return value;
                }
                return 0;
            }
            _ => {
                unreachable!()
            }
        }
    }
    fn prepare_write<C: CpuView>(&self, cpu: &C, addr: u8, value: u8) -> Self::WriteValue {
        (addr, value)
    }
    fn write(&mut self, (addr, value): Self::WriteValue) {
        match addr {
            SFR_SCON => {
                // println!("Serial: Set SCON {:02X}", value);
                self.scon = value;
            }
            SFR_SBUF => {
                self.sbuf_write = Some(value);
                self.send_tick_delay = 0;
            }
            _ => {
                unreachable!()
            }
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
                        cpu.interrupt(Interrupt::Timer0);
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
                        cpu.interrupt(Interrupt::Timer1);
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
            SFR_TCON => {
                return self.tcon;
            }
            SFR_TMOD => {
                return self.tmod;
            }
            SFR_TH0 => {
                return self.th0;
            }
            SFR_TL0 => {
                return self.tl0;
            }
            SFR_TH1 => {
                return self.th1;
            }
            SFR_TL1 => {
                return self.tl1;
            }
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
