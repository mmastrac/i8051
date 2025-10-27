use std::cell::RefCell;
use std::sync::mpsc;

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

#[derive(Debug, Default)]
pub struct Timer {
    tcon: u8,
    tmod: u8,
    th0: u8,
    tl0: u8,
    th1: u8,
    tl1: u8,
}

impl Timer {
    pub fn prepare_tick(&self, cpu: &mut Cpu, ctx: &impl CpuContext) -> (bool, bool) {
        let tr0 = (self.tcon & (1 << 4)) != 0;
        let tr1 = (self.tcon & (1 << 6)) != 0;
        let mut res = (false, false);

        if tr0 {
            let gate0 = (self.tmod & 0x4) != 0;
            if !gate0 || cpu.sfr(SFR_P3, ctx) & (1 << 0) != 0 {
                res.0 = true;
            }
        }

        if tr1 {
            let gate1 = (self.tmod & 0x40) != 0;
            if !gate1 || cpu.sfr(SFR_P3, ctx) & (1 << 0) != 0 {
                res.1 = true;
            }
        }

        res
    }

    pub fn tick(&mut self, cpu: &mut Cpu, (t0, t1): (bool, bool)) {
        if t0 {
            match self.tmod & 0x03 {
                1 => {
                    self.tl0 = self.tl0.wrapping_add(1);
                    if self.tl0 == 0 {
                        self.th0 = self.th0.wrapping_add(1);
                    }
                    if self.th0 == 0 && self.tl0 == 0 {
                        self.tcon |= 1 << 5;
                        cpu.interrupt(Interrupt::Timer0);
                    }
                }
                _ => {}
            }
        }

        if t1 {
            match (self.tmod & 0x0c) >> 4 {
                1 => {
                    self.tl1 = self.tl1.wrapping_add(1);
                    if self.tl1 == 0 {
                        self.th1 = self.th1.wrapping_add(1);
                    }
                    if self.th1 == 0 && self.tl1 == 0 {
                        self.tcon |= 1 << 7;
                        cpu.interrupt(Interrupt::Timer1);
                    }
                }
                _ => {}
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
