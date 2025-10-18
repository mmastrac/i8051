use std::sync::mpsc;

use crate::{
    PortMapper,
    sfr::{SFR_SBUF, SFR_SCON},
};

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
    sbuf_read: Option<u8>,
    sbuf_write: Option<u8>,
    recv_tick_delay: u16,
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
                sbuf_read: None,
                sbuf_write: None,
                scon: 1 << 4,
                recv_tick_delay: 0,
                send_tick_delay: 0,
            },
            in_tx,
            out_rx,
        )
    }
}

impl PortMapper for Serial {
    fn interest(&self, addr: u8) -> bool {
        addr == SFR_SCON || addr == SFR_SBUF
    }
    fn read(&mut self, addr: u8) -> u8 {
        match addr {
            SFR_SCON => {
                return self.scon;
            }
            SFR_SBUF => {
                if let Some(value) = self.sbuf_read.take() {
                    self.recv_tick_delay = 0;
                    return value;
                }
                return 0;
            }
            _ => {
                unreachable!()
            }
        }
    }
    fn write(&mut self, addr: u8, value: u8) {
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
    fn tick(&mut self) {
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
            self.recv_tick_delay = self.recv_tick_delay.wrapping_add(1);
            if self.recv_tick_delay >= 20 {
                self.recv_tick_delay = 0;
                if let Ok(value) = self.input_queue.try_recv() {
                    self.sbuf_read = Some(value);
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
