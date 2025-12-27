//! Various memory mapper implementations for the i8051 microcontroller.

use crate::{CpuView, MemoryMapper, ReadOnlyMemoryMapper};

pub struct RAM {
    ram: [u8; 0x10000],
}

impl MemoryMapper for RAM {
    type WriteValue = (u16, u8);
    fn len(&self) -> u32 {
        0x10000
    }
    fn read<C: CpuView>(&self, _cpu: &C, addr: u32) -> u8 {
        self.ram[addr as usize]
    }
    fn prepare_write<C: CpuView>(&self, _cpu: &C, addr: u32, value: u8) -> Self::WriteValue {
        (addr as u16, value)
    }
    fn write(&mut self, (addr, value): Self::WriteValue) {
        self.ram[addr as usize] = value;
    }
}

impl Default for RAM {
    fn default() -> Self {
        Self::new()
    }
}

impl RAM {
    pub fn new() -> Self {
        Self { ram: [0; 0x10000] }
    }
    pub fn read(&self, addr: u16) -> u8 {
        self.ram[addr as usize]
    }
    pub fn write(&mut self, addr: u16, value: u8) {
        self.ram[addr as usize] = value;
    }
}

pub struct ROM {
    rom: Vec<u8>,
}

impl ROM {
    pub fn new(contents: Vec<u8>) -> Self {
        Self { rom: contents }
    }
}

impl ReadOnlyMemoryMapper for ROM {
    fn len(&self) -> u32 {
        self.rom.len() as u32
    }
    fn read<C: CpuView>(&self, _cpu: &C, addr: u32) -> u8 {
        if addr >= self.rom.len() as u32 {
            return 0;
        }
        self.rom[addr as usize]
    }
}
