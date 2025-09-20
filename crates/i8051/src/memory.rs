use crate::cpu::MemoryMapper;

pub struct RAM {
    ram: [u8; 0x10000],
}

impl MemoryMapper for RAM {
    fn read(&self, addr: u16) -> u8 {
        self.ram[addr as usize]
    }
    fn write(&mut self, addr: u16, value: u8) {
        self.ram[addr as usize] = value;
    }
}

impl RAM {
    pub fn new() -> Self {
        Self { ram: [0; 0x10000] }
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

impl MemoryMapper for ROM {
    fn read(&self, addr: u16) -> u8 {
        self.rom[addr as usize]
    }
    fn write(&mut self, _: u16, _: u8) {
        // do nothing
    }
}
