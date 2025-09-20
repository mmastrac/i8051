use std::fs;
use std::path::PathBuf;

use i8051::memory::{RAM, ROM};
use i8051::{Cpu, MemoryMapper};
use i8051::{PortMapper, sfr::*};

use clap::Parser;

#[derive(Parser)]
struct Args {
    /// ROM file to load
    #[arg(value_name = "ROM_FILE")]
    rom_file: PathBuf,

    /// Maximum number of instructions to execute
    #[arg(short, long, default_value = "1000")]
    max_instructions: u64,

    /// Enable trace output
    #[arg(short, long)]
    trace: bool,
}

struct Ports {
    ram: [u8; 128],
}

impl PortMapper for Ports {
    fn read(&self, addr: u8) -> u8 {
        println!("PORT read {:02X}", addr);
        self.ram[addr as usize - 128]
    }
    fn write(&mut self, addr: u8, value: u8) {
        println!("PORT write {:02X} = {:02X}", addr, value);
        self.ram[addr as usize - 128] = value;
    }
    fn read_latch(&self, addr: u8) -> u8 {
        println!("PORT read latch {:02X}", addr);
        self.ram[addr as usize - 128]
    }
}

pub fn main() {
    let args = Args::parse();

    let mut cpu = Cpu::new();

    let mut ram = RAM::new();
    let mut code = ROM::new(fs::read(&args.rom_file).unwrap());
    let mut ports = Ports { ram: [0; 128] };

    let mut instruction_count = 0;
    loop {
        let (bytes, mnemonic) = cpu.decode_pc(&mut code);
        if args.trace {
            println!(
                "{pc:04X}: {:10} {mnemonic}",
                bytes
                    .iter()
                    .map(|b| format!("{:02X}", b))
                    .collect::<Vec<_>>()
                    .join(" "),
                pc = cpu.pc
            );
            println!(
                "  A={:02X?}  B={:02X?}  DPTR={:04X?}  C={} OV={} AC={} Z={}",
                cpu.a(),
                cpu.b(),
                cpu.dptr(),
                cpu.psw(PSW_C),
                cpu.psw(PSW_OV),
                cpu.psw(PSW_AC),
                cpu.psw(PSW_Z)
            );
            print!("  ");
            for i in 0..8 {
                print!("R{}={:02X?} ", i, cpu.r(i));
            }
            println!();
        }
        instruction_count += 1;
        if !cpu.step(&mut ram, &mut code, &mut ports) {
            println!(
                "CPU halted at 0x{:04X} after {} instructions",
                cpu.pc, instruction_count
            );
            break;
        }
        if instruction_count >= args.max_instructions {
            println!(
                "CPU halted at 0x{:04X} after {} instructions",
                cpu.pc, instruction_count
            );
            break;
        }
    }

    println!(
        "  A={:02X?}  B={:02X?}  DPTR={:04X?}  C={} OV={} AC={} Z={}",
        cpu.a(),
        cpu.b(),
        cpu.dptr(),
        cpu.psw(PSW_C),
        cpu.psw(PSW_OV),
        cpu.psw(PSW_AC),
        cpu.psw(PSW_Z)
    );
    print!("  ");
    for i in 0..8 {
        print!("R{}={:02X?} ", i, cpu.r(i));
    }
    println!();

    println!(
        "MEM: {:02X} {:02X} {:02X} {:02X}",
        ram.read(0x8000),
        ram.read(0x8001),
        ram.read(0x8002),
        ram.read(0x8003)
    );
}
