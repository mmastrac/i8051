use divan::Bencher;
use i8051::{Cpu, Instruction, memory::ROM};

#[divan::bench]
fn decode() -> Instruction {
    let bytes = [0x02, 0x11, 0x22];
    Instruction::decode_from_bytes(0x1000, &bytes)
}

#[divan::bench]
fn execute_1000(bencher: Bencher) {
    let program = &[
        0x24, 0x01, // add A, 1
        0xC0, 0xE0, // push A
        0xD0, 0xE0, // pop A
        0x80, 0xF8, // sjmp 0x1000
    ];
    let rom = program.to_vec();
    let mut ctx = ((), (), ROM::new(rom));
    let mut cpu = Cpu::new();

    bencher.bench_local(|| {
        for _ in 0..1000 {
            cpu.step(&mut ctx);
        }
    });
}

fn main() {
    // Run registered benchmarks.
    divan::main();
}
