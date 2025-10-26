# i8051 Emulator

This crate provides a fast, `unsafe`-free emulator for the i8051
microcontroller.

The 8051 (a.k.a. the MCS-51) is a 8-bit microcontroller that was very common in
the 1980s and 1990s, and is still used in many modern devices.

There are a number of different variants of the 8051, and this crate supports a
CPU like the original 8051, but with a zero-flag extension.

## Usage

```rust
use i8051::Cpu;

let mut cpu = Cpu::new();
let mut ram = RAM::new();
let mut code = ROM::new(fs::read(&args.rom_file).unwrap());
let mut ports = Ports { ram: [0; 128] };

loop {
    if !cpu.step(&mut ram, &mut code, &mut ports) {
        break;
    }
}
```

## Debugging

The [`i8051-debug-tui`](https://crates.io/crates/i8051-debug-tui) crate provides
a terminal UI debugger for the i8051 emulator.

## Supported Instructions

The [instruction set](https://docs.rs/i8051/latest/i8051/ops/index.html) is from
the standard MCS-51.
