# i8051 Emulator

This crate provides an emulator for the i8051 microcontroller.

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
