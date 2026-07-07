# Testbed sample ROMs

These are the inputs for the multi-CPU testbed (`cargo run --example disasm`) and
the `tests/testbed.rs` regression check.

| File | CPU | Entry | Provenance | SHA256 |
|------|-----|-------|------------|--------|
| `lk201-8051.bin` | `i8051` | `0x0` | DEC LK201 keyboard, 8051 variant (part `23-004M2-00`). | a3b86875773dacad040b21d51a542baa333d9e3aa9af798332197c33d651f160 |
| `6502_functional_test.bin` | `mos6502` | `0x400` | [Klaus Dormann 6502 functional test](https://github.com/Klaus2m5/6502_65C02_functional_tests) (`bin_files/6502_functional_test.bin`). | fa12bfc761e6f9057e4cc01a665a7b800ff01ae91f598af1e39a1201d01953fd |
| `lk201-6805.bin` | `m6805` | `0x100` | DEC LK201 keyboard, "green LED" 6805 variant (part `23-001s9-00`). | cab9eba8c9d2fe121b0d80b8a92314e9732726ed43a8f7732e8d8b347995906c |

Run one directly:

```sh
cargo run --example disasm -- m6805 crates/i8051-disassembler/samples/lk201-6805.bin 0x100
```
