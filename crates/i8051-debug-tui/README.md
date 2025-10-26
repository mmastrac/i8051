# i8051-debug-tui

A terminal UI debugger for the [i8051](https://crates.io/crates/i8051) emulator
crate, built using [ratatui](https://crates.io/crates/ratatui) and
[crossterm](https://crates.io/crates/crossterm).

## Usage

An example integration is shown below.

```rust
use i8051_debug_tui::{crossterm, Debugger, DebuggerState};

let mut debugger = Debugger::new(Default::default())?;
debugger.enter()?;
let mut instruction_count = 0_usize;
loop {
    match debugger.debugger_state() {
        DebuggerState::Quit => {
            debugger.exit()?;
            break;
        }
        DebuggerState::Paused => {
            debugger.render(&cpu, &mut context)?;
            let event = crossterm::event::poll(Duration::from_millis(100))?;
            if event {
                let event = crossterm::event::read()?;
                if debugger.handle_event(event, &mut cpu, &mut context) {
                    cpu.step(&mut context);
                    debugger.render(&cpu, &mut context)?;
                }
            }
        }
        DebuggerState::Running => {
            instruction_count += 1;
            if instruction_count % 0x10000 == 0 {
                debugger.render(&cpu, &mut context)?;
                let event = crossterm::event::poll(Duration::from_millis(0))?;
                if event {
                    let event = crossterm::event::read()?;
                    if debugger.handle_event(event, &mut cpu, &mut context) {
                        cpu.step(&mut context);
                        debugger.render(&cpu, &mut context)?;
                    }
                }
            }
            cpu.step(&mut context);
            if debugger.breakpoints().contains(&cpu.pc_ext(&context)) {
                debugger.pause();
            }
            breakpoints.run(true, &mut cpu, &mut context);
        }
    }
}

debugger.exit()?;
```
