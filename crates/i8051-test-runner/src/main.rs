use std::fs::{self, File};
use std::io::Write;
use std::path::PathBuf;
use std::time::Duration;

use i8051::memory::{RAM, ROM};
use i8051::sfr::*;
use i8051::{Cpu, CpuContext, CpuView, PortMapper};
use i8051_debug_tui::{Debugger, DebuggerConfig, KeyAction};

use clap::Parser;
use crossterm::event::{self, Event, KeyCode, KeyEvent};

macro_rules! debug_log {
    ($file:expr, $($arg:tt)*) => {
        if let Some(ref mut f) = $file {
            let _ = writeln!(f, $($arg)*);
            let _ = f.flush();
        }
    };
}

#[derive(Parser)]
struct Args {
    /// ROM file to load
    #[arg(value_name = "ROM_FILE")]
    rom_file: PathBuf,

    /// Maximum number of instructions to execute
    #[arg(short, long, default_value = "10000")]
    max_instructions: u64,

    /// Enable trace output
    #[arg(short, long)]
    trace: bool,

    /// Enable debug mode with TUI
    #[arg(short, long)]
    debug: bool,
}

struct Ports {
    ram: [u8; 128],
}

impl PortMapper for Ports {
    type WriteValue = (u8, u8);
    fn read<C: CpuView>(&self, _cpu: &C, addr: u8) -> u8 {
        println!("PORT read {:02X}", addr);
        self.ram[addr as usize - 128]
    }
    fn prepare_write<C: CpuView>(&self, _cpu: &C, addr: u8, value: u8) -> Self::WriteValue {
        (addr, value)
    }
    fn write(&mut self, (addr, value): Self::WriteValue) {
        println!("PORT write {:02X} = {:02X}", addr, value);
        self.ram[addr as usize - 128] = value;
    }
    fn read_latch<C: CpuView>(&self, _cpu: &C, addr: u8) -> u8 {
        println!("PORT read latch {:02X}", addr);
        self.ram[addr as usize - 128]
    }
    fn interest<C: CpuView>(&self, _cpu: &C, _addr: u8) -> bool {
        true
    }
}

pub fn main() {
    let args = Args::parse();

    let mut cpu = Cpu::new();

    let ram = RAM::new();
    let code = ROM::new(fs::read(&args.rom_file).unwrap());
    let ports = Ports { ram: [0; 128] };
    let mut context = (ports, ram, code);

    if args.debug {
        run_debug_mode(&mut cpu, &mut context, args.max_instructions);
    } else {
        run_normal_mode(&mut cpu, &mut context, &args);
    }
}

fn run_debug_mode(cpu: &mut Cpu, context: &mut impl CpuContext, max_instructions: u64) {
    use std::sync::{Arc, Mutex};
    let panic_info: Arc<Mutex<Option<String>>> = Default::default();

    let panic_info_clone = panic_info.clone();
    std::panic::set_hook(Box::new(move |panic_info| {
        *panic_info_clone.lock().unwrap() =
            if let Some(s) = panic_info.payload().downcast_ref::<&str>() {
                Some(format!("panic occurred: {s:?}"))
            } else if let Some(s) = panic_info.payload().downcast_ref::<String>() {
                Some(format!("panic occurred: {s:?}"))
            } else {
                Some("panic occurred".into())
            };
    }));

    run_debug_mode_inner(cpu, context, max_instructions);
    println!("Debugger exited");

    _ = std::panic::take_hook();

    if let Some(panic) = panic_info.lock().unwrap().take() {
        eprintln!("{panic}");
    }
}

fn run_debug_mode_inner(cpu: &mut Cpu, context: &mut impl CpuContext, max_instructions: u64) {
    let mut log_file = File::create("/tmp/i8051-debug.log").ok();
    debug_log!(log_file, "=== Debug session started ===");

    let config = DebuggerConfig::default();
    let mut debugger = Debugger::new(config).expect("Failed to create debugger");

    debugger.enter().expect("Failed to enter debug mode");

    let mut instruction_count = 0;
    let mut running = false;
    let mut should_quit = false;
    let mut cpu_halted = false;

    // Initial render
    debug_log!(log_file, "Initial render at PC={:04X}", cpu.pc);
    if let Err(e) = debugger.render(cpu, context, None) {
        debug_log!(log_file, "ERROR: Initial render failed: {}", e);
        should_quit = true;
    }

    let mut loop_count = 0;
    while !should_quit {
        loop_count += 1;
        if loop_count % 10 == 0 {
            debug_log!(log_file, "Loop iteration {}, PC={:04X}", loop_count, cpu.pc);
        }

        // Check for input with a small timeout
        match event::poll(Duration::from_millis(100)) {
            Ok(true) => {
                match event::read() {
                    Ok(Event::Key(KeyEvent { code, .. })) => {
                        match code {
                            KeyCode::Char('q') => {
                                debug_log!(log_file, "User pressed 'q' to quit");
                                should_quit = true;
                            }
                            KeyCode::Char('s') => {
                                // Step
                                debug_log!(log_file, "Step command at PC={:04X}", cpu.pc);
                                if cpu_halted {
                                    debug_log!(log_file, "CPU already halted, cannot step");
                                } else if cpu.step(context) {
                                    instruction_count += 1;
                                    debug_log!(
                                        log_file,
                                        "Stepped to PC={:04X}, count={}",
                                        cpu.pc,
                                        instruction_count
                                    );
                                } else {
                                    debug_log!(
                                        log_file,
                                        "CPU halted at PC={:04X} after {} instructions",
                                        cpu.pc,
                                        instruction_count
                                    );
                                    cpu_halted = true;
                                }
                                debug_log!(log_file, "About to render after step...");
                                match debugger.render(cpu, context, Some(KeyAction::Step)) {
                                    Ok(_) => {
                                        debug_log!(log_file, "Render completed successfully");
                                    }
                                    Err(e) => {
                                        debug_log!(log_file, "ERROR: Render failed on step: {}", e);
                                        should_quit = true;
                                    }
                                }
                            }
                            KeyCode::Char('r') => {
                                // Toggle run mode
                                running = !running;
                                debug_log!(log_file, "Run mode toggled: {}", running);
                                if let Err(e) = debugger.render(cpu, context, Some(KeyAction::Run))
                                {
                                    debug_log!(
                                        log_file,
                                        "ERROR: Render failed on run toggle: {}",
                                        e
                                    );
                                    should_quit = true;
                                }
                            }
                            KeyCode::Char('b') => {
                                // Toggle breakpoint
                                debugger.toggle_breakpoint(cpu.pc_ext(context));
                                debug_log!(log_file, "Toggled breakpoint at {:04X}", cpu.pc);
                                if let Err(e) =
                                    debugger.render(cpu, context, Some(KeyAction::ToggleBreakpoint))
                                {
                                    debug_log!(
                                        log_file,
                                        "ERROR: Render failed on breakpoint toggle: {}",
                                        e
                                    );
                                    should_quit = true;
                                }
                            }
                            KeyCode::Esc => {
                                debug_log!(log_file, "Escape pressed, quitting");
                                should_quit = true;
                            }
                            other => {
                                debug_log!(log_file, "Unhandled key: {:?}", other);
                            }
                        }
                    }
                    Ok(other_event) => {
                        debug_log!(log_file, "Received non-key event: {:?}", other_event);
                    }
                    Err(e) => {
                        debug_log!(log_file, "ERROR: Failed to read event: {}", e);
                        should_quit = true;
                    }
                }
            }
            Ok(false) => {
                // Timeout, no event
            }
            Err(e) => {
                debug_log!(log_file, "ERROR: Failed to poll events: {}", e);
                should_quit = true;
            }
        }

        // If running, execute instructions
        if running && !cpu_halted {
            if cpu.step(context) {
                instruction_count += 1;

                // Check for breakpoint
                if debugger.breakpoints().contains(&cpu.pc_ext(context)) {
                    running = false;
                    debug_log!(log_file, "Hit breakpoint at {:04X}", cpu.pc);
                    if let Err(e) = debugger.render(cpu, context, None) {
                        debug_log!(log_file, "ERROR: Render failed at breakpoint: {}", e);
                        should_quit = true;
                    }
                }

                if instruction_count >= max_instructions {
                    running = false;
                    debug_log!(log_file, "Reached max instructions: {}", max_instructions);
                    if let Err(e) = debugger.render(cpu, context, None) {
                        debug_log!(log_file, "ERROR: Render failed at max instructions: {}", e);
                        should_quit = true;
                    }
                }
            } else {
                // CPU halted
                running = false;
                cpu_halted = true;
                debug_log!(
                    log_file,
                    "CPU halted at {:04X} after {} instructions",
                    cpu.pc,
                    instruction_count
                );
                if let Err(e) = debugger.render(cpu, context, None) {
                    debug_log!(log_file, "ERROR: Render failed on CPU halt: {}", e);
                    should_quit = true;
                }
            }
        }
    }

    debug_log!(
        log_file,
        "Exiting debug loop: should_quit={}, cpu_halted={}, running={}",
        should_quit,
        cpu_halted,
        running
    );
    debugger.exit().expect("Failed to exit debug mode");
    debug_log!(log_file, "=== Debug session ended ===");
}

fn run_normal_mode(cpu: &mut Cpu, context: &mut impl CpuContext, args: &Args) {
    let mut instruction_count = 0;
    loop {
        let instruction = cpu.decode_pc(context);
        if args.trace {
            println!(
                "{pc:04X}: {:10} {instruction}",
                instruction
                    .bytes()
                    .iter()
                    .map(|b| format!("{:02X}", b))
                    .collect::<Vec<_>>()
                    .join(" "),
                pc = instruction.pc(),
            );
            println!(
                "  A={:02X?}  B={:02X?}  DPTR={:04X?}  C={} OV={} AC={} Z={}",
                cpu.a(),
                cpu.b(),
                cpu.dptr(),
                cpu.psw(PSW_C),
                cpu.psw(PSW_OV),
                cpu.psw(PSW_AC),
                cpu.psw(PSW_F0)
            );
            print!("  ");
            for i in 0..8 {
                print!("R{}={:02X?} ", i, cpu.r(i));
            }
            println!();
        }
        instruction_count += 1;
        if !cpu.step(context) {
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
        "  A={:02X?}  B={:02X?}  DPTR={:04X?}  C={} OV={} AC={} F0={}",
        cpu.a(),
        cpu.b(),
        cpu.dptr(),
        cpu.psw(PSW_C),
        cpu.psw(PSW_OV),
        cpu.psw(PSW_AC),
        cpu.psw(PSW_F0)
    );
    print!("  ");
    for i in 0..8 {
        print!("R{}={:02X?} ", i, cpu.r(i));
    }
    println!();

    let cpu_view = (cpu, context);
    println!(
        "MEM: {:02X} {:02X} {:02X} {:02X}",
        cpu_view.read_xdata(0x8000),
        cpu_view.read_xdata(0x8001),
        cpu_view.read_xdata(0x8002),
        cpu_view.read_xdata(0x8003)
    );
}
