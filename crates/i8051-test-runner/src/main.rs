use std::fs::{self, File};
use std::io::{self, IsTerminal, Write, stdout};
use std::path::PathBuf;
use std::time::Duration;

use i8051::memory::{RAM, ROM};
use i8051::peripheral::Timer;
use i8051::{Cpu, CpuView, PortMapper};
use i8051::{Flag, sfr::*};
use i8051_debug_tui::{Debugger, DebuggerConfig, TracingCollector};

use clap::Parser;
use i8051_debug_tui::crossterm::event::{self, Event, KeyCode, KeyEvent};
use tracing::{Level, info};
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

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

    /// Enable verbose output
    #[arg(short, long)]
    verbose: bool,
}

struct Ports {
    ram: [u8; 128],
}

impl PortMapper for Ports {
    type WriteValue = (u8, u8);
    fn read<C: CpuView>(&self, _cpu: &C, addr: u8) -> u8 {
        info!(
            "PORT read {:02X} = {:02X}",
            addr,
            self.ram[addr as usize - 128]
        );
        self.ram[addr as usize - 128]
    }
    fn prepare_write<C: CpuView>(&self, _cpu: &C, addr: u8, value: u8) -> Self::WriteValue {
        (addr, value)
    }
    fn write(&mut self, (addr, value): Self::WriteValue) {
        info!("PORT write {:02X} = {:02X}", addr, value);
        self.ram[addr as usize - 128] = value;
    }
    fn read_latch<C: CpuView>(&self, _cpu: &C, addr: u8) -> u8 {
        info!("PORT read latch {:02X}", addr);
        self.ram[addr as usize - 128]
    }
    fn interest<C: CpuView>(&self, _cpu: &C, _addr: u8) -> bool {
        true
    }
}

pub fn main() {
    let args = Args::parse();

    if args.debug {
        // ?
    } else {
        let level = if args.verbose {
            Level::TRACE
        } else {
            Level::INFO
        };
        let format = tracing_subscriber::fmt::format()
            .with_target(false)
            .with_line_number(false)
            .with_level(false)
            .without_time();
        if stdout().is_terminal() {
            tracing_subscriber::fmt()
                .with_max_level(level)
                .event_format(format)
                .log_internal_errors(false)
                .init();
        } else {
            tracing_subscriber::fmt()
                .with_ansi(false)
                .with_max_level(level)
                .event_format(format)
                .log_internal_errors(false)
                .with_writer(io::stdout)
                .init();
        }
    }

    let mut cpu = Cpu::new();

    let ram = RAM::new();
    let code = ROM::new(fs::read(&args.rom_file).unwrap());
    let ports = Ports { ram: [0; 128] };
    let timer = Timer::default();
    let mut context = ((timer, ports), ram, code);

    if args.debug {
        run_debug_mode(&mut cpu, &mut context, args.max_instructions);
    } else {
        run_normal_mode(&mut cpu, &mut context, &args);
    }
}

fn run_debug_mode(cpu: &mut Cpu, context: &mut ((Timer, Ports), RAM, ROM), max_instructions: u64) {
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

fn run_debug_mode_inner(
    cpu: &mut Cpu,
    context: &mut ((Timer, Ports), RAM, ROM),
    max_instructions: u64,
) {
    use i8051_debug_tui::DebuggerState;

    let mut log_file = File::create("/tmp/i8051-debug.log").ok();
    debug_log!(log_file, "=== Debug session started ===");

    let config = DebuggerConfig::default();
    let mut debugger =
        Debugger::new(config, TracingCollector::new(1000)).expect("Failed to create debugger");

    debugger.enter().expect("Failed to enter debug mode");
    tracing_subscriber::registry()
        .with(debugger.tracing_collector())
        .init();

    let mut instruction_count = 0;
    let mut should_quit = false;
    let mut cpu_halted = false;

    // Initial render
    debug_log!(log_file, "Initial render at PC={:04X}", cpu.pc);
    if let Err(e) = debugger.render(cpu, context) {
        debug_log!(log_file, "ERROR: Initial render failed: {}", e);
        should_quit = true;
    }

    let mut loop_count = 0;
    while !should_quit {
        loop_count += 1;
        if loop_count % 100 == 0 {
            debug_log!(log_file, "Loop iteration {}, PC={:04X}", loop_count, cpu.pc);
        }

        // Check for input with a small timeout
        let poll_timeout = if debugger.debugger_state() == DebuggerState::Running {
            Duration::from_millis(0) // Small delay when running to batch instructions
        } else {
            Duration::from_millis(100)
        };

        match event::poll(poll_timeout) {
            Ok(true) => {
                match event::read() {
                    Ok(Event::Key(KeyEvent {
                        code: KeyCode::Char('q'),
                        ..
                    })) => {
                        debug_log!(log_file, "User pressed 'q' to quit");
                        should_quit = true;
                    }
                    Ok(event) => {
                        debug_log!(log_file, "Handling event: {:?}", event);

                        let was_running = debugger.debugger_state() == DebuggerState::Running;

                        // Let debugger handle the event
                        let should_step = debugger.handle_event(event, cpu, context);

                        if should_step && !cpu_halted {
                            debug_log!(log_file, "Step command at PC={:04X}", cpu.pc);
                            if cpu.step(context) {
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
                            let tick = context.0.0.prepare_tick(cpu, context);
                            context.0.0.tick(cpu, tick);
                            context.0.1.ram[(SFR_P3 - 0x80) as usize] =
                                !context.0.1.ram[(SFR_P3 - 0x80) as usize];
                        }

                        // Render after handling event (unless we're still running - render happens after batch)
                        let now_running = debugger.debugger_state() == DebuggerState::Running;
                        if (!now_running || !was_running)
                            && let Err(e) = debugger.render(cpu, context)
                        {
                            debug_log!(log_file, "ERROR: Render failed: {}", e);
                            should_quit = true;
                        }
                    }
                    Err(e) => {
                        debug_log!(log_file, "ERROR: Failed to read event: {}", e);
                        should_quit = true;
                    }
                }
            }
            Ok(false) => {
                // Timeout, no event - but we might need to run
            }
            Err(e) => {
                debug_log!(log_file, "ERROR: Failed to poll events: {}", e);
                should_quit = true;
            }
        }

        // If running, execute a batch of instructions
        if debugger.debugger_state() == DebuggerState::Running && !cpu_halted {
            // Execute instructions in batches for better performance
            const BATCH_SIZE: usize = 1000;
            for _ in 0..BATCH_SIZE {
                if cpu.step(context) {
                    instruction_count += 1;

                    // Check for breakpoint
                    if debugger.breakpoints().contains(&cpu.pc_ext(context)) {
                        debug_log!(log_file, "Hit breakpoint at {:04X}", cpu.pc);
                        debugger.pause();
                        if let Err(e) = debugger.render(cpu, context) {
                            debug_log!(log_file, "ERROR: Render failed at breakpoint: {}", e);
                            should_quit = true;
                        }
                        break; // Exit batch loop on breakpoint
                    }

                    if instruction_count >= max_instructions {
                        debug_log!(log_file, "Reached max instructions: {}", max_instructions);
                        debugger.pause();
                        if let Err(e) = debugger.render(cpu, context) {
                            debug_log!(log_file, "ERROR: Render failed at max instructions: {}", e);
                            should_quit = true;
                        }
                        break; // Exit batch loop
                    }
                } else {
                    // CPU halted
                    cpu_halted = true;
                    debugger.pause();
                    debug_log!(
                        log_file,
                        "CPU halted at {:04X} after {} instructions",
                        cpu.pc,
                        instruction_count
                    );
                    if let Err(e) = debugger.render(cpu, context) {
                        debug_log!(log_file, "ERROR: Render failed on CPU halt: {}", e);
                        should_quit = true;
                    }
                    break; // Exit batch loop on halt
                }
                let tick = context.0.0.prepare_tick(cpu, context);
                context.0.0.tick(cpu, tick);
                context.0.1.ram[(SFR_P3 - 0x80) as usize] =
                    !context.0.1.ram[(SFR_P3 - 0x80) as usize];
            }

            // Render after batch execution
            if debugger.debugger_state() == DebuggerState::Running
                && let Err(e) = debugger.render(cpu, context)
            {
                debug_log!(log_file, "ERROR: Render failed during run: {}", e);
                should_quit = true;
            }
        }
    }

    debug_log!(
        log_file,
        "Exiting debug loop: should_quit={}, cpu_halted={}",
        should_quit,
        cpu_halted
    );
    debugger.exit().expect("Failed to exit debug mode");
    debug_log!(log_file, "=== Debug session ended ===");
}

fn run_normal_mode(cpu: &mut Cpu, context: &mut ((Timer, Ports), RAM, ROM), args: &Args) {
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
                cpu.psw(Flag::C),
                cpu.psw(Flag::OV),
                cpu.psw(Flag::AC),
                cpu.psw(Flag::F0)
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
        let tick = context.0.0.prepare_tick(cpu, context);
        context.0.0.tick(cpu, tick);
        context.0.1.ram[(SFR_P3 - 0x80) as usize] = !context.0.1.ram[(SFR_P3 - 0x80) as usize];
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
        cpu.psw(Flag::C),
        cpu.psw(Flag::OV),
        cpu.psw(Flag::AC),
        cpu.psw(Flag::F0)
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
