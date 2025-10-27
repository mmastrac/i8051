use std::collections::BTreeMap;

use crate::sfr::{PSW_AC, PSW_C, PSW_F0, PSW_OV};
use crate::{Cpu, CpuContext};

use tracing::{Level, info};

pub enum Action {
    /// Log a message to the console.
    Log(String),
    /// Set a register to a value.
    Set(String, u16),
    /// Enable or disable tracing of instructions.
    SetTraceInstructions(bool),
    /// Enable or disable automatic tracing of registers.
    SetTraceRegisters(bool),
    /// Trace current instruction.
    TraceInstructions,
    /// Trace the current state of the CPU.
    TraceRegisters,
    /// Run an arbitrary function.
    Run(Box<dyn Fn(&mut Cpu)>),
}

impl Action {
    fn run(&self, cpu: &mut Cpu, breakpoints: &mut BreakpointState, ctx: &mut impl CpuContext) {
        match self {
            Self::Log(message) => info!("[BP] {}", message),
            Self::Set(register, value) => match register.as_str() {
                "A" => cpu.a_set(*value as u8),
                "B" => cpu.b_set(*value as u8),
                "DPTR" => cpu.dptr_set(*value),
                "PSW" => cpu.psw_set(*value as u8, true),
                "SP" => cpu.sp_set(*value as u8),
                "PC" => cpu.pc = *value,
                _ => panic!("Unknown register: {}", register),
            },
            Self::SetTraceInstructions(value) => breakpoints.trace_instructions = *value,
            Self::SetTraceRegisters(value) => breakpoints.trace_registers = *value,
            Self::TraceInstructions => {
                info!("{:#}", cpu.decode_pc(ctx));
            }
            Self::TraceRegisters => {
                if tracing::enabled!(Level::INFO) {
                    let regs = format!(
                        "  A={:02X?}  B={:02X?}  DPTR={:04X?}  C={} OV={} AC={} F0={}",
                        cpu.a(),
                        cpu.b(),
                        cpu.dptr(),
                        cpu.psw(PSW_C) as u8,
                        cpu.psw(PSW_OV) as u8,
                        cpu.psw(PSW_AC) as u8,
                        cpu.psw(PSW_F0) as u8
                    );

                    info!("{}", regs);

                    let mut regs = String::from("  ");
                    for i in 0..8 {
                        regs.push_str(&format!("R{}={:02X?} ", i, cpu.r(i)));
                    }
                    regs.push('\n');
                    info!("{}", regs);
                }
            }
            Self::Run(func) => func(cpu),
        }
    }
}

#[derive(Default)]
struct BreakpointState {
    trace_instructions: bool,
    trace_registers: bool,
}

pub struct Breakpoints {
    breakpoints_before: BTreeMap<u32, Vec<Action>>,
    breakpoints_after: BTreeMap<u32, Vec<Action>>,
    state: BreakpointState,
}

impl Breakpoints {
    pub fn new() -> Self {
        Self {
            breakpoints_before: BTreeMap::new(),
            breakpoints_after: BTreeMap::new(),
            state: Default::default(),
        }
    }

    pub fn add(&mut self, before: bool, addr: u32, action: Action) {
        if before {
            self.breakpoints_before
                .entry(addr)
                .or_default()
                .push(action);
        } else {
            self.breakpoints_after.entry(addr).or_default().push(action);
        }
    }

    pub fn remove(&mut self, addr: u32) {
        self.breakpoints_before.remove(&addr);
        self.breakpoints_after.remove(&addr);
    }

    pub fn clear(&mut self) {
        self.breakpoints_before.clear();
        self.breakpoints_after.clear();
    }

    pub fn run(&mut self, before: bool, cpu: &mut Cpu, ctx: &mut impl CpuContext) {
        let pc = cpu.pc_ext(ctx);

        let actions = if before {
            self.breakpoints_before
                .get(&pc)
                .map(|actions| actions.as_slice())
                .unwrap_or(&[])
        } else {
            self.breakpoints_after
                .get(&pc)
                .map(|actions| actions.as_slice())
                .unwrap_or(&[])
        };
        for action in actions {
            action.run(cpu, &mut self.state, ctx);
        }
        if self.state.trace_instructions && before {
            Action::TraceInstructions.run(cpu, &mut self.state, ctx);
        }
        if self.state.trace_registers && !before {
            Action::TraceRegisters.run(cpu, &mut self.state, ctx);
        }
    }
}
