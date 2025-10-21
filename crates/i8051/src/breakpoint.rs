use std::collections::BTreeMap;

use crate::sfr::{PSW_AC, PSW_C, PSW_F0, PSW_OV};
use crate::{Cpu, CpuContext};

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
}

impl Action {
    fn run(&self, cpu: &mut Cpu, breakpoints: &mut BreakpointState, ctx: &mut impl CpuContext) {
        match self {
            Self::Log(message) => println!("[BP] {}", message),
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
                println!("{:#}", cpu.decode_pc(ctx));
            }
            Self::TraceRegisters => {
                println!(
                    "  A={:02X?}  B={:02X?}  DPTR={:04X?}  C={} OV={} AC={} F0={}",
                    cpu.a(),
                    cpu.b(),
                    cpu.dptr(),
                    cpu.psw(PSW_C) as u8,
                    cpu.psw(PSW_OV) as u8,
                    cpu.psw(PSW_AC) as u8,
                    cpu.psw(PSW_F0) as u8
                );
                print!("  ");
                for i in 0..8 {
                    print!("R{}={:02X?} ", i, cpu.r(i));
                }
                println!();
            }
        }
    }
}

#[derive(Default)]
struct BreakpointState {
    trace_instructions: bool,
    trace_registers: bool,
}

pub struct Breakpoints {
    breakpoints_before: BTreeMap<u16, Vec<Action>>,
    breakpoints_after: BTreeMap<u16, Vec<Action>>,
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

    pub fn add(&mut self, before: bool, addr: u16, action: Action) {
        if before {
            self.breakpoints_before
                .entry(addr)
                .or_default()
                .push(action);
        } else {
            self.breakpoints_after.entry(addr).or_default().push(action);
        }
    }

    pub fn remove(&mut self, addr: u16) {
        self.breakpoints_before.remove(&addr);
        self.breakpoints_after.remove(&addr);
    }

    pub fn clear(&mut self) {
        self.breakpoints_before.clear();
        self.breakpoints_after.clear();
    }

    pub fn run(&mut self, before: bool, cpu: &mut Cpu, ctx: &mut impl CpuContext) {
        let pc = cpu.pc;
        if self.state.trace_instructions && before {
            Action::TraceInstructions.run(cpu, &mut self.state, ctx);
        }

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
        if self.state.trace_registers && !before {
            Action::TraceRegisters.run(cpu, &mut self.state, ctx);
        }
    }
}
