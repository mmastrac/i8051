use std::borrow::Cow;
use std::collections::BTreeSet;
use std::io;

use i8051::sfr::{SFR_A, SFR_B, SFR_DPH, SFR_DPL, SFR_SP, PSW_AC, PSW_C, PSW_F0, PSW_OV};
use i8051::{Cpu, CpuContext, Register};
use ratatui::{
    Frame, Terminal,
    backend::CrosstermBackend,
    crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers},
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Paragraph, Wrap},
};

// Re-export crossterm from ratatui so users get the matching version
pub use ratatui::crossterm;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DebuggerState {
    /// The CPU is paused.
    Paused,
    /// The CPU is running.
    Running,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DebuggerFocus {
    Code,
    Reg(Register),
    Output,
}

impl DebuggerFocus {
    pub fn next(self) -> Self {
        use DebuggerFocus::*;
        const ORDER: [DebuggerFocus; 15] = [
            Code,
            Reg(Register::PC),
            Reg(Register::SP),
            Reg(Register::R(0)),
            Reg(Register::R(1)),
            Reg(Register::R(2)),
            Reg(Register::R(3)),
            Reg(Register::R(4)),
            Reg(Register::R(5)),
            Reg(Register::R(6)),
            Reg(Register::R(7)),
            Reg(Register::A),
            Reg(Register::B),
            Reg(Register::DPTR),
            Output,
        ];
        
        let index = ORDER.iter().position(|r| r == &self).unwrap_or(0);
        ORDER[(index + 1) % ORDER.len()]
    }

    /// Returns zero if not editable, otherwise the length.
    #[allow(dead_code)]
    pub fn edit_length(self) -> usize {
        use DebuggerFocus::*;
        match self {
            Code | Output => 0,
            Reg(Register::PC) | Reg(Register::DPTR) => 4,
            Reg(Register::SP) | Reg(Register::A) | Reg(Register::B) | Reg(Register::R(_)) => 2,
            _ => 0,
        }
    }
}

struct DebuggerInternalState {
    /// The current TUI widget focus of the debugger.
    focus: DebuggerFocus,
    /// Current debugger state
    state: DebuggerState,
    /// The edit buffer for in-place register editing
    edit_buffer: String,
    /// Whether we're currently editing (if true, edit_buffer is shown in place of the focused register)
    is_editing: bool,
}

/// Configuration for the debugger UI
pub struct DebuggerConfig {
    pub step_key_label: Cow<'static, str>,
    pub run_key_label: Cow<'static, str>,
    pub toggle_breakpoint_key_label: Cow<'static, str>,
}

impl Default for DebuggerConfig {
    fn default() -> Self {
        Self {
            step_key_label: Cow::Borrowed("s"),
            run_key_label: Cow::Borrowed("r"),
            toggle_breakpoint_key_label: Cow::Borrowed("b"),
        }
    }
}

/// TUI Debugger for the i8051 emulator
pub struct Debugger {
    config: DebuggerConfig,
    breakpoints: BTreeSet<u32>,
    terminal: Terminal<CrosstermBackend<io::Stdout>>,
    state: DebuggerInternalState,
}

impl Debugger {
    /// Create a new debugger with the given configuration
    pub fn new(config: DebuggerConfig) -> io::Result<Self> {
        let stdout = io::stdout();
        let backend = CrosstermBackend::new(stdout);
        let terminal = Terminal::new(backend)?;

        Ok(Self {
            config,
            breakpoints: BTreeSet::new(),
            terminal,
            state: DebuggerInternalState {
                focus: DebuggerFocus::Code,
                state: DebuggerState::Paused,
                edit_buffer: String::new(),
                is_editing: false,
            },
        })
    }

    /// Get a reference to the breakpoints set
    pub fn breakpoints(&self) -> &BTreeSet<u32> {
        &self.breakpoints
    }

    /// Get the current debugger state
    pub fn debugger_state(&self) -> DebuggerState {
        self.state.state
    }

    /// Pause the debugger (set to paused state)
    pub fn pause(&mut self) {
        self.state.state = DebuggerState::Paused;
    }

    /// Toggle a breakpoint at the given address
    pub fn toggle_breakpoint(&mut self, addr: u32) {
        if self.breakpoints.contains(&addr) {
            self.breakpoints.remove(&addr);
        } else {
            self.breakpoints.insert(addr);
        }
    }

    /// Handle a crossterm event and update internal state
    /// Returns true if we should step the CPU, false otherwise
    pub fn handle_event(&mut self, event: Event, cpu: &mut Cpu, ctx: &mut impl CpuContext) -> bool {
        // Handle editing mode
        if self.state.is_editing {
            match event {
                Event::Key(KeyEvent { code: KeyCode::Esc, .. }) => {
                    // Cancel editing
                    self.state.is_editing = false;
                    self.state.edit_buffer.clear();
                    return false;
                }
                Event::Key(KeyEvent { code: KeyCode::Enter, .. }) => {
                    // Apply the edit
                    if let DebuggerFocus::Reg(register) = self.state.focus {
                        if let Ok(value) = u32::from_str_radix(&self.state.edit_buffer, 16) {
                            self.apply_register_edit(cpu, ctx, register, value);
                        }
                    }
                    self.state.is_editing = false;
                    self.state.edit_buffer.clear();
                    return false;
                }
                Event::Key(KeyEvent { code: KeyCode::Backspace, .. }) => {
                    self.state.edit_buffer.pop();
                    return false;
                }
                Event::Key(KeyEvent { code: KeyCode::Char(c), .. }) if c.is_ascii_hexdigit() => {
                    // Get max length for this register
                    let max_len = match self.state.focus {
                        DebuggerFocus::Reg(Register::PC) | DebuggerFocus::Reg(Register::DPTR) => 4,
                        _ => 2,
                    };
                    if self.state.edit_buffer.len() < max_len {
                        self.state.edit_buffer.push(c.to_ascii_uppercase());
                    }
                    return false;
                }
                _ => return false,
            }
        }

        // Handle normal mode
        match event {
            Event::Key(KeyEvent { code: KeyCode::Char('c'), modifiers: KeyModifiers::CONTROL, .. }) => {
                if self.state.state == DebuggerState::Running {
                    self.state.state = DebuggerState::Paused;
                }
                false
            }
            Event::Key(KeyEvent { code: KeyCode::Tab, .. }) => {
                self.state.focus = self.state.focus.next();
                false
            }
            Event::Key(KeyEvent { code: KeyCode::Char('s'), .. }) if self.state.focus == DebuggerFocus::Code => {
                // Step when code is focused
                true
            }
            Event::Key(KeyEvent { code: KeyCode::Char('r'), .. }) if self.state.focus == DebuggerFocus::Code => {
                // Toggle run/pause
                self.state.state = match self.state.state {
                    DebuggerState::Paused => DebuggerState::Running,
                    DebuggerState::Running => DebuggerState::Paused,
                };
                false
            }
            Event::Key(KeyEvent { code: KeyCode::Char('b'), .. }) if self.state.focus == DebuggerFocus::Code => {
                // Toggle breakpoint
                self.toggle_breakpoint(cpu.pc_ext(ctx));
                false
            }
            Event::Key(KeyEvent { code: KeyCode::Char(c), .. }) 
                if matches!(self.state.focus, DebuggerFocus::Reg(_)) && c.is_ascii_hexdigit() => {
                // Start editing the currently focused register
                self.state.is_editing = true;
                self.state.edit_buffer.clear();
                self.state.edit_buffer.push(c.to_ascii_uppercase());
                false
            }
            _ => false,
        }
    }

    fn apply_register_edit(&mut self, cpu: &mut Cpu, _ctx: &mut impl CpuContext, register: Register, value: u32) {
        use i8051::sfr::SFR_SP;
        match register {
            Register::PC => cpu.pc = value as u16,
            Register::SP => cpu.internal_ram[SFR_SP as usize] = value as u8,
            Register::A => cpu.internal_ram[SFR_A as usize] = value as u8,
            Register::B => cpu.internal_ram[SFR_B as usize] = value as u8,
            Register::DPTR => {
                cpu.internal_ram[SFR_DPL as usize] = (value & 0xFF) as u8;
                cpu.internal_ram[SFR_DPH as usize] = ((value >> 8) & 0xFF) as u8;
            }
            Register::R(n) => {
                let base = (cpu.internal_ram[i8051::sfr::SFR_PSW as usize] & 0x18) << 3;
                cpu.internal_ram[(base + n) as usize] = value as u8;
            }
            _ => {}
        }
    }

    /// Render the debugger UI
    pub fn render(
        &mut self,
        cpu: &Cpu,
        ctx: &mut impl CpuContext,
    ) -> io::Result<()> {
        let config = &self.config;
        let breakpoints = &self.breakpoints;
        let focus = self.state.focus;
        let state = self.state.state;
        let is_editing = self.state.is_editing;
        let edit_buffer = &self.state.edit_buffer;

        self.terminal.draw(|f| {
            render_frame(f, cpu, ctx, config, breakpoints, focus, state, is_editing, edit_buffer);
        })?;
        Ok(())
    }
}

fn render_frame(
    f: &mut Frame,
    cpu: &Cpu,
    ctx: &mut impl CpuContext,
    config: &DebuggerConfig,
    breakpoints: &BTreeSet<u32>,
    focus: DebuggerFocus,
    state: DebuggerState,
    is_editing: bool,
    edit_buffer: &str,
) {
    // Create the main layout with a vertical separator
    let main_chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage(50),
            Constraint::Length(1), // Vertical separator
            Constraint::Percentage(50),
        ])
        .split(f.area());

    let right_chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
        .split(main_chunks[2]);

    // Render the three panes
    render_disassembly(f, main_chunks[0], cpu, ctx, config, breakpoints, focus, state);
    
    // Render vertical separator
    let separator_lines: Vec<Line> = (0..main_chunks[1].height)
        .map(|_| Line::from("│"))
        .collect();
    let separator = Paragraph::new(separator_lines).style(Style::default().fg(Color::DarkGray));
    f.render_widget(separator, main_chunks[1]);
    
    render_registers(f, right_chunks[0], cpu, focus, is_editing, edit_buffer);
    render_output(f, right_chunks[1], focus);
}

fn render_disassembly(
    f: &mut Frame,
    area: Rect,
    cpu: &Cpu,
    ctx: &mut impl CpuContext,
    config: &DebuggerConfig,
    breakpoints: &BTreeSet<u32>,
    focus: DebuggerFocus,
    state: DebuggerState,
) {
    let pc = cpu.pc_ext(ctx);
    let mut lines = Vec::new();

    // Add title with key bindings and state
    let state_str = match state {
        DebuggerState::Running => "[RUNNING]",
        DebuggerState::Paused => "[PAUSED]",
    };
    
    let focus_indicator = if focus == DebuggerFocus::Code { "▶ " } else { "  " };
    
    let title = format!(
        "{}{} Code {} - {}: Step | {}: Run | {}: Toggle BP | Tab: Switch | q: Quit ",
        focus_indicator,
        state_str,
        "",
        config.step_key_label, 
        config.run_key_label, 
        config.toggle_breakpoint_key_label
    );

    let title_style = if focus == DebuggerFocus::Code {
        Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)
    } else {
        Style::default()
    };

    lines.push(Line::from(Span::styled(title, title_style)));
    lines.push(Line::from("─".repeat(area.width as usize)));

    // Calculate how many instructions we can fit in the available area
    let available_height = area.height.saturating_sub(2) as usize; // -2 for title and separator
    let max_lines = available_height.min(100);

    // Start with the current instruction at the top
    let mut current_pc = pc;
    let mut seen_addresses = std::collections::HashSet::new();

    for i in 0..max_lines {
        // Prevent infinite loops if we somehow visit the same address twice
        if !seen_addresses.insert(current_pc) {
            lines.push(Line::from(format!(
                "  [Loop detected at {:04X}]",
                current_pc
            )));
            break;
        }

        // Safety check: if we've somehow gone past 64KB, stop
        if i > 0 && current_pc == 0 {
            lines.push(Line::from("  [Wrapped to 0000]"));
            break;
        }

        let instruction = cpu.decode(ctx, current_pc);
        let bytes_str = instruction
            .bytes()
            .iter()
            .map(|b| format!("{:02X}", b))
            .collect::<Vec<_>>()
            .join(" ");

        let has_breakpoint = breakpoints.contains(&current_pc);
        let is_current = current_pc == pc;

        let bp_marker = if has_breakpoint { "●" } else { " " };
        let pc_marker = if is_current { ">" } else { " " };

        let line_text = format!(
            "{}{} {:04X}: {:12} {}",
            bp_marker,
            pc_marker,
            current_pc,
            bytes_str,
            instruction.decode()
        );

        let style = if is_current {
            Style::default()
                .fg(Color::Yellow)
                .add_modifier(Modifier::BOLD)
        } else if has_breakpoint {
            Style::default().fg(Color::Red)
        } else {
            Style::default()
        };

        lines.push(Line::from(Span::styled(line_text, style)));

        // Move to next instruction
        current_pc = current_pc.wrapping_add(instruction.bytes().len() as _);
    }

    let paragraph = Paragraph::new(lines).wrap(Wrap { trim: false });

    f.render_widget(paragraph, area);
}

fn render_registers(f: &mut Frame, area: Rect, cpu: &Cpu, focus: DebuggerFocus, is_editing: bool, edit_buffer: &str) {
    let mut lines = Vec::new();

    let is_reg_focused = matches!(focus, DebuggerFocus::Reg(_));
    let focus_indicator = if is_reg_focused { "▶ " } else { "  " };
    let title = format!("{}Registers - Tab to select, type hex to edit", focus_indicator);
    
    let title_style = if is_reg_focused {
        Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)
    } else {
        Style::default()
    };

    lines.push(Line::from(Span::styled(title, title_style)));
    lines.push(Line::from("─".repeat(area.width as usize)));

    // Helper function to format a register value with optional editing
    let format_value = |reg: Register, default_value: String| -> String {
        if is_editing && focus == DebuggerFocus::Reg(reg) {
            // Show edit buffer with cursor (using underscore)
            let max_len: usize = match reg {
                Register::PC | Register::DPTR => 4,
                _ => 2,
            };
            format!("{}_", edit_buffer) + &"_".repeat(max_len.saturating_sub(edit_buffer.len() + 1))
        } else {
            default_value
        }
    };

    // Helper function to create a styled register value
    let reg_style = |reg: Register| {
        if focus == DebuggerFocus::Reg(reg) {
            if is_editing {
                Style::default().fg(Color::Green).add_modifier(Modifier::BOLD | Modifier::UNDERLINED)
            } else {
                Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)
            }
        } else {
            Style::default()
        }
    };

    // Compact format: PC and SP on one line
    let sp = cpu.internal_ram[SFR_SP as usize];
    let pc_sp_line = vec![
        Span::styled(
            format!("PC: {}", format_value(Register::PC, format!("{:04X}", cpu.pc))), 
            reg_style(Register::PC)
        ),
        Span::raw("  "),
        Span::styled(
            format!("SP: {}", format_value(Register::SP, format!("{:02X}", sp))), 
            reg_style(Register::SP)
        ),
    ];
    lines.push(Line::from(pc_sp_line));
    
    // R0-R7 on one or two lines depending on width
    let mut r_spans = Vec::new();
    for i in 0..8 {
        if i > 0 {
            r_spans.push(Span::raw(" "));
        }
        r_spans.push(Span::styled(
            format!("R{}:{}", i, format_value(Register::R(i), format!("{:02X}", cpu.r(i)))),
            reg_style(Register::R(i))
        ));
    }
    lines.push(Line::from(r_spans));

    // A, B, DPTR on one line
    let abc_line = vec![
        Span::styled(
            format!("A:{}", format_value(Register::A, format!("{:02X}", cpu.a()))), 
            reg_style(Register::A)
        ),
        Span::raw(" "),
        Span::styled(
            format!("B:{}", format_value(Register::B, format!("{:02X}", cpu.b()))), 
            reg_style(Register::B)
        ),
        Span::raw(" "),
        Span::styled(
            format!("DPTR:{}", format_value(Register::DPTR, format!("{:04X}", cpu.dptr()))), 
            reg_style(Register::DPTR)
        ),
    ];
    lines.push(Line::from(abc_line));
    lines.push(Line::from(""));

    // PSW flags
    lines.push(Line::from(format!(
        "C:{}  OV:{}  AC:{}  F0:{}",
        if cpu.psw(PSW_C) { "1" } else { "0" },
        if cpu.psw(PSW_OV) { "1" } else { "0" },
        if cpu.psw(PSW_AC) { "1" } else { "0" },
        if cpu.psw(PSW_F0) { "1" } else { "0" }
    )));

    let paragraph = Paragraph::new(lines).wrap(Wrap { trim: false });

    f.render_widget(paragraph, area);
}

fn render_output(f: &mut Frame, area: Rect, focus: DebuggerFocus) {
    let mut lines = Vec::new();

    let focus_indicator = if focus == DebuggerFocus::Output { "▶ " } else { "  " };
    let title = format!("{}Output", focus_indicator);
    
    let title_style = if focus == DebuggerFocus::Output {
        Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)
    } else {
        Style::default()
    };

    lines.push(Line::from(Span::styled(title, title_style)));
    lines.push(Line::from("─".repeat(area.width as usize)));
    lines.push(Line::from(""));

    let paragraph = Paragraph::new(lines).wrap(Wrap { trim: false });

    f.render_widget(paragraph, area);
}

impl Debugger {
    /// Enter alternate screen and enable raw mode
    pub fn enter(&mut self) -> io::Result<()> {
        crossterm::terminal::enable_raw_mode()?;
        crossterm::execute!(io::stdout(), crossterm::terminal::EnterAlternateScreen,)?;
        self.terminal.clear()?;
        Ok(())
    }

    /// Exit alternate screen and disable raw mode
    pub fn exit(&mut self) -> io::Result<()> {
        crossterm::terminal::disable_raw_mode()?;
        crossterm::execute!(io::stdout(), crossterm::terminal::LeaveAlternateScreen,)?;
        Ok(())
    }
}

impl Drop for Debugger {
    fn drop(&mut self) {
        let _ = self.exit();
    }
}
