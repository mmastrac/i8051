use std::borrow::Cow;
use std::collections::BTreeSet;
use std::io;

use i8051::sfr::*;
use i8051::{Cpu, CpuContext};
use ratatui::{
    Frame, Terminal,
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph, Wrap},
};

/// Actions that can be triggered by key presses
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeyAction {
    Step,
    Run,
    ToggleBreakpoint,
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
        })
    }

    /// Get a reference to the breakpoints set
    pub fn breakpoints(&self) -> &BTreeSet<u32> {
        &self.breakpoints
    }

    /// Toggle a breakpoint at the given address
    pub fn toggle_breakpoint(&mut self, addr: u32) {
        if self.breakpoints.contains(&addr) {
            self.breakpoints.remove(&addr);
        } else {
            self.breakpoints.insert(addr);
        }
    }

    /// Render the debugger UI
    pub fn render(
        &mut self,
        cpu: &Cpu,
        ctx: &mut impl CpuContext,
        key: Option<KeyAction>,
    ) -> io::Result<()> {
        let config = &self.config;
        let breakpoints = &self.breakpoints;

        self.terminal.draw(|f| {
            render_frame(f, cpu, ctx, key, config, breakpoints);
        })?;
        Ok(())
    }
}

fn render_frame(
    f: &mut Frame,
    cpu: &Cpu,
    ctx: &mut impl CpuContext,
    _key: Option<KeyAction>,
    config: &DebuggerConfig,
    breakpoints: &BTreeSet<u32>,
) {
    // Create the main layout: left pane for disassembly, right pane split into registers and output
    let chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
        .split(f.area());

    let right_chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
        .split(chunks[1]);

    // Render the three panes
    render_disassembly(f, chunks[0], cpu, ctx, config, breakpoints);
    render_registers(f, right_chunks[0], cpu);
    render_output(f, right_chunks[1]);
}

fn render_disassembly(
    f: &mut Frame,
    area: Rect,
    cpu: &Cpu,
    ctx: &mut impl CpuContext,
    config: &DebuggerConfig,
    breakpoints: &BTreeSet<u32>,
) {
    let pc = cpu.pc_ext(ctx);
    let mut lines = Vec::new();

    // Add title with key bindings
    let title = format!(
        " Disassembly - {}: Step | {}: Run | {}: Toggle BP | q: Quit ",
        config.step_key_label, config.run_key_label, config.toggle_breakpoint_key_label
    );

    // Calculate how many instructions we can fit in the available area
    let available_height = area.height.saturating_sub(2) as usize; // -2 for borders
    // Cap at a reasonable maximum to prevent performance issues
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

    let paragraph = Paragraph::new(lines)
        .block(Block::default().title(title).borders(Borders::ALL))
        .wrap(Wrap { trim: false });

    f.render_widget(paragraph, area);
}

fn render_registers(f: &mut Frame, area: Rect, cpu: &Cpu) {
    let mut lines = Vec::new();

    // Main registers
    lines.push(Line::from(format!("PC:   {:04X}", cpu.pc)));
    lines.push(Line::from(format!("A:    {:02X}", cpu.a())));
    lines.push(Line::from(format!("B:    {:02X}", cpu.b())));
    lines.push(Line::from(format!("DPTR: {:04X}", cpu.dptr())));
    lines.push(Line::from(""));

    // PSW flags
    lines.push(Line::from(format!(
        "C:    {}",
        if cpu.psw(PSW_C) { "1" } else { "0" }
    )));
    lines.push(Line::from(format!(
        "OV:   {}",
        if cpu.psw(PSW_OV) { "1" } else { "0" }
    )));
    lines.push(Line::from(format!(
        "AC:   {}",
        if cpu.psw(PSW_AC) { "1" } else { "0" }
    )));
    lines.push(Line::from(format!(
        "F0:   {}",
        if cpu.psw(PSW_F0) { "1" } else { "0" }
    )));
    lines.push(Line::from(""));

    // R0-R7 registers
    for i in 0..8 {
        lines.push(Line::from(format!("R{}:   {:02X}", i, cpu.r(i))));
    }

    let paragraph = Paragraph::new(lines)
        .block(Block::default().title(" Registers ").borders(Borders::ALL))
        .wrap(Wrap { trim: false });

    f.render_widget(paragraph, area);
}

fn render_output(f: &mut Frame, area: Rect) {
    let lines = vec![Line::from("")];

    let paragraph = Paragraph::new(lines)
        .block(Block::default().title(" Output ").borders(Borders::ALL))
        .wrap(Wrap { trim: false });

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
