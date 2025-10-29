use std::borrow::Cow;
use std::cell::Cell;
use std::collections::BTreeSet;
use std::io;

use i8051::{ControlFlow, Cpu, CpuContext, Flag, Register};
use ratatui::text::Text;
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
    /// The debugger is quitting.
    Quit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DebuggerFocus {
    Code,
    Reg(Register),
    Output,
}

impl DebuggerFocus {
    const ORDER: [DebuggerFocus; 15] = [
        DebuggerFocus::Code,
        DebuggerFocus::Reg(Register::PC),
        DebuggerFocus::Reg(Register::SP),
        DebuggerFocus::Reg(Register::R(0)),
        DebuggerFocus::Reg(Register::R(1)),
        DebuggerFocus::Reg(Register::R(2)),
        DebuggerFocus::Reg(Register::R(3)),
        DebuggerFocus::Reg(Register::R(4)),
        DebuggerFocus::Reg(Register::R(5)),
        DebuggerFocus::Reg(Register::R(6)),
        DebuggerFocus::Reg(Register::R(7)),
        DebuggerFocus::Reg(Register::A),
        DebuggerFocus::Reg(Register::B),
        DebuggerFocus::Reg(Register::DPTR),
        DebuggerFocus::Output,
    ];

    pub fn next(self) -> Self {
        let index = Self::ORDER.iter().position(|r| r == &self).unwrap_or(0);
        Self::ORDER[(index + 1) % Self::ORDER.len()]
    }

    pub fn prev(self) -> Self {
        let index = Self::ORDER.iter().position(|r| r == &self).unwrap_or(0);
        if index == 0 {
            Self::ORDER[Self::ORDER.len() - 1]
        } else {
            Self::ORDER[index - 1]
        }
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
    /// The current code window state
    code_window: CodeWindowState,
}

struct CodeWindowState {
    /// The first address to display in the code window
    start: Cell<u32>,
    /// The last height of the code window
    last_height: Cell<usize>,
    /// The focus address to display in the code window, or None to use the PC
    focus: Option<u32>,
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
                code_window: CodeWindowState {
                    start: Cell::new(0),
                    last_height: Cell::new(10),
                    focus: None,
                },
            },
        })
    }

    /// Get a reference to the breakpoints set
    pub fn breakpoints(&self) -> &BTreeSet<u32> {
        &self.breakpoints
    }

    /// Get a mutable reference to the breakpoints set
    pub fn breakpoints_mut(&mut self) -> &mut BTreeSet<u32> {
        &mut self.breakpoints
    }

    /// Get the current debugger state
    pub fn debugger_state(&self) -> DebuggerState {
        self.state.state
    }

    /// Pause the debugger (set to paused state)
    pub fn pause(&mut self) {
        self.state.state = DebuggerState::Paused;
        self.state.code_window.focus = None;
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
                Event::Key(KeyEvent {
                    code: KeyCode::Esc, ..
                }) => {
                    // Cancel editing
                    self.state.is_editing = false;
                    self.state.edit_buffer.clear();
                    return false;
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Enter,
                    ..
                }) => {
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
                Event::Key(KeyEvent {
                    code: KeyCode::Backspace,
                    ..
                }) => {
                    self.state.edit_buffer.pop();
                    return false;
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Char(c),
                    ..
                }) if c.is_ascii_hexdigit() => {
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
            Event::Key(KeyEvent {
                code: KeyCode::Char('c'),
                modifiers: KeyModifiers::CONTROL,
                ..
            }) => {
                if self.state.state == DebuggerState::Running {
                    self.state.state = DebuggerState::Paused;
                }
                false
            }
            Event::Key(KeyEvent {
                code: KeyCode::Tab,
                modifiers,
                ..
            }) => {
                if modifiers.contains(KeyModifiers::SHIFT) {
                    self.state.focus = self.state.focus.prev();
                } else {
                    self.state.focus = self.state.focus.next();
                }
                false
            }
            Event::Key(KeyEvent {
                code: KeyCode::BackTab,
                ..
            }) => {
                // BackTab is also Shift+Tab on some terminals
                self.state.focus = self.state.focus.prev();
                false
            }
            Event::Key(KeyEvent {
                code: KeyCode::Char('q'),
                ..
            }) => {
                self.state.state = DebuggerState::Quit;
                true
            }
            Event::Key(KeyEvent {
                code: KeyCode::Char('s'),
                ..
            }) if self.state.focus == DebuggerFocus::Code => {
                // Step when code is focused, removing any custom address selection
                self.state.code_window.focus = None;
                true
            }
            Event::Key(KeyEvent {
                code: KeyCode::Char('r'),
                ..
            }) if self.state.focus == DebuggerFocus::Code => {
                // Toggle run/pause
                self.state.state = match self.state.state {
                    DebuggerState::Paused => DebuggerState::Running,
                    DebuggerState::Running => DebuggerState::Paused,
                    DebuggerState::Quit => DebuggerState::Quit,
                };
                false
            }
            Event::Key(KeyEvent {
                code: KeyCode::Char('b'),
                ..
            }) if self.state.focus == DebuggerFocus::Code => {
                // Toggle breakpoint
                let addr = self.state.code_window.focus.unwrap_or(cpu.pc_ext(ctx));
                self.toggle_breakpoint(addr);
                false
            }
            Event::Key(KeyEvent {
                code: KeyCode::Up, ..
            }) if self.state.focus == DebuggerFocus::Code => {
                // Scroll code window up (decrement start address)
                let current = self.state.code_window.focus.unwrap_or(cpu.pc_ext(ctx));

                // If we're at the top of the window, jump back 10 instructions and
                // try to resynchronize
                let mut start = self.state.code_window.start.get();
                if start == current {
                    start = start.saturating_sub(10);
                }

                // Render instructions and see if we can go back further
                let instructions = cpu.decode_range(ctx, start, current, 100);
                if let Some(current) = instructions
                    .iter()
                    .position(|instruction| instruction.pc() == current)
                {
                    self.state.code_window.start.set(instructions[0].pc());
                    self.state.code_window.focus =
                        Some(instructions[current.saturating_sub(1)].pc());
                } else {
                    // We failed to resynchronize, so just jump back 1 byte
                    self.state.code_window.start.set(start);
                    self.state.code_window.focus = Some(current.saturating_sub(1));
                }
                false
            }
            Event::Key(KeyEvent {
                code: KeyCode::Down,
                ..
            }) if self.state.focus == DebuggerFocus::Code => {
                // Scroll code window down (increment start address)
                let current = self.state.code_window.focus.unwrap_or(cpu.pc_ext(ctx));
                let height = self.state.code_window.last_height.get();
                let instructions =
                    cpu.decode_range(ctx, self.state.code_window.start.get(), current, 100);
                let len = cpu.decode(ctx, current).len();
                if let Some(current) = instructions
                    .iter()
                    .position(|instruction| instruction.pc() == current)
                {
                    let scroll = height.saturating_sub(current);
                    if scroll <= 2 {
                        self.state.code_window.start.set(instructions[2].pc());
                    } else {
                        self.state.code_window.start.set(instructions[0].pc());
                    }
                }
                self.state.code_window.focus = Some(current.saturating_add(len as u32));

                false
            }
            Event::Key(KeyEvent {
                code: KeyCode::Char(c),
                ..
            }) if matches!(self.state.focus, DebuggerFocus::Reg(_)) && c.is_ascii_hexdigit() => {
                // Start editing the currently focused register
                self.state.is_editing = true;
                self.state.edit_buffer.clear();
                self.state.edit_buffer.push(c.to_ascii_uppercase());
                false
            }
            _ => false,
        }
    }

    fn apply_register_edit(
        &mut self,
        cpu: &mut Cpu,
        _ctx: &mut impl CpuContext,
        register: Register,
        value: u32,
    ) {
        cpu.register_set(register, value as _);
    }

    /// Render the debugger UI
    pub fn render(&mut self, cpu: &Cpu, ctx: &mut impl CpuContext) -> io::Result<()> {
        let config = &self.config;
        let breakpoints = &self.breakpoints;
        let focus = self.state.focus;
        let state = self.state.state;
        let is_editing = self.state.is_editing;
        let edit_buffer = &self.state.edit_buffer;
        let code_window = &self.state.code_window;

        self.terminal.draw(|f| {
            render_frame(
                f,
                cpu,
                ctx,
                config,
                breakpoints,
                focus,
                state,
                is_editing,
                edit_buffer,
                &code_window,
            );
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
    code_window: &CodeWindowState,
) {
    // Create main layout with status bar at bottom
    let main_layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Min(0),    // Main content area
            Constraint::Length(1), // Status bar
        ])
        .split(f.area());

    let content_area = main_layout[0];
    let status_area = main_layout[1];

    // Create the main content layout with a vertical separator
    let main_chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage(50),
            Constraint::Length(1), // Vertical separator
            Constraint::Percentage(50),
        ])
        .split(content_area);

    let right_chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Min(25), Constraint::Percentage(50)])
        .split(main_chunks[2]);

    // Render the three panes
    if state == DebuggerState::Paused {
        render_disassembly(f, main_chunks[0], cpu, ctx, breakpoints, focus, code_window);
    }

    // Render vertical separator
    let separator_lines: Vec<Line> = (0..main_chunks[1].height)
        .map(|_| Line::from("│"))
        .collect();
    let separator = Paragraph::new(separator_lines).style(Style::default().fg(Color::DarkGray));
    f.render_widget(separator, main_chunks[1]);

    render_registers(f, right_chunks[0], cpu, focus, is_editing, edit_buffer);

    // Render status bar at the bottom
    let status_text = format!(
        " {} │ {}: Step │ {}: Run │ {}: Breakpoint │ Tab/Shift+Tab: Switch │ q: Quit ",
        if state == DebuggerState::Running {
            "RUNNING"
        } else {
            "PAUSED "
        },
        config.step_key_label,
        config.run_key_label,
        config.toggle_breakpoint_key_label,
    );

    let status_style = if state == DebuggerState::Running {
        Style::default()
            .bg(Color::Green)
            .fg(Color::Black)
            .add_modifier(Modifier::BOLD)
    } else {
        Style::default()
            .bg(Color::Blue)
            .fg(Color::White)
            .add_modifier(Modifier::BOLD)
    };

    let status_bar = Paragraph::new(Line::from(Span::styled(status_text, status_style)));
    f.render_widget(status_bar, status_area);
}

fn render_disassembly(
    f: &mut Frame,
    area: Rect,
    cpu: &Cpu,
    ctx: &mut impl CpuContext,
    breakpoints: &BTreeSet<u32>,
    focus: DebuggerFocus,
    code_window: &CodeWindowState,
) {
    let pc = cpu.pc_ext(ctx);
    let mut lines = Vec::new();
    let code_window_focus = matches!(focus, DebuggerFocus::Code);

    // Calculate how many instructions we can fit in the available area
    let available_height = area.height as usize;
    let max_lines = available_height.min(100);
    code_window.last_height.set(max_lines);

    // Decode the instructions for the code window, caching the start value if we need to.
    let focus_addr = code_window.focus.unwrap_or(pc);
    let mut instructions = cpu.decode_range(ctx, code_window.start.get(), focus_addr, max_lines);
    if !instructions
        .iter()
        .any(|instruction| instruction.pc() == focus_addr)
    {
        instructions = cpu.decode_range(ctx, focus_addr.saturating_sub(10), focus_addr, max_lines);
    }
    code_window.start.set(instructions.first().unwrap().pc());

    fn control_flow_addr(
        cpu: &Cpu,
        ctx: &impl CpuContext,
        control_flow: ControlFlow,
    ) -> Option<(&'static str, u32)> {
        match control_flow {
            ControlFlow::Call(_, pc) => Some((" ↦ ", cpu.pc_ext_addr(ctx, pc))),
            ControlFlow::Choice(_, pc) => Some((" ↔ ", cpu.pc_ext_addr(ctx, pc))),
            _ => None,
        }
    }

    let control_flow = instructions
        .iter()
        .find(|instruction| instruction.pc() == focus_addr)
        .map(|instruction| instruction.control_flow())
        .and_then(|control_flow| control_flow_addr(cpu, ctx, control_flow));
    let control_flow_pc = instructions
        .iter()
        .find(|instruction| instruction.pc() == pc)
        .map(|instruction| instruction.control_flow())
        .and_then(|control_flow| control_flow_addr(cpu, ctx, control_flow));

    for instruction in instructions {
        let bytes_str = instruction
            .bytes()
            .iter()
            .map(|b| format!("{:02X}", b))
            .collect::<Vec<_>>()
            .join(" ");

        let current_pc = instruction.pc();
        let has_breakpoint = breakpoints.contains(&current_pc);
        let is_current = current_pc == pc;
        let is_focused = current_pc == focus_addr;

        let bp_marker = if has_breakpoint { "●" } else { " " };
        let pc_marker = if is_current { ">" } else { " " };

        let control_flow_marker = control_flow.and_then(|(marker, addr)| {
            if addr == current_pc {
                Some(marker)
            } else {
                None
            }
        });
        let control_flow_marker_pc = control_flow_pc.and_then(|(marker, addr)| {
            if addr == current_pc {
                Some(marker)
            } else {
                None
            }
        });

        let line_text = format!(
            "{}{} {:04X}: {:12} {} ",
            bp_marker,
            pc_marker,
            current_pc,
            bytes_str,
            instruction.decode(),
        );

        let style = if is_current {
            Style::default()
                .fg(Color::Yellow)
                .add_modifier(Modifier::BOLD)
        } else if has_breakpoint {
            Style::default().fg(Color::Red)
        } else {
            Style::default()
        }
        .bg(if is_focused {
            if is_current {
                Color::Yellow
            } else {
                Color::Green
            }
        } else if code_window_focus {
            Color::Black
        } else {
            Color::Reset
        });

        let mut line = Line::default().patch_style(style).spans([line_text]);
        line.push_span(Span::styled(
            control_flow_marker
                .or(control_flow_marker_pc)
                .unwrap_or_default(),
            Style::default().bg(Color::Blue),
        ));
        let text = Text::from(line);
        lines.push(text);
    }

    for (row, line) in area.rows().zip(lines) {
        f.render_widget(line, row);
    }
}

fn render_registers(
    f: &mut Frame,
    area: Rect,
    cpu: &Cpu,
    focus: DebuggerFocus,
    is_editing: bool,
    edit_buffer: &str,
) {
    let mut lines = Vec::new();

    let is_reg_focused = matches!(focus, DebuggerFocus::Reg(_));

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
                Style::default()
                    .fg(if is_reg_focused {
                        Color::LightGreen
                    } else {
                        Color::Green
                    })
                    .add_modifier(Modifier::BOLD | Modifier::UNDERLINED)
            } else {
                Style::default()
                    .fg(if is_reg_focused {
                        Color::LightYellow
                    } else {
                        Color::Yellow
                    })
                    .add_modifier(Modifier::BOLD)
            }
        } else {
            Style::default()
        }
    };

    // Compact format: PC and SP on one line
    let sp = cpu.sp();
    let pc_sp_line = vec![
        Span::styled(
            format!(
                "PC: {}",
                format_value(Register::PC, format!("{:04X}", cpu.pc))
            ),
            reg_style(Register::PC),
        ),
        Span::raw(" "),
        Span::styled(
            format!("SP: {}", format_value(Register::SP, format!("{:02X}", sp))),
            reg_style(Register::SP),
        ),
        Span::raw(" "),
        Span::styled(
            format!(
                "IP: {}",
                format_value(Register::IP, format!("{:02X}", cpu.ip()))
            ),
            reg_style(Register::IP),
        ),
        Span::raw(" "),
        Span::styled(
            format!(
                "IE: {}",
                format_value(Register::IE, format!("{:02X}", cpu.ie()))
            ),
            reg_style(Register::IE),
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
            format!(
                "R{}:{}",
                i,
                format_value(Register::R(i), format!("{:02X}", cpu.r(i)))
            ),
            reg_style(Register::R(i)),
        ));
    }
    lines.push(Line::from(r_spans));

    // A, B, DPTR on one line
    let abc_line = vec![
        Span::styled(
            format!(
                "A:{}",
                format_value(Register::A, format!("{:02X}", cpu.a()))
            ),
            reg_style(Register::A),
        ),
        Span::raw(" "),
        Span::styled(
            format!(
                "B:{}",
                format_value(Register::B, format!("{:02X}", cpu.b()))
            ),
            reg_style(Register::B),
        ),
        Span::raw(" "),
        Span::styled(
            format!(
                "DPTR:{}",
                format_value(Register::DPTR, format!("{:04X}", cpu.dptr()))
            ),
            reg_style(Register::DPTR),
        ),
    ];
    lines.push(Line::from(abc_line));
    lines.push(Line::from(""));

    // PSW flags
    let mut flag_spans = Vec::new();
    for (i, flag) in Flag::all().iter().enumerate() {
        if i > 0 {
            flag_spans.push(Span::raw(" "));
        }
        let style = if cpu.psw(*flag) {
            Style::default().add_modifier(Modifier::BOLD)
        } else {
            Style::default().add_modifier(Modifier::DIM)
        };
        flag_spans.push(Span::styled(flag.short_name(), style));
    }
    lines.push(Line::from(flag_spans));
    lines.push(Line::from(""));

    // Internal RAM display (128 bytes)
    lines.push(Line::from(Span::styled(
        "Internal RAM:",
        Style::default().add_modifier(Modifier::BOLD),
    )));

    // Display 16 bytes per line for 8 lines (128 bytes total)
    for row in 0..8 {
        let addr = row * 16;
        let mut spans = vec![Span::raw(format!("{:02X}: ", addr))];

        for col in 0..16 {
            let byte_addr = addr + col;
            let byte_val = cpu.internal_ram[byte_addr];
            spans.push(Span::raw(format!("{:02X} ", byte_val)));
        }

        lines.push(Line::from(spans));
    }

    let paragraph = Paragraph::new(lines)
        .wrap(Wrap { trim: false })
        .style(Style::default().bg(if is_reg_focused {
            Color::Black
        } else {
            Color::Reset
        }));

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
