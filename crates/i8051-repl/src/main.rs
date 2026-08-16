use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use anyhow::{Context, Result};
use clap::Parser;
use i8051_disassembler::store::dsl;
use i8051_disassembler_service::{Category, Completion, Controller, Session, VerbInfo, complete};
use rustyline::completion::{Completer, Pair};
use rustyline::error::ReadlineError;
use rustyline::history::DefaultHistory;
use rustyline::{Context as LineContext, Editor, Helper, Highlighter, Hinter, Validator};

#[derive(Parser)]
#[command(
    name = "i8051-repl",
    about = "Interactive DSL REPL for the i8051 disassembler"
)]
struct Cli {
    db: PathBuf,
    #[arg(long)]
    diff: Option<PathBuf>,
    #[arg(long)]
    history: Option<PathBuf>,
}

#[derive(Helper, Hinter, Highlighter, Validator)]
struct DslHelper {
    catalog: Vec<VerbInfo>,
    controller: Rc<RefCell<Controller>>,
}

impl Completer for DslHelper {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &LineContext<'_>,
    ) -> rustyline::Result<(usize, Vec<Pair>)> {
        let controller = self.controller.borrow();
        let Completion { start, candidates } =
            complete(&self.catalog, line, pos, Some(controller.session()));
        let pairs = candidates
            .into_iter()
            .map(|c| Pair {
                display: c.display,
                replacement: c.replacement,
            })
            .collect();
        Ok((start, pairs))
    }
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let session =
        Session::open_layered(&cli.db, cli.diff.as_deref()).context("loading database")?;
    let controller = Rc::new(RefCell::new(Controller::new(session)));
    let history = cli.history.unwrap_or_else(|| default_history(&cli.db));

    let mut rl: Editor<DslHelper, DefaultHistory> = Editor::new()?;
    rl.set_helper(Some(DslHelper {
        catalog: controller.borrow().catalog(),
        controller: controller.clone(),
    }));
    let _ = rl.load_history(&history);

    println!("i8051-repl. Tab to complete, :help for meta-commands, Ctrl-D to quit.");

    loop {
        match rl.readline("8051> ") {
            Ok(line) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }
                let _ = rl.add_history_entry(line);
                if let Some(meta) = line.strip_prefix(':') {
                    if run_meta(meta, &mut controller.borrow_mut()) {
                        break;
                    }
                    continue;
                }
                match controller.borrow_mut().exec(line) {
                    Ok(value) => println!("{}", render(line, &value)),
                    Err(e) => {
                        let msg = e.to_string();
                        if let Some(caret) =
                            i8051_disassembler_service::caret_diagnostic(line, &msg)
                        {
                            eprintln!("{caret}");
                        }
                        eprintln!("error: {msg}");
                    }
                }
            }
            Err(ReadlineError::Interrupted) => continue,
            Err(ReadlineError::Eof) => break,
            Err(e) => {
                eprintln!("error: {e}");
                break;
            }
        }
    }

    let _ = rl.save_history(&history);
    Ok(())
}

fn run_meta(meta: &str, controller: &mut Controller) -> bool {
    let mut parts = meta.split_whitespace();
    match parts.next().unwrap_or("") {
        "q" | "quit" | "exit" => return true,
        "help" | "h" | "" => print_help(),
        "catalog" | "verbs" => print_catalog(controller, parts.next()),
        "save" => match controller.save() {
            Ok(report) => println!(
                "saved {} record(s) to {} ({})",
                report.commands,
                report.path.display(),
                if report.diff { "diff" } else { "full" }
            ),
            Err(e) => eprintln!("save failed: {e}"),
        },
        other => eprintln!("unknown meta-command `:{other}` (try :help)"),
    }
    false
}

fn print_catalog(controller: &Controller, filter: Option<&str>) {
    let catalog = controller.catalog();
    if let Some(v) = filter.and_then(|f| catalog.iter().find(|v| v.name == f)) {
        println!("{}: {}", v.name, v.description);
        for a in &v.args {
            let opt = if a.required { "" } else { " (optional)" };
            println!("  {}{}: {}", a.name, opt, a.description);
        }
        println!("  e.g. {}", v.example);
        return;
    }
    for category in [Category::Edit, Category::Query, Category::Nav] {
        let verbs: Vec<&VerbInfo> = catalog
            .iter()
            .filter(|v| v.category == category)
            .filter(|v| filter.is_none_or(|f| v.name.starts_with(f)))
            .collect();
        if verbs.is_empty() {
            continue;
        }
        println!("{}:", category_label(category));
        for v in verbs {
            let args: Vec<String> = v.args.iter().map(|a| a.name.clone()).collect();
            println!(
                "  {:<40} {}",
                format!("{}({})", v.name, args.join(", ")),
                first_sentence(&v.description)
            );
        }
    }
}

fn first_sentence(doc: &str) -> &str {
    match doc.find(". ") {
        Some(i) => &doc[..i + 1],
        None => doc,
    }
}

fn print_help() {
    println!("Type a command and press Enter, for example:");
    for example in [
        dsl!(status()),
        dsl!(listing(space = "CODE", start = 0, count = 40)),
        dsl!(set_label(address = { "CODE:0x100" }, label = "reset")),
        dsl!(navigate(address = { "CODE:0x100" })),
    ] {
        println!("  {example}");
    }
    println!(
        "\nWrite addresses bare (CODE:0x100) and text in quotes. Tab completes command and
argument names. {} prints one command's syntax with a filled example.

Meta-commands:
  :catalog [prefix]   list commands, or one command's syntax card
  :save               write edits to the database file (also: save())
  :help               this help
  :quit               exit (or Ctrl-D)",
        dsl!(help(command = "set_note"))
    );
}

fn category_label(category: Category) -> &'static str {
    match category {
        Category::Edit => "edit",
        Category::Query => "query",
        Category::Nav => "nav",
    }
}

fn render(line: &str, value: &serde_json::Value) -> String {
    let verb = i8051_disassembler::store::parse_call(line)
        .map(|(name, _)| name)
        .unwrap_or_default();
    i8051_disassembler_service::render_human(&verb, value).unwrap_or_else(|| pretty(value))
}

fn pretty(value: &serde_json::Value) -> String {
    serde_json::to_string_pretty(value).unwrap_or_else(|_| value.to_string())
}

fn default_history(db: &Path) -> PathBuf {
    db.parent()
        .filter(|p| !p.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."))
        .join(".i8051-repl-history")
}
