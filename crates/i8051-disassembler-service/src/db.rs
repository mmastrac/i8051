use std::fmt;
use std::path::{Path, PathBuf};

use i8051_disassembler::commands::{Command, Environment};
use i8051_disassembler::db::Db;
use i8051_disassembler::store::{from_dsl, to_dsl, to_dsl_many};
use serde::{Deserialize, Serialize};

use crate::{FsEnvironment, Session};

#[derive(Serialize, Deserialize)]
struct DbRecord {
    command: String,
}

pub(crate) struct Source {
    base: PathBuf,
    diff: Option<PathBuf>,
}

#[derive(Debug)]
/// A database file could not be used.
pub struct DbFileError(String);

impl fmt::Display for DbFileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for DbFileError {}

impl Session {
    /// Load a database from one file.
    pub fn open(path: &Path) -> Result<Self, DbFileError> {
        Self::open_layered(path, None)
    }

    /// Load a base file plus an overlay.
    pub fn open_layered(base: &Path, diff: Option<&Path>) -> Result<Self, DbFileError> {
        let mut records = read_records(base)?;
        if let Some(diff) = diff.filter(|d| d.exists()) {
            records.extend(read_records(diff)?);
        }
        let env = Box::new(FsEnvironment::new(parent_dir(base)));
        let mut session = Self::from_commands(&records, env)
            .map_err(|e| DbFileError(format!("{}: {e}", base.display())))?;
        session.source = Some(Source {
            base: base.to_path_buf(),
            diff: diff.map(Path::to_path_buf),
        });
        Ok(session)
    }

    /// Whether this session has somewhere to save.
    pub fn is_saveable(&self) -> bool {
        self.source.is_some()
    }

    /// Write the session back to its files.
    pub fn save(&self) -> Result<SaveReport, DbFileError> {
        let source = self
            .source
            .as_ref()
            .ok_or_else(|| DbFileError("session has no file to save to".into()))?;
        match &source.diff {
            Some(diff_path) => {
                let base_records = read_records(&source.base)?;
                let base_db = build_db(&base_records, self.env.as_ref())?;
                let commands = self.db.diff_from(&base_db);
                let count = write_records(diff_path, &commands)?;
                Ok(SaveReport { path: diff_path.clone(), commands: count, diff: true })
            }
            None => {
                let commands = self.db.to_commands();
                let count = write_records(&source.base, &commands)?;
                Ok(SaveReport { path: source.base.clone(), commands: count, diff: false })
            }
        }
    }
}

#[derive(Serialize)]
/// What a save wrote, and where.
pub struct SaveReport {
    /// The file written.
    pub path: PathBuf,
    /// How many command records were written.
    pub commands: usize,
    pub diff: bool,
}

fn read_records(path: &Path) -> Result<Vec<String>, DbFileError> {
    let err = |e: String| DbFileError(format!("{}: {e}", path.display()));
    let text = std::fs::read_to_string(path).map_err(|e| err(e.to_string()))?;
    if text.trim_start().starts_with('[') {
        let records: Vec<DbRecord> = serde_json::from_str(&text).map_err(|e| err(e.to_string()))?;
        Ok(records.into_iter().map(|r| r.command).collect())
    } else {
        Ok(i8051_disassembler::store::split_commands(&text))
    }
}

fn is_dsl_path(path: &Path) -> bool {
    path.extension().is_some_and(|ext| ext.eq_ignore_ascii_case("dsl"))
}

fn write_records(path: &Path, commands: &[Box<dyn Command>]) -> Result<usize, DbFileError> {
    let err = |e: String| DbFileError(format!("{}: {e}", path.display()));
    let text = if is_dsl_path(path) {
        format!("{}\n", to_dsl_many(commands))
    } else {
        let records: Vec<DbRecord> = commands
            .iter()
            .map(|c| DbRecord { command: to_dsl(c.as_ref()) })
            .collect();
        serde_json::to_string_pretty(&records).map_err(|e| err(e.to_string()))?
    };
    let tmp = path.with_extension("tmp");
    std::fs::write(&tmp, text).map_err(|e| err(e.to_string()))?;
    std::fs::rename(&tmp, path).map_err(|e| err(e.to_string()))?;
    Ok(commands.len())
}

fn build_db(records: &[String], env: &dyn Environment) -> Result<Db, DbFileError> {
    let mut db = Db::new();
    for (i, dsl) in records.iter().enumerate() {
        let command = from_dsl(dsl).map_err(|e| DbFileError(format!("record {i}: {e}")))?;
        db.apply(command, Some(env))
            .map_err(|e| DbFileError(format!("record {i}: {e:?}")))?;
    }
    Ok(db)
}

fn parent_dir(path: &Path) -> PathBuf {
    path.parent()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| PathBuf::from("."))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tmp(name: &str) -> PathBuf {
        std::env::temp_dir().join(format!("i8051-{name}-{}.json", std::process::id()))
    }

    #[test]
    fn open_builds_a_session_and_contextualizes_errors() {
        let path = tmp("open");
        std::fs::write(&path, r#"[{"command": "set_cpu(name=\"i8051\")"}]"#).unwrap();
        let session = Session::open(&path).expect("open");
        assert!(session.memory_map().is_empty());

        std::fs::write(&path, "not json").unwrap();
        let err = match Session::open(&path) {
            Err(e) => e.to_string(),
            Ok(_) => panic!("expected a parse error"),
        };
        assert!(err.contains("i8051-open-"), "missing path context: {err}");
        std::fs::remove_file(&path).ok();
    }

    #[test]
    fn layered_save_writes_only_the_diff() {
        let base = tmp("base");
        let diff = tmp("diff");
        std::fs::write(
            &base,
            r#"[{"command": "set_cpu(name=\"i8051\")"},
                {"command": "set_label(address=CODE:0x0, label=\"reset\")"}]"#,
        )
        .unwrap();

        let mut session = Session::open_layered(&base, Some(&diff)).expect("open");
        session.apply(r#"set_comment(address=CODE:0x0, comment="entry")"#).unwrap();
        let report = session.save().expect("save");
        assert!(report.diff);

        let written = std::fs::read_to_string(&diff).unwrap();
        assert!(written.contains("set_comment"), "diff: {written}");
        assert!(!written.contains("set_label"), "base fact leaked into diff: {written}");
        assert!(!written.contains("set_cpu"), "base fact leaked into diff: {written}");

        let reloaded = Session::open_layered(&base, Some(&diff)).expect("reopen");
        let listing = reloaded.disassembly();
        assert!(listing.contains("reset"));
        assert!(listing.contains("entry"));

        std::fs::remove_file(&base).ok();
        std::fs::remove_file(&diff).ok();
    }

    #[test]
    fn dsl_document_loads_comments_and_round_trips_as_dsl() {
        let path =
            std::env::temp_dir().join(format!("i8051-doc-{}.dsl", std::process::id()));
        std::fs::write(
            &path,
            "# a hand-authored program\nset_cpu(name=\"i8051\")\n\nset_label(address=CODE:0x0, label=\"reset\")\n",
        )
        .unwrap();

        let mut session = Session::open(&path).expect("open dsl");
        assert!(session.disassembly().contains("reset"));

        session.apply(r#"set_comment(address=CODE:0x0, comment="entry")"#).unwrap();
        let report = session.save().expect("save");
        assert!(!report.diff);
        let written = std::fs::read_to_string(&path).unwrap();
        assert!(!written.trim_start().starts_with('['), "should be DSL, got JSON: {written}");
        assert!(!written.contains(r#""command""#), "should be DSL, got JSON: {written}");
        assert!(written.contains("set_label(address=CODE:0x0, label=\"reset\", local=False, provisional=False)"));
        assert!(written.contains("set_comment"));

        let reloaded = Session::open(&path).expect("reopen dsl");
        assert!(reloaded.disassembly().contains("reset"));
        assert!(reloaded.disassembly().contains("entry"));

        std::fs::remove_file(&path).ok();
    }

    #[test]
    fn in_memory_session_is_not_saveable() {
        let env = Box::new(crate::MemoryEnvironment::new());
        let session = Session::from_commands([r#"set_cpu(name="i8051")"#], env).unwrap();
        assert!(!session.is_saveable());
        assert!(session.save().is_err());
    }
}
