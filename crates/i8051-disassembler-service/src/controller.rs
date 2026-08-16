use i8051_disassembler::store::dsl;
use serde::Serialize;

use crate::{Refusal, ServiceError, Session};

#[derive(Debug, Serialize, PartialEq, Eq)]
/// Where the session is looking.
pub struct Location {
    pub address: Option<String>,
    pub can_back: bool,
    pub can_forward: bool,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
/// An edit's undo DSL and stack depths.
pub struct EditResult {
    pub undo: Vec<String>,
    pub address: Option<String>,
    pub undo_depth: usize,
    pub redo_depth: usize,
}

struct NavEntry {
    address: String,
    local: bool,
}

/// A session plus undo and navigation state.
pub struct Controller {
    session: Session,
    undo: Vec<Vec<String>>,
    redo: Vec<Vec<String>>,
    history: Vec<NavEntry>,
    cursor: usize,
    revision: u64,
    /// The revision last persisted by [`Controller::save`].
    saved_revision: u64,
}

impl Controller {
    /// Drive this session.
    pub fn new(session: Session) -> Self {
        Self {
            session,
            undo: Vec::new(),
            redo: Vec::new(),
            history: Vec::new(),
            cursor: 0,
            revision: 0,
            saved_revision: 0,
        }
    }

    /// The session, for read-only queries.
    pub fn session(&self) -> &Session {
        &self.session
    }

    /// Write the session back to its files.
    pub fn save(&mut self) -> Result<crate::SaveReport, crate::DbFileError> {
        let report = self.session.save()?;
        self.saved_revision = self.revision;
        Ok(report)
    }

    /// Bumped on every edit.
    pub fn revision(&self) -> u64 {
        self.revision
    }

    /// Whether there are unsaved edits.
    pub fn is_dirty(&self) -> bool {
        self.revision != self.saved_revision
    }

    fn check_covers_entry_points(&self, dsl: &str) -> Result<(), ServiceError> {
        let Ok(command) = i8051_disassembler::store::from_dsl(dsl) else {
            return Ok(());
        };
        let any = command.as_any();
        let range = any
            .downcast_ref::<i8051_disassembler::commands::MarkData>()
            .map(|c| c.range)
            .or_else(|| {
                any.downcast_ref::<i8051_disassembler::commands::MarkUnknown>()
                    .map(|c| c.range)
            });
        let Some(range) = range else { return Ok(()) };
        let Some(platform) = self.session.db.platform() else {
            return Ok(());
        };
        let region = self.session.db.region(range.space);

        let covered: Vec<_> = platform
            .entry_points()
            .iter()
            .filter(|e| {
                e.space == range.space
                    && (range.range.start..range.range.end).contains(&e.offset)
                    && !region.is_some_and(|r| r.platform_address_disabled(e.offset))
            })
            .collect();
        let Some(first) = covered.first() else {
            return Ok(());
        };

        let vectors: Vec<String> = covered
            .iter()
            .map(|e| format!("{} ({})", range.space.dsl_addr(e.offset), e.name))
            .collect();
        let at = range.space.dsl_addr(first.offset);
        Err(ServiceError::refused(
            Refusal::RangeCoversVectors { vectors },
            vec![
                dsl!(auto_disassemble(address = {at}) # "if that vector is in use"),
                dsl!(disable_platform_address(address = {at}, reason = "...")
                    # "if it is provably unused"),
            ],
        ))
    }

    fn check_barrier_sweep(&self, dsl: &str) -> Result<(), ServiceError> {
        use i8051_disassembler::db::{EquivalentAt, EquivalentKind};

        let Ok(command) = i8051_disassembler::store::from_dsl(dsl) else {
            return Ok(());
        };
        let Some(auto) = command
            .as_any()
            .downcast_ref::<i8051_disassembler::commands::AutoDisassemble>()
        else {
            return Ok(());
        };
        let space = auto.address.space;
        let offset = auto.address.offset;
        let Some(region) = self.session.db.region(space) else {
            return Ok(());
        };
        let EquivalentAt::Defined { start, range } = region.get_equivalent(offset) else {
            return Ok(());
        };
        let kind = match range.equivalent.kind() {
            EquivalentKind::Data => "data",
            EquivalentKind::Unknown => "unknown",
            EquivalentKind::Code => return Ok(()),
        };
        let end = range.end;
        Err(ServiceError::refused(
            Refusal::BarrierStopsAuto {
                at: space.dsl_addr(offset),
                barrier: space.dsl_range(start, end),
                marked: kind.to_string(),
            },
            vec![
                dsl!(clear_equivalents(addresses = {space.dsl_set(start, end)})
                    # "if these bytes are code"),
                dsl!(auto_disassemble(address = {space.dsl_addr(offset)})
                    # "after clearing the barrier"),
            ],
        ))
    }

    fn check_swallows_branch_target(&self, dsl: &str) -> Result<(), ServiceError> {
        use i8051_disassembler::address::{AddressValue, PhysicalAddr, XrefType};

        let Ok(command) = i8051_disassembler::store::from_dsl(dsl) else {
            return Ok(());
        };
        let any = command.as_any();
        let range = any
            .downcast_ref::<i8051_disassembler::commands::MarkData>()
            .map(|c| c.range)
            .or_else(|| {
                any.downcast_ref::<i8051_disassembler::commands::MarkUnknown>()
                    .map(|c| c.range)
            });
        let Some(range) = range else { return Ok(()) };
        let space = range.space;
        let bounds = range.range;

        let mut hits: Vec<(AddressValue, Vec<AddressValue>)> = Vec::new();
        for offset in bounds.start..bounds.end {
            let target = PhysicalAddr { space, offset };
            let mut from: Vec<AddressValue> = self
                .session
                .db
                .xrefs_to(&target)
                .into_iter()
                .filter(|x| matches!(x.xref_type, XrefType::Call | XrefType::Jump))
                .map(|x| x.from.offset)
                .filter(|o| !(bounds.start..bounds.end).contains(o))
                .collect();
            if from.is_empty() {
                continue;
            }
            from.sort_unstable();
            from.dedup();
            hits.push((offset, from));
        }
        let Some((first_target, first_sources)) = hits.first() else {
            return Ok(());
        };

        let listed: Vec<String> = hits
            .iter()
            .take(4)
            .map(|(target, from)| {
                let from: Vec<String> = from.iter().map(|o| space.dsl_addr(*o)).collect();
                format!("{} (from {})", space.dsl_addr(*target), from.join(", "))
            })
            .collect();
        let target = space.dsl_addr(*first_target);
        Err(ServiceError::refused(
            Refusal::RangeSwallowsTargets {
                omitted: hits.len().saturating_sub(4),
                targets: listed,
                first_target: target.clone(),
                first_source: space.dsl_addr(first_sources[0]),
                sources: first_sources.len(),
            },
            vec![
                dsl!(mark_data(
                    range = {space.dsl_range(bounds.start, *first_target)},
                    data_type = DataType::Byte
                ) # "if {target} is a real routine, stop the range at it"),
                dsl!(auto_disassemble(address = {target}) # "then decode it"),
            ],
        ))
    }

    fn check_cpu_still_needed(&self, dsl: &str) -> Result<(), ServiceError> {
        let Ok(command) = i8051_disassembler::store::from_dsl(dsl) else {
            return Ok(());
        };
        if command
            .as_any()
            .downcast_ref::<i8051_disassembler::commands::ClearCpu>()
            .is_none()
        {
            return Ok(());
        }
        let Some(name) = self.session.db.platform().map(|p| p.name().to_string()) else {
            return Ok(());
        };
        let decoded = self
            .session
            .status(None)
            .map(|s| s.coverage.code)
            .unwrap_or(0);
        if decoded == 0 {
            return Ok(());
        }
        Err(ServiceError::refused(
            Refusal::CpuStillNeeded { cpu: name, decoded: u64::from(decoded) },
            Vec::new(),
        ))
    }

    fn check_duplicate_label(&self, dsl: &str) -> Result<(), ServiceError> {
        let Ok(command) = i8051_disassembler::store::from_dsl(dsl) else {
            return Ok(());
        };
        let Some(set) = command
            .as_any()
            .downcast_ref::<i8051_disassembler::commands::SetLabel>()
        else {
            return Ok(());
        };
        let Ok(name) = i8051_disassembler::commands::normalize_label(&set.label) else {
            return Ok(());
        };
        if set.local {
            return self.check_duplicate_local(set, &name);
        }
        let here = set.address.space.dsl_addr(set.address.offset);
        let clash = self
            .session
            .symbols(None)
            .unwrap_or_default()
            .into_iter()
            .find(|s| s.name == name && s.addr != here);
        let Some(clash) = clash else { return Ok(()) };
        let holder = clash.addr;
        Err(ServiceError::refused(
            Refusal::LabelTaken { label: name.to_string(), holder: holder.clone() },
            vec![
                dsl!(set_label(address = {here}, label = "...")
                    # "a name that distinguishes it from {holder}"),
                dsl!(set_label(address = {here}, label = "{name}", local = True)
                    # "if this is a spot inside a routine, not a routine of its own"),
                dsl!(set_label(address = {holder}, label = "...")
                    # "to free the name if this is its better home"),
            ],
        ))
    }

    fn check_duplicate_local(
        &self,
        set: &i8051_disassembler::commands::SetLabel,
        name: &str,
    ) -> Result<(), ServiceError> {
        let space = set.address.space;
        let Some(region) = self.session.db.region(space) else {
            return Ok(());
        };
        let Some(scope) = region.scope_of(set.address.offset) else {
            return Ok(());
        };
        let clash = region
            .labels()
            .find(|&(at, other)| {
                at != set.address.offset
                    && other == name
                    && region.is_local_label(at)
                    && region.scope_of(at) == Some(scope)
            })
            .map(|(at, _)| space.dsl_addr(at));
        let Some(clash) = clash else { return Ok(()) };
        let here = set.address.space.dsl_addr(set.address.offset);
        Err(ServiceError::refused(
            Refusal::LocalLabelTaken { label: name.to_string(), holder: clash },
            vec![dsl!(set_label(address = {here}, label = "...", local = True)
                # "a name unused in this routine")],
        ))
    }

    fn check_provisional_label(dsl: &str) -> Result<(), ServiceError> {
        let Ok(command) = i8051_disassembler::store::from_dsl(dsl) else {
            return Ok(());
        };
        let Some(set) = command
            .as_any()
            .downcast_ref::<i8051_disassembler::commands::SetLabel>()
        else {
            return Ok(());
        };
        let Ok(name) = i8051_disassembler::commands::normalize_label(&set.label) else {
            return Ok(());
        };
        if !i8051_disassembler::labels::is_provisional_name(&name) {
            return Ok(());
        }
        let here = set.address.space.dsl_addr(set.address.offset);
        Err(ServiceError::refused(
            Refusal::GeneratedLabel { label: name },
            vec![
                dsl!(set_label(address = {here}, label = "...")
                    # "a name that says what the code does, e.g. uart_tx"),
                dsl!(set_note(address = {here}, note = Note(content = "..."))
                    # "record what you know if you cannot tell yet"),
            ],
        ))
    }

    fn check_speculative_decode(&self, dsl: &str) -> Result<(), ServiceError> {
        let Ok(command) = i8051_disassembler::store::from_dsl(dsl) else {
            return Ok(());
        };
        let Some(range) = command
            .as_any()
            .downcast_ref::<i8051_disassembler::commands::DisassembleRange>()
        else {
            return Ok(());
        };
        if range.force {
            return Ok(());
        }

        let space = range.range.space;
        let decode = self
            .session
            .db
            .peek_linear(space, range.range.start, range.range.end);
        let mut reasons = Vec::new();
        if decode.out_of_range_targets > 0 {
            reasons.push(format!(
                "{} branch target(s) point outside the loaded image",
                decode.out_of_range_targets
            ));
        }
        if decode.self_misaligned_targets > 0 {
            reasons.push(format!(
                "{} branch target(s) land midway through another instruction in the same range",
                decode.self_misaligned_targets
            ));
        }
        if decode.misaligned_targets > 0 {
            reasons.push(format!(
                "{} branch target(s) land inside existing instructions",
                decode.misaligned_targets
            ));
        }
        if reasons.is_empty() {
            return Ok(());
        }
        Err(ServiceError::refused(
            Refusal::RangeDoesNotDecode {
                count: decode.lines.len(),
                reasons,
            },
            vec![
                dsl!(auto_disassemble(address = {space.dsl_addr(range.range.start)})
                    # "follows flow and stops where it stops"),
                dsl!(mark_data(
                    range = {space.dsl_range(range.range.start, range.range.end)},
                    data_type = DataType::Byte
                ) # "if the whole range is data"),
                dsl!(disassemble_range(
                    range = {space.dsl_range(range.range.start, range.range.end)},
                    force = True
                ) # "to decode the bytes as-is"),
            ],
        ))
    }

    /// Every verb a frontend can invoke.
    pub fn catalog(&self) -> Vec<crate::VerbInfo> {
        crate::verbs::catalog()
    }

    /// Run one verb from JSON arguments.
    pub fn invoke(
        &mut self,
        name: &str,
        args: &serde_json::Map<String, serde_json::Value>,
    ) -> Result<serde_json::Value, ServiceError> {
        if crate::verbs::is_edit(name) {
            let edit = self.apply_named(name, args)?;
            return crate::verbs::json(crate::EditResponse::new(self.session(), edit));
        }
        crate::verbs::dispatch(self, name, args)
            .unwrap_or_else(|| Err(crate::verbs::unknown_verb(name)))
    }

    /// Run one verb from a typed line.
    pub fn exec(&mut self, line: &str) -> Result<serde_json::Value, ServiceError> {
        let (name, kwargs) = i8051_disassembler::store::parse_call(line)
            .map_err(|e| ServiceError::Parse(e.to_string()))?;
        if crate::verbs::is_edit(&name) {
            let edit = self.apply(line)?;
            return crate::verbs::json(crate::EditResponse::new(self.session(), edit));
        }
        let args = crate::verbs::kwargs_to_json(kwargs);
        crate::verbs::dispatch(self, &name, &args)
            .unwrap_or_else(|| Err(crate::verbs::unknown_verb(&name)))
    }

    pub fn apply(&mut self, dsl: &str) -> Result<EditResult, ServiceError> {
        self.check_cpu_still_needed(dsl)?;
        self.check_covers_entry_points(dsl)?;
        self.check_swallows_branch_target(dsl)?;
        self.check_barrier_sweep(dsl)?;
        self.check_speculative_decode(dsl)?;
        Self::check_provisional_label(dsl)?;
        self.check_duplicate_label(dsl)?;
        let inverse = self.session.apply(dsl)?;
        self.undo.push(inverse.clone());
        self.redo.clear();
        self.revision += 1;
        Ok(self.edit_result(inverse))
    }

    /// Apply a structured call, undoably.
    pub fn apply_named(
        &mut self,
        name: &str,
        args: &serde_json::Map<String, serde_json::Value>,
    ) -> Result<EditResult, ServiceError> {
        let dsl = crate::build_command_dsl(name, args)?;
        self.apply(&dsl)
    }

    /// Take back the last edit.
    pub fn undo(&mut self) -> Result<EditResult, ServiceError> {
        let Some(group) = self.undo.pop() else {
            return Ok(self.edit_result(Vec::new()));
        };
        let reinverse = self.apply_group(&group)?;
        self.redo.push(reinverse);
        self.revision += 1;
        Ok(self.edit_result(group))
    }

    /// Reapply the last undone edit.
    pub fn redo(&mut self) -> Result<EditResult, ServiceError> {
        let Some(group) = self.redo.pop() else {
            return Ok(self.edit_result(Vec::new()));
        };
        let reinverse = self.apply_group(&group)?;
        self.undo.push(reinverse);
        self.revision += 1;
        Ok(self.edit_result(group))
    }

    fn apply_group(&mut self, group: &[String]) -> Result<Vec<String>, ServiceError> {
        let mut inverses: Vec<Vec<String>> = Vec::with_capacity(group.len());
        for dsl in group {
            inverses.push(self.session.apply(dsl)?);
        }
        Ok(inverses.into_iter().rev().flatten().collect())
    }

    fn edit_result(&self, undo: Vec<String>) -> EditResult {
        let address = undo.first().and_then(|dsl| crate::command_focus(dsl));
        EditResult {
            undo,
            address,
            undo_depth: self.undo.len(),
            redo_depth: self.redo.len(),
        }
    }

    /// The current navigation location.
    pub fn location(&self) -> Location {
        Location {
            address: self.history.get(self.cursor).map(|e| e.address.clone()),
            can_back: !self.history.is_empty() && self.cursor > 0,
            can_forward: !self.history.is_empty() && self.cursor + 1 < self.history.len(),
        }
    }

    /// Go to an address, recording history.
    pub fn navigate(&mut self, address: &str) -> Result<Location, ServiceError> {
        self.go(address, false)
    }

    /// Go without recording history.
    pub fn navigate_local(&mut self, address: &str) -> Result<Location, ServiceError> {
        self.go(address, true)
    }

    fn go(&mut self, address: &str, local: bool) -> Result<Location, ServiceError> {
        crate::parse_addr(address)?;
        let current = self.history.get(self.cursor);
        if current.map(|e| e.address.as_str()) == Some(address) {
            return Ok(self.location());
        }
        if local && current.is_some_and(|e| e.local) {
            self.history.truncate(self.cursor + 1);
            self.history[self.cursor].address = address.to_string();
        } else {
            let keep = if self.history.is_empty() {
                0
            } else {
                self.cursor + 1
            };
            self.history.truncate(keep);
            self.history.push(NavEntry {
                address: address.to_string(),
                local,
            });
            self.cursor = self.history.len() - 1;
        }
        Ok(self.location())
    }

    /// Back through navigation history.
    pub fn back(&mut self) -> Location {
        if !self.history.is_empty() && self.cursor > 0 {
            self.cursor -= 1;
        }
        self.location()
    }

    /// Forward through navigation history.
    pub fn forward(&mut self) -> Location {
        if self.cursor + 1 < self.history.len() {
            self.cursor += 1;
        }
        self.location()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::MemoryEnvironment;

    #[test]
    fn generated_name_refused_live() {
        let mut c = controller();
        let err = c
            .apply(r#"set_label(address=CODE:0x0, label="sub_0000")"#)
            .expect_err("a generated name should be refused, not silently accepted");
        let text = format!("{err:?}");
        assert!(
            text.contains("what the code does"),
            "unhelpful message: {text}"
        );

        assert!(
            c.apply(r#"set_label(address=CODE:0x0, label="\"loc_0000\"")"#)
                .is_err(),
            "quotes should not smuggle a generated name past the check"
        );

        c.apply(r#"set_label(address=CODE:0x0, label="reset_entry")"#)
            .expect("real name");
        assert!(c.session().disassembly().contains("reset_entry"));

        let env = Box::new(MemoryEnvironment::new().with_file("fw.bin", vec![0x00, 0x00, 0x22]));
        Session::from_commands(
            [
                r#"set_cpu(name="i8051")"#,
                r#"map_bytes(address=CODE:0x0, file="fw.bin", file_offset=0x0, size=0x3)"#,
                r#"set_label(address=CODE:0x0, label="sub_0000")"#,
            ],
            env,
        )
        .expect("a stored provisional label must not break loading");
    }

    #[test]
    fn provisional_name_settles() {
        let mut c = controller();
        c.apply(r#"set_label(address=CODE:0x0, label="maybe_entry", provisional=True)"#)
            .expect("a working guess is storable");
        assert!(c.session().disassembly().contains("maybe_entry"));

        c.apply(r#"set_label(address=CODE:0x0, label="reset_entry")"#)
            .expect("settle");
        assert!(c.session().disassembly().contains("reset_entry"));
    }

    #[test]
    fn duplicate_name_refused() {
        let mut c = controller();
        c.apply(r#"set_label(address=CODE:0x0, label="entry")"#)
            .expect("first use");
        let err = c
            .apply(r#"set_label(address=CODE:0x4, label="entry")"#)
            .expect_err("a duplicate name should be refused");
        let text = format!("{err:?}");
        assert!(text.contains("LabelTaken"), "{text}");
        assert!(text.contains("CODE:0x0"), "holder shown: {text}");

        c.apply(r#"set_label(address=CODE:0x0, label="entry")"#)
            .expect("same address is a rename");
    }

    #[test]
    fn clear_cpu_refused() {
        let mut c = controller();
        let before = c.session().status(None).unwrap().coverage.code;
        assert!(before > 0, "fixture should have decoded code");

        let err = c
            .apply("clear_cpu()")
            .expect_err("clearing a decoded database is refused");
        assert!(format!("{err:?}").contains("CpuStillNeeded"), "{err:?}");
        assert_eq!(
            c.session().status(None).unwrap().coverage.code,
            before,
            "the refusal must leave the session untouched"
        );

        let env = Box::new(MemoryEnvironment::new().with_file("fw.bin", vec![0x00, 0x00, 0x22]));
        let session = Session::from_commands([r#"set_cpu(name="i8051")"#], env).unwrap();
        let mut fresh = Controller::new(session);
        fresh
            .apply("clear_cpu()")
            .expect("nothing is decoded yet, so it is still correctable");
    }

    #[test]
    fn mark_over_target_refused() {
        let mut c = calling_controller();
        let err = c
            .apply("mark_data(range=CODE:0x34..0x35, data_type=DataType::Byte)")
            .expect_err("a call target must not be swept into a data range");
        let text = format!("{err:?}");
        assert!(
            text.contains("CODE:0x34"),
            "the target should be named: {text}"
        );
        assert!(
            text.contains("CODE:0x30"),
            "the caller should be named: {text}"
        );

        c.apply("mark_data(range=CODE:0x33..0x34, data_type=DataType::Byte)")
            .expect("a range that stops at the target is allowed");
    }

    #[test]
    fn sweep_into_barrier_refused() {
        let mut c = calling_controller();
        c.apply("mark_data(range=CODE:0x33..0x34, data_type=DataType::Byte)")
            .expect("0x33 is reached by fallthrough, not by a branch");

        let err = c
            .apply("auto_disassemble(address=CODE:0x33)")
            .expect_err("a root inside a barrier decodes nothing");
        let text = format!("{err:?}");
        assert!(
            text.contains("clear_equivalents"),
            "the way out should be named: {text}"
        );

        c.apply("clear_equivalents(addresses=CODE:{0x33..0x34})")
            .expect("drop the barrier");
        c.apply("auto_disassemble(address=CODE:0x33)")
            .expect("the address decodes once it is free");
    }

    #[test]
    fn live_vector_refused() {
        let mut c = vector_controller();

        let err = c
            .apply("mark_data(range=CODE:0x23..0x26, data_type=DataType::Byte)")
            .expect_err("a range starting at a live vector must not settle it");
        let text = format!("{err:?}");
        assert!(
            text.contains("INT_serial"),
            "the vector should be named: {text}"
        );
        assert!(
            text.contains("disable_platform_address"),
            "the way to retire it should be named: {text}"
        );

        c.apply(r#"disable_platform_address(address=CODE:0x23, reason="IE never enables ES")"#)
            .expect("a vector can be retired with a reason");
        c.apply("mark_data(range=CODE:0x23..0x26, data_type=DataType::Byte)")
            .expect("a retired vector's bytes classify like any others");
    }

    fn vector_controller() -> Controller {
        let mut image = vec![0x00u8; 0x26];
        image[0x23..0x26].copy_from_slice(&[0x02, 0x09, 0x97]); // LJMP 0x0997
        let env = Box::new(MemoryEnvironment::new().with_file("fw.bin", image));
        let session = Session::from_commands(
            [
                r#"set_cpu(name="i8051")"#,
                r#"map_bytes(address=CODE:0x0, file="fw.bin", file_offset=0x0, size=0x26)"#,
            ],
            env,
        )
        .expect("build session");
        Controller::new(session)
    }

    fn calling_controller() -> Controller {
        let mut image = vec![0x00u8; 0x40];
        image[0x0..0x3].copy_from_slice(&[0x02, 0x00, 0x30]); // LJMP 0x30
        image[0x30..0x33].copy_from_slice(&[0x12, 0x00, 0x34]); // LCALL 0x34
        image[0x33] = 0x22; // RET
        image[0x34] = 0x22; // RET
        let env = Box::new(MemoryEnvironment::new().with_file("fw.bin", image));
        let session = Session::from_commands(
            [
                r#"set_cpu(name="i8051")"#,
                r#"map_bytes(address=CODE:0x0, file="fw.bin", file_offset=0x0, size=0x40)"#,
                "auto_disassemble(address=CODE:0x0)",
            ],
            env,
        )
        .expect("build session");
        Controller::new(session)
    }

    fn controller() -> Controller {
        let env = Box::new(MemoryEnvironment::new().with_file("fw.bin", vec![0x00, 0x00, 0x22]));
        let session = Session::from_commands(
            [
                r#"set_cpu(name="i8051")"#,
                r#"map_bytes(address=CODE:0x0, file="fw.bin", file_offset=0x0, size=0x3)"#,
                "auto_disassemble(address=CODE:0x0)",
            ],
            env,
        )
        .expect("build session");
        Controller::new(session)
    }

    #[test]
    fn undo_redo_round_trips() {
        let mut c = controller();
        assert!(!c.session().disassembly().contains("\nreset:"));

        let applied = c
            .apply(r#"set_label(address=CODE:0x0, label="reset")"#)
            .unwrap();
        assert_eq!(applied.undo_depth, 1);
        assert_eq!(applied.redo_depth, 0);
        assert_eq!(applied.address.as_deref(), Some("CODE:0x0"));
        assert!(c.session().disassembly().contains("\nreset:"));

        let undone = c.undo().unwrap();
        assert_eq!(undone.undo_depth, 0);
        assert_eq!(undone.redo_depth, 1);
        assert!(!c.session().disassembly().contains("\nreset:"));

        let redone = c.redo().unwrap();
        assert_eq!(redone.undo_depth, 1);
        assert_eq!(redone.redo_depth, 0);
        assert!(c.session().disassembly().contains("\nreset:"));
    }

    #[test]
    fn edit_clears_redo() {
        let mut c = controller();
        c.apply(r#"set_label(address=CODE:0x0, label="a")"#)
            .unwrap();
        c.undo().unwrap();
        assert_eq!(c.redo().unwrap().undo_depth, 1); // redo works...
        c.undo().unwrap(); // ...back to armed redo
        let forked = c
            .apply(r#"set_label(address=CODE:0x0, label="b")"#)
            .unwrap();
        assert_eq!(forked.redo_depth, 0, "a fresh edit forks history");
        assert_eq!(c.redo().unwrap().undo, Vec::<String>::new());
    }

    #[test]
    fn undo_redo_empty() {
        let mut c = controller();
        assert_eq!(c.undo().unwrap().undo, Vec::<String>::new());
        assert_eq!(c.redo().unwrap().undo, Vec::<String>::new());
    }

    #[test]
    fn navigation_walks_history() {
        let mut c = controller();
        assert_eq!(c.location().address, None);
        assert!(!c.location().can_back && !c.location().can_forward);

        c.navigate("CODE:0x0").unwrap();
        c.navigate("CODE:0x1").unwrap();
        let at2 = c.navigate("CODE:0x2").unwrap();
        assert_eq!(at2.address.as_deref(), Some("CODE:0x2"));
        assert!(at2.can_back && !at2.can_forward);

        let back = c.back();
        assert_eq!(back.address.as_deref(), Some("CODE:0x1"));
        assert!(back.can_back && back.can_forward);

        let fwd = c.forward();
        assert_eq!(fwd.address.as_deref(), Some("CODE:0x2"));
        assert!(!fwd.can_forward);
    }

    #[test]
    fn local_moves_coalesce() {
        let mut c = controller();
        c.navigate("CODE:0x0").unwrap(); // a jump
        c.navigate_local("CODE:0x1").unwrap();
        c.navigate_local("CODE:0x2").unwrap();
        let at = c.navigate_local("CODE:0x3").unwrap();
        assert_eq!(at.address.as_deref(), Some("CODE:0x3"));

        let back = c.back();
        assert_eq!(back.address.as_deref(), Some("CODE:0x0"));
        assert!(!back.can_back, "the jump is the oldest entry");
        assert_eq!(c.forward().address.as_deref(), Some("CODE:0x3"));
    }

    #[test]
    fn navigating_forks_forward() {
        let mut c = controller();
        c.navigate("CODE:0x0").unwrap();
        c.navigate("CODE:0x1").unwrap();
        c.navigate("CODE:0x2").unwrap();
        c.back(); // at 0x1, forward is 0x2
        let forked = c.navigate("CODE:0x10").unwrap();
        assert_eq!(forked.address.as_deref(), Some("CODE:0x10"));
        assert!(!forked.can_forward, "the old forward entry is discarded");
        assert_eq!(c.back().address.as_deref(), Some("CODE:0x1"));
        assert_eq!(c.back().address.as_deref(), Some("CODE:0x0"));
    }

    #[test]
    fn renavigating_is_noop() {
        let mut c = controller();
        c.navigate("CODE:0x0").unwrap();
        c.navigate("CODE:0x1").unwrap();
        let again = c.navigate("CODE:0x1").unwrap();
        assert!(again.can_back && !again.can_forward);
        assert_eq!(c.back().address.as_deref(), Some("CODE:0x0"));
        assert!(!c.back().can_back);
    }

    #[test]
    fn navigate_rejects_garbage() {
        let mut c = controller();
        assert!(matches!(
            c.navigate("not-an-address"),
            Err(ServiceError::Parse(_))
        ));
        assert_eq!(c.location().address, None);
    }

    #[test]
    fn apply_named_records_undo() {
        let mut c = controller();
        let args = serde_json::json!({ "address": "CODE:0x0", "label": "reset" });
        let edit = c
            .apply_named("set_label", args.as_object().unwrap())
            .unwrap();
        assert_eq!(edit.undo_depth, 1);
        assert_eq!(edit.address.as_deref(), Some("CODE:0x0"));
        assert!(c.session().disassembly().contains("\nreset:"));
        c.undo().unwrap();
        assert!(!c.session().disassembly().contains("\nreset:"));
    }

    #[test]
    fn apply_named_reports_errors() {
        let mut c = controller();
        let empty = serde_json::Map::new();
        assert!(matches!(
            c.apply_named("nope", &empty),
            Err(ServiceError::Parse(_))
        ));
        let partial = serde_json::json!({ "address": "CODE:0x0" });
        assert!(matches!(
            c.apply_named("set_label", partial.as_object().unwrap()),
            Err(ServiceError::Parse(_))
        ));
    }

    #[test]
    fn exec_routes_all_categories() {
        let mut c = controller();

        let edit = c
            .exec(r#"set_label(address=CODE:0x0, label="reset")"#)
            .unwrap();
        assert_eq!(edit["address"], "CODE:0x0");
        assert!(c.session().disassembly().contains("\nreset:"));

        let listing = c.exec(r#"listing(space="CODE")"#).unwrap();
        assert_eq!(listing["space"], "CODE");
        assert!(listing["lines"].is_array());

        let nav = c.exec("navigate(address=CODE:0x2)").unwrap();
        assert_eq!(nav["address"], "CODE:0x2");
        assert_eq!(nav["can_back"], false);
    }

    #[test]
    fn exec_rejects_unknown_verb() {
        let mut c = controller();
        assert!(matches!(
            c.exec("frobnicate(x=1)"),
            Err(ServiceError::Parse(_))
        ));
    }
}
