//! Computing a "diff DB": the commands that turn one DB's state into another's.
//!
//! A user maintains a *base* DB by hand (memory layout, disassembly roots, known
//! data). Everything discovered on top of it is saved as a diff: the minimal
//! command sequence D such that applying D on top of the base reproduces the
//! working state. Facts identical to the base are omitted; facts added, changed,
//! or removed relative to the base are captured.

use std::collections::HashSet;

use crate::commands::{
    AutoDisassemble, ClearAutoDisassembleRoot, ClearBytes, ClearComment, ClearEquivalents,
    ClearFunction, ClearLabel, ClearNote, Command, DisassembleRange, MapBytes, MarkData,
    MarkUnknown, OverrideOperand, SetComment, SetConstantBytes, SetFunction, SetLabel, SetNote,
    boxed,
};
use crate::db::Db;
use crate::store::to_dsl;

impl Db {
    /// The commands that transform `base`'s state into this DB's state: a clear
    /// for every base fact this DB changed or dropped, then a set for every fact
    /// this DB has that base does not. Both sides are taken from the canonical
    /// [`to_commands`](Db::to_commands) form, so identical facts cancel out.
    ///
    /// Applying the result on top of `base` reproduces this DB exactly. Clears
    /// come first so a shrunk range is wiped before the survivor is re-set.
    pub fn diff_from(&self, base: &Db) -> Vec<Box<dyn Command>> {
        let base_cmds = base.to_commands();
        let cur_cmds = self.to_commands();
        let base_dsl: HashSet<String> = base_cmds.iter().map(|c| to_dsl(c.as_ref())).collect();
        let cur_dsl: HashSet<String> = cur_cmds.iter().map(|c| to_dsl(c.as_ref())).collect();

        let mut diff = Vec::new();
        for b in &base_cmds {
            if cur_dsl.contains(&to_dsl(b.as_ref())) {
                continue;
            }
            if let Some(clear) = clear_for(b.as_ref()) {
                diff.push(clear);
            }
        }
        for c in cur_cmds {
            if !base_dsl.contains(&to_dsl(c.as_ref())) {
                diff.push(c);
            }
        }
        diff
    }
}

/// The command that undoes one canonical state-producing command, for clearing a
/// base fact the working state changed or removed. `None` for a command with no
/// clear counterpart (there is none among [`Db::to_commands`]'s output).
fn clear_for(cmd: &dyn Command) -> Option<Box<dyn Command>> {
    let any = cmd.as_any();
    if let Some(c) = any.downcast_ref::<SetLabel>() {
        return Some(boxed(ClearLabel::new((c.address.space, c.address.offset))));
    }
    if let Some(c) = any.downcast_ref::<SetComment>() {
        return Some(boxed(ClearComment::new((c.address.space, c.address.offset))));
    }
    if let Some(c) = any.downcast_ref::<SetFunction>() {
        return Some(boxed(ClearFunction::new((c.address.space, c.address.offset))));
    }
    if let Some(c) = any.downcast_ref::<AutoDisassemble>() {
        return Some(boxed(ClearAutoDisassembleRoot::new((
            c.address.space,
            c.address.offset,
        ))));
    }
    if let Some(c) = any.downcast_ref::<OverrideOperand>() {
        return Some(boxed(OverrideOperand::new(
            (c.address.space, c.address.offset),
            c.index,
            None,
        )));
    }
    if let Some(c) = any.downcast_ref::<MapBytes>() {
        let range = c.address.offset..c.address.offset + c.size;
        return Some(boxed(ClearBytes::new((c.address.space, range))));
    }
    if let Some(c) = any.downcast_ref::<SetConstantBytes>() {
        return Some(boxed(ClearBytes::new((c.range.space, c.range.range))));
    }
    if let Some(c) = any.downcast_ref::<MarkData>() {
        return Some(boxed(ClearEquivalents::new((
            c.range.space,
            c.range.range,
        ))));
    }
    if let Some(c) = any.downcast_ref::<MarkUnknown>() {
        return Some(boxed(ClearEquivalents::new((
            c.range.space,
            c.range.range,
        ))));
    }
    if let Some(c) = any.downcast_ref::<DisassembleRange>() {
        return Some(boxed(ClearEquivalents::new((
            c.range.space,
            c.range.range,
        ))));
    }
    if let Some(c) = any.downcast_ref::<SetNote>() {
        return Some(boxed(ClearNote::new(c.note.id.clone())));
    }
    None
}

#[cfg(test)]
mod tests {
    use crate::commands::Environment;
    use crate::db::Db;
    use crate::platform::i8051::CODE;
    use crate::store::{from_dsl_many, to_dsl_many};

    /// Build a DB by applying DSL commands to a fresh i8051 database.
    fn db(commands: &str) -> Db {
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        // A small constant-byte program so labels/comments have somewhere to land.
        db.region_mut(CODE)
            .set_bytes("t.bin", 0, 0x0, &[0x00, 0x22, 0x00, 0x22, 0x00, 0x22, 0x00, 0x22]);
        for command in from_dsl_many(commands).expect("parse") {
            db.apply(command, None::<&dyn Environment>).expect("apply");
        }
        db
    }

    /// Apply a diff on top of a base and assert it reproduces the target exactly.
    fn assert_roundtrip(base_src: &str, target_src: &str) {
        let base = db(base_src);
        let target = db(target_src);
        let diff = target.diff_from(&base);

        let mut reconstructed = db(base_src);
        for command in diff {
            reconstructed
                .apply(command, None::<&dyn Environment>)
                .expect("apply diff");
        }
        assert_eq!(
            to_dsl_many(&reconstructed.to_commands()),
            to_dsl_many(&target.to_commands()),
        );
    }

    #[test]
    fn identical_states_have_an_empty_diff() {
        let base = db(r#"set_label(address=CODE:0x0, label="reset")"#);
        assert!(base.diff_from(&base).is_empty());
    }

    #[test]
    fn added_facts_become_sets() {
        let base = db("");
        let target = db(r#"set_label(address=CODE:0x0, label="reset")"#);
        let diff = to_dsl_many(&target.diff_from(&base));
        assert!(diff.contains("set_label(address=CODE:0x0"), "diff: {diff}");
        assert_roundtrip("", r#"set_label(address=CODE:0x0, label="reset")"#);
    }

    #[test]
    fn changed_facts_reset_over_base() {
        assert_roundtrip(
            r#"set_label(address=CODE:0x0, label="old")"#,
            r#"set_label(address=CODE:0x0, label="new")"#,
        );
    }

    #[test]
    fn removed_facts_emit_a_clear() {
        let base = db(r#"set_label(address=CODE:0x0, label="reset")
set_comment(address=CODE:0x2, comment="hi")"#);
        // Working state kept the comment but dropped the label.
        let target = db(r#"set_comment(address=CODE:0x2, comment="hi")"#);
        let diff = to_dsl_many(&target.diff_from(&base));
        assert!(diff.contains("clear_label(addresses=CODE:{0x0})"), "diff: {diff}");
        assert!(!diff.contains("clear_comment"), "unchanged comment must not clear: {diff}");
        assert_roundtrip(
            r#"set_label(address=CODE:0x0, label="reset")
set_comment(address=CODE:0x2, comment="hi")"#,
            r#"set_comment(address=CODE:0x2, comment="hi")"#,
        );
    }
}
