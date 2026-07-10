//! Completeness assessment: how far a disassembly is from "done".
//!
//! [`assess`] walks the database and derives an ordered worklist of concrete,
//! located problems, plus the coverage and gate status that summarize them.
//! "Done" is not a single boolean but an empty worklist at a chosen [`Gate`];
//! acting on an item can surface new ones, so a consumer re-runs the assessment
//! after each edit rather than caching it.
//!
//! Everything here is derived from existing database facts (the equivalent map,
//! the cross-reference index, and labels); no new state is read or stored.

use std::collections::BTreeMap;

use serde::Serialize;

use crate::address::{AddressSpace, AddressValue, XrefType};
use crate::db::Db;
use crate::labels::LabelKind;
use crate::region::LeakKind;

/// The phases of a disassembly, in dependency order: nothing later is
/// trustworthy until the earlier phases are clear (code must be decoded before
/// its bytes can be classified, and named once decoded).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Phase {
    /// Close the control-flow graph: every call/jump target decoded, no run
    /// leaking into non-code.
    Decode,
    /// Classify every mapped byte as code or typed data (none left undefined).
    Classify,
    /// Give every referenced address a real name (no provisional labels).
    Name,
}

impl Phase {
    const ALL: [Phase; 3] = [Phase::Decode, Phase::Classify, Phase::Name];

    fn rank(self) -> u8 {
        match self {
            Phase::Decode => 0,
            Phase::Classify => 1,
            Phase::Name => 2,
        }
    }

    fn name(self) -> &'static str {
        match self {
            Phase::Decode => "decode",
            Phase::Classify => "classify",
            Phase::Name => "name",
        }
    }
}

/// How much an unresolved item matters, for ordering the worklist.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Severity {
    High,
    Medium,
    Low,
}

impl Severity {
    fn rank(self) -> u8 {
        match self {
            Severity::High => 0,
            Severity::Medium => 1,
            Severity::Low => 2,
        }
    }
}

/// How complete a disassembly must be to count as done. Each rung adds the
/// requirements of the ones before it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Gate {
    /// Control flow closed and every byte classified. Names may be provisional.
    Structural,
    /// Also every referenced address named.
    Named,
}

impl Gate {
    fn required(self) -> &'static [Phase] {
        match self {
            Gate::Structural => &[Phase::Decode, Phase::Classify],
            Gate::Named => &[Phase::Decode, Phase::Classify, Phase::Name],
        }
    }
}

/// One concrete, located problem in the worklist.
#[derive(Debug, Clone, Serialize)]
pub struct Item {
    /// A stable identifier: `<phase>/<kind>/<address>`.
    pub id: String,
    pub phase: Phase,
    /// The problem kind, e.g. `unfollowed_target` or `undefined_bytes`.
    pub kind: &'static str,
    pub severity: Severity,
    /// The address to look at, e.g. `CODE:0x95f`.
    pub address: String,
    /// The affected range, when the item spans one.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub range: Option<String>,
    /// A one-line explanation.
    pub detail: String,
    /// Command(s) that would resolve the item, usually runnable verbatim.
    pub suggested: Vec<String>,
    /// Ordering key; not serialized.
    #[serde(skip)]
    sort: (u8, u8, usize, AddressValue),
}

/// Mapped-byte coverage, summed across spaces.
#[derive(Debug, Clone, Copy, Default, Serialize)]
pub struct Coverage {
    pub total: AddressValue,
    pub code: AddressValue,
    pub data: AddressValue,
    pub undefined: AddressValue,
}

/// The result of an assessment: the gauge (coverage, gate status) plus the
/// ordered worklist that drives the disassembly to done.
#[derive(Debug, Clone, Serialize)]
pub struct Completeness {
    /// The gate this assessment was measured against.
    pub gate: Gate,
    /// Whether the worklist is empty at `gate`.
    pub done: bool,
    /// The earliest phase with outstanding work (where to focus), if any.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub phase: Option<Phase>,
    /// The required phases still holding back `done`.
    pub blocking: Vec<Phase>,
    pub coverage: Coverage,
    /// How many items of each kind, for a quick summary without the full list.
    pub counts: BTreeMap<&'static str, usize>,
    /// The full worklist, ordered by phase, then severity, then address.
    pub items: Vec<Item>,
}

/// Assess the database against the default [`Gate::Named`].
pub fn assess(db: &Db) -> Completeness {
    assess_at(db, Gate::Named)
}

/// Assess the database against `gate`.
pub fn assess_at(db: &Db, gate: Gate) -> Completeness {
    let spaces = db.spaces();
    let mut items = Vec::new();
    let mut coverage = Coverage::default();

    for (rank, &space) in spaces.iter().enumerate() {
        let Some(region) = db.region(space) else {
            continue;
        };

        let usage = region.coverage();
        coverage.code += usage.code;
        coverage.data += usage.data;
        coverage.undefined += usage.undefined;

        for target in region.unresolved_control_targets() {
            let verb = match target.kind {
                XrefType::Call => "call",
                _ => "jump",
            };
            let addr = fmt_addr(space, target.target);
            let from = fmt_addr(space, target.from);
            let (kind, detail) = if target.misaligned {
                (
                    "misaligned_target",
                    format!("{verb} from {from} to {addr} lands inside an existing instruction"),
                )
            } else {
                (
                    "unfollowed_target",
                    format!("{verb} from {from} to {addr} has not been disassembled"),
                )
            };
            items.push(item(
                Phase::Decode,
                kind,
                Severity::High,
                space,
                rank,
                target.target,
                None,
                detail,
                vec![format!("auto_disassemble(address={addr})")],
            ));
        }

        for leak in region.flow_leaks() {
            let from = fmt_addr(space, leak.from);
            let to = fmt_addr(space, leak.to);
            let (kind, severity, anchor, detail, suggested) = match leak.kind {
                LeakKind::IntoUndefined => (
                    "flow_into_undefined",
                    Severity::Medium,
                    leak.to,
                    format!("code at {from} runs into undefined bytes at {to}"),
                    vec![format!("auto_disassemble(address={to})")],
                ),
                LeakKind::IntoData => (
                    "flow_into_data",
                    Severity::Low,
                    leak.to,
                    format!("code at {from} runs into data at {to}"),
                    Vec::new(),
                ),
                LeakKind::OffEnd => (
                    "flow_off_end",
                    Severity::Low,
                    leak.from,
                    format!("code at {from} runs past the end of mapped bytes"),
                    Vec::new(),
                ),
            };
            items.push(item(
                Phase::Decode,
                kind,
                severity,
                space,
                rank,
                anchor,
                None,
                detail,
                suggested,
            ));
        }

        for (start, end) in region.undefined_spans() {
            let range = fmt_range(space, start, end);
            let count = end - start;
            items.push(item(
                Phase::Classify,
                "undefined_bytes",
                Severity::Medium,
                space,
                rank,
                start,
                Some(range.clone()),
                format!("{count} undefined byte(s) at {range}, neither code nor typed data"),
                vec![
                    format!("disassemble_range(range={range})"),
                    format!("mark_data(range={range}, data_type=DataType::Byte)"),
                ],
            ));
        }

        for (offset, label_kind) in region.provisional_labels() {
            let addr = fmt_addr(space, offset);
            let role = match label_kind {
                LabelKind::Sub => "subroutine",
                LabelKind::Loc => "jump target",
            };
            items.push(item(
                Phase::Name,
                "provisional_label",
                Severity::Low,
                space,
                rank,
                offset,
                None,
                format!("{addr} is a referenced {role} with only an auto-generated name"),
                vec![format!("set_label(address={addr}, label=\"...\")")],
            ));
        }
    }

    items.sort_by_key(|it| it.sort);

    let mut counts: BTreeMap<&'static str, usize> = BTreeMap::new();
    for it in &items {
        *counts.entry(it.kind).or_default() += 1;
    }

    coverage.total = coverage.code + coverage.data + coverage.undefined;

    let has = |phase: Phase| items.iter().any(|it| it.phase == phase);
    let phase = Phase::ALL.into_iter().find(|&p| has(p));
    let blocking: Vec<Phase> = gate.required().iter().copied().filter(|&p| has(p)).collect();
    let done = blocking.is_empty();

    Completeness {
        gate,
        done,
        phase,
        blocking,
        coverage,
        counts,
        items,
    }
}

#[allow(clippy::too_many_arguments)]
fn item(
    phase: Phase,
    kind: &'static str,
    severity: Severity,
    space: AddressSpace,
    space_rank: usize,
    offset: AddressValue,
    range: Option<String>,
    detail: String,
    suggested: Vec<String>,
) -> Item {
    let address = fmt_addr(space, offset);
    Item {
        id: format!("{}/{}/{}", phase.name(), kind, address),
        phase,
        kind,
        severity,
        address,
        range,
        detail,
        suggested,
        sort: (phase.rank(), severity.rank(), space_rank, offset),
    }
}

fn fmt_addr(space: AddressSpace, offset: AddressValue) -> String {
    format!("{}:{:#x}", space.dsl_name(), offset)
}

fn fmt_range(space: AddressSpace, start: AddressValue, end: AddressValue) -> String {
    format!("{}:{:#x}..{:#x}", space.dsl_name(), start, end)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commands::{AutoDisassemble, DisassembleRange, MapBytes, SetLabel, boxed};
    use crate::platform::i8051::CODE;

    /// A tiny i8051 image, fully reachable from 0x0: `LCALL 0x4` / `RET`, then
    /// the called subroutine `INC A` / `RET` at 0x4.
    const IMAGE: [u8; 6] = [
        0x12, 0x00, 0x04, // 0x0: LCALL 0x4
        0x22, // 0x3: RET
        0x04, // 0x4: INC A
        0x22, // 0x5: RET
    ];

    struct Env;
    impl crate::commands::Environment for Env {
        fn load_file_bytes(
            &self,
            _f: &str,
            offset: usize,
            size: AddressValue,
        ) -> Result<Vec<u8>, std::io::Error> {
            Ok(IMAGE[offset..offset + size as usize].to_vec())
        }
    }

    fn db_with(commands: Vec<Box<dyn crate::commands::Command>>) -> Db {
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(
            boxed(MapBytes::new((CODE, 0), "img", 0usize, IMAGE.len() as AddressValue)),
            Some(&Env),
        )
        .unwrap();
        for command in commands {
            db.apply(command, Some(&Env)).unwrap();
        }
        db
    }

    #[test]
    fn flags_undefined_bytes_and_unfollowed_call_target() {
        // Disassemble only the reset routine [0x0, 0x4). The LCALL target at 0x4
        // is never followed, and bytes 0x4..0x6 stay undefined.
        let db = db_with(vec![boxed(DisassembleRange::new((CODE, 0u32..4u32)))]);
        let report = assess_at(&db, Gate::Named);

        assert!(!report.done);
        assert_eq!(report.phase, Some(Phase::Decode));
        assert_eq!(report.blocking, vec![Phase::Decode, Phase::Classify, Phase::Name]);

        // The unfollowed call target to 0x4 is the top (decode/high) item.
        let top = &report.items[0];
        assert_eq!(top.kind, "unfollowed_target");
        assert_eq!(top.address, "CODE:0x4");
        assert_eq!(top.suggested, vec!["auto_disassemble(address=CODE:0x4)"]);

        // Undefined bytes and the provisional subroutine label are also flagged.
        assert!(report.counts.contains_key("undefined_bytes"));
        assert!(report.counts.contains_key("provisional_label"));
    }

    #[test]
    fn fully_decoded_and_named_is_done() {
        // Auto-disassemble from both roots (follows the call), name the routines.
        let db = db_with(vec![
            boxed(AutoDisassemble::new((CODE, 0u32))),
            boxed(SetLabel::new((CODE, 0u32), "reset".to_string())),
            boxed(SetLabel::new((CODE, 4u32), "inc_a".to_string())),
        ]);
        let report = assess_at(&db, Gate::Named);

        assert!(report.done, "unexpected items: {:?}", report.items);
        assert_eq!(report.phase, None);
        assert!(report.blocking.is_empty());
        assert_eq!(report.coverage.undefined, 0);
        assert_eq!(report.coverage.code, IMAGE.len() as AddressValue);
    }

    #[test]
    fn structural_gate_ignores_provisional_labels() {
        // Everything decoded and classified, but routines keep auto names.
        let db = db_with(vec![boxed(AutoDisassemble::new((CODE, 0u32)))]);

        let structural = assess_at(&db, Gate::Structural);
        assert!(structural.done, "structural: {:?}", structural.items);

        let named = assess_at(&db, Gate::Named);
        assert!(!named.done);
        assert_eq!(named.phase, Some(Phase::Name));
        assert_eq!(named.blocking, vec![Phase::Name]);
    }
}
