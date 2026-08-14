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

use crate::address::{AddressSpace, AddressValue, PhysicalAddr, XrefType};
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
    /// Record what every named routine does: a `set_note` on each (the durable,
    /// detailed account future passes surface), plus a `set_comment` when its
    /// purpose is clear enough to state in one line.
    Document,
}

impl Phase {
    fn rank(self) -> u8 {
        match self {
            Phase::Decode => 0,
            Phase::Classify => 1,
            Phase::Name => 2,
            Phase::Document => 3,
        }
    }

    fn name(self) -> &'static str {
        match self {
            Phase::Decode => "decode",
            Phase::Classify => "classify",
            Phase::Name => "name",
            Phase::Document => "document",
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
    /// Also every named routine documented with a note (the default): the
    /// disassembly carries its own explanation forward, not just its structure.
    Documented,
}

impl Gate {
    fn required(self) -> &'static [Phase] {
        match self {
            Gate::Structural => &[Phase::Decode, Phase::Classify],
            Gate::Named => &[Phase::Decode, Phase::Classify, Phase::Name],
            Gate::Documented => {
                &[Phase::Decode, Phase::Classify, Phase::Name, Phase::Document]
            }
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
    /// Ordering key, not serialized.
    #[serde(skip)]
    sort: (u8, u8, u8, u32, AddressValue, usize, AddressValue),
}

impl Item {
    /// Sort behind peers without removing it. Not suppressed, just sinks down
    /// the worklist.
    fn deferred(mut self) -> Self {
        self.sort.2 = 1;
        self
    }

    /// Heavily-referenced targets first.
    fn ranked_by_callers(mut self, callers: usize, first_caller: AddressValue) -> Self {
        self.sort.3 = u32::MAX - u32::try_from(callers).unwrap_or(u32::MAX);
        self.sort.4 = first_caller;
        self
    }
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

/// Assess the database against the default [`Gate::Documented`].
pub fn assess(db: &Db) -> Completeness {
    assess_at(db, Gate::Documented)
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

        // Coverage shrinks silently when bytes go missing.
        for (start, end) in region.mapping_gaps() {
            let from = fmt_addr(space, start);
            let to = fmt_addr(space, end);
            items.push(item(
                Phase::Decode,
                "unmapped_gap",
                Severity::Medium,
                space,
                rank,
                start,
                Some(fmt_range(space, start, end)),
                format!(
                    "nothing is mapped at {from}..{to}, but there are bytes on both sides. The \
                     image has a hole: {} byte(s) that are not mapped. Map them from an image \
                     file or fill them if they are genuinely absent.",
                    end - start
                ),
                vec![
                    format!(
                        "map_bytes(address={from}, file=\"...\", file_offset=0x{start:x}, size=0x{:x})",
                        end - start
                    ),
                    format!("set_constant_bytes(range={}, value=0x0)", fmt_range(space, start, end)),
                ],
            ));
        }

        for target in region.unresolved_control_targets() {
            let verb = match target.kind {
                XrefType::Call => "call",
                _ => "jump",
            };
            let addr = fmt_addr(space, target.target);
            let from = fmt_addr(space, target.from);
            // The whole decoded run.
            let extent_end = region
                .instruction_range(target.from)
                .map(|(_, end)| end)
                .unwrap_or(target.from + 1);
            let extent_start = code_run_start(db, space, target.from);
            let extent = format!("{}:{extent_start:#x}..{extent_end:#x}", space.dsl_name());
            let retire = retire_vector_first(db, space, extent_start, extent_end);
            // `mark_data` is for undefined bytes only, so decoded ones have to
            // be cleared first.
            let reclassify: Vec<String> =
                match settle_reference_first(db, space, extent_start, extent_end) {
                    Some(blocked) => vec![blocked],
                    None => vec![
                        format!(
                            "clear_equivalents(addresses={}:{{{extent_start:#x}..{extent_end:#x}}})",
                            space.dsl_name()
                        ),
                        format!(
                            "mark_data(range={extent}, data_type=DataType::Byte)  # if these bytes \
                             are not code"
                        ),
                    ],
                };
            let (kind, anchor, detail, suggested) = if !region.has_byte(target.target) {
                {
                    let space_name = space.dsl_name();
                    let bits = region.covering_address_bits();
                    let mut suggested = vec![format!("peek(address={from}, lines=4)")];
                    suggested.extend(retire.clone());
                    suggested.extend(reclassify.clone());
                    if region.address_bits().is_none() {
                        suggested.push(format!(
                            "set_address_bits(space=\"{space_name}\", bits={bits})"
                        ));
                    }
                    (
                        "target_outside_image",
                        target.from,
                        format!(
                            "{verb} from {from} to {addr} is outside the mapped image, so it \
                             cannot be followed. These bytes may not be code, the image may be \
                             incomplete, or you may need to limit the address width. `peek`ing the \
                             source may help decide"
                        ),
                        suggested,
                    )
                }
            } else if target.misaligned {
                let covering = region
                    .covering_instruction(target.target)
                    .map(|(start, end)| format!("{start:#x}..{end:#x}"))
                    .unwrap_or_else(|| format!("{:#x}", target.target));
                (
                    "misaligned_target",
                    target.from,
                    format!(
                        "{verb} from {from} to {addr} lands inside the instruction at {}:{covering}, \
                         not at its start. Either the bytes at {from} are not code (e.g.: a branch \
                         decoded out of filler) or that instruction is decoded from the wrong \
                         offset. `peek`ing from both ends may help decide",
                        space.dsl_name()
                    ),
                    {
                        let mut suggested = vec![format!("peek(address={from}, lines=4)")];
                        suggested.extend(retire.clone());
                        suggested.extend(reclassify.clone());
                        suggested.push(format!(
                            "clear_equivalents(addresses={}:{{{covering}}})",
                            space.dsl_name()
                        ));
                        suggested.push(format!("auto_disassemble(address={addr})"));
                        suggested
                    },
                )
            } else if let crate::db::EquivalentAt::Defined { start, range } =
                region.get_equivalent(target.target)
                && range.equivalent.kind() != crate::db::EquivalentKind::Code
            {
                let space_name = space.dsl_name();
                let barrier = format!("{space_name}:{start:#x}..{:#x}", range.end);
                let what = match range.equivalent.kind() {
                    crate::db::EquivalentKind::Data => "data",
                    _ => "unknown",
                };
                (
                    "unfollowed_target",
                    target.target,
                    format!(
                        "{verb} from {from} to {addr} has not been disassembled, and {addr} sits \
                         inside {barrier}, which is marked {what}. Either those \
                         bytes are code and the barrier is wrong, or the {verb} at {from} is \
                         filler that only looks like one. `peek`ing both may help decide"
                    ),
                    {
                        let mut suggested = vec![
                            format!("peek(address={addr}, lines=4)"),
                            format!("peek(address={from}, lines=4)"),
                            format!(
                                "clear_equivalents(addresses={space_name}:{{{start:#x}..{:#x}}})  \
                                 # if {addr} is code",
                                range.end
                            ),
                            format!(
                                "auto_disassemble(address={addr})  # after clearing the barrier"
                            ),
                        ];
                        suggested.extend(retire.clone());
                        suggested.extend(reclassify.clone());
                        suggested
                    },
                )
            } else {
                (
                    "unfollowed_target",
                    target.target,
                    format!("{verb} from {from} to {addr} has not been disassembled"),
                    vec![format!("auto_disassemble(address={addr})")],
                )
            };
            items.push(item(
                Phase::Decode,
                kind,
                Severity::High,
                space,
                rank,
                anchor,
                None,
                detail,
                suggested,
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
                LeakKind::IntoData => {
                    let code_end = region
                        .instruction_range(leak.from)
                        .map(|(_, end)| end)
                        .unwrap_or(leak.from + 1);
                    let code_start = code_run_start(db, space, leak.from);
                    let code_extent = fmt_range(space, code_start, code_end);
                    let (data_start, data_end) = match region.get_equivalent(leak.to) {
                        crate::db::EquivalentAt::Defined { start, range } => (start, range.end),
                        crate::db::EquivalentAt::Undefined(_) => (leak.to, leak.to + 1),
                    };
                    let data_extent = format!("{data_start:#x}..{data_end:#x}");
                    let as_code = format!(
                        "clear_equivalents(addresses={}:{{{data_extent}}})  # if {to} is code",
                        space.dsl_name()
                    );
                    let then_decode =
                        format!("auto_disassemble(address={to})  # after clearing the barrier");
                    let mut as_filler: Vec<String> =
                        retire_vector_first(db, space, code_start, code_end)
                            .into_iter()
                            .collect();
                    as_filler.push(format!(
                        "clear_equivalents(addresses={}:{{{code_start:#x}..{code_end:#x}}})",
                        space.dsl_name()
                    ));
                    as_filler.push(format!(
                        "mark_data(range={code_extent}, data_type=DataType::Byte)  # if {from} is \
                         filler rather than code"
                    ));
                    let decode = db.peek_linear(space, leak.to, data_end);
                    let decodes_like_code = decode.out_of_range_targets == 0
                        && decode.misaligned_targets == 0
                        && decode.self_misaligned_targets == 0;

                    let mut suggested = vec![format!("peek(address={from}, lines=4)")];
                    let evidence = if decodes_like_code {
                        suggested.push(as_code);
                        suggested.push(then_decode);
                        suggested.extend(as_filler);
                        format!("The bytes at {to} do decode like code, so try that reading first")
                    } else {
                        suggested.extend(as_filler);
                        suggested.push(as_code);
                        suggested.push(then_decode);
                        format!(
                            "Decoding {to} as code branches outside the image or into the middle \
                             of an instruction, so the filler reading is the likelier one"
                        )
                    };
                    (
                        "flow_into_data",
                        Severity::Low,
                        leak.to,
                        format!(
                            "code at {from} runs into data at {to}, so execution would fall out \
                             of code and into bytes classified as data. One of the two is wrong: \
                             either {to} is really code and the barrier over it should go, or \
                             {from} is filler that decoded as code and belongs with the data. \
                             {evidence}"
                        ),
                        suggested,
                    )
                }
                LeakKind::OffEnd => {
                    let code_end = region
                        .instruction_range(leak.from)
                        .map(|(_, end)| end)
                        .unwrap_or(leak.from + 1);
                    let code_start = code_run_start(db, space, leak.from);
                    let code_extent = fmt_range(space, code_start, code_end);
                    let mut suggested = vec![format!("peek(address={from}, lines=4)")];
                    suggested.extend(retire_vector_first(db, space, code_start, code_end));
                    suggested.push(format!(
                        "clear_equivalents(addresses={}:{{{code_start:#x}..{code_end:#x}}})",
                        space.dsl_name()
                    ));
                    suggested.push(format!(
                        "mark_data(range={code_extent}, data_type=DataType::Byte)  # if {from} is \
                         filler rather than code"
                    ));
                    suggested.push(format!(
                        "map_bytes(address={to}, file=\"...\", file_offset=0x0, size=0x0)  # if \
                         the image continues past what is loaded"
                    ));
                    (
                        "flow_off_end",
                        Severity::Low,
                        leak.from,
                        format!(
                            "code at {from} runs past the end of mapped bytes, so the next \
                             instruction would come from nothing. Either {from} is filler that \
                             decoded as code, or the image is incomplete and continues past what \
                             is loaded. `peek` it and decide"
                        ),
                        suggested,
                    )
                }
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
            // Give enough context to name the routine from this item alone
            // (a caller and the first instruction), so the model does not have
            // to go read the listing before it can commit a name.
            let mut detail = format!("{addr}: unnamed {role}");
            if let Some(caller) = first_caller(db, space, offset) {
                detail.push_str(&format!(", referenced from {caller}"));
            }
            if let Some(insn) = region.instruction_text(offset) {
                detail.push_str(&format!("; starts `{insn}`"));
            }
            // Either mark means an earlier pass already engaged with this
            // address and still did not name it.
            let draft = region.is_draft_label(offset);
            let noted = !db.get_notes_overlapping(space, offset..offset + 1).is_empty();
            if draft {
                let current = region.get_label(offset).unwrap_or_default();
                detail.push_str(&format!(
                    "; currently the working name `{current}` — sharpen it, or set it again \
                     without provisional once you are satisfied"
                ));
            } else if noted {
                detail.push_str(
                    "; a note already covers this address, so an earlier pass studied it \
                     without naming it — read that note first",
                );
            }
            let suggested = if draft {
                vec![format!("set_label(address={addr}, label=\"...\")")]
            } else if matches!(label_kind, LabelKind::Loc) {
                // Reached only by jumps.
                vec![
                    format!("set_label(address={addr}, label=\".loop\", local=True)"),
                    format!("set_label(address={addr}, label=\"...\")"),
                ]
            } else {
                vec![
                    format!("set_label(address={addr}, label=\"...\")"),
                    format!("set_label(address={addr}, label=\"...\", provisional=True)"),
                ]
            };
            let incoming = db.xrefs_to(&PhysicalAddr { space, offset });
            let first_ref = incoming.iter().map(|x| x.from.offset).min().unwrap_or(offset);
            let entry = item(
                Phase::Name,
                "provisional_label",
                Severity::Low,
                space,
                rank,
                offset,
                None,
                detail,
                suggested,
            )
            .ranked_by_callers(incoming.len(), first_ref);
            items.push(if draft || noted { entry.deferred() } else { entry });
        }
        
        for (offset, refs, first, inferred) in region.unnamed_pointer_targets() {
            let addr = fmt_addr(space, offset);
            let from = fmt_addr(space, first);
            let (plural, verb) = if refs == 1 { ("site", "loads") } else { ("sites", "load") };
            let detail = if inferred {
                format!(
                    "{addr}: {refs} {plural} {verb} this address as a pointer (first {from}), \
                     but it is unnamed. Reading {from} may help decide."
                )
            } else {
                format!(
                    "{addr}: unnamed data, addressed as a pointer by {refs} {plural} \
                     (first {from}). Reading {from} may help."
                )
            };
            items.push(
                item(
                    Phase::Name,
                    "unnamed_data",
                    Severity::Low,
                    space,
                    rank,
                    offset,
                    None,
                    detail,
                    vec![format!("set_label(address={addr}, label=\"...\")")],
                )
                .ranked_by_callers(refs, first),
            );
        }

        // A named routine with no note is understood but unrecorded.
        for (offset, name) in region.named_routines() {
            // A note whose range covers the entry byte documents the routine.
            if !db.get_notes_overlapping(space, offset..offset + 1).is_empty() {
                continue;
            }
            let addr = fmt_addr(space, offset);
            let has_comment = region.get_comment(offset).is_some();
            let mut detail = format!(
                "{addr} ({name}) is named but has no note. Record what it does and how \
                 it works as a `set_note`, the detailed account future passes read"
            );
            let mut suggested =
                vec![format!("set_note(address={addr}, note=Note(content=\"...\"))")];
            if !has_comment {
                detail.push_str("; add a one-line `set_comment` too if its purpose is clear");
                suggested.push(format!("set_comment(address={addr}, comment=\"...\")"));
            }
            items.push(item(
                Phase::Document,
                "undocumented_routine",
                Severity::Low,
                space,
                rank,
                offset,
                None,
                detail,
                suggested,
            ));
        }
    }

    // The CPU's own vectors. Entry points whether or not anything references
    // them.
    for entry in db.undecoded_entry_points() {
        let addr = fmt_addr(entry.space, entry.offset);
        let name = entry.name;
        let reason = entry.reason;
        items.push(item(
            Phase::Decode,
            "undecoded_entry_point",
            Severity::Medium,
            entry.space,
            0,
            entry.offset,
            None,
            format!(
                "{addr} ({name}) is a possible vector: the hardware transfers control here on \
                 {reason}, but the bytes are not decoded as code. `peek` them. A jump or call \
                 landing inside the image means the vector is in use and those bytes are its \
                 handler. Bytes that decode to nothing coherent may mean the interrupt is never \
                 enabled."
            ),
            vec![
                format!("peek(address={addr}, lines=4)"),
                format!("auto_disassemble(address={addr})  # if the vector is in use"),
                format!(
                    "disable_platform_address(address={addr}, reason=\"...\")  # if this \
                     interrupt is never enabled — what the firmware writes to IE says which"
                ),
            ],
        ));
    }

    // An instruction with an ambiguous operand.
    for (site, value, spaces) in db.undecided_operands() {
        let at = fmt_addr(site.space, site.offset);
        let candidates: Vec<String> = spaces.iter().map(|s| fmt_addr(*s, value)).collect();
        let detail = format!(
            "{at} loads 0x{value:x} as an address, but it is uncertain which memory space it \
             refers to {}.",
             candidates.join(" or ")
        );
        let mut suggested: Vec<String> = spaces
            .iter()
            .map(|s| format!("set_operand_pointer(address={at}, space=\"{}\")", s.dsl_name()))
            .collect();
        suggested.push(format!("set_operand_value(address={at})"));
        items.push(
            item(
                Phase::Document,
                "undecided_operand",
                Severity::Low,
                site.space,
                usize::MAX,
                site.offset,
                None,
                detail,
                suggested,
            )
            .deferred(),
        );
    }

    items.sort_by_key(|it| it.sort);

    let mut counts: BTreeMap<&'static str, usize> = BTreeMap::new();
    for it in &items {
        *counts.entry(it.kind).or_default() += 1;
    }

    coverage.total = coverage.code + coverage.data + coverage.undefined;

    let has = |phase: Phase| items.iter().any(|it| it.phase == phase);
    // The earliest *gate-relevant* phase with work: measuring against `named`
    // should not point at leftover `document` items the gate does not require.
    let phase = gate.required().iter().copied().find(|&p| has(p));
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
    // Validate the suggestion DSL in debug mode so we don't accidentally hand
    // off a bad one.
    #[cfg(debug_assertions)]
    for suggestion in &suggested {
        if let Err(e) = crate::store::parse_call(suggestion) {
            panic!("worklist suggestion for `{kind}` does not parse: {suggestion:?}: {e}");
        }
    }
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
        sort: (phase.rank(), severity.rank(), 0, u32::MAX, 0, space_rank, offset),
    }
}

/// A referenced-from phrase for a provisional label: the first call/jump site,
/// with the caller's own name when it has one.
fn first_caller(db: &Db, space: AddressSpace, offset: AddressValue) -> Option<String> {
    let xrefs = db.xrefs_to(&PhysicalAddr { space, offset });
    let edge = xrefs
        .iter()
        .find(|x| matches!(x.xref_type, XrefType::Call | XrefType::Jump))?;
    let from = fmt_addr(edge.from.space, edge.from.offset);
    match db.region(edge.from.space).and_then(|r| r.get_label(edge.from.offset)) {
        Some(name) => Some(format!("{from} ({name})")),
        None => Some(from),
    }
}

fn is_live_entry_point(db: &Db, space: AddressSpace, offset: AddressValue) -> bool {
    let Some(platform) = db.platform() else {
        return false;
    };
    platform
        .entry_points()
        .iter()
        .any(|e| e.space == space && e.offset == offset)
        && !db.region(space).is_some_and(|r| r.platform_address_disabled(offset))
}

fn code_run_start(db: &Db, space: AddressSpace, from: AddressValue) -> AddressValue {
    let Some(region) = db.region(space) else {
        return from;
    };
    let mut start = from;
    // Bounded because a pathological decode could otherwise walk the image.
    for _ in 0..64 {
        if start == 0 || region.get_label(start).is_some() {
            break;
        }
        if !db.xrefs_to(&PhysicalAddr { space, offset: start }).is_empty() {
            break;
        }
        match region.get_equivalent(start - 1) {
            crate::db::EquivalentAt::Defined { start: prev_start, range }
                if range.equivalent.kind() == crate::db::EquivalentKind::Code
                    && range.end == start =>
            {
                // A live vector's instruction belongs to that vector.
                if is_live_entry_point(db, space, prev_start) {
                    break;
                }
                start = prev_start
            }
            _ => break,
        }
    }
    start
}

fn settle_reference_first(
    db: &Db,
    space: AddressSpace,
    start: AddressValue,
    end: AddressValue,
) -> Option<String> {
    let bounds = start..end;
    let (target, from) = (start..end).find_map(|offset| {
        db.xrefs_to(&PhysicalAddr { space, offset })
            .into_iter()
            .find(|x| {
                matches!(x.xref_type, XrefType::Call | XrefType::Jump)
                    && !bounds.contains(&x.from.offset)
            })
            .map(|x| (offset, x.from.offset))
    })?;
    let target = fmt_addr(space, target);
    let from = fmt_addr(space, from);
    Some(format!(
        "peek(address={from}, lines=4)  # {target} cannot be marked data while {from} branches \
         to it; settle whether {from} is code first"
    ))
}

fn retire_vector_first(
    db: &Db,
    space: AddressSpace,
    start: AddressValue,
    end: AddressValue,
) -> Option<String> {
    let platform = db.platform()?;
    let region = db.region(space);
    let entry = platform.entry_points().iter().find(|e| {
        e.space == space
            && (start..end).contains(&e.offset)
            && !region.is_some_and(|r| r.platform_address_disabled(e.offset))
    })?;
    let at = fmt_addr(space, entry.offset);
    Some(format!(
        "disable_platform_address(address={at}, reason=\"...\")  # {at} ({}) is a vector; its \
         bytes cannot be classified until it is retired",
        entry.name
    ))
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
    use crate::commands::{AutoDisassemble, DisassembleRange, MapBytes, SetLabel, UnmapBytes, boxed};
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
        let db = db_with(vec![boxed(DisassembleRange::new((CODE, 0u32..4u32), false))]);
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
            boxed(SetLabel::new((CODE, 0u32), "reset".to_string(), false, false)),
            boxed(SetLabel::new((CODE, 4u32), "inc_a".to_string(), false, false)),
        ]);
        let report = assess_at(&db, Gate::Named);

        assert!(report.done, "unexpected items: {:?}", report.items);
        assert_eq!(report.phase, None);
        assert!(report.blocking.is_empty());
        assert_eq!(report.coverage.undefined, 0);
        assert_eq!(report.coverage.code, IMAGE.len() as AddressValue);
    }

    #[test]
    fn named_but_unnoted_routine_blocks_the_documented_gate() {
        use crate::commands::SetNote;

        // Fully decoded and named: done at `named`, `inc_a` missing a note.
        let mut db = db_with(vec![
            boxed(AutoDisassemble::new((CODE, 0u32))),
            boxed(SetLabel::new((CODE, 0u32), "reset".to_string(), false, false)),
            boxed(SetLabel::new((CODE, 4u32), "inc_a".to_string(), false, false)),
        ]);

        assert!(assess_at(&db, Gate::Named).done);

        let doc = assess_at(&db, Gate::Documented);
        assert!(!doc.done);
        assert_eq!(doc.phase, Some(Phase::Document));
        assert_eq!(doc.blocking, vec![Phase::Document]);
        let item = doc
            .items
            .iter()
            .find(|it| it.kind == "undocumented_routine")
            .expect("undocumented_routine item");
        assert_eq!(item.address, "CODE:0x4");

        // Noting the routine clears the document phase.
        let note = crate::note::Note::new(None, "increments A");
        db.apply(boxed(SetNote::new((CODE, 4u32..5u32), note)), Some(&Env)).unwrap();
        let after = assess_at(&db, Gate::Documented);
        assert!(
            after.done,
            "remaining: {:?}",
            after.items.iter().map(|i| (i.kind, i.address.clone())).collect::<Vec<_>>()
        );
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

    /// Noting an address must reorder it, not retire it.
    #[test]
    fn a_noted_provisional_label_sorts_last_but_still_blocks_done() {
        use crate::commands::SetNote;

        let db = db_with(vec![boxed(AutoDisassemble::new((CODE, 0u32)))]);
        let before = assess_at(&db, Gate::Named);
        let count = |c: &Completeness| {
            c.items.iter().filter(|i| i.kind == "provisional_label").count()
        };
        assert!(count(&before) >= 1, "expected an unnamed target");
        assert!(!before.done);

        let mut db = db;
        let note = crate::note::Note::new(None, "no clue man");
        db.apply(boxed(SetNote::new((CODE, 4u32..5u32), note)), Some(&Env))
            .unwrap();

        let after = assess_at(&db, Gate::Named);
        let noted = after
            .items
            .iter()
            .find(|i| i.kind == "provisional_label" && i.address.contains("0x4"))
            .expect("the noted address must stay on the worklist");
        assert_eq!(noted.sort.2, 1, "a studied address sorts late");
        assert!(noted.detail.contains("read that note first"), "{}", noted.detail);

        // Ordering only: still present, still blocking.
        assert!(!after.done, "noting an address must not retire it");
        assert_eq!(count(&after), count(&before));
    }

    #[test]
    fn unnamed_targets_are_ranked_by_how_many_reference_them() {
        let db = db_with(vec![boxed(AutoDisassemble::new((CODE, 0u32)))]);
        let report = assess_at(&db, Gate::Named);
        let ranked: Vec<(u32, &str)> = report
            .items
            .iter()
            .filter(|i| i.kind == "provisional_label")
            .map(|i| (i.sort.3, i.address.as_str()))
            .collect();
        assert!(!ranked.is_empty());
        // sort.3 is inverted, so it must be non-decreasing across the list.
        assert!(
            ranked.windows(2).all(|w| w[0].0 <= w[1].0),
            "worklist not ordered by caller count: {ranked:?}"
        );
    }

    #[test]
    fn undecoded_vectors_are_raised_and_retiring_one_settles_it() {
        use crate::commands::DisablePlatformAddress;

        let db = db_with(vec![]);
        let open = |db: &Db| {
            assess_at(db, Gate::Structural)
                .items
                .iter()
                .filter(|i| i.kind == "undecoded_entry_point")
                .count()
        };
        let before = open(&db);
        assert!(before > 0, "the CPU's vectors start undecided");

        let mut db = db;
        let undo = db
            .apply(
                boxed(DisablePlatformAddress {
                    address: (CODE, 0x3u32).into(),
                    reason: "IE=0x00 never enables EX0".to_string(),
                }),
                Some(&Env),
            )
            .unwrap();
        assert_eq!(open(&db), before - 1);

        for command in undo {
            db.apply(command, Some(&Env)).unwrap();
        }
        assert_eq!(open(&db), before, "restoring reopens the question");
    }

    #[test]
    fn a_decoded_vector_leaves_the_worklist() {
        let db = db_with(vec![boxed(AutoDisassemble::new((CODE, 0u32)))]);
        let report = assess_at(&db, Gate::Structural);
        let raised: Vec<&str> = report
            .items
            .iter()
            .filter(|i| i.kind == "undecoded_entry_point")
            .map(|i| i.address.as_str())
            .collect();
        assert!(!raised.contains(&"CODE:0x0"), "reset is decoded: {raised:?}");
    }

    /// A hole with bytes on both sides means something removed them.
    #[test]
    fn a_hole_between_mapped_bytes_is_reported_but_the_tail_is_not() {
        let mut db = db_with(vec![]);
        assert!(
            !assess_at(&db, Gate::Structural)
                .counts
                .contains_key("unmapped_gap"),
            "a contiguous image has no gap"
        );

        // Unmap two bytes from the middle.
        db.apply(boxed(UnmapBytes::new((CODE, 2u32..4u32))), Some(&Env))
            .unwrap();
        let report = assess_at(&db, Gate::Structural);
        let gap = report
            .items
            .iter()
            .find(|i| i.kind == "unmapped_gap")
            .expect("a hole between mapped bytes must be reported");
        assert_eq!(gap.range.as_deref(), Some("CODE:0x2..0x4"));
        assert!(gap.detail.contains("2 byte(s)"), "{}", gap.detail);
    }

    #[test]
    fn a_target_outside_the_image_asks_about_the_source() {
        // LJMP 0xF004 at 0x0, then RET. The target is far outside a 4-byte image.
        struct Img;
        impl crate::commands::Environment for Img {
            fn load_file_bytes(
                &self,
                _f: &str,
                offset: usize,
                size: AddressValue,
            ) -> Result<Vec<u8>, std::io::Error> {
                const BYTES: [u8; 4] = [0x02, 0xF0, 0x04, 0x22];
                Ok(BYTES[offset..offset + size as usize].to_vec())
            }
        }
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(boxed(MapBytes::new((CODE, 0), "img", 0usize, 4u32)), Some(&Img))
            .unwrap();
        db.apply(boxed(AutoDisassemble::new((CODE, 0u32))), Some(&Img))
            .unwrap();

        let report = assess_at(&db, Gate::Structural);
        let item = report
            .items
            .iter()
            .find(|i| i.kind == "target_outside_image")
            .expect("a target outside the image must be raised");
        assert_eq!(item.address, "CODE:0x0", "anchored on the source, not 0xf004");
        // All three readings are offered, including the wiring one.
        assert!(item.suggested.iter().any(|c| c.starts_with("peek(address=CODE:0x0")));
        assert!(
            item.suggested.iter().any(|c| c.starts_with("set_address_bits(")),
            "{:?}",
            item.suggested
        );
    }

    #[test]
    fn narrowing_the_address_lines_retires_the_item() {
        struct Img;
        impl crate::commands::Environment for Img {
            fn load_file_bytes(
                &self,
                _f: &str,
                offset: usize,
                size: AddressValue,
            ) -> Result<Vec<u8>, std::io::Error> {
                // LJMP 0xF002 — with three lines decoded that is 0x2, in range.
                const BYTES: [u8; 8] =
                    [0x02, 0xF0, 0x02, 0x22, 0x00, 0x22, 0x00, 0x22];
                Ok(BYTES[offset..offset + size as usize].to_vec())
            }
        }
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(boxed(MapBytes::new((CODE, 0), "img", 0usize, 8u32)), Some(&Img))
            .unwrap();
        db.apply(boxed(AutoDisassemble::new((CODE, 0u32))), Some(&Img))
            .unwrap();
        assert!(
            assess_at(&db, Gate::Structural)
                .counts
                .contains_key("target_outside_image")
        );

        db.apply(
            boxed(crate::commands::SetAddressBits {
                space: "CODE".to_string(),
                bits: 3,
            }),
            Some(&Img),
        )
        .unwrap();
        assert!(
            !assess_at(&db, Gate::Structural)
                .counts
                .contains_key("target_outside_image"),
            "declaring the wiring answers it"
        );
    }

    #[test]
    fn flow_into_data_offers_both_readings() {
        use crate::commands::MarkData;
        use crate::db::DataType;

        // `INC A` at 0x4 falls through into the `RET` at 0x5, classified data.
        let db = db_with(vec![
            boxed(DisassembleRange::new((CODE, 4u32..5u32), false)),
            boxed(MarkData::new((CODE, 5u32..6u32), DataType::Byte)),
        ]);
        let report = assess_at(&db, Gate::Structural);
        let item = report
            .items
            .iter()
            .find(|i| i.kind == "flow_into_data")
            .expect("falling out of code");

        assert!(!item.suggested.is_empty(), "an item with no command is a dead end");
        // Both readings, each runnable.
        assert!(
            item.suggested.iter().any(|c| c.contains("clear_equivalents")),
            "{:?}",
            item.suggested
        );
        assert!(
            item.suggested.iter().any(|c| c.contains("mark_data")),
            "{:?}",
            item.suggested
        );
        // And it says which way the bytes actually lean.
        assert!(
            item.detail.contains("decode like code") || item.detail.contains("likelier one"),
            "{}",
            item.detail
        );
    }

    /// Running off the end is either: filler that decoded as code, or an image
    /// that continues past what is loaded.
    #[test]
    fn flow_off_the_end_offers_both_readings() {
        struct Img;
        impl crate::commands::Environment for Img {
            fn load_file_bytes(
                &self,
                _f: &str,
                offset: usize,
                size: AddressValue,
            ) -> Result<Vec<u8>, std::io::Error> {
                // NOP / INC A — the last instruction falls through off the end.
                const BYTES: [u8; 2] = [0x00, 0x04];
                Ok(BYTES[offset..offset + size as usize].to_vec())
            }
        }
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(boxed(MapBytes::new((CODE, 0), "img", 0usize, 2u32)), Some(&Img))
            .unwrap();
        db.apply(boxed(DisassembleRange::new((CODE, 0u32..2u32), false)), Some(&Img))
            .unwrap();

        let report = assess_at(&db, Gate::Structural);
        let item = report
            .items
            .iter()
            .find(|i| i.kind == "flow_off_end")
            .expect("code running off");
        assert!(item.suggested.iter().any(|c| c.contains("mark_data")), "{:?}", item.suggested);
        assert!(item.suggested.iter().any(|c| c.contains("map_bytes")), "{:?}", item.suggested);
        assert!(item.detail.contains("incomplete"), "{}", item.detail);
    }

    /// A lookup table stays anonymous until it is referenced.
    #[test]
    fn data_addressed_by_pointer_is_asked_about() {
        struct Img;
        impl crate::commands::Environment for Img {
            fn load_file_bytes(
                &self,
                _f: &str,
                offset: usize,
                size: AddressValue,
            ) -> Result<Vec<u8>, std::io::Error> {
                // MOV DPTR,#0x0006 / RET, then a two-byte table.
                const BYTES: [u8; 8] =
                    [0x90, 0x00, 0x06, 0x22, 0x00, 0x00, 0xAA, 0xBB];
                Ok(BYTES[offset..offset + size as usize].to_vec())
            }
        }
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(boxed(MapBytes::new((CODE, 0), "img", 0usize, 8u32)), Some(&Img))
            .unwrap();
        db.apply(boxed(AutoDisassemble::new((CODE, 0u32))), Some(&Img))
            .unwrap();

        let report = assess_at(&db, Gate::Named);
        let item = report
            .items
            .iter()
            .find(|i| i.kind == "unnamed_data")
            .expect("a pointer target nothing names must be raised");
        assert_eq!(item.address, "CODE:0x6");
        // The reference is inferred, so the item asks rather than asserts.
        assert!(item.detail.contains("may help decide"), "{}", item.detail);

        // Naming it retires the item.
        db.apply(
            boxed(SetLabel::new((CODE, 6u32), "jump_table".to_string(), false, false)),
            Some(&Img),
        )
        .unwrap();
        assert!(
            !assess_at(&db, Gate::Named).counts.contains_key("unnamed_data"),
            "naming it"
        );
    }

    #[test]
    fn an_undecided_operand_is_recorded_with_both_candidates() {
        struct Img;
        impl crate::commands::Environment for Img {
            fn load_file_bytes(
                &self,
                _f: &str,
                offset: usize,
                size: AddressValue,
            ) -> Result<Vec<u8>, std::io::Error> {
                const BYTES: [u8; 4] = [0x90, 0x12, 0x34, 0x22];
                Ok(BYTES[offset..offset + size as usize].to_vec())
            }
        }
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(boxed(MapBytes::new((CODE, 0), "img", 0usize, 4u32)), Some(&Img))
            .unwrap();
        db.apply(boxed(AutoDisassemble::new((CODE, 0u32))), Some(&Img))
            .unwrap();

        let report = assess_at(&db, Gate::Documented);
        let item = report
            .items
            .iter()
            .find(|i| i.kind == "undecided_operand")
            .expect("MOV DPTR,#addr is ambiguous");
        assert_eq!(item.address, "CODE:0x0", "keyed by the instruction");
        assert!(
            item.suggested.iter().any(|c| c.contains("space=\"CODE\"")),
            "{:?}",
            item.suggested
        );
        assert!(
            item.suggested.iter().any(|c| c.contains("space=\"XDATA\"")),
            "{:?}",
            item.suggested
        );
        assert!(
            item.suggested.iter().any(|c| c.starts_with("set_operand_value(")),
            "{:?}",
            item.suggested
        );

        // Deciding it retires the item.
        db.apply(
            boxed(crate::commands::SetOperandValue {
                address: (CODE, 0u32).into(),
            }),
            Some(&Img),
        )
        .unwrap();
        assert!(
            !assess_at(&db, Gate::Documented)
                .counts
                .contains_key("undecided_operand")
        );
    }

    /// A jump-only target is a spot inside a routine.
    #[test]
    fn a_jump_only_target_is_offered_a_local_name() {
        struct Img;
        impl crate::commands::Environment for Img {
            fn load_file_bytes(
                &self,
                _f: &str,
                offset: usize,
                size: AddressValue,
            ) -> Result<Vec<u8>, std::io::Error> {
                // MOV R0,#5 / NOP / SJMP 0x2 / RET — 0x2 is reached only by jump.
                const BYTES: [u8; 6] = [0x78, 0x05, 0x00, 0x80, 0xFD, 0x22];
                Ok(BYTES[offset..offset + size as usize].to_vec())
            }
        }
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(boxed(MapBytes::new((CODE, 0), "img", 0usize, 6u32)), Some(&Img))
            .unwrap();
        db.apply(boxed(AutoDisassemble::new((CODE, 0u32))), Some(&Img))
            .unwrap();

        let report = assess_at(&db, Gate::Named);
        let item = report
            .items
            .iter()
            .find(|i| i.kind == "provisional_label" && i.address == "CODE:0x2")
            .expect("the jump target");
        assert!(item.detail.contains("jump target"), "{}", item.detail);
        assert_eq!(
            item.suggested.first().map(String::as_str),
            Some("set_label(address=CODE:0x2, label=\".loop\", local=True)"),
            "a scoped name comes first: {:?}",
            item.suggested
        );
    }

    /// A filler run must never start on a live vector. Swallowing one produces a
    /// filler reading that retires the handler and marks it data, and the vector
    /// guard permits exactly that, so following the advice buries a live handler.
    #[test]
    fn a_filler_run_stops_at_a_live_vector() {
        // 0xb (INT_timer0) holds `LJMP 0x0100`; 0xe..0x10 is filler after it that
        // falls into data at 0x10.
        let mut image = vec![0x00u8; 0x110];
        image[0x0..0x3].copy_from_slice(&[0x02, 0x01, 0x00]); // reset: LJMP 0x100
        image[0xb..0xe].copy_from_slice(&[0x02, 0x01, 0x00]); // INT_timer0: LJMP 0x100
        image[0x100] = 0x22; // RET
        struct Fixture(Vec<u8>);
        impl crate::commands::Environment for Fixture {
            fn load_file_bytes(
                &self,
                _f: &str,
                offset: usize,
                size: AddressValue,
            ) -> Result<Vec<u8>, std::io::Error> {
                Ok(self.0[offset..offset + size as usize].to_vec())
            }
        }
        let env = Fixture(image);
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(boxed(MapBytes::new((CODE, 0), "img", 0usize, 0x110u32)), Some(&env)).unwrap();
        db.apply(boxed(AutoDisassemble::new((CODE, 0xbu32))), Some(&env)).unwrap();
        db.apply(boxed(DisassembleRange::new((CODE, 0xeu32..0x10u32), true)), Some(&env)).unwrap();

        // The run leading into 0x10 must stop after the vector's own instruction.
        assert_eq!(
            code_run_start(&db, CODE, 0xf),
            0xe,
            "the run must not reach back over INT_timer0 at 0xb"
        );
        assert!(is_live_entry_point(&db, CODE, 0xb), "0xb is a vector nobody retired");
    }

    /// A working name is shown but not settled: the address keeps its place on
    /// the naming worklist, deferred behind untouched ones, until a later pass
    /// commits a real name.
    #[test]
    fn a_provisional_label_stays_on_the_worklist_until_settled() {
        use crate::commands::{AutoDisassemble, SetLabel, boxed};

        let mut db = db_with(vec![boxed(AutoDisassemble::new((CODE, 0u32)))]);
        let listed = |db: &Db| {
            assess_at(db, Gate::Named)
                .items
                .iter()
                .filter(|i| i.kind == "provisional_label" && i.address.contains("0x4"))
                .map(|i| (i.sort.2, i.detail.clone()))
                .next()
        };
        assert!(listed(&db).is_some(), "0x4 is an unnamed call target to begin with");

        db.apply(boxed(SetLabel::new((CODE, 4u32), "maybe_inc".to_string(), true, false)), Some(&Env))
            .unwrap();
        let (deferred, detail) = listed(&db).expect("a working name must stay listed");
        assert_eq!(deferred, 1, "a working name should sort behind untouched addresses");
        assert!(detail.contains("maybe_inc"), "the item should show the current guess: {detail}");

        db.apply(boxed(SetLabel::new((CODE, 4u32), "inc_a".to_string(), false, false)), Some(&Env))
            .unwrap();
        assert!(listed(&db).is_none(), "settling the name retires the item");
    }

    /// Marking an address that code branches to is refused, so an item must not
    /// suggest it. The clear in front of the mark runs first, which leaves the
    /// caller half applied with the item still standing.
    #[test]
    fn a_run_that_is_branched_to_is_never_suggested_for_marking() {
        // 0x4 is the `LCALL` target, so the run entered there carries a branch.
        let db = db_with(vec![
            boxed(DisassembleRange::new((CODE, 0u32..4u32), false)),
            boxed(DisassembleRange::new((CODE, 4u32..6u32), false)),
        ]);

        for item in assess_at(&db, Gate::Named).items {
            for suggestion in item.suggested.iter().filter(|s| s.starts_with("mark_data")) {
                let range = suggestion
                    .split("range=CODE:")
                    .nth(1)
                    .and_then(|s| s.split(',').next())
                    .expect("a mark_data suggestion names its range");
                let (start, end) = range.split_once("..").expect("a range has both bounds");
                let start = AddressValue::from_str_radix(start.trim_start_matches("0x"), 16);
                let end = AddressValue::from_str_radix(end.trim_start_matches("0x"), 16);
                let (Ok(start), Ok(end)) = (start, end) else { continue };
                assert!(
                    settle_reference_first(&db, CODE, start, end).is_none(),
                    "{} suggests marking a branched-to run: {suggestion}",
                    item.kind
                );
            }
        }
    }

    /// A database saved before the classification guards existed can hold a
    /// call target inside a data range. `auto_disassemble` cannot clear that on
    /// its own, so the item has to name the barrier and the verb that drops it.
    #[test]
    fn an_unfollowed_target_behind_a_barrier_says_to_clear_the_barrier() {
        use crate::commands::MarkData;
        use crate::db::DataType;

        let db = db_with(vec![
            boxed(DisassembleRange::new((CODE, 0u32..4u32), false)),
            boxed(MarkData::new((CODE, 4u32..6u32), DataType::Byte)),
        ]);
        let report = assess_at(&db, Gate::Named);
        let item = report
            .items
            .iter()
            .find(|i| i.kind == "unfollowed_target")
            .expect("the call at 0x0 still points into the data range");

        assert!(item.detail.contains("marked data"), "{}", item.detail);
        assert!(
            item.suggested.iter().any(|s| s.starts_with("clear_equivalents")),
            "the barrier has to be droppable from the suggestions: {:?}",
            item.suggested
        );
    }

    /// A vector slot decoded out of filler branches somewhere impossible, and
    /// the way out is to classify its bytes. That is refused while the vector is
    /// live, so the item has to say to retire it first or it suggests a command
    /// the caller cannot run.
    #[test]
    fn classifying_a_vector_slot_is_suggested_with_the_retire_step_first() {
        // 0x13 (INT_ext1) holds `LJMP 0x1FD1`, past the end of a 0x26-byte image.
        let mut image = vec![0x00u8; 0x26];
        image[0x13..0x16].copy_from_slice(&[0x02, 0x1F, 0xD1]);
        struct SmallEnv(Vec<u8>);
        impl crate::commands::Environment for SmallEnv {
            fn load_file_bytes(
                &self,
                _f: &str,
                offset: usize,
                size: AddressValue,
            ) -> Result<Vec<u8>, std::io::Error> {
                Ok(self.0[offset..offset + size as usize].to_vec())
            }
        }
        let env = SmallEnv(image);
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(boxed(MapBytes::new((CODE, 0), "img", 0usize, 0x26u32)), Some(&env)).unwrap();
        db.apply(boxed(DisassembleRange::new((CODE, 0x13u32..0x16u32), true)), Some(&env)).unwrap();

        let item = assess_at(&db, Gate::Named)
            .items
            .into_iter()
            .find(|i| i.kind == "target_outside_image")
            .expect("the branch leaves the image");
        let retire = item
            .suggested
            .iter()
            .position(|s| s.starts_with("disable_platform_address"))
            .expect("retiring the vector must be offered");
        let mark = item
            .suggested
            .iter()
            .position(|s| s.starts_with("mark_data"))
            .expect("classifying the bytes must be offered");
        assert!(retire < mark, "the retire step has to come first: {:?}", item.suggested);
    }

    /// Both readings of a flow leak are runnable, so their order decides which
    /// one gets taken. When the data side does not decode like code, the filler
    /// reading has to come first: an even-handed list put the code reading on top
    /// and an overnight run cycled between the two for six sessions, decoding the
    /// bytes one session and reverting them the next.
    #[test]
    fn flow_into_data_puts_the_likelier_reading_first() {
        use crate::commands::MarkData;
        use crate::db::DataType;

        // `NOP` at 0x30 falls into 0x31, which decodes to `LJMP 0xFFFD`: a target
        // past the end of a 0x40-byte image, so those bytes are not code.
        let mut image = vec![0x00u8; 0x40];
        image[0x31..0x34].copy_from_slice(&[0x02, 0xFF, 0xFD]);
        struct Fixture(Vec<u8>);
        impl crate::commands::Environment for Fixture {
            fn load_file_bytes(
                &self,
                _f: &str,
                offset: usize,
                size: AddressValue,
            ) -> Result<Vec<u8>, std::io::Error> {
                Ok(self.0[offset..offset + size as usize].to_vec())
            }
        }
        let env = Fixture(image);
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(boxed(MapBytes::new((CODE, 0), "img", 0usize, 0x40u32)), Some(&env)).unwrap();
        db.apply(boxed(DisassembleRange::new((CODE, 0x30u32..0x31u32), true)), Some(&env)).unwrap();
        db.apply(boxed(MarkData::new((CODE, 0x31u32..0x34u32), DataType::Byte)), Some(&env))
            .unwrap();

        let item = assess_at(&db, Gate::Named)
            .items
            .into_iter()
            .find(|i| i.kind == "flow_into_data")
            .expect("code at 0x30 runs into data at 0x31");
        let filler = item
            .suggested
            .iter()
            .position(|s| s.starts_with("mark_data"))
            .expect("the filler reading must be offered");
        // The code reading is the clear tagged for it: the filler reading opens
        // with a clear of its own, since `mark_data` needs undefined bytes.
        let as_code = item
            .suggested
            .iter()
            .position(|s| s.starts_with("clear_equivalents") && s.contains("is code"))
            .expect("the code reading must be offered");
        assert!(
            filler < as_code,
            "bytes that branch out of the image should lead with the filler reading: {:?}",
            item.suggested
        );
        assert!(
            item.detail.contains("filler reading is the likelier one"),
            "the item should say which the bytes favour: {}",
            item.detail
        );
    }

    /// `mark_data` takes undefined bytes only, so every suggestion that
    /// reclassifies a decoded run has to clear it first and cover the whole run.
    /// Offered alone over one instruction, the mark refuses on decoded bytes, and
    /// where it lands it leaves the predecessor leaking into the new data.
    #[test]
    fn reclassifying_a_decoded_run_clears_it_first_and_covers_all_of_it() {
        // 0x4 is the call target, so the run entered there ends at the `RET`.
        let db = db_with(vec![boxed(AutoDisassemble::new((CODE, 0u32)))]);
        let report = assess_at(&db, Gate::Named);

        for item in report.items.iter().filter(|i| {
            matches!(i.kind, "target_outside_image" | "misaligned_target" | "flow_into_data")
        }) {
            let mark = item.suggested.iter().position(|s| s.starts_with("mark_data"));
            let Some(mark) = mark else { continue };
            let cleared = item.suggested[..mark]
                .iter()
                .any(|s| s.starts_with("clear_equivalents") && !s.contains("is code"));
            assert!(
                cleared,
                "{} marks without clearing first: {:?}",
                item.kind, item.suggested
            );
        }
    }
}
