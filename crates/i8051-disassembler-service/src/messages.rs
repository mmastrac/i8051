//! Audience-specific prose for worklist facts.
//!
//! Templates live in `messages/<audience>/<kind>[.verbose].j2` and render
//! straight off a serialized [`ItemKind`].

use std::sync::OnceLock;

use i8051_disassembler::analysis::completeness::ItemKind;
use include_dir::{Dir, include_dir};
use minijinja::{Environment, UndefinedBehavior};

static EMBEDDED: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/messages");
static ENV: OnceLock<Environment<'static>> = OnceLock::new();

/// Who reads the text.
#[derive(Debug, Clone, Copy)]
pub enum Audience {
    /// A person at a listing.
    Human,
    /// The model driving the disassembly.
    Llm,
}

/// How much prose to include.
#[derive(Debug, Clone, Copy)]
pub enum Level {
    /// The fact alone.
    Terse,
    /// Full remediation reasoning.
    Verbose,
}

/// Render one fact as text.
pub fn render_item(what: &ItemKind, audience: Audience, level: Level) -> Result<String, String> {
    let value = serde_json::to_value(what).map_err(|e| e.to_string())?;
    render_value(&value, audience, level)
}

/// Render a serialized fact as text.
pub fn render_value(
    what: &serde_json::Value,
    audience: Audience,
    level: Level,
) -> Result<String, String> {
    let slug = what
        .get("kind")
        .and_then(serde_json::Value::as_str)
        .ok_or("fact has no kind tag")?;
    let name = template_name(slug, audience, level);
    let template = env().get_template(&name).map_err(|e| e.to_string())?;
    template
        .render(minijinja::Value::from_serialize(what))
        .map_err(|e| e.to_string())
}

fn template_name(slug: &str, audience: Audience, level: Level) -> String {
    match (audience, level) {
        (Audience::Human, _) => format!("human/{slug}.j2"),
        (Audience::Llm, Level::Terse) => format!("llm/{slug}.j2"),
        (Audience::Llm, Level::Verbose) => format!("llm/{slug}.verbose.j2"),
    }
}

fn env() -> &'static Environment<'static> {
    ENV.get_or_init(|| {
        let mut env = Environment::new();
        env.set_undefined_behavior(UndefinedBehavior::Strict);
        for dir in EMBEDDED.dirs() {
            for file in dir.files() {
                let name = file.path().to_str().expect("template path is utf8");
                let source = file.contents_utf8().expect("template is utf8");
                env.add_template(name, source).expect("template parses");
            }
        }
        env
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use i8051_disassembler::analysis::completeness::{Flow, UnfollowedBarrier};

    fn samples() -> Vec<ItemKind> {
        let all = vec![
            ItemKind::UnmappedGap {
                from: "CODE:0x2".into(),
                to: "CODE:0x4".into(),
                len: 2,
            },
            ItemKind::TargetOutsideImage {
                verb: Flow::Jump,
                from: "CODE:0x0".into(),
                to: "CODE:0xf004".into(),
            },
            ItemKind::MisalignedTarget {
                verb: Flow::Jump,
                from: "CODE:0x0".into(),
                to: "CODE:0x3".into(),
                covering: "CODE:0x2..0x4".into(),
            },
            ItemKind::UnfollowedTarget {
                verb: Flow::Call,
                from: "CODE:0x0".into(),
                to: "CODE:0x4".into(),
                barrier: None,
            },
            ItemKind::UnfollowedTarget {
                verb: Flow::Call,
                from: "CODE:0x0".into(),
                to: "CODE:0x4".into(),
                barrier: Some(UnfollowedBarrier {
                    range: "CODE:0x4..0x6".into(),
                    marked: "data".into(),
                }),
            },
            ItemKind::FlowIntoData {
                from: "CODE:0x4".into(),
                to: "CODE:0x5".into(),
                barrier: "CODE:0x5..0x6".into(),
                decodes_like_code: true,
            },
            ItemKind::FlowIntoUndefined {
                from: "CODE:0x4".into(),
                to: "CODE:0x5".into(),
            },
            ItemKind::FlowOffEnd {
                from: "CODE:0x4".into(),
                to: "CODE:0x6".into(),
            },
            ItemKind::UndecodedEntryPoint {
                at: "CODE:0xb".into(),
                name: "INT_timer0".into(),
                reason: "timer 0 overflow".into(),
            },
            ItemKind::UndefinedBytes {
                range: "CODE:0x2..0x4".into(),
                count: 2,
            },
            ItemKind::ProvisionalLabel {
                at: "CODE:0x4".into(),
                role: "subroutine".into(),
                caller: Some("CODE:0x0 (reset)".into()),
                starts: Some("INC A".into()),
                working_name: Some("maybe_inc".into()),
                noted: false,
            },
            ItemKind::UnnamedData {
                at: "CODE:0x6".into(),
                refs: 2,
                first: "CODE:0x0".into(),
                inferred: true,
            },
            ItemKind::UndocumentedRoutine {
                at: "CODE:0x4".into(),
                name: "inc_a".into(),
                has_comment: false,
            },
            ItemKind::UndecidedOperand {
                at: "CODE:0x0".into(),
                value: "0x30".into(),
                candidates: vec!["CODE:0x30".into(), "XDATA:0x30".into()],
            },
        ];
        // A new variant must add a sample.
        for kind in &all {
            match kind {
                ItemKind::UnmappedGap { .. }
                | ItemKind::TargetOutsideImage { .. }
                | ItemKind::MisalignedTarget { .. }
                | ItemKind::UnfollowedTarget { .. }
                | ItemKind::FlowIntoData { .. }
                | ItemKind::FlowIntoUndefined { .. }
                | ItemKind::FlowOffEnd { .. }
                | ItemKind::UndecodedEntryPoint { .. }
                | ItemKind::UndefinedBytes { .. }
                | ItemKind::ProvisionalLabel { .. }
                | ItemKind::UnnamedData { .. }
                | ItemKind::UndocumentedRoutine { .. }
                | ItemKind::UndecidedOperand { .. } => {}
            }
        }
        all
    }

    const FORMS: [(Audience, Level); 3] = [
        (Audience::Human, Level::Terse),
        (Audience::Llm, Level::Terse),
        (Audience::Llm, Level::Verbose),
    ];

    #[test]
    fn every_kind_renders() {
        for kind in samples() {
            for (audience, level) in FORMS {
                let text = render_item(&kind, audience, level)
                    .unwrap_or_else(|e| panic!("{}/{audience:?}/{level:?}: {e}", kind.slug()));
                assert!(!text.trim().is_empty(), "{}", kind.slug());
            }
        }
    }

    fn refusal_samples() -> Vec<crate::Refusal> {
        use crate::Refusal;
        let all = vec![
            Refusal::RangeCoversVectors {
                vectors: vec!["CODE:0xb (INT_timer0)".into()],
            },
            Refusal::BarrierStopsAuto {
                at: "CODE:0x33".into(),
                barrier: "CODE:0x33..0x34".into(),
                marked: "data".into(),
            },
            Refusal::RangeSwallowsTargets {
                targets: vec!["CODE:0x34 (from CODE:0x30)".into()],
                omitted: 0,
                first_target: "CODE:0x34".into(),
                first_source: "CODE:0x30".into(),
                sources: 1,
            },
            Refusal::RangeDoesNotDecode {
                count: 3,
                reasons: vec!["1 branch target(s) point outside the loaded image".into()],
            },
            Refusal::CpuStillNeeded {
                cpu: "i8051".into(),
                decoded: 3,
            },
            Refusal::LabelTaken {
                label: "entry".into(),
                holder: "CODE:0x0".into(),
            },
            Refusal::LocalLabelTaken {
                label: ".loop".into(),
                holder: "CODE:0x8".into(),
            },
            Refusal::GeneratedLabel {
                label: "sub_0000".into(),
            },
            Refusal::NothingMapped {
                at: "CODE:0x8".into(),
            },
        ];
        // A new variant must add a sample.
        for refusal in &all {
            match refusal {
                Refusal::RangeCoversVectors { .. }
                | Refusal::BarrierStopsAuto { .. }
                | Refusal::RangeSwallowsTargets { .. }
                | Refusal::RangeDoesNotDecode { .. }
                | Refusal::CpuStillNeeded { .. }
                | Refusal::LabelTaken { .. }
                | Refusal::LocalLabelTaken { .. }
                | Refusal::GeneratedLabel { .. }
                | Refusal::NothingMapped { .. } => {}
            }
        }
        all
    }

    #[test]
    fn every_refusal_renders() {
        for refusal in refusal_samples() {
            let value = serde_json::to_value(&refusal).expect("serialize");
            for (audience, level) in FORMS {
                let text = render_value(&value, audience, level)
                    .unwrap_or_else(|e| panic!("{value}/{audience:?}/{level:?}: {e}"));
                assert!(!text.trim().is_empty(), "{value}");
            }
        }
    }

    #[test]
    fn slug_matches_tag() {
        for kind in samples() {
            let value = serde_json::to_value(&kind).expect("serialize");
            assert_eq!(value["kind"], kind.slug());
        }
    }

    #[test]
    fn missing_field_errors() {
        let bare = serde_json::json!({ "kind": "unmapped_gap" });
        let err = render_value(&bare, Audience::Human, Level::Terse);
        assert!(err.is_err(), "{err:?}");
    }
}
