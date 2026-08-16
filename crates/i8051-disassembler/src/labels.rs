use std::{
    collections::{BTreeMap, HashMap},
    ops,
};

use crate::address::{AddressSpace, AddressValue, PhysicalAddr};

#[derive(Default)]
pub(crate) struct ImplicitLabels {
    labels: BTreeMap<AddressSpace, Labels>,
}

impl ImplicitLabels {
    pub fn insert_if_absent(&mut self, space: AddressSpace, offset: AddressValue, name: &str) {
        self.labels
            .entry(space)
            .or_default()
            .labels
            .entry(offset)
            .or_insert_with(|| name.to_string());
    }
}

impl ops::Deref for ImplicitLabels {
    type Target = BTreeMap<AddressSpace, Labels>;

    fn deref(&self) -> &Self::Target {
        &self.labels
    }
}

#[derive(Default)]
pub(crate) struct Labels {
    labels: BTreeMap<AddressValue, String>,
}

impl ops::Deref for Labels {
    type Target = BTreeMap<AddressValue, String>;

    fn deref(&self) -> &Self::Target {
        &self.labels
    }
}

/// What an auto-generated code label is named after, following Ghidra: a call
/// target is a `sub_`, a plain jump target a `loc_`.
#[derive(Clone, Copy)]
pub(crate) enum LabelKind {
    Sub,
    Loc,
}

/// Provisional name: starts with `sub_` or `loc_` and is followed by hex digits.
pub fn is_provisional_name(name: &str) -> bool {
    let Some(hex) = name
        .strip_prefix("sub_")
        .or_else(|| name.strip_prefix("loc_"))
    else {
        return false;
    };
    hex.len() >= 4 && hex.chars().all(|c| c.is_ascii_hexdigit())
}

#[derive(Default)]
pub(crate) struct LabelCollector {
    labels: HashMap<PhysicalAddr, LabelKind>,
}

impl LabelCollector {
    /// Record an implicit code label at `target`. A call anywhere upgrades it to
    /// `sub_`, which outranks a `loc_` from a plain jump.
    pub fn collect(&mut self, target: PhysicalAddr, kind: LabelKind) {
        self.labels
            .entry(target)
            .and_modify(|existing| {
                if matches!(kind, LabelKind::Sub) {
                    *existing = LabelKind::Sub;
                }
            })
            .or_insert(kind);
    }

    pub fn into_implicit_labels(self) -> ImplicitLabels {
        let mut implicit_labels: BTreeMap<AddressSpace, Labels> = BTreeMap::new();
        for (addr, kind) in self.labels {
            let name = match kind {
                LabelKind::Sub => format!("sub_{:04X}", addr.offset),
                LabelKind::Loc => format!("loc_{:04X}", addr.offset),
            };
            implicit_labels
                .entry(addr.space)
                .or_default()
                .labels
                .insert(addr.offset, name);
        }
        ImplicitLabels {
            labels: implicit_labels,
        }
    }
}
