use std::{
    collections::{BTreeMap, HashMap},
    ops,
};

use crate::address::{AddressSpace, AddressValue, PhysicalAddr};

#[derive(Default)]
pub(crate) struct ImplicitLabels {
    labels: BTreeMap<AddressSpace, Labels>,
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
