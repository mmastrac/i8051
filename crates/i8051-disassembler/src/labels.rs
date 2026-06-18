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

#[derive(Default)]
pub(crate) struct LabelCollector {
    labels: HashMap<PhysicalAddr, Option<String>>,
}

impl LabelCollector {
    pub fn collect(&mut self, space: AddressSpace, addr: AddressValue, label: Option<String>) {
        self.labels.insert(
            PhysicalAddr {
                space,
                offset: addr,
            },
            label,
        );
    }

    pub fn into_implicit_labels(self) -> ImplicitLabels {
        let mut implicit_labels = BTreeMap::new();
        for (addr, ref_name) in self.labels {
            if let Some(ref_name) = ref_name {
                implicit_labels
                    .entry(addr.space)
                    .or_insert_with(Labels::default)
                    .labels
                    .insert(addr.offset, ref_name);
            } else {
                implicit_labels
                    .entry(addr.space)
                    .or_insert_with(Labels::default)
                    .labels
                    .insert(addr.offset, format!("loc_{addr:04X}", addr = addr.offset));
            }
        }
        ImplicitLabels {
            labels: implicit_labels,
        }
    }
}
