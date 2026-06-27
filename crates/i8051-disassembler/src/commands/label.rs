use crate::address::{SpaceAddressSet, SpaceAddressValue};
use crate::db::{Db, Error};

use super::{Apply, Command, Environment, boxed};

#[cfg(test)]
use crate::address::AddressSpace;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SetLabel {
    pub address: SpaceAddressValue,
    pub label: String,
}

register!(SetLabel(
    /// Name the code `address` with `label`.
    address: SpaceAddressValue,
    label: String,
));

impl Apply for SetLabel {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { address, label } = self;
        let SpaceAddressValue { space, offset } = address;
        let region = db.region_mut(space);
        let before = region.get_label(offset).map(str::to_owned);
        region.set_label(offset, &label);
        Ok(match before {
            Some(label) => vec![boxed(SetLabel { address, label })],
            None => vec![boxed(ClearLabel::new((space, offset)))],
        })
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ClearLabel {
    pub addresses: SpaceAddressSet,
}

register!(ClearLabel(
    /// Remove labels at the given `addresses`.
    addresses: SpaceAddressSet,
));

impl Apply for ClearLabel {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { addresses } = self;
        let space = addresses.space;
        let region = db.region_mut(space);
        let mut undo = Vec::new();
        for range in addresses.ranges() {
            for (offset, label) in region.clear_labels_in(range) {
                undo.push(boxed(SetLabel::new((space, offset), label)));
            }
        }
        Ok(undo)
    }
}

// A clear command's `SpaceAddressSet` renders as the optimal `CODE:{...}` form.
serialize_test!(
    clear_label_address_set,
    "clear_label(addresses=CODE:{0x10, 0x20})",
    ClearLabel {
        addresses: {
            let mut set = SpaceAddressSet::new(AddressSpace::Code);
            set.insert_address(0x10);
            set.insert_address(0x20);
            set
        },
    }
);
