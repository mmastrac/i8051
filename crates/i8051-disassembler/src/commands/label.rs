use crate::address::{AddressSpace, AddressValue, SpaceAddressValue};
use crate::db::{Db, Error};

use super::{Apply, Command, Environment, boxed};

register_commands!(SetLabel, ClearLabel);

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SetLabel {
    pub address: SpaceAddressValue,
    pub label: String,
}

impl SetLabel {
    pub fn new(space: AddressSpace, offset: AddressValue, label: impl Into<String>) -> Self {
        Self {
            address: (space, offset).into(),
            label: label.into(),
        }
    }
}

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
            None => vec![boxed(ClearLabel { address })],
        })
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ClearLabel {
    pub address: SpaceAddressValue,
}

impl ClearLabel {
    pub fn new(space: AddressSpace, offset: AddressValue) -> Self {
        Self {
            address: (space, offset).into(),
        }
    }
}

impl Apply for ClearLabel {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { address } = self;
        let SpaceAddressValue { space, offset } = address;
        let region = db.region_mut(space);
        let before = region.get_label(offset).map(str::to_owned);
        region.clear_label(offset);
        Ok(match before {
            Some(label) => vec![boxed(SetLabel { address, label })],
            None => vec![],
        })
    }
}
