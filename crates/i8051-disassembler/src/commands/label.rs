use crate::address::SpaceAddressValue;
use crate::db::{Db, Error};
use crate::store::fields;

use super::Command;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SetLabel {
    #[serde(with = "fields::space_address")]
    pub address: SpaceAddressValue,
    pub label: String,
}

impl SetLabel {
    pub fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn super::Environment>,
    ) -> Result<Vec<Command>, Error> {
        let Self { address, label } = self;
        let (space, offset) = address;
        let region = db.region_mut(space);
        let before = region.get_label(offset).map(str::to_owned);
        region.set_label(offset, &label);
        Ok(match before {
            Some(label) => vec![Command::SetLabel(SetLabel { address, label })],
            None => vec![Command::ClearLabel(ClearLabel { address })],
        })
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ClearLabel {
    #[serde(with = "fields::space_address")]
    pub address: SpaceAddressValue,
}

impl ClearLabel {
    pub fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn super::Environment>,
    ) -> Result<Vec<Command>, Error> {
        let Self { address } = self;
        let (space, offset) = address;
        let region = db.region_mut(space);
        let before = region.get_label(offset).map(str::to_owned);
        region.clear_label(offset);
        Ok(match before {
            Some(label) => vec![Command::SetLabel(SetLabel { address, label })],
            None => vec![],
        })
    }
}
