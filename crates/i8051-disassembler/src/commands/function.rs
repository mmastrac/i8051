use crate::address::SpaceAddressValue;
use crate::db::{Db, Error, Function};
use crate::store::fields;

use super::Command;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SetFunction {
    #[serde(with = "fields::space_address")]
    pub address: SpaceAddressValue,
    pub function: Function,
}

impl SetFunction {
    pub fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn super::Environment>,
    ) -> Result<Vec<Command>, Error> {
        let Self { address, function } = self;
        let (space, offset) = address;
        let region = db.region_mut(space);
        let before = region.get_function(offset).cloned();
        region.set_function(function);
        Ok(match before {
            Some(function) => vec![Command::SetFunction(SetFunction { address, function })],
            None => vec![Command::ClearFunction(ClearFunction { address })],
        })
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ClearFunction {
    #[serde(with = "fields::space_address")]
    pub address: SpaceAddressValue,
}

impl ClearFunction {
    pub fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn super::Environment>,
    ) -> Result<Vec<Command>, Error> {
        let Self { address } = self;
        let (space, offset) = address;
        let region = db.region_mut(space);
        let before = region.get_function(offset).cloned();
        region.clear_function(offset);
        Ok(match before {
            Some(function) => vec![Command::SetFunction(SetFunction { address, function })],
            None => vec![],
        })
    }
}
