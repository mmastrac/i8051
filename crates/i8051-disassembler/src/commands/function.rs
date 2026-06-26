use crate::address::{AddressSpace, AddressValue, SpaceAddressValue};
use crate::db::{Db, Error, Function};

use super::{Apply, Command, Environment, boxed};

register_commands!(SetFunction, ClearFunction);

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SetFunction {
    pub address: SpaceAddressValue,
    pub function: Function,
}

impl SetFunction {
    pub fn new(space: AddressSpace, offset: AddressValue, function: Function) -> Self {
        Self {
            address: (space, offset).into(),
            function,
        }
    }
}

impl Apply for SetFunction {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { address, function } = self;
        let SpaceAddressValue { space, offset } = address;
        let region = db.region_mut(space);
        let before = region.get_function(offset).cloned();
        region.set_function(function);
        Ok(match before {
            Some(function) => vec![boxed(SetFunction { address, function })],
            None => vec![boxed(ClearFunction { address })],
        })
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ClearFunction {
    pub address: SpaceAddressValue,
}

impl ClearFunction {
    pub fn new(space: AddressSpace, offset: AddressValue) -> Self {
        Self {
            address: (space, offset).into(),
        }
    }
}

impl Apply for ClearFunction {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { address } = self;
        let SpaceAddressValue { space, offset } = address;
        let region = db.region_mut(space);
        let before = region.get_function(offset).cloned();
        region.clear_function(offset);
        Ok(match before {
            Some(function) => vec![boxed(SetFunction { address, function })],
            None => vec![],
        })
    }
}
