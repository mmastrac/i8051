use crate::address::{AddressSpace, AddressValue, SpaceAddressSet, SpaceAddressValue};
use crate::db::{Db, Error, Function};

use super::{Apply, Command, Environment, boxed};

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SetFunction {
    pub address: SpaceAddressValue,
    pub function: Function,
}

register!(SetFunction(
    /// Define a function at the code address `offset`.
    space: AddressSpace,
    offset: AddressValue,
    function: Function,
) {
    Self {
        address: (space, offset).into(),
        function,
    }
});

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
            None => vec![boxed(ClearFunction::new(space, offset))],
        })
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ClearFunction {
    pub addresses: SpaceAddressSet,
}

register!(ClearFunction(
    /// Remove the function defined at the code address `offset`.
    space: AddressSpace,
    offset: AddressValue,
) {
    let mut addresses = SpaceAddressSet::new(space);
    addresses.insert_address(offset);
    Self { addresses }
});

impl ClearFunction {
    /// Clear every function in a set of addresses.
    pub fn from_set(addresses: SpaceAddressSet) -> Self {
        Self { addresses }
    }
}

impl Apply for ClearFunction {
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
            for (offset, function) in region.clear_functions_in(range) {
                undo.push(boxed(SetFunction::new(space, offset, function)));
            }
        }
        Ok(undo)
    }
}
