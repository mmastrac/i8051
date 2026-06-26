use crate::address::{AddressRange, AddressSpace, AddressValue, SpaceAddressValue};
use crate::db::{Db, Error};

use super::{Apply, ClearEquivalents, Command, Environment, boxed};

register_commands!(AutoDisassemble);

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct AutoDisassemble {
    pub address: SpaceAddressValue,
}

impl AutoDisassemble {
    pub fn new(space: AddressSpace, start: AddressValue) -> Self {
        Self {
            address: (space, start).into(),
        }
    }
}

impl Apply for AutoDisassemble {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { address } = self;
        let SpaceAddressValue { space, offset: start } = address;
        let region = db.region_mut(space);
        let addresses = region.auto_disassemble(start).success;
        Ok(addresses
            .into_iter()
            .map(|addr| {
                boxed(ClearEquivalents {
                    range: (space, AddressRange::new(addr, addr + 1)).into(),
                })
            })
            .collect())
    }
}
