use crate::address::{AddressSpace, AddressValue, SpaceAddressSet, SpaceAddressValue};
use crate::db::{Db, Error};

use super::{Apply, ClearEquivalents, Command, Environment, boxed};

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct AutoDisassemble {
    pub address: SpaceAddressValue,
}

register!(AutoDisassemble(
    /// Recursively disassemble code starting at `offset`, following control
    /// flow and marking reachable bytes as code.
    space: AddressSpace,
    offset: AddressValue,
) {
    Self {
        address: (space, offset).into(),
    }
});

impl Apply for AutoDisassemble {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { address } = self;
        let SpaceAddressValue {
            space,
            offset: start,
        } = address;
        let region = db.region_mut(space);
        let cleared = region.auto_disassemble(start).success;
        if cleared.is_empty() {
            return Ok(vec![]);
        }
        // The undo is a single `ClearEquivalents` over the coalesced set of all
        // newly-disassembled addresses, rather than one command per address.
        let mut addresses = SpaceAddressSet::new(space);
        for addr in cleared {
            addresses.insert_address(addr);
        }
        Ok(vec![boxed(ClearEquivalents::from_set(addresses))])
    }
}
