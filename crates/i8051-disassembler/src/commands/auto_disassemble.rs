use crate::address::{AddressRange, SpaceAddressValue};
use crate::db::{Db, Error};
use crate::store::fields;

use super::{ClearEquivalents, Command};

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct AutoDisassemble {
    #[serde(with = "fields::space_address")]
    pub address: SpaceAddressValue,
}

impl AutoDisassemble {
    pub fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn super::Environment>,
    ) -> Result<Vec<Command>, Error> {
        let Self { address } = self;
        let (space, start) = address;
        let region = db.region_mut(space);
        let addresses = region.auto_disassemble(start).success;
        Ok(addresses
            .into_iter()
            .map(|addr| {
                Command::ClearEquivalents(ClearEquivalents {
                    range: (space, AddressRange::new(addr, addr + 1)),
                })
            })
            .collect())
    }
}
