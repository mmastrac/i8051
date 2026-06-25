use serde::{Deserialize, Serialize};

use crate::address::{AddressRange, SpaceAddressRange, SpaceAddressValue};
use crate::db::{Db, Equivalent, Error};

use super::Command;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SetEquivalent {
    pub address: SpaceAddressValue,
    pub equivalent: Equivalent,
}

impl SetEquivalent {
    pub fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn super::Environment>,
    ) -> Result<Vec<Command>, Error> {
        let Self { address, equivalent } = self;
        let (space, offset) = address;
        let region = db.region_mut(space);
        let span = region.equivalent_span(offset, &equivalent)?;
        let before = region.snapshot_equivalents(offset, span);
        region.set_equivalent(offset, equivalent)?;
        let mut undo = vec![Command::ClearEquivalents(ClearEquivalents {
            range: (space, AddressRange::new(offset, offset + span)),
        })];
        for (start, range) in before {
            undo.push(Command::SetEquivalent(SetEquivalent {
                address: (space, start),
                equivalent: range.equivalent,
            }));
        }
        Ok(undo)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClearEquivalents {
    pub range: SpaceAddressRange,
}

impl ClearEquivalents {
    pub fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn super::Environment>,
    ) -> Result<Vec<Command>, Error> {
        let Self { range } = self;
        let (space, address_range) = range;
        let offset = address_range.start;
        let size = address_range.end - address_range.start;
        let region = db.region_mut(space);
        let before = region.snapshot_equivalents(offset, size);
        region.clear_equivalents(offset, size);
        let mut undo = Vec::new();
        for (start, range) in before {
            undo.push(Command::SetEquivalent(SetEquivalent {
                address: (space, start),
                equivalent: range.equivalent,
            }));
        }
        Ok(undo)
    }
}
