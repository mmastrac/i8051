use serde::{Deserialize, Serialize};

use crate::address::SpaceAddressValue;
use crate::db::{Db, Error};

use super::Command;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SetComment {
    pub address: SpaceAddressValue,
    pub comment: String,
}

impl SetComment {
    pub fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn super::Environment>,
    ) -> Result<Vec<Command>, Error> {
        let Self { address, comment } = self;
        let (space, offset) = address;
        let region = db.region_mut(space);
        let before = region.get_comment(offset).map(str::to_owned);
        region.set_comment(offset, &comment);
        Ok(match before {
            Some(comment) => vec![Command::SetComment(SetComment { address, comment })],
            None => vec![Command::ClearComment(ClearComment { address })],
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClearComment {
    pub address: SpaceAddressValue,
}

impl ClearComment {
    pub fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn super::Environment>,
    ) -> Result<Vec<Command>, Error> {
        let Self { address } = self;
        let (space, offset) = address;
        let region = db.region_mut(space);
        let before = region.get_comment(offset).map(str::to_owned);
        region.clear_comment(offset);
        Ok(match before {
            Some(comment) => vec![Command::SetComment(SetComment { address, comment })],
            None => vec![],
        })
    }
}
