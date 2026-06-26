use crate::address::{AddressSpace, AddressValue, SpaceAddressValue};
use crate::db::{Db, Error};

use super::{Apply, Command, Environment, boxed};

register_commands!(SetComment, ClearComment);

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SetComment {
    pub address: SpaceAddressValue,
    pub comment: String,
}

impl SetComment {
    pub fn new(space: AddressSpace, offset: AddressValue, comment: impl Into<String>) -> Self {
        Self {
            address: (space, offset).into(),
            comment: comment.into(),
        }
    }
}

impl Apply for SetComment {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { address, comment } = self;
        let SpaceAddressValue { space, offset } = address;
        let region = db.region_mut(space);
        let before = region.get_comment(offset).map(str::to_owned);
        region.set_comment(offset, &comment);
        Ok(match before {
            Some(comment) => vec![boxed(SetComment { address, comment })],
            None => vec![boxed(ClearComment { address })],
        })
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ClearComment {
    pub address: SpaceAddressValue,
}

impl ClearComment {
    pub fn new(space: AddressSpace, offset: AddressValue) -> Self {
        Self {
            address: (space, offset).into(),
        }
    }
}

impl Apply for ClearComment {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { address } = self;
        let SpaceAddressValue { space, offset } = address;
        let region = db.region_mut(space);
        let before = region.get_comment(offset).map(str::to_owned);
        region.clear_comment(offset);
        Ok(match before {
            Some(comment) => vec![boxed(SetComment { address, comment })],
            None => vec![],
        })
    }
}
