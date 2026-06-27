use crate::address::{SpaceAddressSet, SpaceAddressValue};
use crate::db::{Db, Error};

use super::{Apply, Command, Environment, boxed};

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SetComment {
    pub address: SpaceAddressValue,
    pub comment: String,
}

register!(SetComment(
    /// Attach `comment` to the code `address`.
    address: SpaceAddressValue,
    comment: String,
));

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
            None => vec![boxed(ClearComment::new((space, offset)))],
        })
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ClearComment {
    pub addresses: SpaceAddressSet,
}

register!(ClearComment(
    /// Remove comments at the given `addresses`.
    addresses: SpaceAddressSet,
));

impl Apply for ClearComment {
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
            for (offset, comment) in region.clear_comments_in(range) {
                undo.push(boxed(SetComment::new((space, offset), comment)));
            }
        }
        Ok(undo)
    }
}
