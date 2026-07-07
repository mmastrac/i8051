use crate::address::SpaceAddressValue;
use crate::db::{Db, Error};

use super::{Apply, Command, Environment, boxed};

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct AutoDisassemble {
    pub address: SpaceAddressValue,
}

register!(AutoDisassemble(
    /// Recursively disassemble from `address`, following control flow. Recorded
    /// as a root, so the saved DB regenerates the code from this one command.
    address: SpaceAddressValue,
));

impl Apply for AutoDisassemble {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        if db.platform().is_none() {
            return Err(Error::NoCpu);
        }
        let Self { address } = self;
        let SpaceAddressValue {
            space,
            offset: start,
        } = address;
        let region = db.region_mut(space);
        let was_root = region.is_auto_root(start);
        region.add_auto_root(start);

        // Only undo the root if this call introduced it (idempotent re-runs).
        let mut undo: Vec<Box<dyn Command>> = Vec::new();
        if !was_root {
            undo.push(boxed(ClearAutoDisassembleRoot { address }));
        }
        Ok(undo)
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ClearAutoDisassembleRoot {
    pub address: SpaceAddressValue,
}

register!(ClearAutoDisassembleRoot(
    /// Remove an auto-disassemble root. Code reachable only through it
    /// re-derives away (code still reached by another root stays). The inverse
    /// of `auto_disassemble`.
    address: SpaceAddressValue,
));

impl Apply for ClearAutoDisassembleRoot {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { address } = self;
        let SpaceAddressValue { space, offset } = address;
        let region = db.region_mut(space);
        if region.remove_auto_root(offset) {
            Ok(vec![boxed(AutoDisassemble { address })])
        } else {
            Ok(vec![])
        }
    }
}
