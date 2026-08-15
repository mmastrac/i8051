use crate::address::SpaceAddressValue;
use crate::db::{Db, Error};

use super::{Apply, Command, Environment, boxed};

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct DisablePlatformAddress {
    pub address: SpaceAddressValue,
    pub reason: String,
}

register!(DisablePlatformAddress(
    /// Record that a platform address is disabled, along with a reason.
    ///
    /// The platform's default addresses are recommended, but not required and
    /// in some cases the addresses are re-used, re-purposed or otherwise would
    /// not analyze properly.
    address: SpaceAddressValue,
    reason: String,
));

impl Apply for DisablePlatformAddress {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { address, reason } = self;
        if reason.trim().is_empty() {
            return Err(Error::InvalidArgument {
                value: reason,
                reason: "a reason is required: say why this does not apply here",
            });
        }
        let claimed = db.platform().is_some_and(|p| {
            p.entry_points()
                .iter()
                .any(|e| e.space == address.space && e.offset == address.offset)
        });
        if !claimed {
            return Err(Error::InvalidArgument {
                value: format!("{}:{:#x}", address.space.dsl_name(), address.offset),
                reason: "the platform doesn't list this address, so there is nothing to disable",
            });
        }
        let previous = db
            .region_mut(address.space)
            .disable_platform_address(address.offset, reason);
        Ok(match previous {
            Some(reason) => vec![boxed(DisablePlatformAddress { address, reason })],
            None => vec![boxed(RestorePlatformAddress { address })],
        })
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct RestorePlatformAddress {
    pub address: SpaceAddressValue,
}

register!(RestorePlatformAddress(
    /// Restore a platform address, undoing `disable_platform_address`.
    address: SpaceAddressValue,
));

impl Apply for RestorePlatformAddress {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { address } = self;
        let previous = db
            .region_mut(address.space)
            .enable_platform_address(address.offset);
        Ok(match previous {
            Some(reason) => vec![boxed(DisablePlatformAddress { address, reason })],
            None => Vec::new(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commands::MapBytes;
    use crate::platform::i8051::CODE;

    struct Env;
    impl crate::commands::Environment for Env {
        fn load_file_bytes(
            &self,
            _f: &str,
            offset: usize,
            size: crate::address::AddressValue,
        ) -> Result<Vec<u8>, std::io::Error> {
            Ok(vec![0u8; offset + size as usize][offset..].to_vec())
        }
    }

    fn mapped() -> Db {
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(
            boxed(MapBytes::new((CODE, 0), "img", 0usize, 0x30u32)),
            Some(&Env),
        )
        .unwrap();
        db
    }

    fn disabled(db: &Db, offset: u32) -> bool {
        db.region(CODE)
            .is_some_and(|r| r.platform_address_disabled(offset))
    }

    #[test]
    fn disabling_vector_round_trips() {
        let mut db = mapped();
        assert!(!disabled(&db, 0x13));

        let undo = db
            .apply(
                boxed(DisablePlatformAddress {
                    address: (CODE, 0x13u32).into(),
                    reason: "IE=0x92 never enables EX1".to_string(),
                }),
                Some(&Env),
            )
            .unwrap();
        assert!(disabled(&db, 0x13));

        let dsl = crate::store::to_dsl_many(&db.to_commands());
        assert!(dsl.contains("disable_platform_address"), "{dsl}");
        assert!(dsl.contains("IE=0x92"), "the reason: {dsl}");

        let mut replayed = Db::new();
        for command in crate::store::from_dsl_many(&dsl).expect("export parses") {
            replayed.apply(command, Some(&Env)).expect("export replays");
        }
        assert!(disabled(&replayed, 0x13), "reloaded: {dsl}");

        for command in undo {
            db.apply(command, Some(&Env)).unwrap();
        }
        assert!(!disabled(&db, 0x13), "restoring reopens");
    }
}
