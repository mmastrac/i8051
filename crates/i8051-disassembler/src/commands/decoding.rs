use crate::address::AddressValue;
use crate::db::{Db, Error};

use super::{Apply, Command, Environment, boxed};

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SetAddressBits {
    pub space: String,
    pub bits: AddressValue,
}

register!(SetAddressBits(
    /// Declare how many address lines the board decodes for `space`.
    ///
    /// Depending on how it is wired, a CPU may have a smaller or larger
    /// effective address space than the ROM.
    space: String,
    bits: AddressValue,
));

impl Apply for SetAddressBits {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { space, bits } = self;
        let target = db.resolve_space(&space)?;
        if bits == 0 || bits > AddressValue::BITS {
            return Err(Error::InvalidArgument {
                value: bits.to_string(),
                reason: "address width must be between 1 and 32 bits",
            });
        }
        let previous = db.region_mut(target).set_address_bits(Some(bits as u8));
        Ok(undo_for(&space, previous))
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ClearAddressBits {
    pub space: String,
}

register!(ClearAddressBits(
    /// Restore `space` to the full address width, undoing `set_address_bits`.
    space: String,
));

impl Apply for ClearAddressBits {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { space } = self;
        let target = db.resolve_space(&space)?;
        let previous = db.region_mut(target).set_address_bits(None);
        Ok(undo_for(&space, previous))
    }
}

fn undo_for(space: &str, previous: Option<u8>) -> Vec<Box<dyn Command>> {
    match previous {
        Some(bits) => vec![boxed(SetAddressBits {
            space: space.to_string(),
            bits: AddressValue::from(bits),
        })],
        None => vec![boxed(ClearAddressBits {
            space: space.to_string(),
        })],
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::address::PhysicalAddr;
    use crate::commands::{AutoDisassemble, MapBytes};
    use crate::platform::i8051::CODE;

    struct Env;
    impl crate::commands::Environment for Env {
        fn load_file_bytes(
            &self,
            _f: &str,
            offset: usize,
            size: AddressValue,
        ) -> Result<Vec<u8>, std::io::Error> {
            // LJMP 0xF004 / RET / NOP / RET — the jump target is outside the
            // image, but its low three bits land back inside it.
            const IMAGE: [u8; 8] = [0x02, 0xF0, 0x04, 0x22, 0x00, 0x22, 0x00, 0x22];
            Ok(IMAGE[offset..offset + size as usize].to_vec())
        }
    }

    /// With the high lines undecoded, a jump "outside" the image reaches the
    /// byte it actually selects, and the reference lands there.
    #[test]
    fn narrowing_resolves_references() {
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(
            boxed(MapBytes::new((CODE, 0), "img", 0usize, 8u32)),
            Some(&Env),
        )
        .unwrap();
        db.apply(boxed(AutoDisassemble::new((CODE, 0u32))), Some(&Env))
            .unwrap();

        let wrapped = PhysicalAddr {
            space: CODE,
            offset: 0x04,
        };
        let raw = PhysicalAddr {
            space: CODE,
            offset: 0xF004,
        };
        assert!(
            db.xrefs_to(&wrapped).is_empty(),
            "unmasked, the jump leaves the image"
        );
        assert_eq!(db.xrefs_to(&raw).len(), 1);

        let undo = db
            .apply(
                boxed(SetAddressBits {
                    space: "CODE".to_string(),
                    bits: 3,
                }),
                Some(&Env),
            )
            .unwrap();
        assert_eq!(db.xrefs_to(&wrapped).len(), 1, "masked, it reaches 0x04");
        assert!(db.xrefs_to(&raw).is_empty(), "and no longer points outside");
        // The wiring is a stated fact, so it must survive an export/reload —
        // and be restated before anything whose targets depend on it.
        let dsl = crate::store::to_dsl_many(&db.to_commands());
        assert!(dsl.contains("set_address_bits"), "{dsl}");
        let mut replayed = Db::new();
        for command in crate::store::from_dsl_many(&dsl).expect("export parses") {
            replayed.apply(command, Some(&Env)).expect("export replays");
        }
        assert_eq!(replayed.xrefs_to(&wrapped).len(), 1, "reloaded: {dsl}");

        for command in undo {
            db.apply(command, Some(&Env)).unwrap();
        }
        assert_eq!(db.xrefs_to(&raw).len(), 1, "undo restores the full width");
    }

    /// A width the hardware cannot have is refused rather than stored.
    #[test]
    fn impossible_width_refused() {
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        for bits in [0u32, 33] {
            let err = db
                .apply(
                    boxed(SetAddressBits {
                        space: "CODE".to_string(),
                        bits,
                    }),
                    None,
                )
                .expect_err("width must be 1..=32");
            assert!(matches!(err, Error::InvalidArgument { .. }), "{err:?}");
        }
        // A typo must not answer as a freshly created region.
        let err = db
            .apply(
                boxed(SetAddressBits {
                    space: "COED".to_string(),
                    bits: 12,
                }),
                None,
            )
            .expect_err("a space this database does not have");
        assert_eq!(
            err.to_string(),
            "unknown address space \"COED\" (did you mean `CODE`?)"
        );
    }
}
