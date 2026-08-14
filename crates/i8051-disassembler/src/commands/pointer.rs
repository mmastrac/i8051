use crate::address::{SpaceAddressSet, SpaceAddressValue};
use crate::db::{Db, Error, OperandType};

use super::{Apply, Command, Environment, boxed};

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SetOperandPointer {
    pub address: SpaceAddressValue,
    pub space: String,
}

register!(SetOperandPointer(
    /// Mark an instruction as loading an address into `space`.
    ///
    /// Registers may be used to address addresses in any number of spaces, so
    /// this command marks an instruction as loading an address into `space`.
    address: SpaceAddressValue,
    space: String,
));

impl Apply for SetOperandPointer {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { address, space } = self;
        let target = db.resolve_space(&space)?;
        let previous = db
            .region_mut(address.space)
            .set_operand_type(address.offset, OperandType::Pointer(target));
        Ok(undo_for(address, previous))
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SetOperandValue {
    pub address: SpaceAddressValue,
}

register!(SetOperandValue(
    /// Mark an instruction as loading a scalar value, not an address.
    address: SpaceAddressValue,
));

impl Apply for SetOperandValue {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { address } = self;
        let previous = db
            .region_mut(address.space)
            .set_operand_type(address.offset, OperandType::Value);
        Ok(undo_for(address, previous))
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ClearOperandType {
    pub addresses: SpaceAddressSet,
}

register!(ClearOperandType(
    /// Forget what these instructions' ambiguous operands resolve to.
    addresses: SpaceAddressSet,
));

impl Apply for ClearOperandType {
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
            for offset in range {
                if let Some(previous) = region.clear_operand_type(offset) {
                    undo.extend(undo_for((space, offset).into(), Some(previous)));
                }
            }
        }
        Ok(undo)
    }
}

fn undo_for(address: SpaceAddressValue, previous: Option<OperandType>) -> Vec<Box<dyn Command>> {
    match previous {
        Some(OperandType::Pointer(space)) => vec![boxed(SetOperandPointer {
            address,
            space: space.dsl_name().to_string(),
        })],
        Some(OperandType::Value) => vec![boxed(SetOperandValue { address })],
        None => vec![boxed(ClearOperandType::new((
            address.space,
            address.offset,
        )))],
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::address::PhysicalAddr;
    use crate::commands::{AutoDisassemble, MapBytes};
    use crate::platform::i8051::{CODE, XDATA};

    struct Env;
    impl crate::commands::Environment for Env {
        fn load_file_bytes(
            &self,
            _f: &str,
            offset: usize,
            size: crate::address::AddressValue,
        ) -> Result<Vec<u8>, std::io::Error> {
            // MOV DPTR,#0x0008 / RET, then two table bytes.
            const IMAGE: [u8; 6] = [0x90, 0x00, 0x08, 0x22, 0xAA, 0xBB];
            Ok(IMAGE[offset..offset + size as usize].to_vec())
        }
    }

    fn fixture() -> Db {
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(
            boxed(MapBytes::new((CODE, 0), "img", 0usize, 6u32)),
            Some(&Env),
        )
        .unwrap();
        db.apply(boxed(AutoDisassemble::new((CODE, 0u32))), Some(&Env))
            .unwrap();
        db
    }

    fn replay(db: &Db) -> Db {
        let dsl = crate::store::to_dsl_many(&db.to_commands());
        let mut fresh = Db::new();
        for command in crate::store::from_dsl_many(&dsl).expect("export parses") {
            fresh.apply(command, Some(&Env)).expect("export replays");
        }
        fresh
    }

    const CODE_8: PhysicalAddr = PhysicalAddr {
        space: CODE,
        offset: 8,
    };
    const XDATA_8: PhysicalAddr = PhysicalAddr {
        space: XDATA,
        offset: 8,
    };

    /// Deciding at the instruction picks one candidate and drops the rest.
    #[test]
    fn deciding_an_operand_collapses_its_candidates() {
        let mut db = fixture();
        assert_eq!(db.xrefs_to(&CODE_8).len(), 1, "both candidates start open");
        assert_eq!(db.xrefs_to(&XDATA_8).len(), 1);
        assert_eq!(
            db.undecided_operands().len(),
            1
        );

        let undo = db
            .apply(
                boxed(SetOperandPointer {
                    address: (CODE, 0u32).into(),
                    space: "CODE".to_string(),
                }),
                None,
            )
            .unwrap();
        assert_eq!(db.xrefs_to(&CODE_8).len(), 1, "there can be only one");
        assert!(db.xrefs_to(&XDATA_8).is_empty(), "the rejected one is gone");
        assert!(
            db.undecided_operands().is_empty(),
            "this is the way"
        );

        let reloaded = replay(&db);
        assert!(
            reloaded.xrefs_to(&XDATA_8).is_empty(),
            "survives a reload"
        );

        for command in undo {
            db.apply(command, None).unwrap();
        }
        assert_eq!(db.xrefs_to(&XDATA_8).len(), 1, "undo reopens");
    }

    /// `MOV DPTR,#0x0008` may just be loading a number.
    #[test]
    fn an_operand_decided_to_be_a_number_references_nothing() {
        let mut db = fixture();
        db.apply(
            boxed(SetOperandValue {
                address: (CODE, 0u32).into(),
            }),
            None,
        )
        .unwrap();

        assert!(
            db.xrefs_to(&CODE_8).is_empty(),
            "a number doesn't reference anything"
        );
        assert!(db.xrefs_to(&XDATA_8).is_empty());
        assert!(replay(&db).xrefs_to(&CODE_8).is_empty(), "and it reloads");
    }

    /// Clearing restores every candidate.
    #[test]
    fn clearing_restores_the_candidates() {
        let mut db = fixture();
        db.apply(
            boxed(SetOperandValue {
                address: (CODE, 0u32).into(),
            }),
            None,
        )
        .unwrap();
        db.apply(boxed(ClearOperandType::new((CODE, 0u32))), None)
            .unwrap();

        assert_eq!(db.xrefs_to(&CODE_8).len(), 1);
        assert_eq!(db.xrefs_to(&XDATA_8).len(), 1);
    }
}
