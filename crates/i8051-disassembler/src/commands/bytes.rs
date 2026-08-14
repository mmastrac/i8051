use crate::address::{AddressValue, SpaceAddressRange, SpaceAddressSet, SpaceAddressValue};
use crate::db::{Db, Error};
use crate::region::ByteRange;

use super::{Apply, Command, Environment, boxed};

#[cfg(test)]
use crate::address::AddressRange;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct MapBytes {
    pub address: SpaceAddressValue,
    pub file: String,
    pub file_offset: usize,
    pub size: AddressValue,
}

register!(MapBytes(
    /// Map `size` bytes from `file` (at `file_offset`) into the address space
    /// starting at `address`.
    address: SpaceAddressValue,
    file: String,
    file_offset: usize,
    size: AddressValue,
));

impl Apply for MapBytes {
    fn apply(
        self,
        db: &mut Db,
        env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self {
            address,
            file,
            file_offset,
            size,
        } = self;
        let SpaceAddressValue { space, offset } = address;
        let region = db.region_mut(space);
        let Some(env) = env else {
            return Err(Error::NoEnvironment);
        };
        let bytes = env
            .load_file_bytes(&file, file_offset, size)
            .map_err(Error::Io)?;
        let size = bytes.len() as AddressValue;
        let before = region.snapshot_byte_ranges(offset, size);
        region.map_bytes(&file, file_offset, offset, &bytes);
        let mut addresses = SpaceAddressSet::new(space);
        addresses.insert(offset..offset + size);
        Ok(undo_byte_ranges(addresses, before))
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct UnmapBytes {
    pub addresses: SpaceAddressSet,
}

register!(UnmapBytes(
    /// Unmap the bytes covered by `addresses`, along with whatever classified
    /// them. The inverse of `map_bytes`.
    addresses: SpaceAddressSet,
));

impl Apply for UnmapBytes {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { addresses } = self;
        let space = addresses.space;
        let region = db.region_mut(space);

        // Refuse to cut a classification
        for range in addresses.ranges() {
            for (start, equivalent) in region.snapshot_equivalents(range.start, range.end - range.start) {
                if start < range.start || equivalent.end > range.end {
                    return Err(Error::PartialEquivalent {
                        at: (space, range.start).into(),
                        existing: equivalent.equivalent.kind(),
                        start,
                        end: equivalent.end,
                    });
                }
            }
        }

        let mut before = Vec::new();
        let mut equivalents = Vec::new();
        for range in addresses.ranges() {
            let offset = range.start;
            let size = range.end - range.start;
            for (start, equivalent) in region.snapshot_equivalents(offset, size) {
                equivalents.push(super::equivalent::restore_equivalent(
                    space, start, equivalent,
                ));
            }
            region.clear_equivalents(offset, size);
            before.extend(region.snapshot_byte_ranges(offset, size));
            region.unmap_bytes(offset, size);
        }

        // Bytes first, so the classifications land on mapped memory.
        let mut undo = undo_byte_ranges(addresses, before);
        undo.extend(equivalents);
        Ok(undo)
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SetConstantBytes {
    pub range: SpaceAddressRange,
    pub value: u8,
}

register!(SetConstantBytes(
    /// Fill the bytes covered by `range` with the constant `value`.
    range: SpaceAddressRange,
    value: u8,
));

impl Apply for SetConstantBytes {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { range, value } = self;
        let SpaceAddressRange {
            space,
            range: address_range,
        } = range;
        let offset = address_range.start;
        let size = address_range.end - address_range.start;
        let region = db.region_mut(space);
        let before = region.snapshot_byte_ranges(offset, size);
        region.set_constant(offset, size, value);
        let mut addresses = SpaceAddressSet::new(space);
        addresses.insert(offset..offset + size);
        Ok(undo_byte_ranges(addresses, before))
    }
}

/// Build the undo for an operation that overwrote the byte ranges in
/// `addresses`: first re-clear that whole set, then restore each prior range.
fn undo_byte_ranges(
    addresses: SpaceAddressSet,
    ranges: Vec<(AddressValue, ByteRange)>,
) -> Vec<Box<dyn Command>> {
    let space = addresses.space;
    let mut undo = vec![boxed(UnmapBytes::new(addresses))];
    for (start, range) in ranges {
        match range {
            ByteRange::Mapped(file, file_offset, data) => {
                undo.push(boxed(MapBytes::new(
                    (space, start),
                    file,
                    file_offset,
                    data.len() as AddressValue,
                )));
            }
            ByteRange::Constant(count, value) => {
                undo.push(boxed(SetConstantBytes::new(
                    (space, start..start + count as AddressValue),
                    value,
                )));
            }
        }
    }
    undo
}

// Address range + a `u8` field, both rendered in hex.
serialize_test!(
    constant_bytes_range_and_byte,
    "set_constant_bytes(range=CODE:0x10..0x20, value=0xFF)",
    SetConstantBytes {
        range: (crate::platform::i8051::CODE, AddressRange::new(0x10, 0x20)).into(),
        value: 0xFF,
    }
);

#[cfg(test)]
mod unmap_bytes_tests {
    use super::*;
    use crate::commands::{MarkUnknown, boxed};
    use crate::db::Db;
    use crate::platform::i8051::CODE;

    struct Env;
    impl crate::commands::Environment for Env {
        fn load_file_bytes(
            &self,
            _f: &str,
            offset: usize,
            size: AddressValue,
        ) -> Result<Vec<u8>, std::io::Error> {
            Ok(vec![0x00; offset + size as usize][offset..].to_vec())
        }
    }

    fn mapped() -> Db {
        let mut db = Db::with_platform(crate::platform::i8051::platform());
        db.apply(
            boxed(MapBytes::new((CODE, 0), "img", 0usize, 0x10u32)),
            Some(&Env),
        )
        .unwrap();
        db
    }

    #[test]
    fn unmapping_takes_the_classification_and_undo_puts_both_back() {
        let mut db = mapped();
        db.apply(boxed(MarkUnknown::new((CODE, 0x8u32..0x9u32))), Some(&Env))
            .unwrap();

        let undo = db
            .apply(boxed(UnmapBytes::new((CODE, 0x8u32..0x9u32))), Some(&Env))
            .unwrap();

        let dsl = crate::store::to_dsl_many(&db.to_commands());
        let mut replayed = Db::new();
        for command in crate::store::from_dsl_many(&dsl).expect("export parses") {
            replayed
                .apply(command, Some(&Env))
                .expect("every exported command must apply to a fresh database");
        }

        for command in undo {
            db.apply(command, Some(&Env)).unwrap();
        }
        let restored = crate::store::to_dsl_many(&db.to_commands());
        assert!(restored.contains("mark_unknown"), "{restored}");
    }

    #[test]
    fn unmapping_part_of_a_classification_is_refused_and_changes_nothing() {
        let mut db = mapped();
        db.apply(boxed(MarkUnknown::new((CODE, 0x4u32..0xCu32))), Some(&Env))
            .unwrap();
        let before = crate::store::to_dsl_many(&db.to_commands());

        let err = db
            .apply(boxed(UnmapBytes::new((CODE, 0x8u32..0x9u32))), Some(&Env))
            .expect_err("a partial cut must not be obeyed");
        assert!(matches!(err, Error::PartialEquivalent { .. }), "{err:?}");

        assert_eq!(crate::store::to_dsl_many(&db.to_commands()), before);
    }
}
