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
pub struct ClearBytes {
    pub addresses: SpaceAddressSet,
}

register!(ClearBytes(
    /// Unmap the bytes covered by `addresses`.
    addresses: SpaceAddressSet,
));

impl Apply for ClearBytes {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self { addresses } = self;
        let space = addresses.space;
        let region = db.region_mut(space);
        let mut before = Vec::new();
        for range in addresses.ranges() {
            let offset = range.start;
            let size = range.end - range.start;
            before.extend(region.snapshot_byte_ranges(offset, size));
            region.clear_bytes(offset, size);
        }
        Ok(undo_byte_ranges(addresses, before))
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
    let mut undo = vec![boxed(ClearBytes::new(addresses))];
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
