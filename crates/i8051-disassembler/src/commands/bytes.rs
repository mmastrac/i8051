use crate::address::{
    AddressRange, AddressSpace, AddressValue, SpaceAddressRange, SpaceAddressSet, SpaceAddressValue,
};
use crate::db::{Db, Error};
use crate::region::ByteRange;

use super::{Apply, Command, Environment, boxed};

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct MapBytes {
    pub address: SpaceAddressValue,
    pub file: String,
    pub file_offset: usize,
    pub size: AddressValue,
}

register!(MapBytes(
    /// Map `size` bytes from `file` (at `file_offset`) into the address space
    /// starting at `offset`.
    space: AddressSpace,
    offset: AddressValue,
    file: impl Into<String>,
    file_offset: usize,
    size: AddressValue,
) {
    Self {
        address: (space, offset).into(),
        file: file.into(),
        file_offset,
        size,
    }
});

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
    /// Unmap the `size` bytes starting at `offset`.
    space: AddressSpace,
    offset: AddressValue,
    size: AddressValue,
) {
    let mut addresses = SpaceAddressSet::new(space);
    addresses.insert(offset..offset + size);
    Self { addresses }
});

impl ClearBytes {
    /// Clear an arbitrary set of byte ranges in one command.
    pub fn from_set(addresses: SpaceAddressSet) -> Self {
        Self { addresses }
    }
}

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
    /// Fill the `size` bytes starting at `offset` with the constant `value`.
    space: AddressSpace,
    offset: AddressValue,
    size: AddressValue,
    value: u8,
) {
    Self {
        range: (space, AddressRange::new(offset, offset + size)).into(),
        value,
    }
});

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
    let mut undo = vec![boxed(ClearBytes::from_set(addresses))];
    for (start, range) in ranges {
        match range {
            ByteRange::Mapped(file, file_offset, data) => {
                undo.push(boxed(MapBytes::new(
                    space,
                    start,
                    file,
                    file_offset,
                    data.len() as AddressValue,
                )));
            }
            ByteRange::Constant(count, value) => {
                undo.push(boxed(SetConstantBytes::new(
                    space,
                    start,
                    count as AddressValue,
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
        range: (AddressSpace::Code, AddressRange::new(0x10, 0x20)).into(),
        value: 0xFF,
    }
);
