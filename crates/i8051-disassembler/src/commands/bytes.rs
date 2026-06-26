use crate::address::{AddressRange, AddressValue, SpaceAddressRange, SpaceAddressValue};
use crate::db::{Db, Error};
use crate::region::ByteRange;

use super::Command;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct MapBytes {
    pub address: SpaceAddressValue,
    pub file: String,
    pub file_offset: usize,
    pub size: AddressValue,
}

impl MapBytes {
    pub fn apply(
        self,
        db: &mut Db,
        env: Option<&dyn super::Environment>,
    ) -> Result<Vec<Command>, Error> {
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
        Ok(undo_byte_ranges(address, size, before))
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ClearBytes {
    pub range: SpaceAddressRange,
}

impl ClearBytes {
    pub fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn super::Environment>,
    ) -> Result<Vec<Command>, Error> {
        let Self { range } = self;
        let SpaceAddressRange { space, range: address_range } = range;
        let offset = address_range.start;
        let size = address_range.end - address_range.start;
        let region = db.region_mut(space);
        let before = region.snapshot_byte_ranges(offset, size);
        region.clear_bytes(offset, size);
        Ok(undo_byte_ranges((space, offset).into(), size, before))
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SetConstantBytes {
    pub range: SpaceAddressRange,
    pub value: u8,
}

impl SetConstantBytes {
    pub fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn super::Environment>,
    ) -> Result<Vec<Command>, Error> {
        let Self { range, value } = self;
        let SpaceAddressRange { space, range: address_range } = range;
        let offset = address_range.start;
        let size = address_range.end - address_range.start;
        let region = db.region_mut(space);
        let before = region.snapshot_byte_ranges(offset, size);
        region.set_constant(offset, size, value);
        Ok(undo_byte_ranges((space, offset).into(), size, before))
    }
}

fn undo_byte_ranges(
    address: SpaceAddressValue,
    size: AddressValue,
    ranges: Vec<(AddressValue, ByteRange)>,
) -> Vec<Command> {
    let SpaceAddressValue { space, offset } = address;
    let mut undo = vec![Command::ClearBytes(ClearBytes {
        range: (space, AddressRange::new(offset, offset + size)).into(),
    })];
    for (start, range) in ranges {
        match range {
            ByteRange::Mapped(file, file_offset, data) => {
                undo.push(Command::MapBytes(MapBytes {
                    address: (space, start).into(),
                    file,
                    file_offset,
                    size: data.len() as AddressValue,
                }));
            }
            ByteRange::Constant(count, value) => {
                undo.push(Command::SetConstantBytes(SetConstantBytes {
                    range: (space, AddressRange::new(start, start + count as AddressValue)).into(),
                    value,
                }));
            }
        }
    }
    undo
}

#[cfg(test)]
use crate::address::AddressSpace;

// Address range + a `u8` field, both rendered in hex.
serialize_test!(
    constant_bytes_range_and_byte,
    "set_constant_bytes(range=CODE:0x10..0x20, value=0xFF)",
    SetConstantBytes {
        range: (AddressSpace::Code, AddressRange::new(0x10, 0x20)).into(),
        value: 0xFF,
    }
);
