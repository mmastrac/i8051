use crate::address::{AddressSpace, AddressValue, SpaceAddressSet};
use crate::db::{Db, Equivalent, EquivalentRange, Error};

use super::{Apply, Command, DisassembleRange, Environment, MarkData, MarkUnknown, boxed};

/// Rebuild a cleared strong equivalent as its verb command, so an undo
/// round-trips through the DSL.
fn restore_equivalent(
    space: AddressSpace,
    start: AddressValue,
    range: EquivalentRange,
) -> Box<dyn Command> {
    match range.equivalent {
        Equivalent::Code => boxed(DisassembleRange::new((space, start..range.end))),
        Equivalent::Data(data_type, size) => {
            boxed(MarkData::new((space, start..start + size), data_type))
        }
        Equivalent::Unknown(size) => boxed(MarkUnknown::new((space, start..start + size))),
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ClearEquivalents {
    pub addresses: SpaceAddressSet,
}

register!(ClearEquivalents(
    /// Clear disassembly equivalents (code islands, data, unknown) over the
    /// given `addresses`, returning them to undefined.
    addresses: SpaceAddressSet,
));

impl Apply for ClearEquivalents {
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
            let offset = range.start;
            let size = range.end - range.start;
            let before = region.snapshot_equivalents(offset, size);
            region.clear_equivalents(offset, size);
            for (start, equivalent_range) in before {
                undo.push(restore_equivalent(space, start, equivalent_range));
            }
        }
        Ok(undo)
    }
}

// A `SpaceAddressSet` field renders as the optimal `CODE:{...}` form: the
// adjacent addresses coalesce into a range, the lone one stays bare.
serialize_test!(
    clear_equivalents_address_set,
    "clear_equivalents(addresses=CODE:{0x10..0x13, 0x20})",
    ClearEquivalents {
        addresses: {
            let mut set = SpaceAddressSet::new(AddressSpace::Code);
            set.insert(0x10..0x13);
            set.insert_address(0x20);
            set
        },
    }
);
