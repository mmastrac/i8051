use crate::address::{SpaceAddressSet, SpaceAddressValue};
use crate::db::{Db, Equivalent, Error};

use super::{Apply, Command, Environment, boxed};

#[cfg(test)]
use crate::address::AddressSpace;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SetEquivalent {
    pub address: SpaceAddressValue,
    pub equivalent: Equivalent,
}

register!(SetEquivalent(
    /// Set the disassembly equivalent (how the bytes are interpreted) at the
    /// code `address`.
    address: SpaceAddressValue,
    equivalent: Equivalent,
));

impl Apply for SetEquivalent {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self {
            address,
            equivalent,
        } = self;
        let SpaceAddressValue { space, offset } = address;
        let region = db.region_mut(space);
        let span = region.equivalent_span(offset, &equivalent)?;
        let before = region.snapshot_equivalents(offset, span);
        region.set_equivalent(offset, equivalent)?;
        let mut undo = vec![boxed(ClearEquivalents::new((space, offset..offset + span)))];
        for (start, range) in before {
            undo.push(boxed(SetEquivalent {
                address: (space, start).into(),
                equivalent: range.equivalent,
            }));
        }
        Ok(undo)
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ClearEquivalents {
    pub addresses: SpaceAddressSet,
}

register!(ClearEquivalents(
    /// Clear disassembly equivalents over the given `addresses`.
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
                undo.push(boxed(SetEquivalent {
                    address: (space, start).into(),
                    equivalent: equivalent_range.equivalent,
                }));
            }
        }
        Ok(undo)
    }
}

#[cfg(test)]
use crate::db::{DataType, OperandOverride};

// `Option` is flattened (`Some(x)` -> `x`, `None` -> `None`) and the
// newtype-variant `OperandOverride::Label` nests inside the list element.
serialize_test!(
    equivalent_code_with_options,
    r#"set_equivalent(address=CODE:0x10, equivalent=Equivalent::Code([OperandOverride::Label("loop"), None]))"#,
    SetEquivalent {
        address: (AddressSpace::Code, 0x10).into(),
        equivalent: Equivalent::Code(vec![Some(OperandOverride::Label("loop".into())), None,]),
    }
);

// Struct-variant (`LabelOffset { label, offset }`) renders as named kwargs,
// sorted by field name.
serialize_test!(
    operand_override_struct_variant,
    r#"set_equivalent(address=CODE:0x0, equivalent=Equivalent::Code([OperandOverride::LabelOffset(label="tbl", offset=0x4)]))"#,
    SetEquivalent {
        address: (AddressSpace::Code, 0x0).into(),
        equivalent: Equivalent::Code(vec![Some(OperandOverride::LabelOffset {
            label: "tbl".into(),
            offset: 4,
        })]),
    }
);

// Recursive boxed enum: `Box<DataType>` is transparent, so the recursion
// nests cleanly through tuple variants.
serialize_test!(
    equivalent_data_recursive_datatype,
    "set_equivalent(address=CODE:0x20, equivalent=Equivalent::Data(DataType::Array(DataType::Byte, 0x10), 0x10))",
    SetEquivalent {
        address: (AddressSpace::Code, 0x20).into(),
        equivalent: Equivalent::Data(DataType::Array(Box::new(DataType::Byte), 0x10), 0x10),
    }
);

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
