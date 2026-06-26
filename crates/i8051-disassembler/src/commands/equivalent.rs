use crate::address::{AddressRange, SpaceAddressRange, SpaceAddressValue};
use crate::db::{Db, Equivalent, Error};

use super::Command;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SetEquivalent {
    pub address: SpaceAddressValue,
    pub equivalent: Equivalent,
}

impl SetEquivalent {
    pub fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn super::Environment>,
    ) -> Result<Vec<Command>, Error> {
        let Self { address, equivalent } = self;
        let SpaceAddressValue { space, offset } = address;
        let region = db.region_mut(space);
        let span = region.equivalent_span(offset, &equivalent)?;
        let before = region.snapshot_equivalents(offset, span);
        region.set_equivalent(offset, equivalent)?;
        let mut undo = vec![Command::ClearEquivalents(ClearEquivalents {
            range: (space, AddressRange::new(offset, offset + span)).into(),
        })];
        for (start, range) in before {
            undo.push(Command::SetEquivalent(SetEquivalent {
                address: (space, start).into(),
                equivalent: range.equivalent,
            }));
        }
        Ok(undo)
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ClearEquivalents {
    pub range: SpaceAddressRange,
}

impl ClearEquivalents {
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
        let before = region.snapshot_equivalents(offset, size);
        region.clear_equivalents(offset, size);
        let mut undo = Vec::new();
        for (start, range) in before {
            undo.push(Command::SetEquivalent(SetEquivalent {
                address: (space, start).into(),
                equivalent: range.equivalent,
            }));
        }
        Ok(undo)
    }
}

#[cfg(test)]
use crate::address::AddressSpace;
#[cfg(test)]
use crate::db::{DataType, OperandOverride};

// `Option` is flattened (`Some(x)` -> `x`, `None` -> `None`) and the
// newtype-variant `OperandOverride::Label` nests inside the list element.
serialize_test!(
    equivalent_code_with_options,
    r#"set_equivalent(address=CODE:0x10, equivalent=Equivalent::Code([OperandOverride::Label("loop"), None]))"#,
    SetEquivalent {
        address: (AddressSpace::Code, 0x10).into(),
        equivalent: Equivalent::Code(vec![
            Some(OperandOverride::Label("loop".into())),
            None,
        ]),
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
