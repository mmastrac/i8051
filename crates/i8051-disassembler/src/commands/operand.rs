use crate::address::SpaceAddressValue;
use crate::db::{Db, Error, OperandOverride};

use super::{Apply, Command, Environment, boxed};


#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct OverrideOperand {
    pub address: SpaceAddressValue,
    pub index: u8,
    pub operand: Option<OperandOverride>,
}

register!(OverrideOperand(
    /// Override the rendered text of operand `index` of the instruction at the
    /// code `address`. Pass `None` to clear the override. Presentation only,
    /// independent of whether the bytes are classified as code.
    address: SpaceAddressValue,
    index: u8,
    operand: Option<OperandOverride>,
));

impl Apply for OverrideOperand {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let Self {
            address,
            index,
            operand,
        } = self;
        let space = address.space;
        let offset = address.offset;
        let region = db.region_mut(space);
        let prior = region.set_operand_override(offset, index, operand);
        Ok(vec![boxed(OverrideOperand {
            address,
            index,
            operand: prior,
        })])
    }
}

// `Some` flattens to the bare value. The struct variant renders as named kwargs.
serialize_test!(
    override_operand_struct_variant,
    r#"override_operand(address=CODE:0x0, index=0x1, operand=OperandOverride::LabelOffset(label="tbl", offset=0x4))"#,
    OverrideOperand {
        address: (crate::platform::i8051::CODE, 0x0).into(),
        index: 1,
        operand: Some(OperandOverride::LabelOffset {
            label: "tbl".into(),
            offset: 4,
        }),
    }
);
