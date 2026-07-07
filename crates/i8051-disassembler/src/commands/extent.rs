use crate::address::SpaceAddressRange;
use crate::db::{DataType, Db, Equivalent, Error};

use super::{Apply, ClearEquivalents, Command, Environment, boxed};


#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct DisassembleRange {
    pub range: SpaceAddressRange,
}

register!(DisassembleRange(
    /// Linearly disassemble `range` as an explicit block of code, decoding each
    /// instruction in turn. Unlike `auto_disassemble` it does not follow control
    /// flow. Use it for code the recursive sweep can't reach.
    range: SpaceAddressRange,
));

impl Apply for DisassembleRange {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let SpaceAddressRange { space, range } = self.range;
        let region = db.region_mut(space);
        let end = region.disassemble_linear(range.start, range.end)?;
        Ok(vec![boxed(ClearEquivalents::new((
            space,
            range.start..end,
        )))])
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct MarkData {
    pub range: SpaceAddressRange,
    pub data_type: DataType,
}

register!(MarkData(
    /// Mark `range` as data of the given `data_type` (a barrier that stops
    /// auto-disassembly).
    range: SpaceAddressRange,
    data_type: DataType,
));

impl Apply for MarkData {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let SpaceAddressRange { space, range } = self.range;
        let size = range.end.saturating_sub(range.start);
        let region = db.region_mut(space);
        // `set_equivalent` rejects strong overlap, so undo is just clearing the range.
        region.set_equivalent(range.start, Equivalent::Data(self.data_type, size))?;
        Ok(vec![boxed(ClearEquivalents::new((
            space,
            range.start..range.start + size,
        )))])
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct MarkUnknown {
    pub range: SpaceAddressRange,
}

register!(MarkUnknown(
    /// Mark `range` as unknown: a typeless barrier that renders as raw bytes and
    /// stops auto-disassembly, without committing to a data type.
    range: SpaceAddressRange,
));

impl Apply for MarkUnknown {
    fn apply(
        self,
        db: &mut Db,
        _env: Option<&dyn Environment>,
    ) -> Result<Vec<Box<dyn Command>>, Error> {
        let SpaceAddressRange { space, range } = self.range;
        let size = range.end.saturating_sub(range.start);
        let region = db.region_mut(space);
        region.set_equivalent(range.start, Equivalent::Unknown(size))?;
        Ok(vec![boxed(ClearEquivalents::new((
            space,
            range.start..range.start + size,
        )))])
    }
}

// `Box<DataType>` is transparent, so the recursion nests through tuple variants.
serialize_test!(
    mark_data_recursive_datatype,
    "mark_data(data_type=DataType::Array(DataType::Byte, 0x10), range=CODE:0x20..0x30)",
    MarkData {
        range: (crate::platform::i8051::CODE, 0x20..0x30).into(),
        data_type: DataType::Array(Box::new(DataType::Byte), 0x10),
    }
);
