use std::marker::PhantomData;
use std::ops::{Bound, Deref, Range, RangeBounds};

use i8051::{ControlFlow, Instruction, Mnemonic};
use rangemap::RangeSet;
use serde::de::value::SeqAccessDeserializer;
use serde::de::{Error as _, SeqAccess, Visitor};
use serde::{Deserialize, Deserializer, Serialize, Serializer};

pub type AddressValue = u32;

// Signals to the serializer/deserializer to use compact syntax.
pub(crate) const ADDRESS_TOKEN: &str = "$dsl::address";
pub(crate) const ADDRESS_RANGE_TOKEN: &str = "$dsl::address_range";
pub(crate) const ADDRESS_SET_TOKEN: &str = "$dsl::address_set";

/// A space-qualified address. Derefs to its [`AddressValue`] offset.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SpaceAddressValue {
    pub space: AddressSpace,
    pub offset: AddressValue,
}

impl Deref for SpaceAddressValue {
    type Target = AddressValue;
    fn deref(&self) -> &Self::Target {
        &self.offset
    }
}

impl From<(AddressSpace, AddressValue)> for SpaceAddressValue {
    fn from((space, offset): (AddressSpace, AddressValue)) -> Self {
        Self { space, offset }
    }
}

impl Serialize for SpaceAddressValue {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer
            .serialize_newtype_struct(ADDRESS_TOKEN, &(self.space.dsl_name(), self.offset as u64))
    }
}

impl<'de> Deserialize<'de> for SpaceAddressValue {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let (space, offset): (String, u64) = deserialize_token(deserializer, ADDRESS_TOKEN)?;
        Ok(Self {
            space: decode_space::<D>(&space)?,
            offset: offset as AddressValue,
        })
    }
}

/// A space-qualified address range. Derefs to its [`AddressRange`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SpaceAddressRange {
    pub space: AddressSpace,
    pub range: AddressRange,
}

impl Deref for SpaceAddressRange {
    type Target = AddressRange;
    fn deref(&self) -> &Self::Target {
        &self.range
    }
}

impl From<(AddressSpace, AddressRange)> for SpaceAddressRange {
    fn from((space, range): (AddressSpace, AddressRange)) -> Self {
        Self { space, range }
    }
}

impl Serialize for SpaceAddressRange {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_newtype_struct(
            ADDRESS_RANGE_TOKEN,
            &(
                self.space.dsl_name(),
                self.range.start as u64,
                self.range.end as u64,
            ),
        )
    }
}

impl<'de> Deserialize<'de> for SpaceAddressRange {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let (space, start, end): (String, u64, u64) =
            deserialize_token(deserializer, ADDRESS_RANGE_TOKEN)?;
        Ok(Self {
            space: decode_space::<D>(&space)?,
            range: AddressRange::new(start as AddressValue, end as AddressValue),
        })
    }
}

/// A set of addresses within one space, stored as coalesced half-open ranges.
/// Serializes to the optimal DSL form `CODE:{0x10..0x13, 0x20}` — adjacent
/// addresses merge into ranges, singletons stay bare.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpaceAddressSet {
    pub space: AddressSpace,
    pub set: RangeSet<AddressValue>,
}

impl SpaceAddressSet {
    pub fn new(space: AddressSpace) -> Self {
        Self {
            space,
            set: RangeSet::new(),
        }
    }

    /// Insert a half-open range (no-op if empty).
    pub fn insert(&mut self, range: Range<AddressValue>) {
        if range.start < range.end {
            self.set.insert(range);
        }
    }

    /// Insert a single address.
    pub fn insert_address(&mut self, address: AddressValue) {
        self.set.insert(address..address + 1);
    }

    pub fn contains(&self, address: AddressValue) -> bool {
        self.set.contains(&address)
    }

    pub fn is_empty(&self) -> bool {
        self.set.is_empty()
    }

    /// The coalesced ranges, in ascending order.
    pub fn ranges(&self) -> impl Iterator<Item = Range<AddressValue>> + '_ {
        self.set.iter().cloned()
    }
}

impl Serialize for SpaceAddressSet {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let ranges: Vec<(u64, u64)> = self
            .set
            .iter()
            .map(|range| (range.start as u64, range.end as u64))
            .collect();
        serializer.serialize_newtype_struct(ADDRESS_SET_TOKEN, &(self.space.dsl_name(), ranges))
    }
}

impl<'de> Deserialize<'de> for SpaceAddressSet {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let (space, ranges): (String, Vec<(u64, u64)>) =
            deserialize_token(deserializer, ADDRESS_SET_TOKEN)?;
        let mut set = Self::new(decode_space::<D>(&space)?);
        for (start, end) in ranges {
            set.insert(start as AddressValue..end as AddressValue);
        }
        Ok(set)
    }
}

fn decode_space<'de, D: Deserializer<'de>>(name: &str) -> Result<AddressSpace, D::Error> {
    AddressSpace::from_dsl_name(name)
        .ok_or_else(|| D::Error::custom(format!("unknown address space {name}")))
}

/// Deserialize the tuple carried inside an address newtype token, tolerating
/// serializers that ignore the token and pass a plain sequence.
fn deserialize_token<'de, T: Deserialize<'de>, D: Deserializer<'de>>(
    deserializer: D,
    token: &'static str,
) -> Result<T, D::Error> {
    struct TokenVisitor<T>(PhantomData<T>);

    impl<'de, T: Deserialize<'de>> Visitor<'de> for TokenVisitor<T> {
        type Value = T;
        fn expecting(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str("a DSL address")
        }
        fn visit_newtype_struct<D: Deserializer<'de>>(self, d: D) -> Result<T, D::Error> {
            T::deserialize(d)
        }
        fn visit_seq<A: SeqAccess<'de>>(self, seq: A) -> Result<T, A::Error> {
            T::deserialize(SeqAccessDeserializer::new(seq))
        }
    }

    deserializer.deserialize_newtype_struct(token, TokenVisitor(PhantomData))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct AddressRange {
    pub start: AddressValue,
    pub end: AddressValue,
}

impl AddressRange {
    pub fn new(start_inclusive: AddressValue, end_exclusive: AddressValue) -> Self {
        Self {
            start: start_inclusive,
            end: end_exclusive,
        }
    }
}

impl<T: RangeBounds<AddressValue>> From<T> for AddressRange {
    fn from(range: T) -> Self {
        let range_start_inclusive = match range.start_bound() {
            Bound::Included(addr) => *addr,
            Bound::Excluded(addr) => addr.saturating_add(1),
            Bound::Unbounded => 0,
        };
        let range_end_inclusive = match range.end_bound() {
            Bound::Included(addr) => *addr,
            Bound::Excluded(addr) => addr.saturating_sub(1),
            Bound::Unbounded => AddressValue::MAX,
        };
        Self {
            start: range_start_inclusive,
            end: range_end_inclusive,
        }
    }
}

/// Explicit emission order for sdas area headers.
pub const AREA_ORDER: [AddressSpace; 5] = [
    AddressSpace::Code,
    AddressSpace::Idata,
    AddressSpace::Sfr,
    AddressSpace::Bit,
    AddressSpace::Xdata,
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum AddressSpace {
    Code,
    Idata,
    Sfr,
    Bit,
    Xdata,
}

impl AddressSpace {
    /// The DSL spelling of this space (`CODE`, `IDATA`, ...).
    pub fn dsl_name(self) -> &'static str {
        match self {
            Self::Code => "CODE",
            Self::Idata => "IDATA",
            Self::Sfr => "SFR",
            Self::Bit => "BIT",
            Self::Xdata => "XDATA",
        }
    }

    /// Parse a DSL space spelling, the inverse of [`dsl_name`](Self::dsl_name).
    pub fn from_dsl_name(name: &str) -> Option<Self> {
        Some(match name {
            "CODE" => Self::Code,
            "IDATA" => Self::Idata,
            "SFR" => Self::Sfr,
            "BIT" => Self::Bit,
            "XDATA" => Self::Xdata,
            _ => return None,
        })
    }

    pub fn area_header(self) -> &'static str {
        match self {
            Self::Code => ".area CODE (CODE,ABS)\n",
            Self::Idata => ".area IDATA (IDATA,ABS)\n",
            Self::Sfr => ".area SFR (SFR,ABS)\n",
            Self::Bit => ".area BIT (BIT,ABS)\n",
            Self::Xdata => ".area XDATA (XDATA,ABS)\n",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct PhysicalAddr {
    pub space: AddressSpace,
    pub offset: AddressValue,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Xref {
    pub from: PhysicalAddr,
    pub to: PhysicalAddr,
    pub xref_type: XrefType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum XrefType {
    Call,
    Jump,
    Data,
}

pub fn branch_target_operand_index(insn: &Instruction) -> Option<usize> {
    branch_target(insn)?;
    match insn.mnemonic() {
        Mnemonic::LJMP | Mnemonic::LCALL | Mnemonic::AJMP | Mnemonic::ACALL | Mnemonic::SJMP => {
            Some(0)
        }
        _ => {
            let decoded = insn.as_string();
            let operand_count = decoded.split_once(' ').map_or(0, |(_, rest)| {
                if rest.is_empty() {
                    0
                } else {
                    rest.split(',').count()
                }
            });
            if operand_count == 0 {
                None
            } else {
                Some(operand_count - 1)
            }
        }
    }
}

pub fn branch_target(insn: &Instruction) -> Option<u32> {
    match insn.control_flow() {
        ControlFlow::Jump { target } => Some(target),
        ControlFlow::Call { target, .. } => Some(target),
        ControlFlow::Choice { branch_target, .. } => Some(branch_target),
        _ => None,
    }
}

pub fn xrefs_from_instruction(instruction: &Instruction, source: PhysicalAddr) -> Vec<Xref> {
    let mut xrefs = Vec::new();
    let mut push = |to_offset: u32, xref_type: XrefType| {
        xrefs.push(Xref {
            from: source,
            to: PhysicalAddr {
                space: source.space,
                offset: to_offset,
            },
            xref_type,
        });
    };

    match instruction.control_flow() {
        ControlFlow::Jump { target } => {
            push(target, xref_type_for(instruction.mnemonic()));
        }
        ControlFlow::Call { target, .. } => push(target, XrefType::Call),
        ControlFlow::Choice { branch_target, .. } => push(branch_target, XrefType::Jump),
        _ => {}
    }

    xrefs
}

pub fn xrefs_to_target(
    instruction: &Instruction,
    source: PhysicalAddr,
    target: &PhysicalAddr,
) -> Vec<Xref> {
    xrefs_from_instruction(instruction, source)
        .into_iter()
        .filter(|xref| xref.to == *target)
        .collect()
}

fn xref_type_for(opcode: Mnemonic) -> XrefType {
    match opcode {
        Mnemonic::LCALL | Mnemonic::ACALL => XrefType::Call,
        _ => XrefType::Jump,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // The `$dsl::address` token is advisory: a non-DSL serializer ignores the
    // newtype name and (de)serializes the inner `(space, offset)` tuple. So the
    // address types still round-trip through stock serde formats like JSON.
    #[test]
    fn space_address_value_json_round_trip() {
        let addr = SpaceAddressValue {
            space: AddressSpace::Xdata,
            offset: 0x1234,
        };
        let json = serde_json::to_string(&addr).unwrap();
        assert_eq!(json, r#"["XDATA",4660]"#);
        assert_eq!(
            serde_json::from_str::<SpaceAddressValue>(&json).unwrap(),
            addr
        );
    }

    #[test]
    fn space_address_range_json_round_trip() {
        let range = SpaceAddressRange {
            space: AddressSpace::Code,
            range: AddressRange::new(0x10, 0x20),
        };
        let json = serde_json::to_string(&range).unwrap();
        assert_eq!(json, r#"["CODE",16,32]"#);
        assert_eq!(
            serde_json::from_str::<SpaceAddressRange>(&json).unwrap(),
            range
        );
    }

    #[test]
    fn unknown_space_is_rejected() {
        let err = serde_json::from_str::<SpaceAddressValue>(r#"["NOPE",0]"#).unwrap_err();
        assert!(err.to_string().contains("unknown address space"));
    }

    #[test]
    fn space_address_set_optimal_and_round_trips() {
        let mut set = SpaceAddressSet::new(AddressSpace::Code);
        for addr in [0x10, 0x11, 0x12, 0x30] {
            set.insert_address(addr);
        }
        set.insert(0x20..0x28);

        // Adjacent addresses coalesce into a range; singletons stay bare.
        let value = crate::store::ser::to_value(&set).unwrap();
        assert_eq!(value.render(), "CODE:{0x10..0x13, 0x20..0x28, 0x30}");

        // Round-trips through the DSL Value AST...
        assert_eq!(
            crate::store::de::from_value::<SpaceAddressSet>(value).unwrap(),
            set
        );
        // ...and through stock serde (JSON: `[space, [[start, end], ...]]`).
        let json = serde_json::to_string(&set).unwrap();
        assert_eq!(json, r#"["CODE",[[16,19],[32,40],[48,49]]]"#);
        assert_eq!(serde_json::from_str::<SpaceAddressSet>(&json).unwrap(), set);
    }
}
