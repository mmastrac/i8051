use std::marker::PhantomData;
use std::ops::{Bound, Deref, Range, RangeBounds};

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

impl From<(AddressSpace, Range<AddressValue>)> for SpaceAddressRange {
    fn from((space, range): (AddressSpace, Range<AddressValue>)) -> Self {
        Self {
            space,
            range: AddressRange::new(range.start, range.end),
        }
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

impl From<(AddressSpace, AddressValue)> for SpaceAddressSet {
    fn from((space, offset): (AddressSpace, AddressValue)) -> Self {
        let mut set = Self::new(space);
        set.insert_address(offset);
        set
    }
}

impl From<(AddressSpace, Range<AddressValue>)> for SpaceAddressSet {
    fn from((space, range): (AddressSpace, Range<AddressValue>)) -> Self {
        let mut set = Self::new(space);
        set.insert(range);
        set
    }
}

impl From<(AddressSpace, AddressRange)> for SpaceAddressSet {
    fn from((space, range): (AddressSpace, AddressRange)) -> Self {
        let mut set = Self::new(space);
        set.insert(range.start..range.end);
        set
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
        .ok_or_else(|| D::Error::custom(format!("address space name too long: {name:?}")))
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
        let start = match range.start_bound() {
            Bound::Included(addr) => *addr,
            Bound::Excluded(addr) => addr.saturating_add(1),
            Bound::Unbounded => 0,
        };
        let end = match range.end_bound() {
            Bound::Included(addr) => addr.saturating_add(1),
            Bound::Excluded(addr) => *addr,
            Bound::Unbounded => AddressValue::MAX,
        };
        Self { start, end }
    }
}

/// A named address space. A processor driver declares which spaces exist (see
/// [`Platform::regions`](crate::platform::Platform::regions)). A space is just
/// its name, so a caller can introduce its own without touching this crate.
///
/// The name is stored inline (up to [`CAP`](Self::CAP) bytes), keeping the type
/// `Copy` and cheap as a map key. The empty name is the default space.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AddressSpace {
    /// NUL-padded ASCII; a leading NUL means the default space.
    name: [u8; Self::CAP],
}

impl AddressSpace {
    /// Maximum space-name length in bytes.
    pub const CAP: usize = 16;

    /// The default (unnamed) space.
    pub const DEFAULT: Self = Self {
        name: [0; Self::CAP],
    };

    /// Construct a space from its name. Panics if longer than [`CAP`](Self::CAP).
    pub const fn new(name: &str) -> Self {
        let bytes = name.as_bytes();
        assert!(bytes.len() <= Self::CAP, "address space name too long");
        let mut buf = [0u8; Self::CAP];
        let mut i = 0;
        while i < bytes.len() {
            buf[i] = bytes[i];
            i += 1;
        }
        Self { name: buf }
    }

    /// The space name (empty for the default space).
    pub fn as_str(&self) -> &str {
        let len = self.name.iter().position(|&b| b == 0).unwrap_or(Self::CAP);
        std::str::from_utf8(&self.name[..len]).unwrap_or("")
    }

    /// Whether this is the default (unnamed) space.
    pub fn is_default(&self) -> bool {
        self.name[0] == 0
    }

    /// The DSL spelling of this space.
    pub fn dsl_name(&self) -> &str {
        self.as_str()
    }

    /// Parse a DSL space spelling. Any name up to [`CAP`](Self::CAP) bytes is
    /// valid — regions are driver-defined, so there is no fixed vocabulary.
    pub fn from_dsl_name(name: &str) -> Option<Self> {
        (name.len() <= Self::CAP).then(|| Self::new(name))
    }
}

impl std::fmt::Debug for AddressSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AddressSpace({:?})", self.as_str())
    }
}

impl Serialize for AddressSpace {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.as_str())
    }
}

impl<'de> Deserialize<'de> for AddressSpace {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let name = String::deserialize(deserializer)?;
        AddressSpace::from_dsl_name(&name)
            .ok_or_else(|| D::Error::custom(format!("address space name too long: {name:?}")))
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
    /// Control flow: `LCALL`/`ACALL`.
    Call,
    /// Control flow: an unconditional or conditional jump/branch.
    Jump,
    /// Data: reads the target byte or bit.
    Read,
    /// Data: writes the target byte or bit.
    Write,
    /// Data: read-modify-write (e.g. `INC direct`, `ANL direct,A`, `CPL bit`).
    ReadWrite,
    /// Data: materializes the target as an address (`MOV DPTR,#addr`), not a
    /// dereference.
    Pointer,
}

#[cfg(test)]
mod tests {
    use super::*;

    const CODE: AddressSpace = AddressSpace::new("CODE");
    const XDATA: AddressSpace = AddressSpace::new("XDATA");

    // The `$dsl::address` token is advisory: a non-DSL serializer ignores the
    // newtype name and (de)serializes the inner `(space, offset)` tuple. So the
    // address types still round-trip through stock serde formats like JSON.
    #[test]
    fn space_address_value_json_round_trip() {
        let addr = SpaceAddressValue {
            space: XDATA,
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
            space: CODE,
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
    fn space_names_are_free_form() {
        // Regions are driver-defined, so any name up to `CAP` bytes is a valid
        // space and round-trips verbatim.
        let addr: SpaceAddressValue = serde_json::from_str(r#"["MYSPACE",0]"#).unwrap();
        assert_eq!(addr.space.dsl_name(), "MYSPACE");
        assert_eq!(serde_json::to_string(&addr).unwrap(), r#"["MYSPACE",0]"#);

        // A name longer than `CAP` bytes is rejected.
        let long = "X".repeat(AddressSpace::CAP + 1);
        let err = serde_json::from_str::<SpaceAddressValue>(&format!(r#"["{long}",0]"#)).unwrap_err();
        assert!(err.to_string().contains("too long"), "{err}");
    }

    #[test]
    fn ergonomic_from_conversions() {
        // `SpaceAddressRange` from a half-open std range (end is exclusive).
        let r: SpaceAddressRange = (CODE, 0x10..0x20).into();
        assert_eq!(r.space, CODE);
        assert_eq!(r.range, AddressRange::new(0x10, 0x20));

        // `SpaceAddressSet` from a singleton, a std range, and an `AddressRange`.
        let single: SpaceAddressSet = (CODE, 0x10).into();
        assert!(single.contains(0x10) && !single.contains(0x11));

        let range: SpaceAddressSet = (CODE, 0x10..0x13).into();
        assert!(range.contains(0x10) && range.contains(0x12) && !range.contains(0x13));

        let from_addr_range: SpaceAddressSet = (CODE, AddressRange::new(0x20, 0x22)).into();
        assert!(from_addr_range.contains(0x21) && !from_addr_range.contains(0x22));
    }

    #[test]
    fn space_address_set_optimal_and_round_trips() {
        let mut set = SpaceAddressSet::new(CODE);
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
