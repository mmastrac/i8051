//! The handful of `#[serde(with = ...)]` adapters for shapes the DSL spells
//! specially. These are genuine domain rules — address syntax and note-id
//! allocation — not per-field serialization plumbing.

use std::collections::{BTreeMap, BTreeSet};

use serde::de::Error as DeError;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::address::{
    AddressRange, AddressSpace, AddressValue, SpaceAddressRange, SpaceAddressValue,
};
use crate::note::{Note, NoteField, NoteId};

/// Magic newtype-struct names the generic [`ser`](crate::store::ser) /
/// [`de`](crate::store::de) recognise to emit DSL address syntax
/// (`CODE:0x10`, `CODE:0x10..0x20`) instead of a plain tuple.
pub const ADDRESS_TOKEN: &str = "$dsl::address";
pub const ADDRESS_RANGE_TOKEN: &str = "$dsl::address_range";

pub mod space_address {
    use super::*;

    pub fn serialize<S: Serializer>(value: &SpaceAddressValue, s: S) -> Result<S::Ok, S::Error> {
        let (space, offset) = *value;
        s.serialize_newtype_struct(ADDRESS_TOKEN, &(encode_space(space), offset as u64))
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<SpaceAddressValue, D::Error> {
        let (space, offset) = <(String, u64)>::deserialize_in(d, ADDRESS_TOKEN)?;
        Ok((decode_space::<D>(&space)?, offset as AddressValue))
    }
}

pub mod space_address_range {
    use super::*;

    pub fn serialize<S: Serializer>(value: &SpaceAddressRange, s: S) -> Result<S::Ok, S::Error> {
        let (space, range) = *value;
        s.serialize_newtype_struct(
            ADDRESS_RANGE_TOKEN,
            &(encode_space(space), range.start as u64, range.end as u64),
        )
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<SpaceAddressRange, D::Error> {
        let (space, start, end) = <(String, u64, u64)>::deserialize_in(d, ADDRESS_RANGE_TOKEN)?;
        Ok((
            decode_space::<D>(&space)?,
            AddressRange::new(start as AddressValue, end as AddressValue),
        ))
    }
}

/// A [`Note`] whose `id` may be omitted in hand-written DSL, in which case a
/// fresh id is allocated from the content.
pub mod note {
    use super::*;

    #[derive(Deserialize)]
    struct Repr {
        #[serde(default)]
        id: Option<NoteId>,
        content: String,
        #[serde(default)]
        tags: BTreeSet<String>,
        #[serde(default)]
        fields: BTreeMap<String, NoteField>,
        #[serde(default)]
        links: BTreeSet<NoteId>,
    }

    pub fn serialize<S: Serializer>(note: &Note, s: S) -> Result<S::Ok, S::Error> {
        note.serialize(s)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<Note, D::Error> {
        let repr = Repr::deserialize(d)?;
        let mut note = match repr.id {
            Some(id) => Note {
                id,
                content: repr.content,
                tags: BTreeSet::new(),
                fields: BTreeMap::new(),
                links: BTreeSet::new(),
            },
            None => Note::new(None, repr.content),
        };
        note.tags = repr.tags;
        note.fields = repr.fields;
        note.links = repr.links;
        Ok(note)
    }
}

/// Helper: deserialize the tuple carried inside one of our address newtype
/// tokens.
trait DeserializeIn<'de>: Sized {
    fn deserialize_in<D: Deserializer<'de>>(d: D, token: &'static str) -> Result<Self, D::Error>;
}

impl<'de, T: Deserialize<'de>> DeserializeIn<'de> for T {
    fn deserialize_in<D: Deserializer<'de>>(d: D, token: &'static str) -> Result<T, D::Error> {
        struct NewtypeVisitor<T>(std::marker::PhantomData<T>);

        impl<'de, T: Deserialize<'de>> serde::de::Visitor<'de> for NewtypeVisitor<T> {
            type Value = T;
            fn expecting(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str("a DSL address")
            }
            fn visit_newtype_struct<D: Deserializer<'de>>(self, d: D) -> Result<T, D::Error> {
                T::deserialize(d)
            }
            // Fallback for serializers that don't honour the newtype token.
            fn visit_seq<A: serde::de::SeqAccess<'de>>(self, seq: A) -> Result<T, A::Error> {
                T::deserialize(serde::de::value::SeqAccessDeserializer::new(seq))
            }
        }

        d.deserialize_newtype_struct(token, NewtypeVisitor(std::marker::PhantomData))
    }
}

fn encode_space(space: AddressSpace) -> &'static str {
    match space {
        AddressSpace::Code => "CODE",
        AddressSpace::Idata => "IDATA",
        AddressSpace::Sfr => "SFR",
        AddressSpace::Bit => "BIT",
        AddressSpace::Xdata => "XDATA",
    }
}

fn decode_space<'de, D: Deserializer<'de>>(name: &str) -> Result<AddressSpace, D::Error> {
    match name {
        "CODE" => Ok(AddressSpace::Code),
        "IDATA" => Ok(AddressSpace::Idata),
        "SFR" => Ok(AddressSpace::Sfr),
        "BIT" => Ok(AddressSpace::Bit),
        "XDATA" => Ok(AddressSpace::Xdata),
        other => Err(D::Error::custom(format!("unknown address space {other}"))),
    }
}
