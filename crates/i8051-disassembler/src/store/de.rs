//! A generic `serde::Deserializer` that drives any [`Deserialize`] type from a
//! DSL [`Value`] AST. Like [`ser`](crate::store::ser), it contains no per-type
//! knowledge: enums, structs, sequences, and maps are reconstructed from the
//! `Value` shape alone.

use std::collections::BTreeMap;

use serde::de::{
    self, DeserializeOwned, DeserializeSeed, EnumAccess, IntoDeserializer, VariantAccess, Visitor,
};
use serde::forward_to_deserialize_any;

use crate::address::{ADDRESS_RANGE_TOKEN, ADDRESS_SET_TOKEN, ADDRESS_TOKEN};
use crate::store::error::DslError;
use crate::store::value::{EnumArgs, Value};

/// Reconstruct a value from the DSL AST.
pub fn from_value<T: DeserializeOwned>(value: Value) -> Result<T, DslError> {
    T::deserialize(ValueDeserializer { value })
}

pub struct ValueDeserializer {
    value: Value,
}

impl<'de> de::Deserializer<'de> for ValueDeserializer {
    type Error = DslError;

    fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, DslError> {
        match self.value {
            Value::Null => visitor.visit_unit(),
            Value::Bool(v) => visitor.visit_bool(v),
            Value::Int(v) => visitor.visit_u64(v),
            Value::String(v) => visitor.visit_string(v),
            Value::List(items) | Value::Set(items) => visitor.visit_seq(SeqAccess::new(items)),
            Value::Map(map) => visitor.visit_map(MapAccess::new(map)),
            Value::Struct { fields, .. } => visitor.visit_map(MapAccess::new(fields)),
            Value::Enum { .. } => visitor.visit_enum(EnumDeserializer { value: self.value }),
            Value::Address { space, offset } => {
                visitor.visit_string(format!("{space}:0x{offset:X}"))
            }
            Value::AddressRange { space, start, end } => {
                visitor.visit_string(format!("{space}:0x{start:X}..0x{end:X}"))
            }
            Value::AddressSet { space, ranges } => {
                let Value::List(tuple) = address_set_tuple(space, ranges) else {
                    unreachable!()
                };
                visitor.visit_seq(SeqAccess::new(tuple))
            }
            Value::Call { name, kwargs } => visitor.visit_map(MapAccess::new(
                std::iter::once((name, Value::Map(kwargs))).collect(),
            )),
        }
    }

    fn deserialize_option<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, DslError> {
        match self.value {
            Value::Null => visitor.visit_none(),
            _ => visitor.visit_some(self),
        }
    }

    fn deserialize_newtype_struct<V: Visitor<'de>>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, DslError> {
        match (name, self.value) {
            (ADDRESS_TOKEN, Value::Address { space, offset }) => {
                visitor.visit_newtype_struct(ValueDeserializer {
                    value: Value::List(vec![Value::String(space), Value::Int(offset)]),
                })
            }
            (ADDRESS_RANGE_TOKEN, Value::AddressRange { space, start, end }) => visitor
                .visit_newtype_struct(ValueDeserializer {
                    value: Value::List(vec![
                        Value::String(space),
                        Value::Int(start),
                        Value::Int(end),
                    ]),
                }),
            (ADDRESS_SET_TOKEN, Value::AddressSet { space, ranges }) => visitor
                .visit_newtype_struct(ValueDeserializer {
                    value: address_set_tuple(space, ranges),
                }),
            // Transparent for everything else.
            (_, value) => visitor.visit_newtype_struct(ValueDeserializer { value }),
        }
    }

    fn deserialize_enum<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, DslError> {
        visitor.visit_enum(EnumDeserializer { value: self.value })
    }

    fn deserialize_map<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, DslError> {
        match self.value {
            Value::Map(map) => visitor.visit_map(MapAccess::new(map)),
            Value::Struct { fields, .. } => visitor.visit_map(MapAccess::new(fields)),
            // `{}` is ambiguous between an empty map and an empty set; the
            // parser picks `Set`, so accept an empty sequence as an empty map.
            Value::Set(items) | Value::List(items) if items.is_empty() => {
                visitor.visit_map(MapAccess::new(BTreeMap::new()))
            }
            other => Err(DslError::new(0, format!("expected a map, got {other:?}"))),
        }
    }

    forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf unit unit_struct seq tuple tuple_struct struct
        identifier ignored_any
    }
}

/// Lower an [`Value::AddressSet`] to the `(space, [[start, end], ...])` tuple
/// the address-set adapter deserializes from.
fn address_set_tuple(space: String, ranges: Vec<(u64, u64)>) -> Value {
    let items = ranges
        .into_iter()
        .map(|(start, end)| Value::List(vec![Value::Int(start), Value::Int(end)]))
        .collect();
    Value::List(vec![Value::String(space), Value::List(items)])
}

struct SeqAccess {
    iter: std::vec::IntoIter<Value>,
}

impl SeqAccess {
    fn new(items: Vec<Value>) -> Self {
        Self {
            iter: items.into_iter(),
        }
    }
}

impl<'de> de::SeqAccess<'de> for SeqAccess {
    type Error = DslError;

    fn next_element_seed<T: DeserializeSeed<'de>>(
        &mut self,
        seed: T,
    ) -> Result<Option<T::Value>, DslError> {
        match self.iter.next() {
            Some(value) => seed.deserialize(ValueDeserializer { value }).map(Some),
            None => Ok(None),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.iter.len())
    }
}

struct MapAccess {
    iter: std::collections::btree_map::IntoIter<String, Value>,
    value: Option<Value>,
}

impl MapAccess {
    fn new(map: BTreeMap<String, Value>) -> Self {
        Self {
            iter: map.into_iter(),
            value: None,
        }
    }
}

impl<'de> de::MapAccess<'de> for MapAccess {
    type Error = DslError;

    fn next_key_seed<K: DeserializeSeed<'de>>(
        &mut self,
        seed: K,
    ) -> Result<Option<K::Value>, DslError> {
        match self.iter.next() {
            Some((key, value)) => {
                self.value = Some(value);
                seed.deserialize(key.into_deserializer()).map(Some)
            }
            None => Ok(None),
        }
    }

    fn next_value_seed<V: DeserializeSeed<'de>>(&mut self, seed: V) -> Result<V::Value, DslError> {
        let value = self
            .value
            .take()
            .ok_or_else(|| DslError::new(0, "map value without key"))?;
        seed.deserialize(ValueDeserializer { value })
    }
}

struct EnumDeserializer {
    value: Value,
}

impl<'de> EnumAccess<'de> for EnumDeserializer {
    type Error = DslError;
    type Variant = VariantDeserializer;

    fn variant_seed<V: DeserializeSeed<'de>>(
        self,
        seed: V,
    ) -> Result<(V::Value, VariantDeserializer), DslError> {
        let Value::Enum { variant, args, .. } = self.value else {
            return Err(DslError::new(0, "expected an enum variant"));
        };
        let variant = seed.deserialize(variant.into_deserializer())?;
        Ok((variant, VariantDeserializer { args }))
    }
}

struct VariantDeserializer {
    args: EnumArgs,
}

impl<'de> VariantAccess<'de> for VariantDeserializer {
    type Error = DslError;

    fn unit_variant(self) -> Result<(), DslError> {
        match self.args {
            EnumArgs::Unit => Ok(()),
            _ => Err(DslError::new(0, "expected a unit variant")),
        }
    }

    fn newtype_variant_seed<T: DeserializeSeed<'de>>(self, seed: T) -> Result<T::Value, DslError> {
        match self.args {
            EnumArgs::Positional(mut values) if values.len() == 1 => {
                seed.deserialize(ValueDeserializer {
                    value: values.remove(0),
                })
            }
            _ => Err(DslError::new(0, "expected a single-value variant")),
        }
    }

    fn tuple_variant<V: Visitor<'de>>(self, _len: usize, visitor: V) -> Result<V::Value, DslError> {
        match self.args {
            EnumArgs::Positional(values) => visitor.visit_seq(SeqAccess::new(values)),
            EnumArgs::Unit => visitor.visit_seq(SeqAccess::new(Vec::new())),
            EnumArgs::Named(_) => Err(DslError::new(0, "expected a tuple variant")),
        }
    }

    fn struct_variant<V: Visitor<'de>>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, DslError> {
        match self.args {
            EnumArgs::Named(fields) => visitor.visit_map(MapAccess::new(fields)),
            EnumArgs::Unit => visitor.visit_map(MapAccess::new(BTreeMap::new())),
            EnumArgs::Positional(_) => Err(DslError::new(0, "expected a struct variant")),
        }
    }
}
