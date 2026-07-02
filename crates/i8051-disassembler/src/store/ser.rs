//! A generic `serde::Serializer` that lowers any [`Serialize`] type into the
//! DSL [`Value`] AST. There is no per-type code here: enum type names, struct
//! names, sequences, and maps all map onto `Value` mechanically.

use std::collections::BTreeMap;

use serde::ser::{self, Serialize};

use crate::address::{ADDRESS_RANGE_TOKEN, ADDRESS_SET_TOKEN, ADDRESS_TOKEN};
use crate::store::error::DslError;
use crate::store::value::{EnumArgs, Value};

/// Lower a value into the DSL AST.
pub fn to_value<T: Serialize>(value: &T) -> Result<Value, DslError> {
    value.serialize(ValueSerializer)
}

pub struct ValueSerializer;

impl ser::Serializer for ValueSerializer {
    type Ok = Value;
    type Error = DslError;

    type SerializeSeq = SeqBuilder;
    type SerializeTuple = SeqBuilder;
    type SerializeTupleStruct = SeqBuilder;
    type SerializeTupleVariant = VariantSeqBuilder;
    type SerializeMap = MapBuilder;
    type SerializeStruct = StructBuilder;
    type SerializeStructVariant = VariantStructBuilder;

    fn serialize_bool(self, v: bool) -> Result<Value, DslError> {
        Ok(Value::Bool(v))
    }

    fn serialize_i8(self, v: i8) -> Result<Value, DslError> {
        Ok(Value::Int(v as u64))
    }
    fn serialize_i16(self, v: i16) -> Result<Value, DslError> {
        Ok(Value::Int(v as u64))
    }
    fn serialize_i32(self, v: i32) -> Result<Value, DslError> {
        Ok(Value::Int(v as u64))
    }
    fn serialize_i64(self, v: i64) -> Result<Value, DslError> {
        Ok(Value::Int(v as u64))
    }
    fn serialize_u8(self, v: u8) -> Result<Value, DslError> {
        Ok(Value::Int(v as u64))
    }
    fn serialize_u16(self, v: u16) -> Result<Value, DslError> {
        Ok(Value::Int(v as u64))
    }
    fn serialize_u32(self, v: u32) -> Result<Value, DslError> {
        Ok(Value::Int(v as u64))
    }
    fn serialize_u64(self, v: u64) -> Result<Value, DslError> {
        Ok(Value::Int(v))
    }
    fn serialize_f32(self, v: f32) -> Result<Value, DslError> {
        Ok(Value::Int(v as u64))
    }
    fn serialize_f64(self, v: f64) -> Result<Value, DslError> {
        Ok(Value::Int(v as u64))
    }

    fn serialize_char(self, v: char) -> Result<Value, DslError> {
        Ok(Value::String(v.to_string()))
    }
    fn serialize_str(self, v: &str) -> Result<Value, DslError> {
        Ok(Value::String(v.to_owned()))
    }
    fn serialize_bytes(self, v: &[u8]) -> Result<Value, DslError> {
        Ok(Value::List(
            v.iter().map(|b| Value::Int(*b as u64)).collect(),
        ))
    }

    fn serialize_none(self) -> Result<Value, DslError> {
        Ok(Value::Null)
    }
    fn serialize_some<T: ?Sized + Serialize>(self, value: &T) -> Result<Value, DslError> {
        value.serialize(self)
    }
    fn serialize_unit(self) -> Result<Value, DslError> {
        Ok(Value::Null)
    }
    fn serialize_unit_struct(self, _name: &'static str) -> Result<Value, DslError> {
        Ok(Value::Null)
    }

    fn serialize_unit_variant(
        self,
        name: &'static str,
        _index: u32,
        variant: &'static str,
    ) -> Result<Value, DslError> {
        Ok(Value::Enum {
            type_name: name.to_owned(),
            variant: variant.to_owned(),
            args: EnumArgs::Unit,
        })
    }

    fn serialize_newtype_struct<T: ?Sized + Serialize>(
        self,
        name: &'static str,
        value: &T,
    ) -> Result<Value, DslError> {
        // Address adapters signal out-of-band through a magic newtype name,
        // carrying their components as a positional tuple.
        match name {
            ADDRESS_TOKEN => {
                let [space, offset] = expect_parts(value.serialize(self)?)?;
                Ok(Value::Address {
                    space: as_space(space)?,
                    offset: as_int(offset)?,
                })
            }
            ADDRESS_RANGE_TOKEN => {
                let [space, start, end] = expect_parts(value.serialize(self)?)?;
                Ok(Value::AddressRange {
                    space: as_space(space)?,
                    start: as_int(start)?,
                    end: as_int(end)?,
                })
            }
            ADDRESS_SET_TOKEN => {
                let [space, ranges] = expect_parts(value.serialize(self)?)?;
                let Value::List(items) = ranges else {
                    return Err(DslError::new(
                        "address set adapter expected a list of ranges",
                    ));
                };
                let ranges = items
                    .into_iter()
                    .map(|item| {
                        let [start, end] = expect_parts(item)?;
                        Ok((as_int(start)?, as_int(end)?))
                    })
                    .collect::<Result<Vec<_>, DslError>>()?;
                Ok(Value::AddressSet {
                    space: as_space(space)?,
                    ranges,
                })
            }
            // Any other newtype struct is transparent (e.g. `#[serde(transparent)]`).
            _ => value.serialize(self),
        }
    }

    fn serialize_newtype_variant<T: ?Sized + Serialize>(
        self,
        name: &'static str,
        _index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Value, DslError> {
        Ok(Value::Enum {
            type_name: name.to_owned(),
            variant: variant.to_owned(),
            args: EnumArgs::Positional(vec![value.serialize(self)?]),
        })
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<SeqBuilder, DslError> {
        Ok(SeqBuilder::default())
    }
    fn serialize_tuple(self, _len: usize) -> Result<SeqBuilder, DslError> {
        Ok(SeqBuilder::default())
    }
    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<SeqBuilder, DslError> {
        Ok(SeqBuilder::default())
    }

    fn serialize_tuple_variant(
        self,
        name: &'static str,
        _index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<VariantSeqBuilder, DslError> {
        Ok(VariantSeqBuilder {
            type_name: name,
            variant,
            items: Vec::new(),
        })
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<MapBuilder, DslError> {
        Ok(MapBuilder {
            map: BTreeMap::new(),
            key: None,
        })
    }

    fn serialize_struct(self, name: &'static str, _len: usize) -> Result<StructBuilder, DslError> {
        Ok(StructBuilder {
            name,
            fields: BTreeMap::new(),
        })
    }

    fn serialize_struct_variant(
        self,
        name: &'static str,
        _index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<VariantStructBuilder, DslError> {
        Ok(VariantStructBuilder {
            type_name: name,
            variant,
            fields: BTreeMap::new(),
        })
    }
}

#[derive(Default)]
pub struct SeqBuilder {
    items: Vec<Value>,
}

impl ser::SerializeSeq for SeqBuilder {
    type Ok = Value;
    type Error = DslError;
    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), DslError> {
        self.items.push(value.serialize(ValueSerializer)?);
        Ok(())
    }
    fn end(self) -> Result<Value, DslError> {
        Ok(Value::List(self.items))
    }
}

impl ser::SerializeTuple for SeqBuilder {
    type Ok = Value;
    type Error = DslError;
    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), DslError> {
        self.items.push(value.serialize(ValueSerializer)?);
        Ok(())
    }
    fn end(self) -> Result<Value, DslError> {
        Ok(Value::List(self.items))
    }
}

impl ser::SerializeTupleStruct for SeqBuilder {
    type Ok = Value;
    type Error = DslError;
    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), DslError> {
        self.items.push(value.serialize(ValueSerializer)?);
        Ok(())
    }
    fn end(self) -> Result<Value, DslError> {
        Ok(Value::List(self.items))
    }
}

pub struct VariantSeqBuilder {
    type_name: &'static str,
    variant: &'static str,
    items: Vec<Value>,
}

impl ser::SerializeTupleVariant for VariantSeqBuilder {
    type Ok = Value;
    type Error = DslError;
    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), DslError> {
        self.items.push(value.serialize(ValueSerializer)?);
        Ok(())
    }
    fn end(self) -> Result<Value, DslError> {
        Ok(Value::Enum {
            type_name: self.type_name.to_owned(),
            variant: self.variant.to_owned(),
            args: EnumArgs::Positional(self.items),
        })
    }
}

pub struct MapBuilder {
    map: BTreeMap<String, Value>,
    key: Option<String>,
}

impl ser::SerializeMap for MapBuilder {
    type Ok = Value;
    type Error = DslError;
    fn serialize_key<T: ?Sized + Serialize>(&mut self, key: &T) -> Result<(), DslError> {
        self.key = Some(as_key(key.serialize(ValueSerializer)?)?);
        Ok(())
    }
    fn serialize_value<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), DslError> {
        let key = self
            .key
            .take()
            .ok_or_else(|| DslError::new("map value without key"))?;
        self.map.insert(key, value.serialize(ValueSerializer)?);
        Ok(())
    }
    fn end(self) -> Result<Value, DslError> {
        Ok(Value::Map(self.map))
    }
}

pub struct StructBuilder {
    name: &'static str,
    fields: BTreeMap<String, Value>,
}

impl ser::SerializeStruct for StructBuilder {
    type Ok = Value;
    type Error = DslError;
    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), DslError> {
        self.fields
            .insert(key.to_owned(), value.serialize(ValueSerializer)?);
        Ok(())
    }
    fn end(self) -> Result<Value, DslError> {
        Ok(Value::Struct {
            name: self.name.to_owned(),
            fields: self.fields,
        })
    }
}

pub struct VariantStructBuilder {
    type_name: &'static str,
    variant: &'static str,
    fields: BTreeMap<String, Value>,
}

impl ser::SerializeStructVariant for VariantStructBuilder {
    type Ok = Value;
    type Error = DslError;
    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), DslError> {
        self.fields
            .insert(key.to_owned(), value.serialize(ValueSerializer)?);
        Ok(())
    }
    fn end(self) -> Result<Value, DslError> {
        Ok(Value::Enum {
            type_name: self.type_name.to_owned(),
            variant: self.variant.to_owned(),
            args: EnumArgs::Named(self.fields),
        })
    }
}

fn expect_parts<const N: usize>(value: Value) -> Result<[Value; N], DslError> {
    let Value::List(items) = value else {
        return Err(DslError::new("address adapter expected a tuple"));
    };
    items
        .try_into()
        .map_err(|_| DslError::new("address adapter tuple has the wrong arity"))
}

fn as_space(value: Value) -> Result<String, DslError> {
    match value {
        Value::String(s) => Ok(s),
        other => Err(DslError::new(format!(
            "expected address space, got {other:?}"
        ))),
    }
}

fn as_int(value: Value) -> Result<u64, DslError> {
    value
        .as_u64()
        .ok_or_else(|| DslError::new(format!("expected integer, got {value:?}")))
}

fn as_key(value: Value) -> Result<String, DslError> {
    match value {
        Value::String(s) => Ok(s),
        Value::Int(v) => Ok(v.to_string()),
        Value::Bool(v) => Ok(v.to_string()),
        other => Err(DslError::new(format!("invalid map key {other:?}"))),
    }
}
