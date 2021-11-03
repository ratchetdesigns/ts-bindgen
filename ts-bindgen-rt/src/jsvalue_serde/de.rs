use crate::jsvalue_serde::error::{Error, ExpectValue, Result};
use serde::{de, Deserialize};
use std::borrow::{Borrow, Cow};
use wasm_bindgen::{convert::IntoWasmAbi, JsCast, JsValue};

pub fn from_jsvalue<'de, T>(jsv: &'de JsValue) -> Result<T>
where
    T: Deserialize<'de>,
{
    let mut deserializer = Deserializer::from_jsvalue(jsv);
    T::deserialize(&mut deserializer)
}

pub struct Deserializer<'de> {
    input: Cow<'de, JsValue>,
}

impl<'de> Deserializer<'de> {
    pub fn from_jsvalue(input: &'de JsValue) -> Self {
        Self {
            input: Cow::Borrowed(input),
        }
    }

    pub fn from_owned_jsvalue(input: JsValue) -> Self {
        Self {
            input: Cow::Owned(input),
        }
    }
}

macro_rules! basic_deserialize {
    ($name:ident, $visit:ident, $type:ident, $converter:ident) => {
        fn $name<V>(self, visitor: V) -> Result<V::Value>
        where
            V: de::Visitor<'de>,
        {
            let value = self.input.$converter().expect_value(stringify!($type))?;
            visitor.$visit(value)
        }
    };
    ($name:ident, $visit:ident, $type:ident, $converter:ident as $target_type:ty) => {
        fn $name<V>(self, visitor: V) -> Result<V::Value>
        where
            V: de::Visitor<'de>,
        {
            let value = self
                .input
                .$converter()
                .map(|x| x as $target_type)
                .expect_value(stringify!($type))?;
            visitor.$visit(value)
        }
    };
}

macro_rules! error_deserialize {
    ($name:ident) => {
        fn $name<V>(self, _visitor: V) -> Result<V::Value>
        where
            V: de::Visitor<'de>,
        {
            Err(Error {})
        }
    };
}

impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

    basic_deserialize!(deserialize_bool, visit_bool, bool, as_bool);

    basic_deserialize!(deserialize_i8, visit_i8, i8, as_f64 as i8);
    basic_deserialize!(deserialize_i16, visit_i16, i16, as_f64 as i16);
    basic_deserialize!(deserialize_i32, visit_i32, i32, as_f64 as i32);
    basic_deserialize!(deserialize_i64, visit_i64, i64, as_f64 as i64);

    basic_deserialize!(deserialize_u8, visit_u8, u8, as_f64 as u8);
    basic_deserialize!(deserialize_u16, visit_u16, u16, as_f64 as u16);
    basic_deserialize!(deserialize_u32, visit_u32, u32, as_f64 as u32);
    basic_deserialize!(deserialize_u64, visit_u64, u64, as_f64 as u64);

    basic_deserialize!(deserialize_f32, visit_f32, f32, as_f64 as f32);
    basic_deserialize!(deserialize_f64, visit_f64, f64, as_f64);

    basic_deserialize!(deserialize_string, visit_string, string, as_string);

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let value = self
            .input
            .as_string()
            .and_then(|x| x.chars().next())
            .expect_value("char")?;
        visitor.visit_char(value)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_string(visitor)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_byte_buf(visitor)
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if self.input.is_null() || self.input.is_undefined() {
            visitor.visit_none()
        } else {
            visitor.visit_some(self)
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if self.input.is_null() {
            visitor.visit_unit()
        } else if self.input.is_undefined() {
            visit_undefined(visitor)
        } else {
            Err(Error {})
        }
    }

    fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_seq(SeqDeserializer::new_from_cow(self.input.clone())?)
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let obj: &js_sys::Object = self.input.dyn_ref().expect_value("object")?;
        let entries = js_sys::Object::entries(obj);
        visitor.visit_map(SeqDeserializer::new_from_cow(Cow::Borrowed(
            entries.as_ref(),
        ))?)
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_enum(EnumDeserializer::new(self))
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_string(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        drop(self);
        visitor.visit_unit()
    }

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        if self.input.is_null() {
            visitor.visit_unit()
        } else if self.input.is_undefined() {
            visit_undefined(visitor)
        } else if let Some(v) = self.input.as_bool() {
            visitor.visit_bool(v)
        } else if let Some(v) = self.input.as_string() {
            visitor.visit_string(v)
        } else if let Some(v) = self.input.as_f64() {
            if js_sys::Number::is_safe_integer(self.input.borrow()) {
                visitor.visit_i64(v as i64)
            } else {
                visitor.visit_f64(v)
            }
        } else if js_sys::Array::is_array(self.input.borrow()) {
            self.deserialize_seq(visitor)
        } else if self.input.is_object() {
            if js_sys::Object::is(
                &js_sys::Object::get_prototype_of(self.input.borrow()),
                &js_sys::Object::get_prototype_of(&js_sys::Object::new()),
            ) {
                self.deserialize_map(visitor)
            } else {
                let input: &JsValue = self.input.borrow();
                visitor.visit_u32(input.clone().into_abi())
            }
        } else if self.input.is_function() {
            visitor.visit_unit() // TODO
        } else {
            Err(Error {})
        }
    }
}

struct EnumDeserializer<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
}

impl<'a, 'de> EnumDeserializer<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        EnumDeserializer { de }
    }
}

impl<'a, 'de> de::EnumAccess<'de> for EnumDeserializer<'a, 'de> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant)>
    where
        V: de::DeserializeSeed<'de>,
    {
        let val = seed.deserialize(&mut *self.de)?;
        Ok((val, self))
    }
}

impl<'de, 'a> de::VariantAccess<'de> for EnumDeserializer<'a, 'de> {
    type Error = Error;

    fn unit_variant(self) -> Result<()> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
    where
        T: de::DeserializeSeed<'de>,
    {
        seed.deserialize(self.de)
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_seq(self.de, visitor)
    }

    fn struct_variant<V>(self, _fields: &'static [&'static str], visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_map(self.de, visitor)
    }
}

struct SeqDeserializer<'a> {
    arr: Cow<'a, js_sys::Array>,
    idx: u32,
    len: u32,
}

impl<'a> SeqDeserializer<'a> {
    fn new_from_cow(input: Cow<'a, JsValue>) -> Result<Self> {
        let arr: &js_sys::Array = input.dyn_ref().expect_value("array")?;
        Ok(Self {
            arr: Cow::Owned(arr.clone()),
            idx: 0,
            len: arr.length(),
        })
    }
}

impl<'de, 'a: 'de> de::SeqAccess<'de> for SeqDeserializer<'a> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: de::DeserializeSeed<'de>,
    {
        if self.idx >= self.len {
            Ok(None)
        } else {
            let item = self.arr.get(self.idx);
            let res = seed
                .deserialize(&mut Deserializer::from_owned_jsvalue(item))
                .map(Some);
            self.idx += 1;
            res
        }
    }
}

impl<'de, 'a> de::MapAccess<'de> for SeqDeserializer<'a> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: de::DeserializeSeed<'de>,
    {
        if self.idx >= self.len {
            Ok(None)
        } else {
            let entry: JsValue = self.arr.get(self.idx);
            let entry: &js_sys::Array = entry.dyn_ref().expect_value("array")?;
            seed.deserialize(&mut Deserializer::from_owned_jsvalue(entry.get(0)))
                .map(Some)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: de::DeserializeSeed<'de>,
    {
        let entry: JsValue = self.arr.get(self.idx);
        let entry: &js_sys::Array = entry.dyn_ref().expect_value("array")?;
        let res = seed.deserialize(&mut Deserializer::from_owned_jsvalue(entry.get(1)));
        self.idx += 1;
        res
    }
}

fn visit_undefined<'de, V>(visitor: V) -> Result<V::Value>
where
    V: de::Visitor<'de>,
{
    visitor.visit_newtype_struct(&mut UndefinedDeserializer)
}

struct UndefinedDeserializer;

impl<'de, 'a> de::Deserializer<'de> for &'a mut UndefinedDeserializer {
    type Error = Error;

    error_deserialize!(deserialize_bool);

    error_deserialize!(deserialize_i8);
    error_deserialize!(deserialize_i16);
    error_deserialize!(deserialize_i32);
    error_deserialize!(deserialize_i64);

    error_deserialize!(deserialize_u8);
    error_deserialize!(deserialize_u16);
    error_deserialize!(deserialize_u32);
    error_deserialize!(deserialize_u64);

    error_deserialize!(deserialize_f32);
    error_deserialize!(deserialize_f64);

    error_deserialize!(deserialize_string);

    error_deserialize!(deserialize_char);

    error_deserialize!(deserialize_str);

    error_deserialize!(deserialize_bytes);

    error_deserialize!(deserialize_byte_buf);

    error_deserialize!(deserialize_option);

    fn deserialize_unit_struct<V>(self, _name: &'static str, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        Err(Error {})
    }

    fn deserialize_newtype_struct<V>(self, _name: &'static str, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        Err(Error {})
    }

    error_deserialize!(deserialize_seq);

    fn deserialize_tuple<V>(self, _len: usize, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        Err(Error {})
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        _visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        Err(Error {})
    }

    error_deserialize!(deserialize_map);

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        Err(Error {})
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        Err(Error {})
    }

    error_deserialize!(deserialize_identifier);

    error_deserialize!(deserialize_ignored_any);

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_none()
    }
}
