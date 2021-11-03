use crate::jsvalue_serde::error::{Error, Result};
use serde::{ser, Serialize};
use wasm_bindgen::{convert::FromWasmAbi, JsValue};

/// Marker name to indicate that we want to serialize a WasmAbi (u32)
/// into a JsValue.
pub const JSVALUE_NEWTYPE_STRUCT: &str = "__tsb__JsValue";
///
/// Marker name to indicate that we want to serialize into an undefined
/// JsValue.
pub const UNDEFINED_UNIT_STRUCT: &str = "__tsb__undefined";

pub fn to_jsvalue<T: Serialize + ?Sized>(val: &T) -> Result<JsValue> {
    let mut s = Serializer::new();
    val.serialize(&mut s)
}

pub struct Serializer;

impl Serializer {
    fn new() -> Self {
        Self
    }
}

macro_rules! def_serializer {
    ($name:ident, $typ:ty) => {
        fn $name(self, v: $typ) -> Result<Self::Ok> {
            Ok(v.into())
        }
    };
}

impl<'a> ser::Serializer for &'a mut Serializer {
    type Ok = JsValue;
    type Error = Error;
    type SerializeSeq = SeqSerializer;
    type SerializeTuple = Self::SerializeSeq;
    type SerializeTupleStruct = Self::SerializeSeq;
    type SerializeTupleVariant = Self::SerializeSeq;
    type SerializeMap = MapSerializer;
    type SerializeStruct = Self::SerializeMap;
    type SerializeStructVariant = Self::SerializeMap;

    def_serializer!(serialize_bool, bool);

    def_serializer!(serialize_i8, i8);
    def_serializer!(serialize_i16, i16);
    def_serializer!(serialize_i32, i32);
    def_serializer!(serialize_i64, i64);

    def_serializer!(serialize_u8, u8);
    def_serializer!(serialize_u16, u16);
    def_serializer!(serialize_u32, u32);
    def_serializer!(serialize_u64, u64);

    def_serializer!(serialize_f32, f32);
    def_serializer!(serialize_f64, f64);

    fn serialize_char(self, v: char) -> Result<Self::Ok> {
        self.serialize_str(&v.to_string())
    }

    def_serializer!(serialize_str, &str);

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok> {
        let arr = js_sys::Array::new_with_length(v.len() as u32);
        for (i, val) in v.iter().enumerate() {
            arr.set(i as u32, (*val).into());
        }
        let jsv: &JsValue = arr.as_ref();
        Ok(jsv.clone())
    }

    fn serialize_none(self) -> Result<Self::Ok> {
        self.serialize_unit()
    }

    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(&mut *self)
    }

    fn serialize_unit(self) -> Result<Self::Ok> {
        Ok(JsValue::null())
    }

    fn serialize_unit_struct(self, name: &'static str) -> Result<Self::Ok> {
        if name == UNDEFINED_UNIT_STRUCT {
            Ok(JsValue::undefined())
        } else {
            self.serialize_unit()
        }
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok> {
        self.serialize_str(variant)
    }

    fn serialize_newtype_struct<T>(self, name: &'static str, value: &T) -> Result<Self::Ok>
    where
        T: ?Sized + Serialize,
    {
        let serialized = value.serialize(&mut *self)?;

        if name == JSVALUE_NEWTYPE_STRUCT {
            let idx = serialized
                .as_f64()
                .expect("__tsb__JsValue always provides a u32") as u32;
            Ok(unsafe { JsValue::from_abi(idx) })
        } else {
            Ok(serialized)
        }
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(&mut *self)
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq> {
        Ok(SeqSerializer::new(len))
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        self.serialize_seq(Some(len))
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        Ok(MapSerializer::new())
    }

    fn serialize_struct(self, _name: &'static str, len: usize) -> Result<Self::SerializeStruct> {
        self.serialize_map(Some(len))
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        Ok(MapSerializer::new())
    }
}

pub struct SeqSerializer {
    out: js_sys::Array,
}

impl SeqSerializer {
    fn new(_len: Option<usize>) -> Self {
        Self {
            out: js_sys::Array::new(),
        }
    }
}

impl ser::SerializeSeq for SeqSerializer {
    type Ok = JsValue;
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.out.push(&to_jsvalue(value)?);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok> {
        let jsv: &JsValue = self.out.as_ref();
        Ok(jsv.clone())
    }
}

impl ser::SerializeTuple for SeqSerializer {
    type Ok = JsValue;
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok> {
        ser::SerializeSeq::end(self)
    }
}

impl ser::SerializeTupleStruct for SeqSerializer {
    type Ok = JsValue;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok> {
        ser::SerializeSeq::end(self)
    }
}

impl ser::SerializeTupleVariant for SeqSerializer {
    type Ok = JsValue;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok> {
        ser::SerializeSeq::end(self)
    }
}

pub struct MapSerializer {
    out: js_sys::Object,
    next_key: Option<JsValue>,
}

impl MapSerializer {
    fn new() -> Self {
        Self {
            out: js_sys::Object::new(),
            next_key: None,
        }
    }
}

impl ser::SerializeMap for MapSerializer {
    type Ok = JsValue;
    type Error = Error;

    fn serialize_key<T>(&mut self, key: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.next_key = Some(to_jsvalue(key)?);
        Ok(())
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        let key = self
            .next_key
            .take()
            .expect("serialize_value called without corresponding serialize_key");
        let value = to_jsvalue(value)?;
        let jsv: &JsValue = self.out.as_ref();
        js_sys::Reflect::set(jsv, &key, &value)?;
        Ok(())
    }

    fn end(self) -> Result<JsValue> {
        let jsv: &JsValue = self.out.as_ref();
        Ok(jsv.clone())
    }
}

impl ser::SerializeStruct for MapSerializer {
    type Ok = JsValue;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        ser::SerializeMap::serialize_key(self, key)?;
        ser::SerializeMap::serialize_value(self, value)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok> {
        ser::SerializeMap::end(self)
    }
}

impl ser::SerializeStructVariant for MapSerializer {
    type Ok = JsValue;
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        ser::SerializeMap::serialize_key(self, key)?;
        ser::SerializeMap::serialize_value(self, value)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok> {
        ser::SerializeMap::end(self)
    }
}
