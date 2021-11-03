extern crate js_sys;
extern crate wasm_bindgen;

mod jsvalue_serde;

use jsvalue_serde::{from_jsvalue, to_jsvalue};
use serde::{de, ser, Deserialize, Serialize};
use std::fmt;
use wasm_bindgen::{
    convert::{FromWasmAbi, IntoWasmAbi},
    JsCast, JsValue,
};

pub struct Null;

pub struct Undefined;

#[derive(Debug)]
pub struct Error;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error")
    }
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        "Error"
    }
}

impl From<Error> for JsValue {
    fn from(_: Error) -> JsValue {
        js_sys::Error::new("rust failure").into()
    }
}

impl From<JsValue> for Error {
    fn from(_: JsValue) -> Error {
        Error
    }
}

impl From<serde_json::Error> for Error {
    fn from(_: serde_json::Error) -> Error {
        Error
    }
}

impl From<Box<dyn std::error::Error>> for Error {
    fn from(_: Box<dyn std::error::Error>) -> Error {
        Error
    }
}

pub trait IntoSerdeOrDefault {
    fn into_serde_or_default<T>(&self) -> Result<T, Box<dyn std::error::Error>>
    where
        T: for<'a> Deserialize<'a>;
}

impl IntoSerdeOrDefault for JsValue {
    fn into_serde_or_default<T>(&self) -> Result<T, Box<dyn std::error::Error>>
    where
        T: for<'a> Deserialize<'a>,
    {
        Ok(from_jsvalue(self)?)
    }
}

pub fn from_serde_or_undefined<T>(val: T) -> Result<JsValue, Box<dyn std::error::Error>>
where
    T: Serialize,
{
    Ok(to_jsvalue(&val)?)
}

pub fn deserialize_as_jsvalue<'de, D, R>(deserializer: D) -> Result<R, D::Error>
where
    D: de::Deserializer<'de>,
    R: JsCast,
{
    struct JsValueVisitor;

    impl<'de> de::Visitor<'de> for JsValueVisitor {
        type Value = JsValue;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("u32 JsValue index")
        }

        fn visit_u32<E>(self, value: u32) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(unsafe { FromWasmAbi::from_abi(value) })
        }
    }

    let jsv = deserializer.deserialize_u32(JsValueVisitor)?;
    jsv.dyn_into().map_err(|_| {
        de::Error::invalid_type(de::Unexpected::Other("bad dynamic cast"), &JsValueVisitor)
    })
}

pub fn serialize_as_jsvalue<S, V>(serializer: S, value: &V) -> Result<S::Ok, S::Error>
where
    S: ser::Serializer,
    V: Clone + IntoWasmAbi<Abi = u32>,
{
    let idx = value.clone().into_abi();
    serializer.serialize_newtype_struct("__tsb__JsValue", &idx)
}

pub fn serialize_undefined<S>(serializer: S) -> Result<S::Ok, S::Error>
where
    S: ser::Serializer,
{
    serializer.serialize_unit_struct("__tsb__undefined")
}

pub fn deserialize_undefined<'de, D>(deserializer: D) -> Result<(), D::Error>
where
    D: de::Deserializer<'de>,
{
    struct JsValueVisitor;

    impl<'de> de::Visitor<'de> for JsValueVisitor {
        type Value = ();

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("newtype then none, representing undefined")
        }

        fn visit_newtype_struct<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
        where
            D: de::Deserializer<'de>,
        {
            deserializer.deserialize_any(self)
        }

        fn visit_none<E>(self) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(())
        }
    }

    // we deserialize undefined's as newtypes wrapping a none
    // TODO: can't figure out why we can't call deserialize_unit here
    deserializer.deserialize_any(JsValueVisitor)
}
