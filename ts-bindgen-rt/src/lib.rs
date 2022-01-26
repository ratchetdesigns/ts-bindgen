extern crate js_sys;
extern crate wasm_bindgen;

pub mod jsvalue_serde;

pub use jsvalue_serde::{from_jsvalue, to_jsvalue};
use jsvalue_serde::{Error as SerdeError, JSVALUE_NEWTYPE_STRUCT, UNDEFINED_UNIT_STRUCT};
use serde::{de, ser};
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

impl From<SerdeError> for Error {
    fn from(_: SerdeError) -> Error {
        Error
    }
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

    let jsv = deserializer.deserialize_any(JsValueVisitor)?;
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
    serializer.serialize_newtype_struct(JSVALUE_NEWTYPE_STRUCT, &idx)
}

pub fn serialize_jsvalue<S, V>(value: &V, serializer: S) -> Result<S::Ok, S::Error>
where
    S: ser::Serializer,
    V: Clone + IntoWasmAbi<Abi = u32>,
{
    let idx = value.clone().into_abi();
    serializer.serialize_newtype_struct(JSVALUE_NEWTYPE_STRUCT, &idx)
}

pub fn serialize_undefined<S>(serializer: S) -> Result<S::Ok, S::Error>
where
    S: ser::Serializer,
{
    serializer.serialize_unit_struct(UNDEFINED_UNIT_STRUCT)
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
