extern crate js_sys;
extern crate wasm_bindgen;

use serde::{Deserialize, Serialize};
use std::fmt;
use wasm_bindgen::JsValue;

pub struct Null;

pub struct Undefined;

pub trait ShouldSkipSerializing {
    fn should_skip_serializing(&self) -> bool;
}

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
        T: Default + Hydrator + for<'a> Deserialize<'a>;
}

impl IntoSerdeOrDefault for JsValue {
    fn into_serde_or_default<T>(&self) -> Result<T, Box<dyn std::error::Error>>
    where
        T: Default + Hydrator + for<'a> Deserialize<'a>,
    {
        if self.is_undefined() {
            Ok(T::default())
        } else {
            let mut res: T = self.into_serde()?;
            res.hydrate_from_js_value(self)?;
            Ok(res)
        }
    }
}

pub fn from_serde_or_undefined<T>(val: T) -> Result<JsValue, Box<dyn std::error::Error>>
where
    T: Hydrator + ShouldSkipSerializing + Serialize,
{
    if val.should_skip_serializing() {
        Ok(JsValue::undefined())
    } else {
        let jsv = JsValue::from_serde(&val)?;
        val.hydrate_to_js_value(&jsv)?;
        Ok(jsv)
    }
}

pub trait Hydrator {
    fn hydrate_from_js_value(&mut self, jsv: &JsValue) -> Result<(), Box<dyn std::error::Error>>;
    fn hydrate_to_js_value(self, jsv: &JsValue) -> Result<(), Box<dyn std::error::Error>>;
}
