extern crate js_sys;
extern crate wasm_bindgen;

use serde::{Deserialize, Serialize};
use wasm_bindgen::JsValue;

pub struct Null;

pub struct Undefined;

pub trait ShouldSkipSerializing {
    fn should_skip_serializing(&self) -> bool;
}

pub struct Error;

impl From<Error> for JsValue {
    fn from(_: Error) -> JsValue {
        js_sys::Error::new("rust failure").into()
    }
}

impl From<serde_json::Error> for Error {
    fn from(_: serde_json::Error) -> Error {
        Error
    }
}

pub trait IntoSerdeOrDefault {
    fn into_serde_or_default<T>(&self) -> serde_json::Result<T>
    where
        T: Default + for<'a> Deserialize<'a>;
}

impl IntoSerdeOrDefault for JsValue {
    fn into_serde_or_default<T>(&self) -> serde_json::Result<T>
    where
        T: Default + for<'a> Deserialize<'a>,
    {
        if self.is_undefined() {
            Ok(T::default())
        } else {
            self.into_serde()
        }
    }
}

pub fn from_serde_or_undefined<T: ShouldSkipSerializing + Serialize>(
    val: &T,
) -> serde_json::Result<JsValue> {
    if val.should_skip_serializing() {
        Ok(JsValue::undefined())
    } else {
        JsValue::from_serde(val)
    }
}
