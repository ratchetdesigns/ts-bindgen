extern crate js_sys;
extern crate wasm_bindgen;

pub struct Null;

pub struct Undefined;

pub trait ShouldSkipSerializing {
    fn should_skip_serializing(&self) -> bool;
}

pub struct Error;

impl From<Error> for wasm_bindgen::JsValue {
    fn from(_: Error) -> wasm_bindgen::JsValue {
        js_sys::Error::new("rust failure").into()
    }
}

impl From<serde_json::Error> for Error {
    fn from(_: serde_json::Error) -> Error {
        Error
    }
}
