mod js_lib;

use wasm_bindgen::JsValue;
use crate::js_lib::root::tmp::test::ts::index::run_test;
use crate::js_lib::root::tmp::test::ts::round_trip_clone::AnyType;

pub fn cloner(t: AnyType) -> Result<AnyType, JsValue> {
    Ok(t.clone())
}

pub fn run_end_to_end_test() -> Result<(), wasm_bindgen::JsValue>{
    assert!(run_test(&cloner, &[1f64])?);
    Ok(())
}
