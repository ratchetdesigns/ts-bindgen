mod js_lib;

use wasm_bindgen::JsValue;
use crate::js_lib::root::tmp::test::ts::index::run_test;
use crate::js_lib::root::tmp::test::ts::round_trip_clone::AnyType;
use crate::js_lib::root::tmp::test::ts::class_method_invoker::ClassMethodInvokerTest;

pub fn cloner(t: AnyType) -> Result<AnyType, JsValue> {
    Ok(t.clone())
}

pub fn run_end_to_end_test() -> Result<(), wasm_bindgen::JsValue>{
    assert!(run_test(&cloner, &[1f64])?);
    let cmi = ClassMethodInvokerTest::new("rust string".to_string());
    assert_eq!(cmi.get_info()?, "hello world rust string");
    Ok(())
}
