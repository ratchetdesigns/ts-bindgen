mod js_lib;

use wasm_bindgen::JsValue;
use crate::js_lib::ts::run_test;
use crate::js_lib::ts::round_trip_clone::AnyType;
use crate::js_lib::ts::class_method_invoker::ClassMethodInvokerTest;
use crate::js_lib::ts::generics::SimpleGeneric;

pub fn cloner(t: AnyType) -> Result<AnyType, JsValue> {
    Ok(t.clone())
}

pub fn generic_cloner<T: Clone>(t: SimpleGeneric<T>) -> Result<SimpleGeneric<T>, JsValue> {
    Ok(t.clone())
}

pub fn run_end_to_end_test() -> Result<(), wasm_bindgen::JsValue>{
    assert!(run_test(&cloner, &generic_cloner, &generic_cloner, &generic_cloner, Box::new([1f64]))?);
    let cmi = ClassMethodInvokerTest::new("rust string".to_string());
    assert_eq!(cmi.get_info()?, "hello world rust string");
    Ok(())
}
