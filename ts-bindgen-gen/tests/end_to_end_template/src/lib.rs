mod js_lib;

use wasm_bindgen::JsValue;
use crate::js_lib::ts::run_test;
use crate::js_lib::ts::round_trip_clone::AnyType;
use crate::js_lib::ts::class_method_invoker::ClassMethodInvokerTest;
use crate::js_lib::ts::generics::SimpleGeneric;
use crate::js_lib::ts::function_overload::{overloaded_FnF64ToF64, overloaded_FnStringToString, Over};
use crate::js_lib::ts::array_passing::{accept_string_array, accept_class_array, return_string_array, return_class_array, C};

pub fn cloner(t: AnyType) -> Result<AnyType, JsValue> {
    Ok(t.clone())
}

pub fn generic_cloner<T: Clone>(t: SimpleGeneric<T>) -> Result<SimpleGeneric<T>, JsValue> {
    Ok(t.clone())
}

fn overload_test() -> Result<(), wasm_bindgen::JsValue> {
    let expected_f = 12.4f64;
    let f = overloaded_FnF64ToF64(expected_f)?;
    let delta = 0.0001f64;
    assert!(f - expected_f < delta && expected_f - f < delta);
    assert_eq!(overloaded_FnStringToString("hello world".to_string())?, "hello world");

    let over = Over::new_FnToOver();
    let f = over.overload_FnF64ToF64(expected_f)?;
    assert!(f - expected_f < delta && expected_f - f < delta);
    assert_eq!(over.overload_FnStringToString("yo yo yo".to_string())?, "yo yo yo");

    let over = Over::new_FnF64ToOver(5f64);
    assert_eq!(over.s()?, "5");

    let over = Over::new_FnStringToOver("hello".to_string());
    assert_eq!(over.s()?, "hello");

    Ok(())
}

fn array_test() -> Result<(), wasm_bindgen::JsValue> {
    let delta = 0.00001;
    let v = vec!["hello".to_string(), "world".to_string()];
    let s_count = accept_string_array(v)?;
    assert!(s_count - 2f64 < delta && 2f64 - s_count < delta);
    let cs = vec![C::new(), C::new(), C::new()];
    let c_count = accept_class_array(cs)?;
    assert!(c_count - 3f64 < delta && 3f64 - c_count < delta);
    let strings = return_string_array()?;
    assert_eq!(strings.get(0).unwrap(), "hello");
    assert_eq!(strings.get(1).unwrap(), "world");
    assert_eq!(return_class_array()?.len(), 2);
    Ok(())
}

pub fn run_end_to_end_test() -> Result<(), wasm_bindgen::JsValue> {
    assert!(run_test(&cloner, &generic_cloner, &generic_cloner, &generic_cloner, Box::new([1f64]))?);
    let cmi = ClassMethodInvokerTest::new("rust string".to_string());
    assert_eq!(cmi.get_info()?, "hello world rust string");
    overload_test()?;
    array_test()?;
    Ok(())
}
