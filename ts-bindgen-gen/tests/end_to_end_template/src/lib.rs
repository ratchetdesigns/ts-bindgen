mod js_lib;

use crate::js_lib::root::tmp::test::ts::index::run_test;

pub fn run_end_to_end_test() -> Result<(), wasm_bindgen::JsValue>{
    assert_eq!(run_test()? as u64, 5u64);
    Ok(())
}
