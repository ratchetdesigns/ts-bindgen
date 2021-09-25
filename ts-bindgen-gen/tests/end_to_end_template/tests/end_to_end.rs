#![cfg(target_arch = "wasm32")]

extern crate wasm_bindgen_test;
use wasm_bindgen_test::*;

extern crate ts_bindgen_gen_end_to_end_test;
use ts_bindgen_gen_end_to_end_test::run_end_to_end_test;

#[wasm_bindgen_test]
fn end_to_end_interface_test() {
    match run_end_to_end_test() {
        Err(err) => {
            println!("Error in testing: {:?}", err);
            panic!("test failed!")
        },
        Ok(_) => assert!(true),
    }
}
