use ts_bindgen_gen::generate_rust_for_typescript;

#[test]
fn basic() {
    let r = generate_rust_for_typescript("./tests/examples/basic");
    println!("R {:?}", r);
}
