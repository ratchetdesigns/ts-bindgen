use ts_bindgen_gen::generate_rust_for_typescript;
use ts_bindgen_gen::StdFs;

#[test]
fn basic() -> std::io::Result<()> {
    // just making sure we don't crash for now...
    generate_rust_for_typescript(StdFs, "./tests/examples/basic");
    Ok(())
}
