use std::fs::File;
use std::io::Write;
use ts_bindgen_gen::generate_rust_for_typescript;

#[test]
fn basic() -> std::io::Result<()> {
    let r = generate_rust_for_typescript("./tests/examples/basic");
    // just making sure we don't crash for now...
    Ok(())
}
