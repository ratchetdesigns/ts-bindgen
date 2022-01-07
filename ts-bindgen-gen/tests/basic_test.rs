use ts_bindgen_gen::{generate_rust_for_typescript, Error, StdFs};

#[test]
fn basic() -> Result<(), Error> {
    // just making sure we don't crash for now...
    generate_rust_for_typescript(StdFs, "./tests/examples/basic")?;
    Ok(())
}
