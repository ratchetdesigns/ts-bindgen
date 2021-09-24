use std::fs::File;
use std::io::Write;
use ts_bindgen_gen::generate_rust_for_typescript;

#[test]
fn basic() -> std::io::Result<()> {
    let r = generate_rust_for_typescript("./tests/examples/basic");
    let mut file = File::create("output.rs")?;
    file.write_all(r.to_string().as_bytes())?;
    println!("R {:?}", r);
    Ok(())
}
