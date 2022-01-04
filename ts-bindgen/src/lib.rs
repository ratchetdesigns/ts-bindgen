//! ts-bindgen provides a library as an easy entrypoint into generating
//! rust wasm-bindgen bindings for a typescript module and provides an
//! executable for doing the same via the command line.
//!
//! If you are using the library, we recommend adding `default-features = false`
//! to your Cargo.toml to avoid pulling in unnecessary crates.
//!
//! Because rustfmt compilation requires the nightly toolchain at this point,
//! the library functions do not support formatting the returned rust code
//! but the ts-bindgen executable will run rustfmt on the generated code
//! by default. You can see
//! [ts-bindgen-web](https://github.com/ratchetdesigns/ts-bindgen/tree/master/ts-bindgen-web)
//! for an example of running rustfmt as a library on the output (in wasm).

use std::path::Path;
use ts_bindgen_gen::{generate_rust_for_typescript, Fs, MemFs};
#[cfg(any(target_arch = "wasm32", target_arch = "wasm64"))]
use wasm_bindgen::prelude::*;

pub fn generate_rust_text_for_typescript<FS, S>(fs: FS, module: S) -> String
where
    S: AsRef<str>,
    FS: Fs + Send + Sync + 'static,
{
    let toks = generate_rust_for_typescript(fs, module);
    toks.to_string()
}

#[cfg_attr(any(target_arch = "wasm32", target_arch = "wasm64"), wasm_bindgen)]
pub fn generate_rust_text_for_typescript_string(ts: String) -> String {
    let file = "/work.d.ts";

    let mut fs: MemFs = Default::default();
    fs.set_cwd(Path::new("/"));
    fs.add_file_at(Path::new(file), ts);

    generate_rust_text_for_typescript(fs, file)
}

#[cfg(test)]
mod test {
    use super::*;

    #[cfg(any(target_arch = "wasm32", target_arch = "wasm64"))]
    use wasm_bindgen_test::*;

    fn remove_whitespace(mut s: String) -> String {
        s.retain(|c| !c.is_whitespace());
        s
    }

    #[cfg_attr(any(target_arch = "wasm32", target_arch = "wasm64"), wasm_bindgen_test)]
    #[cfg_attr(not(any(target_arch = "wasm32", target_arch = "wasm64")), test)]
    fn test_generate_rust_text_for_typescript_string() {
        let ts = r#"
            export interface Abc {
                hello: number;
                world: string
            }
        "#;
        let expected_contents = [
            "pub mod work",
            "pub struct Abc",
            "pub world: String",
            "pub hello: f64",
            "pub trait Abc_Trait",
        ];

        let result = remove_whitespace(generate_rust_text_for_typescript_string(ts.to_string()));
        let result_ok = expected_contents
            .iter()
            .all(|c| result.contains(&remove_whitespace(c.to_string())));

        assert!(result_ok);
    }
}
