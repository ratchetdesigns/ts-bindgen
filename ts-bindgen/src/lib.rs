#![deny(missing_docs)]

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

pub use ts_bindgen_gen::{Error, Fs, MemFs, StdFs};

use std::path::Path;
use ts_bindgen_gen::generate_rust_for_typescript;

/// Given a [filesystem](`FS`) and a reference to a typescript definition module (e.g. "moment" to
/// refer to a moment module in node_modules, "./my-module" to refer to my-module.d.ts or
/// my-module/index.d.ts or my-module's package.json typings reference, etc.), return a String
/// of rust code for wasm-bindgen bindings to the module.
///
/// ```rust
/// use ts_bindgen::{generate_rust_string_for_typescript, MemFs};
/// use std::path::Path;
///
/// # fn main() -> Result<(), ts_bindgen::Error> {
/// let fs = {
///     let mut fs: MemFs = Default::default();
///     fs.set_cwd(Path::new("/"));
///     fs.add_file_at(
///         Path::new("/my-module.d.ts"),
///         r#"
///             export declare interface MyInterface {
///                 someNumber: number;
///                 aString: string;
///             }
///         "#.to_string(),
///     );
///     fs
/// };
///
/// let rust = generate_rust_string_for_typescript(fs, "./my-module")?;
///
/// assert!(rust.contains("pub struct MyInterface"));
///
/// # Ok(())
/// # }
pub fn generate_rust_string_for_typescript<FS, S>(fs: FS, module: S) -> Result<String, Error>
where
    S: AsRef<str>,
    FS: Fs + Send + Sync + 'static,
{
    let toks = generate_rust_for_typescript(fs, module)?;
    Ok(toks.to_string())
}

/// Given typescript definitions as a string, return a String of rust code for wasm-bindgen bindings to the typescript definitions.
/// The rust will be generated in a module named according to `rust_namespace`.
///
/// ```rust
/// use ts_bindgen::generate_rust_string_for_typescript_string;
/// use std::path::Path;
///
/// # fn main() -> Result<(), ts_bindgen::Error> {
/// let ts = r#"
///     export declare interface MyInterface {
///         someNumber: number;
///         aString: string;
///     }
/// "#.to_string();
///
/// let rust = generate_rust_string_for_typescript_string("some-module", ts)?;
///
/// assert!(rust.contains("pub mod some_module"));
/// assert!(rust.contains("pub struct MyInterface"));
///
/// # Ok(())
/// # }
pub fn generate_rust_string_for_typescript_string(
    rust_namespace: &str,
    ts: String,
) -> Result<String, Error> {
    let file = format!("/{}.d.ts", rust_namespace);

    let fs = {
        let mut fs: MemFs = Default::default();
        fs.set_cwd(Path::new("/"));
        fs.add_file_at(Path::new(&file), ts);
        fs
    };

    generate_rust_string_for_typescript(fs, file)
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

        let rust_result = generate_rust_string_for_typescript_string("work", ts.to_string());
        assert!(rust_result.is_ok());
        let rust = rust_result.unwrap();

        let result = remove_whitespace(rust);
        let result_ok = expected_contents
            .iter()
            .all(|c| result.contains(&remove_whitespace(c.to_string())));

        assert!(result_ok);
    }
}
