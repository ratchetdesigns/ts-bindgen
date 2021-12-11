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
    toks.to_string() // TODO: rustfmt
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
            interface Abc {
                hello: number;
                world: string
            }
        "#;
        let expected = r#"
            # [cfg (target_family = "wasm")]
            pub mod work {
                use wasm_bindgen :: prelude :: * ;
                # [derive (Clone , serde :: Serialize , serde :: Deserialize)]
                pub struct Abc {
                    # [serde (rename = "world")]
                    pub world : String ,
                    # [serde (rename = "hello")]
                    pub hello : f64
                }
                trait AbcTrait {
                    fn world (& self) -> String ;
                    fn set_world (& mut self , value : String) -> () ;
                    fn hello (& self) -> f64 ;
                    fn set_hello (& mut self , value : f64) -> () ;
                }
            }
        "#;

        let result = generate_rust_text_for_typescript_string(ts.to_string());
        assert_eq!(
            remove_whitespace(result),
            remove_whitespace(expected.to_string())
        );
    }
}
