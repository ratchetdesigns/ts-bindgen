extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::punctuated::Punctuated;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, Token, LitStr, Result as ParseResult};
use std::path::PathBuf;
use std::fs::File;
use serde_json::Value;

#[proc_macro]
pub fn import_ts(input: TokenStream) -> TokenStream {
    println!("HERE: {:?}", input);
    let import_args = parse_macro_input!(input as ImportArgs);
    import_args.modules.into_iter().map(|module| {
        let import = get_ts_file(&module);
        println!("IMPORT {:?}", import);
        "hi".to_string()
    }).collect::<Vec<String>>();
    (quote! {
        struct Hello {}
    }).into()
}

struct ImportArgs {
    modules: Vec<String>,
}

impl Parse for ImportArgs {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        let modules = Punctuated::<LitStr, Token![,]>::parse_terminated(input)?;
        Ok(ImportArgs {
            modules: modules.into_iter().map(|m| m.value()).collect(),
        })
    }
}

fn get_ts_file(import: &str) -> std::io::Result<PathBuf> {
    let cwd = std::env::current_dir()?;
    let mut path = cwd.clone();

    if import.starts_with(".") {
        Ok(path.join(import))
    } else {
        loop {
            let possible_node_modules = path.join("node_modules");
            if possible_node_modules.is_dir() {
                let import_path = possible_node_modules.join(import);
                if import_path.is_dir() {
                    // module path
                    // check package.json for typings
                    let pkg_json_path = import_path.join("package.json");
                    let file = File::open(&pkg_json_path)?;
                    let pkg: Value = serde_json::from_reader(file)?;
                    let types_rel_path = pkg.as_object()
                        .ok_or(std::io::Error::new(std::io::ErrorKind::InvalidData, format!("Bad package.json (expected top-level object) found in {}", pkg_json_path.display())))?
                        .get("types")
                        .ok_or(std::io::Error::new(std::io::ErrorKind::InvalidData, format!("Bad package.json (expected 'types' property) found in {}", pkg_json_path.display())))?
                        .as_str()
                        .ok_or(std::io::Error::new(std::io::ErrorKind::InvalidData, format!("Bad package.json (expected 'types' to be a string) found in {}", pkg_json_path.display())))?;

                    let types_path = import_path.join(types_rel_path);
                    if !types_path.is_file() {
                        break Err(std::io::Error::new(std::io::ErrorKind::NotFound, format!("Package.json, {}, specified non-existent file for types, {}", pkg_json_path.display(), types_path.display())))
                    }

                    break Ok(types_path)
                } else if import_path.exists() {
                    // must be a module + file path
                    break Ok(import_path)
                } else {
                    break Err(std::io::Error::new(std::io::ErrorKind::NotFound, format!("Could not find module, {}, in node_modules directory, {}", import, possible_node_modules.display())))
                }
            } else {
                if !path.pop() {
                    break Err(std::io::Error::new(std::io::ErrorKind::NotFound, format!("Could not find node_modules directory starting at {}", cwd.display())))
                }
            }
        }
    }
}
