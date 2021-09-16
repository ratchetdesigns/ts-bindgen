mod codegen;
mod flattened_ir;
mod identifier;
mod ir;
mod mod_def;
mod module_resolution;
mod parse;

// TODO: aliases should point to modules
// TODO: when generating code, use include_str! to make the compiler think we have a dependency on
// any ts files we use so we recompile when they do:
// https://github.com/rustwasm/wasm-bindgen/pull/1295/commits/b762948456617ee263de8e43b3636bd3a4d1da75

use codegen::ModDef;
use parse::TsTypes;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;

pub fn generate_rust_for_typescript<S: AsRef<str>>(module: S) -> TokenStream2 {
    let tt = TsTypes::try_new(module.as_ref()).expect("tt error");
    use std::borrow::Borrow;
    let mod_def: ModDef = tt.types_by_name_by_file.borrow().into();

    quote! { #mod_def }
}
