mod codegen;
mod flattened_ir;
mod identifier;
mod ir;
mod ir_transform;
mod mod_def;
mod module_resolution;
mod parse;
mod target_enriched_ir;

// TODO: aliases should point to modules
// TODO: when generating code, use include_str! to make the compiler think we have a dependency on
// any ts files we use so we recompile when they do:
// https://github.com/rustwasm/wasm-bindgen/pull/1295/commits/b762948456617ee263de8e43b3636bd3a4d1da75

use crate::ir_transform::to_final_ir;
use codegen::ModDef;
use parse::TsTypes;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use std::borrow::Borrow;

pub fn generate_rust_for_typescript<S: AsRef<str>>(module: S) -> TokenStream2 {
    let tt = TsTypes::try_new(module.as_ref()).expect("tt error");
    let tbnbf = tt.types_by_name_by_file;
    //let final_ir = to_final_ir(tbnbf.clone());
    let mod_def: ModDef = tbnbf.borrow().into();

    quote! { #mod_def }
}
