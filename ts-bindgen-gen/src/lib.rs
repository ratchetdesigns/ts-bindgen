mod codegen;
mod fs;
mod identifier;
mod ir;
mod mod_def;
mod module_resolution;
mod parse;

#[cfg(test)]
mod generators;

use crate::fs::StdFs;
use crate::ir::to_final_ir;
use codegen::ModDef;
use parse::{ArcFs, TsTypes};
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use std::sync::Arc;

pub fn generate_rust_for_typescript<S: AsRef<str>>(module: S) -> TokenStream2 {
    let fs = StdFs;
    let tt = TsTypes::try_new(Arc::new(fs) as ArcFs, module.as_ref()).expect("tt error");
    let tbnbf = tt.into_types_by_name_by_file();
    let final_ir = to_final_ir(tbnbf);
    let mod_def: ModDef = (&*final_ir.borrow()).into();

    quote! { #mod_def }
}
