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
use codegen::{ModDef, WithFs};
use parse::{ArcFs, TsTypes};
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use std::sync::Arc;

pub fn generate_rust_for_typescript<S: AsRef<str>>(module: S) -> TokenStream2 {
    let fs = StdFs;
    let arc_fs = Arc::new(fs) as ArcFs;
    let tt = TsTypes::try_new(arc_fs.clone(), module.as_ref()).expect("tt error");
    let tbnbf = tt.into_types_by_name_by_file();
    let final_ir = to_final_ir(tbnbf);
    let mod_def = ModDef::new(&*arc_fs, &*final_ir.borrow());
    let mod_def = WithFs {
        data: &mod_def,
        fs: &*arc_fs,
    };

    quote! { #mod_def }
}
