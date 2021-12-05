mod codegen;
mod fs;
mod identifier;
mod ir;
mod mod_def;
mod module_resolution;
mod parse;

use crate::ir::to_final_ir;
use codegen::ModDef;
use parse::TsTypes;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;

pub fn generate_rust_for_typescript<S: AsRef<str>>(module: S) -> TokenStream2 {
    let tt = TsTypes::try_new(module.as_ref()).expect("tt error");
    let tbnbf = tt.types_by_name_by_file;
    let final_ir = to_final_ir(tbnbf);
    let mod_def: ModDef = (&*final_ir.borrow()).into();

    quote! { #mod_def }
}
