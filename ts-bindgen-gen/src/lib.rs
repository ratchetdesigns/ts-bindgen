mod codegen;
mod fs;
mod identifier;
mod ir;
mod mod_def;
mod module_resolution;
mod parse;

#[cfg(test)]
mod generators;

pub use crate::fs::{Fs, MemFs, StdFs};
use crate::ir::to_final_ir;
use codegen::{ModDef, WithFs};
use parse::{ArcFs, TsTypes};
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use std::path::Path;
use std::sync::Arc;

/// Given a filesystem and a module specifier (path to typescript definition file or node module
/// found in `fs.cwd().join("node_modules")`), return a TokenStream representing the rust
/// wasm-bindgen bindings.
pub fn generate_rust_for_typescript<S, FS>(fs: FS, module: S) -> TokenStream2
where
    S: AsRef<str>,
    FS: Fs + Send + Sync + 'static,
{
    generate_rust_for_typescript_with_file_processor(fs, module, |_| {})
}

/// Given a filesystem and a module specifier (path to typescript definition file or node module
/// found in `fs.cwd().join("node_modules")`), return a TokenStream representing the rust
/// wasm-bindgen bindings and an iterator over all typescript paths considered.
pub fn generate_rust_for_typescript_with_file_processor<S, FS, F>(
    fs: FS,
    module: S,
    process_file: F,
) -> TokenStream2
where
    S: AsRef<str>,
    FS: Fs + Send + Sync + 'static,
    F: FnMut(&Path),
{
    let arc_fs = Arc::new(fs) as ArcFs;
    let tt = TsTypes::try_new(arc_fs.clone(), module.as_ref()).expect("tt error");
    let tbnbf = tt.into_types_by_name_by_file();
    let final_ir = to_final_ir(tbnbf);
    let final_ir = &*final_ir.borrow();
    let mod_def = ModDef::new(&*arc_fs, final_ir);
    let mod_defs = mod_def
        .children
        .iter()
        .map(|data| WithFs { data, fs: &*arc_fs });

    final_ir
        .keys()
        .map(std::borrow::Borrow::borrow)
        .for_each(process_file);

    quote! { #(#mod_defs)* }
}
