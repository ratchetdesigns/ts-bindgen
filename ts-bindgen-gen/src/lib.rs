#![deny(missing_docs)]

//! ts-bindgen-gen contains the core implementation of the [`ts-bindgen`] generation of rust
//! wasm-bindgen bindings to typescript definitions.

mod codegen;
mod error;
mod fs;
mod identifier;
mod ir;
mod mod_def;
mod module_resolution;
mod parse;

#[cfg(test)]
mod generators;

pub use crate::error::Error;
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
///
/// ```rust
/// use ts_bindgen_gen::{generate_rust_for_typescript, MemFs};
/// use std::path::Path;
///
/// # fn main() -> Result<(), ts_bindgen_gen::Error> {
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
/// let rust = generate_rust_for_typescript(fs, "./my-module")?;
///
/// assert!(rust.to_string().contains("pub struct MyInterface"));
///
/// # Ok(())
/// # }
pub fn generate_rust_for_typescript<S, FS>(fs: FS, module: S) -> Result<TokenStream2, Error>
where
    S: AsRef<str>,
    FS: Fs + Send + Sync + 'static,
{
    generate_rust_for_typescript_with_file_processor(fs, module, |_| {})
}

/// Given a filesystem and a module specifier (path to typescript definition file or node module
/// found in `fs.cwd().join("node_modules")`), return a TokenStream representing the rust
/// wasm-bindgen bindings. `file_processor` is invoked with the absolute path of every typescript
/// file we process. This is intended to allow any necessary post-processing and does not affect
/// the binding generation.
///
/// ```rust
/// use ts_bindgen_gen::{generate_rust_for_typescript_with_file_processor, MemFs};
/// use std::path::{Path, PathBuf};
///
/// # fn main() -> Result<(), ts_bindgen_gen::Error> {
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
/// let mut files: Vec<PathBuf> = Default::default();
/// let mut file_processor = |file: &Path| {
///     files.push(file.to_path_buf());
/// };
///
/// let rust = generate_rust_for_typescript_with_file_processor(fs, "./my-module", file_processor)?;
///
/// assert!(rust.to_string().contains("pub struct MyInterface"));
/// assert!(files.contains(&PathBuf::from("/my-module.d.ts")));
///
/// # Ok(())
/// # }
pub fn generate_rust_for_typescript_with_file_processor<S, FS, F>(
    fs: FS,
    module: S,
    process_file: F,
) -> Result<TokenStream2, Error>
where
    S: AsRef<str>,
    FS: Fs + Send + Sync + 'static,
    F: FnMut(&Path),
{
    let arc_fs = Arc::new(fs) as ArcFs;
    let tbnbf = TsTypes::parse(arc_fs.clone(), module.as_ref())?;
    let final_ir = to_final_ir(tbnbf, arc_fs.clone());
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

    Ok(quote! {
        #![allow(clippy::let_and_return, clippy::type_complexity, clippy::unused_unit, clippy::manual_non_exhaustive)]
        #(#mod_defs)*
    })
}
