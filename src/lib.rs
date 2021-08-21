extern crate proc_macro;

mod ir;
mod parse;
mod module_resolution;
mod codegen;

// TODO: aliases should point to modules
// TODO: when generating code, use include_str! to make the compiler think we have a dependency on
// any ts files we use so we recompile when they do:
// https://github.com/rustwasm/wasm-bindgen/pull/1295/commits/b762948456617ee263de8e43b3636bd3a4d1da75

use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2};
use quote::{quote, format_ident, ToTokens, TokenStreamExt};
use std::collections::HashMap;
use std::fs::File;
use std::path::{Path, PathBuf, Component};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, LitStr, Result as ParseResult, Token, parse_str as parse_syn_str};
use ir::{Type, TypeInfo, NamespaceImport, Indexer, Member, Func, Param, EnumMember, TypeName, TypeIdent};
use parse::TsTypes;
use codegen::ModDef;

#[proc_macro]
pub fn import_ts(input: TokenStream) -> TokenStream {
    let import_args = parse_macro_input!(input as ImportArgs);
    let mods = import_args
        .modules
        .iter()
        .map(|module| {
            let tt = TsTypes::try_new(&module).expect("tt error");
            use std::borrow::Borrow;
            let mod_def: ModDef = tt.types_by_name_by_file.borrow().into();
            let mod_toks = quote! { #mod_def };
            // let mod_toks = quote! { };

            let mut file = std::fs::File::create("output.rs").expect("failed to create file");
            std::io::Write::write_all(&mut file, mod_toks.to_string().as_bytes()).expect("failed to write");

            mod_toks
        })
        .collect::<Vec<TokenStream2>>();
    (quote! {
        #(#mods)*
    })
    .into()
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
