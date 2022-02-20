extern crate proc_macro;

// TODO: when generating code, use include_str! to make the compiler think we have a dependency on
// any ts files we use so we recompile when they do:
// https://github.com/rustwasm/wasm-bindgen/pull/1295/commits/b762948456617ee263de8e43b3636bd3a4d1da75

use proc_macro::TokenStream;
use quote::quote;
use std::fs::File;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, LitStr, Result as ParseResult, Token};
use ts_bindgen_gen::{generate_rust_for_typescript, StdFs};

#[proc_macro]
pub fn import_ts(input: TokenStream) -> TokenStream {
    let import_args = parse_macro_input!(input as ImportArgs);
    let mods = import_args
        .modules
        .iter()
        .map(|module| {
            let mod_toks = generate_rust_for_typescript(StdFs, module)
                .expect("failed to generate typescript rust bindings");

            let mut file = File::create("output.rs").expect("failed to create file");
            std::io::Write::write_all(&mut file, mod_toks.to_string().as_bytes())
                .expect("failed to write");

            mod_toks
        })
        .collect::<Vec<_>>();
    (quote! {
        mod ts {
            #(#mods)*
        }
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
