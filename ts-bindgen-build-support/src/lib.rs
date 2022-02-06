extern crate proc_macro;

use cargo_whatfeatures::Registry;
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::quote;

#[proc_macro]
pub fn with_web_sys_types(form: TokenStream) -> TokenStream {
    let types = _web_sys_types();

    let types = types.iter().map(|t| Ident::new(t, Span::call_site()));

    let form: TokenStream2 = form.into();

    quote! {
        macro_rules! __with_web_sys_types {
            ($($item:ident),*) => {
                #form
            }
        }

        __with_web_sys_types!(#(#types),*);
    }
    .into()
}

fn _web_sys_types() -> Vec<String> {
    let workspace = Registry::from_local()
        .unwrap()
        .maybe_latest("web-sys")
        .unwrap()
        .get_features()
        .unwrap();
    workspace
        .map
        .values()
        .flat_map(|features| features.features.keys())
        .cloned()
        .collect()
}
