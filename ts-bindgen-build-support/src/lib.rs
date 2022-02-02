extern crate proc_macro;

use cargo_whatfeatures::Registry;
use proc_macro::{Ident, Span, TokenStream, Punct, Spacing, TokenTree};
use std::iter::Iterator;

#[proc_macro]
pub fn web_sys_types(_item: TokenStream) -> TokenStream {
    let types = _web_sys_types();

    types
        .iter()
        .map(|t| Ident::new(t, Span::mixed_site()))
        .flat_map(|id| {
            Box::new(
                [id.into(), Punct::new(',', Spacing::Alone).into()].into_iter()
            ) as Box<dyn Iterator<Item = TokenTree>>
        })
        .collect()
}

fn _web_sys_types() -> Vec<String> {
    let workspace =
        Registry::from_local()
            .unwrap()
            .maybe_latest("web-sys")
            .unwrap()
            .get_features()
            .unwrap();
    workspace.map.values()
        .flat_map(|features| features.features.keys())
        .cloned()
        .collect()
}
