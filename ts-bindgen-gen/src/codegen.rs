use crate::flattened_ir::{
    Alias, Builtin, Enum, EnumMember, FlatType, FlattenedTypeInfo, Func, Indexer, Interface,
    Intersection, NamespaceImport, Param, TypeIdent, TypeRef, Union,
};
use crate::identifier::{
    make_identifier, to_camel_case_ident, to_ident, to_snake_case_ident, Identifier,
};
pub use crate::mod_def::ModDef;
use crate::mod_def::ToModPathIter;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use std::collections::HashMap;
use std::iter;
use syn::Token;

impl ToTokens for Identifier {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        toks.append_separated(self.type_parts.iter(), <Token![::]>::default());
        if !self.type_params.is_empty() {
            toks.extend(iter::once("<".parse::<TokenStream2>().unwrap()));
            let mut type_params = self.type_params.iter();
            if let Some(tp) = type_params.next() {
                tp.to_tokens(toks);
            }
            type_params.for_each(|tp| {
                toks.extend(iter::once(",".parse::<TokenStream2>().unwrap()));
                tp.to_tokens(toks);
            });
            toks.extend(iter::once(">".parse::<TokenStream2>().unwrap()));
        }
    }
}

impl ToTokens for ModDef {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let mod_name = &self.name;
        let types = &self.types;
        let children = &self.children;

        // TODO: would be nice to do something like use super::super::... as ts_bindgen_root and be
        // able to refer to it in future use clauses. just need to get the nesting level here
        let our_toks = quote! {
            #[cfg(target_arch = "wasm32")]
            pub mod #mod_name {
                use wasm_bindgen::prelude::*;

                #(#types)*

                #(#children)*
            }
        };

        toks.append_all(our_toks);
    }
}

impl ToTokens for EnumMember {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let id = to_camel_case_ident(&self.id);
        let our_toks = {
            if let Some(value) = &self.value {
                quote! {
                    #id = #value
                }
            } else {
                quote! {
                    #id
                }
            }
        };
        toks.append_all(our_toks);
    }
}

trait ToNsPath<T: ?Sized> {
    // TODO: would love to return a generic ToTokens...
    fn to_ns_path(&self, current_mod: &T) -> TokenStream2;
}

impl<T, U> ToNsPath<T> for U
where
    T: ToModPathIter,
    U: ToModPathIter + ?Sized,
{
    fn to_ns_path(&self, current_mod: &T) -> TokenStream2 {
        let ns_len = current_mod.to_mod_path_iter().count();
        let mut use_path = vec![format_ident!("super").into(); ns_len];
        use_path.extend(self.to_mod_path_iter());
        quote! {
            #(#use_path)::*
        }
    }
}

fn to_unique_ident<T: Fn(&str) -> bool>(mut desired: String, taken: &T) -> Identifier {
    while taken(&desired) {
        desired += "_";
    }

    to_ident(&desired)
}

fn get_recursive_fields(
    Interface {
        extends, fields, ..
    }: &Interface,
) -> HashMap<String, TypeRef> {
    fields
        .iter()
        .map(|(n, t)| (n.clone(), t.clone()))
        // TODO: need to recursively expand base class fields
        /*.chain(
            extends
                .iter()
                .flat_map(|base| get_recursive_fields(base).into_iter()),
        )*/
        .collect()
}

impl ToTokens for Param {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let param_name = to_snake_case_ident(&self.name);
        let typ = &self.type_info;
        let full_type = if self.is_variadic {
            quote! {
                &[#typ]
            }
        } else {
            quote! {
                #typ
            }
        };

        let our_toks = quote! {
            #param_name: #full_type
        };
        toks.extend(our_toks);
    }
}

struct NamedFunc<'a> {
    func: &'a Func,
    js_name: &'a str,
}

impl<'a> ToTokens for NamedFunc<'a> {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let fn_name = to_snake_case_ident(self.js_name);

        let param_toks: Vec<TokenStream2> =
            self.func.params.iter().map(|p| quote! { #p }).collect();

        let return_type = &self.func.return_type;

        let our_toks = quote! {
            fn #fn_name(#(#param_toks),*) -> #return_type;
        };

        toks.extend(our_toks);
    }
}

trait Named {
    fn to_name(&self) -> (&str, Identifier);
}

impl Named for Builtin {
    fn to_name(&self) -> (&str, Identifier) {
        match self {
            Builtin::PrimitiveAny => ("JsValue", to_ident("JsValue").into()),
            Builtin::PrimitiveNumber => ("f64", to_ident("f64").into()),
            Builtin::PrimitiveObject => (
                "std::collections::HashMap<String, JsValue>",
                make_identifier!(std::collections::HashMap<String, JsValue>),
            ),
            Builtin::PrimitiveBoolean => ("bool", to_ident("bool").into()),
            Builtin::PrimitiveBigInt => ("u64", to_ident("u64").into()),
            Builtin::PrimitiveString => ("String", to_ident("String").into()),
            Builtin::PrimitiveSymbol => ("js_sys::Symbol", make_identifier!(js_sys::Symbol)),
            // TODO
            Builtin::PrimitiveVoid => ("()", to_ident("()").into()),
            Builtin::PrimitiveUndefined => (
                "ts_bindgen_rt::Undefined",
                make_identifier!(ts_bindgen_rt::Undefined),
            ),
            Builtin::PrimitiveNull => {
                ("ts_bindgen_rt::Null", make_identifier!(ts_bindgen_rt::Null))
            }
            Builtin::BuiltinDate => ("js_sys::Date", make_identifier!(js_sys::Date)),
            Builtin::LitNumber => Builtin::PrimitiveNumber.to_name(),
            Builtin::LitBoolean => Builtin::PrimitiveBoolean.to_name(),
            Builtin::LitString => Builtin::PrimitiveString.to_name(),
            Builtin::BuiltinPromise => ("js_sys::Promise", make_identifier!(js_sys::Promise)),
            Builtin::Array => ("Vec", to_ident("Vec").into()),
            Builtin::Fn => ("Fn", to_ident("Fn").into()),
            Builtin::Map => ("HashMap", to_ident("HashMap").into()),
            Builtin::Optional => ("Option", to_ident("Option").into()),
            Builtin::Variadic => ("", to_ident("").into()),
        }
    }
}

impl Named for TypeIdent {
    fn to_name(&self) -> (&str, Identifier) {
        match self {
            TypeIdent::Builtin(builtin) => builtin.to_name(),
            TypeIdent::GeneratedName { .. } => {
                panic!("expected all generated names to be resolved")
            }
            TypeIdent::LocalName(n) => (n, to_camel_case_ident(n)),
            TypeIdent::Name { file, name } => (name, to_camel_case_ident(name)),
            TypeIdent::DefaultExport(_) => panic!("didn't expect default exports"),
            TypeIdent::QualifiedName { name_parts, .. } => {
                let n = name_parts.last().expect("bad qualified name");
                (n, to_camel_case_ident(n))
            }
        }
    }
}

trait ExtraFieldAttrs {
    fn extra_field_attrs(&self) -> Box<dyn Iterator<Item = TokenStream2>>;
}

impl ExtraFieldAttrs for TypeRef {
    fn extra_field_attrs(&self) -> Box<dyn Iterator<Item = TokenStream2>> {
        self.referent.extra_field_attrs()
    }
}

impl ExtraFieldAttrs for TypeIdent {
    fn extra_field_attrs(&self) -> Box<dyn Iterator<Item = TokenStream2>> {
        if let TypeIdent::Builtin(b) = self {
            b.extra_field_attrs()
        } else {
            Box::new(iter::empty())
        }
    }
}

impl ExtraFieldAttrs for Builtin {
    fn extra_field_attrs(&self) -> Box<dyn Iterator<Item = TokenStream2>> {
        match self {
            // TODO: figure out how to represent undefined for Builtin::PrimitiveObject
            Builtin::PrimitiveUndefined => Box::new(iter::once(quote! {
                skip_serializing
            })),
            Builtin::Optional => Box::new(iter::once(quote! {
                skip_serializing_if = "Option::is_none"
            })),
            _ => Box::new(iter::empty()),
        }
    }
}

trait IsUninhabited {
    fn is_uninhabited(&self) -> bool;
}

impl IsUninhabited for FlattenedTypeInfo {
    fn is_uninhabited(&self) -> bool {
        match self {
            FlattenedTypeInfo::Ref(r) => r.is_uninhabited(),
            FlattenedTypeInfo::Union(Union { types }) => {
                types.iter().all(IsUninhabited::is_uninhabited)
            }
            _ => false,
        }
    }
}

impl IsUninhabited for TypeRef {
    fn is_uninhabited(&self) -> bool {
        match self.referent {
            TypeIdent::Builtin(Builtin::PrimitiveNull) => true,
            TypeIdent::Builtin(Builtin::PrimitiveUndefined) => true,
            TypeIdent::Builtin(Builtin::PrimitiveVoid) => true,
            _ => false,
        }
    }
}

impl ToTokens for FlatType {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let (js_name, name) = self.name.to_name();

        let our_toks = match &self.info {
            FlattenedTypeInfo::Interface(iface) => {
                let Interface { indexer, .. } = iface;
                let extended_fields = &iface.fields; //TODO: get_recursive_fields(iface);

                let mut field_toks = extended_fields
                    .iter()
                    .map(|(js_field_name, typ)| {
                        let field_name = to_snake_case_ident(js_field_name);
                        let extra_attrs = typ.extra_field_attrs();
                        let attrs =
                            iter::once(quote! { rename = #js_field_name }).chain(extra_attrs);
                        quote! {
                            #[serde(#(#attrs),*)]
                            pub #field_name: #typ
                        }
                    })
                    .collect::<Vec<TokenStream2>>();

                if let Some(Indexer {
                    readonly: _,
                    value_type,
                }) = &indexer
                {
                    let extra_fields_name = to_unique_ident("extra_fields".to_string(), &|x| {
                        extended_fields.contains_key(x)
                    });

                    field_toks.push(quote! {
                        #[serde(flatten)]
                        pub #extra_fields_name: std::collections::HashMap<String, #value_type>
                    });
                }

                quote! {
                    #[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
                    pub struct #name {
                        #(#field_toks),*
                    }
                }
            }
            FlattenedTypeInfo::Enum(Enum { members }) => {
                quote! {
                    #[wasm_bindgen]
                    #[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
                    pub enum #name {
                        #(#members),*
                    }
                }
            }
            FlattenedTypeInfo::Alias(Alias { target }) => {
                let use_path = target.to_ns_path(&self.name);

                quote! {
                    use #use_path as #name;
                }
            }
            //FlattenedTypeInfo::Ref(_) => panic!("ref isn't a top-level type"),
            //FlattenedTypeInfo::Array { .. } => panic!("Array isn't a top-level type"),
            //FlattenedTypeInfo::Optional { .. } => panic!("Optional isn't a top-level type"),
            FlattenedTypeInfo::Union(Union { types }) => {
                let members = types.iter().map(|t| {
                    let t_str = quote! { #t }
                        .to_string()
                        .replace("<", "Of")
                        .replace(">", "")
                        .replace("&", "")
                        .replace("[", "")
                        .replace("]", "");
                    let case = to_camel_case_ident(format!("{}Case", t_str));

                    if t.is_uninhabited() {
                        quote! {
                            #case
                        }
                    } else {
                        quote! {
                            #case(#t)
                        }
                    }
                });

                quote! {
                    #[wasm_bindgen]
                    #[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
                    pub enum #name {
                        #(#members),*
                    }
                }
            }
            /*TypeInfo::Intersection {
                types: Vec<TypeInfo>,
            },
            TypeInfo::Mapped {
                value_type: Box<TypeInfo>,
            },
            TypeInfo::LitNumber {
                n: f64,
            },
            TypeInfo::LitString {
                s: String,
            },
            TypeInfo::LitBoolean {
                b: bool,
            },
            TypeInfo::Func(func) => {
                let js_name = self.name.to_name();
                let attrs = {
                    let mut attrs = vec![quote! { js_name = #js_name }];
                    if func.is_variadic() {
                        attrs.push(quote! { variadic });
                    }
                    attrs
                };
                let func = NamedFunc { js_name, func };

                quote! {
                    #[wasm_bindgen]
                    extern "C" {
                        #[wasm_bindgen(#(#attrs),*)]
                        #func
                    }
                }
            }
            TypeInfo::Constructor {
                params: Vec<Param>,
                return_type: Box<TypeInfo>,
            },
            TypeInfo::Class(Class {
                super_class,
                members,
            }) => {
                let mut attrs = vec![quote! { js_name = #js_name }];
                if let Some(TypeRef {
                    referent,
                    type_params,
                }) = super_class.as_ref().map(|sc| &**sc)
                {
                    let super_name = to_camel_case_ident(referent.to_name());

                    attrs.push(quote! {
                        extends = #super_name
                    });
                }

                let members: Vec<TokenStream2> = members
                    .iter()
                    .map(|(member_js_name, member)| match member {
                        Member::Constructor(ctor) => {
                            let param_toks: Vec<TokenStream2> =
                                ctor.params.iter().map(|p| quote! { #p }).collect();

                            quote! {
                                #[wasm_bindgen(constructor)]
                                fn new(#(#param_toks),*) -> #name;
                            }
                        }
                        Member::Method(func) => {
                            let f = NamedFunc {
                                js_name: member_js_name,
                                func,
                            };
                            let mut attrs = vec![
                                quote! {js_name = #member_js_name},
                                quote! {method},
                                quote! {js_class = #js_name},
                            ];
                            if func.is_variadic() {
                                attrs.push(quote! { variadic });
                            }

                            quote! {
                                #[wasm_bindgen(#(#attrs),*)]
                                #f
                            }
                        }
                        Member::Property(typ) => {
                            let member_name = to_snake_case_ident(member_js_name);
                            let setter_name = format_ident!("set_{}", member_name);
                            quote! {
                                #[wasm_bindgen(method, structural, getter = #member_js_name)]
                                fn #member_name(this: &#name) -> #typ;

                                #[wasm_bindgen(method, structural, setter = #member_js_name)]
                                fn #setter_name(this: &#name, value: #typ);
                            }
                        }
                    })
                    .collect();

                quote! {
                    #[wasm_bindgen]
                    extern "C" {
                        #[wasm_bindgen(#(#attrs),*)]
                        type #name;

                        #(#members)*
                    }
                }
            }
            TypeInfo::Var {
                type_info: Box<TypeInfo>,
            },*/
            FlattenedTypeInfo::NamespaceImport(NamespaceImport::All { src }) => {
                let ns = src.as_path().to_ns_path(&self.name);
                let vis = if self.is_exported {
                    let vis = format_ident!("pub");
                    quote! { #vis }
                } else {
                    quote! {}
                };
                let name = to_snake_case_ident(js_name);

                quote! {
                    #vis use #ns as #name;
                }
            }
            FlattenedTypeInfo::NamespaceImport(NamespaceImport::Default { src }) => {
                let ns = src.as_path().to_ns_path(&self.name);
                let vis = if self.is_exported {
                    let vis = format_ident!("pub");
                    quote! { #vis }
                } else {
                    quote! {}
                };
                let default_export = to_ident("default");

                quote! {
                    #vis use #ns::#default_export as #name;
                }
            }
            FlattenedTypeInfo::NamespaceImport(NamespaceImport::Named {
                src,
                name: item_name,
            }) => {
                let ns = src.as_path().to_ns_path(&self.name);
                let vis = if self.is_exported {
                    let vis = format_ident!("pub");
                    quote! { #vis }
                } else {
                    quote! {}
                };
                let item_name = to_camel_case_ident(item_name);

                quote! {
                    #vis use #ns::#item_name as #name;
                }
            }
            _ => {
                quote! {}
            }
        };

        toks.append_all(our_toks);
    }
}

impl ToTokens for FlattenedTypeInfo {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let our_toks = match &self {
            FlattenedTypeInfo::Interface(_) => {
                panic!("interface in type info");
            }
            FlattenedTypeInfo::Enum(_) => {
                panic!("enum in type info");
            }
            FlattenedTypeInfo::Ref(TypeRef {
                referent,
                type_params: _,
            }) => {
                let (_, local_name) = referent.to_name();

                quote! {
                    #local_name
                }
            }
            FlattenedTypeInfo::Alias(Alias { target }) => {
                // TODO: need to get the local name for the alias (stored on the Type right now)
                let (_, local_name) = target.to_name();

                quote! {
                    #local_name
                }
            }
            FlattenedTypeInfo::Array { item_type } => {
                quote! {
                    Vec<#item_type>
                }
            }
            FlattenedTypeInfo::Optional { item_type } => {
                quote! {
                    Option<#item_type>
                }
            }
            FlattenedTypeInfo::Union(Union { types: _ }) => {
                // TODO
                quote! {}
            }
            FlattenedTypeInfo::Intersection(Intersection { types: _ }) => {
                // TODO
                quote! {}
            }
            FlattenedTypeInfo::Mapped { value_type } => {
                quote! {
                    std::collections::HashMap<String, #value_type>
                }
            }
            FlattenedTypeInfo::Func(Func {
                params,
                type_params: _,
                return_type,
            }) => {
                let param_toks: Vec<TokenStream2> = params
                    .iter()
                    .map(|p| {
                        let typ = &p.type_info;

                        if p.is_variadic {
                            quote! {
                                &[#typ]
                            }
                        } else {
                            quote! {
                                #typ
                            }
                        }
                    })
                    .collect();

                quote! {
                    &Closure<dyn Fn(#(#param_toks),*) -> #return_type>
                }
            }
            /*
            FlattenedTypeInfo::Constructor {
                params: Vec<Param>,
                return_type: Box<TypeInfo>,
            },
            FlattenedTypeInfo::Class(Class {
                members: HashMap<String, Member>,
            }),
            FlattenedTypeInfo::Var {
                type_info: Box<TypeInfo>,
            },
            FlattenedTypeInfo::GenericType {
                name: String,
                constraint: Box<TypeInfo>,
            },*/
            FlattenedTypeInfo::NamespaceImport(_) => panic!("namespace import in type info"),
            _ => {
                quote! {}
            }
        };

        toks.append_all(our_toks);
    }
}

impl ToTokens for TypeRef {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let our_toks = {
            let (_, name) = self.referent.to_name();
            if self.type_params.is_empty() {
                quote! { #name }
            } else {
                let type_params = self.type_params.iter().map(|p| quote! { #p });
                quote! { #name<#(#type_params),*> }
            }
        };

        toks.append_all(our_toks);
    }
}
