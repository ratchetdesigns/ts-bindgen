use crate::identifier::{
    make_identifier, to_camel_case_ident, to_ident, to_snake_case_ident, to_unique_ident,
    Identifier,
};
pub use crate::mod_def::ModDef;
use crate::mod_def::ToModPathIter;
use crate::target_enriched_ir::{
    Alias, Builtin, Enum, EnumMember, Func, Indexer, Interface, Intersection, NamespaceImport,
    Param, TargetEnrichedType, TargetEnrichedTypeInfo, TypeIdent, TypeRef, Union,
};
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::iter;
use std::path::PathBuf;
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

        let imports = if types.is_empty() {
            quote! {}
        } else {
            quote! {
                use wasm_bindgen::prelude::*;
            }
        };

        // TODO: would be nice to do something like use super::super::... as ts_bindgen_root and be
        // able to refer to it in future use clauses. just need to get the nesting level here
        let our_toks = quote! {
            #[cfg(target_arch = "wasm32")]
            pub mod #mod_name {
                #imports

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

fn get_recursive_fields(
    Interface {
        extends, fields, ..
    }: &Interface,
) -> HashMap<String, TypeRef> {
    fields
        .iter()
        .map(|(n, t)| (n.clone(), t.clone()))
        .chain(extends.iter().flat_map(|base| {
            let resolved_type = base.resolve_target_type();
            let base = resolved_type
                .as_ref()
                .and_then(TypesByIdentByPathCell::get_type_info)
                .expect("cannot resolve base type for interface");
            if let TargetEnrichedTypeInfo::Interface(i) = base {
                get_recursive_fields(i).into_iter()
            } else {
                panic!("expected an interface as the base type for an interface");
            }
        }))
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
            // TODO: make a wrapper in rt to allow objects to be null or undefined
            Builtin::PrimitiveObject => (
                "std::collections::HashMap<String, JsValue>",
                make_identifier!(std::collections::HashMap<String, JsValue>),
            ),
            Builtin::PrimitiveBoolean => ("bool", to_ident("bool").into()),
            Builtin::PrimitiveBigInt => ("u64", to_ident("u64").into()),
            Builtin::PrimitiveString => ("String", to_ident("String").into()),
            Builtin::PrimitiveSymbol => ("js_sys::Symbol", make_identifier!(js_sys::Symbol)),
            // TODO: is this correct?
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
            Builtin::Map => ("std::collections::HashMap", make_identifier!(std::collections::HashMap)),
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
            TypeIdent::Name { file: _, name } => (name, to_camel_case_ident(name)),
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
        let resolved_type = self.resolve_target_type();
        let resolved_extra_attrs = resolved_type
            .as_ref()
            .and_then(TypesByIdentByPathCell::get_type_info)
            .map(|ti| ti.extra_field_attrs())
            .unwrap_or_else(|| Box::new(iter::empty()));

        Box::new(
            self.referent
                .extra_field_attrs()
                .chain(resolved_extra_attrs),
        )
    }
}

impl ExtraFieldAttrs for TargetEnrichedTypeInfo {
    fn extra_field_attrs(&self) -> Box<dyn Iterator<Item = TokenStream2>> {
        match self {
            TargetEnrichedTypeInfo::Union(_) => Box::new(iter::once(quote! {
                skip_serializing_if = "ts_bindgen_rt::ShouldSkipSerializing::should_skip_serializing"
            })),
            _ => Box::new(iter::empty()),
        }
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
            Builtin::PrimitiveAny => Box::new(iter::once(quote! {
                skip_serializing_if = "JsValue::is_undefined"
            })),
            _ => Box::new(iter::empty()),
        }
    }
}

// TODO: we only need this silly container because we get a Ref rather than a & because we had to
// leave types_by_ident_by_path as an Rc<RefCell<...>> instead of just an Rc<..>
enum TypesByIdentByPathCell<'a> {
    // TODO: I want a &'a PathBuf but that gets tricky to look up in the map for some reason
    Lookup(
        Ref<'a, HashMap<PathBuf, HashMap<TypeIdent, TargetEnrichedType>>>,
        PathBuf,
        TypeIdent,
    ),
    Direct(&'a TargetEnrichedTypeInfo),
    Owned(TargetEnrichedTypeInfo),
}

impl std::fmt::Debug for TypesByIdentByPathCell<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypesByIdentByPathCell::Lookup(_, f1, f2) => f
                .debug_struct("TypesByIdentByPathCell")
                .field("1", f1)
                .field("2", f2)
                .finish(),
            _ => f
                .debug_struct("TypesByIdentByPathCell")
                .field("elided", &true)
                .finish(),
        }
    }
}

impl<'a> TypesByIdentByPathCell<'a> {
    fn get_type_info(&'a self) -> Option<&'a TargetEnrichedTypeInfo> {
        match self {
            TypesByIdentByPathCell::Direct(t) => Some(t),
            TypesByIdentByPathCell::Owned(t) => Some(&t),
            TypesByIdentByPathCell::Lookup(r, path, n) => r
                .get(path)
                .and_then(|t_by_n| t_by_n.get(&n))
                .map(|t| &t.info),
        }
    }
}

trait ResolveTargetType {
    fn resolve_target_type(&self) -> Option<TypesByIdentByPathCell>;
}

impl ResolveTargetType for TargetEnrichedType {
    fn resolve_target_type(&self) -> Option<TypesByIdentByPathCell> {
        self.info.resolve_target_type()
    }
}

impl ResolveTargetType for TypeRef {
    fn resolve_target_type(&self) -> Option<TypesByIdentByPathCell> {
        match &self.referent {
            TypeIdent::LocalName(n) => Some(TypesByIdentByPathCell::Lookup(
                RefCell::borrow(&self.context.types_by_ident_by_path),
                self.context.path.clone(),
                // TODO: this is weird. we should not need clones here and we should not
                // have the option of looking up a Builtin
                // TODO: specifically,
                // 1. convert all TypeIdent::Name-s into TypeIdent::LocalName-s
                //    (correctness issue)
                // 2. get rid of GeneratedName in our ir pipeline
                TypeIdent::LocalName(n.clone()),
            )),
            TypeIdent::Name { file, name } => Some(TypesByIdentByPathCell::Lookup(
                RefCell::borrow(&self.context.types_by_ident_by_path),
                file.clone(),
                TypeIdent::Name {
                    file: file.clone(),
                    name: name.clone(),
                },
            )),
            TypeIdent::DefaultExport(path) => Some(TypesByIdentByPathCell::Lookup(
                RefCell::borrow(&self.context.types_by_ident_by_path),
                path.clone(),
                TypeIdent::DefaultExport(path.clone()),
            )),
            TypeIdent::QualifiedName { file, name_parts } => {
                let mut final_file = file.clone();
                let mut final_name = None;
                let types_by_ident_by_path = RefCell::borrow(&self.context.types_by_ident_by_path);

                for n in name_parts {
                    let t = types_by_ident_by_path.get(file).and_then(|t_by_n| {
                        t_by_n.get(&TypeIdent::Name {
                            file: final_file.clone(),
                            name: n.clone(),
                        })
                    });
                    if let Some(ty) = t {
                        let resolved_type = ty.resolve_target_type();
                        // TODO: silly to clone on every iteration but i need to figure out how
                        // to get resolved_type to live long enough to just pass along the ref
                        if let Some(target_type) =
                            resolved_type.as_ref().and_then(|tt| tt.get_type_info())
                        {
                            final_file = match target_type {
                                TargetEnrichedTypeInfo::Interface(i) => i.context.path.clone(),
                                TargetEnrichedTypeInfo::Enum(e) => e.context.path.clone(),
                                TargetEnrichedTypeInfo::Alias(a) => a.context.path.clone(),
                                TargetEnrichedTypeInfo::Ref(r) => r.context.path.clone(),
                                TargetEnrichedTypeInfo::Union(u) => u.context.path.clone(),
                                TargetEnrichedTypeInfo::Intersection(i) => i.context.path.clone(),
                                TargetEnrichedTypeInfo::Func(f) => f.context.path.clone(),
                                TargetEnrichedTypeInfo::Constructor(c) => c.context.path.clone(),
                                TargetEnrichedTypeInfo::Class(c) => c.context.path.clone(),
                                _ => final_file,
                            };
                            final_name = Some(&ty.name);
                        } else {
                            panic!("bad qualfiied name lookup");
                        }
                    } else {
                        panic!("bad qualified name lookup");
                    }
                }

                final_name.and_then(|final_name| {
                    let final_name = final_name.clone();

                    Some(TypesByIdentByPathCell::Lookup(
                        RefCell::borrow(&self.context.types_by_ident_by_path),
                        final_file,
                        final_name,
                    ))
                })
            }
            _ => Some(TypesByIdentByPathCell::Owned(TargetEnrichedTypeInfo::Ref(
                self.clone(),
            ))),
        }
    }
}

impl ResolveTargetType for TargetEnrichedTypeInfo {
    fn resolve_target_type(&self) -> Option<TypesByIdentByPathCell> {
        match self {
            TargetEnrichedTypeInfo::Ref(r) => r.resolve_target_type(),
            _ => Some(TypesByIdentByPathCell::Direct(self)),
        }
    }
}

trait IsUninhabited {
    fn is_uninhabited(&self) -> bool;
}

impl IsUninhabited for TargetEnrichedTypeInfo {
    fn is_uninhabited(&self) -> bool {
        match self {
            TargetEnrichedTypeInfo::Ref(r) => r.is_uninhabited(),
            TargetEnrichedTypeInfo::Union(Union { types, .. }) => {
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

fn type_to_union_case_name(typ: &TargetEnrichedTypeInfo) -> Identifier {
    let t_str = quote! { #typ }
        .to_string()
        .replace("<", "Of")
        .replace(">", "")
        .replace("&", "")
        .replace("[", "")
        .replace("]", "");
    to_camel_case_ident(format!("{}Case", t_str))
}

impl ToTokens for TargetEnrichedType {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let (js_name, name) = self.name.to_name();

        let our_toks = match &self.info {
            TargetEnrichedTypeInfo::Interface(iface) => {
                let Interface { indexer, .. } = iface;
                let extended_fields = get_recursive_fields(iface);

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
                    ..
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
            TargetEnrichedTypeInfo::Enum(Enum { members, .. }) => {
                quote! {
                    #[wasm_bindgen]
                    #[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
                    pub enum #name {
                        #(#members),*
                    }
                }
            }
            TargetEnrichedTypeInfo::Alias(Alias { target, .. }) => {
                let use_path = target.to_ns_path(&self.name);

                quote! {
                    use #use_path as #name;
                }
            }
            //TargetEnrichedTypeInfo::Ref(_) => panic!("ref isn't a top-level type"),
            //TargetEnrichedTypeInfo::Array { .. } => panic!("Array isn't a top-level type"),
            //TargetEnrichedTypeInfo::Optional { .. } => panic!("Optional isn't a top-level type"),
            TargetEnrichedTypeInfo::Union(Union { types, .. }) => {
                let members = types.iter().map(|t| {
                    let case = type_to_union_case_name(t);

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

                let (undefined_members, not_undefined_members): (Vec<_>, Vec<_>) =
                    types.iter().partition(|t| match t {
                        TargetEnrichedTypeInfo::Ref(t)
                            if t.referent == TypeIdent::Builtin(Builtin::PrimitiveUndefined) =>
                        {
                            true
                        }
                        _ => false,
                    });
                let undefined_member_cases = undefined_members.iter().map(|t| {
                    let case = type_to_union_case_name(t);
                    quote! {
                        #name::#case => true,
                    }
                });

                let skip_serializing_cases = if not_undefined_members.is_empty() {
                    quote! { _ => true }
                } else if undefined_members.is_empty() {
                    quote! { _ => false }
                } else {
                    quote! {
                        #(#undefined_member_cases),*
                        _ => false,
                    }
                };

                quote! {
                    #[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
                    pub enum #name {
                        #(#members),*
                    }

                    impl ts_bindgen_rt::ShouldSkipSerializing for #name {
                        fn should_skip_serializing(&self) -> bool {
                            match self {
                                #skip_serializing_cases
                            }
                        }
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
            TargetEnrichedTypeInfo::NamespaceImport(NamespaceImport::All { src }) => {
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
            TargetEnrichedTypeInfo::NamespaceImport(NamespaceImport::Default { src }) => {
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
            TargetEnrichedTypeInfo::NamespaceImport(NamespaceImport::Named {
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

impl ToTokens for TargetEnrichedTypeInfo {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let our_toks = match &self {
            TargetEnrichedTypeInfo::Interface(_) => {
                panic!("interface in type info");
            }
            TargetEnrichedTypeInfo::Enum(_) => {
                panic!("enum in type info");
            }
            TargetEnrichedTypeInfo::Ref(TypeRef {
                referent,
                type_params: _,
                ..
            }) => {
                let (_, local_name) = referent.to_name();

                quote! {
                    #local_name
                }
            }
            TargetEnrichedTypeInfo::Alias(Alias { target, .. }) => {
                // TODO: need to get the local name for the alias (stored on the Type right now)
                let (_, local_name) = target.to_name();

                quote! {
                    #local_name
                }
            }
            TargetEnrichedTypeInfo::Array { item_type, .. } => {
                quote! {
                    Vec<#item_type>
                }
            }
            TargetEnrichedTypeInfo::Optional { item_type, .. } => {
                quote! {
                    Option<#item_type>
                }
            }
            TargetEnrichedTypeInfo::Union(Union { types: _, .. }) => {
                // TODO
                quote! {}
            }
            TargetEnrichedTypeInfo::Intersection(Intersection { types: _, .. }) => {
                // TODO
                quote! {}
            }
            TargetEnrichedTypeInfo::Mapped { value_type, .. } => {
                quote! {
                    std::collections::HashMap<String, #value_type>
                }
            }
            TargetEnrichedTypeInfo::Func(Func {
                params,
                type_params: _,
                return_type,
                ..
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
            TargetEnrichedTypeInfo::Constructor {
                params: Vec<Param>,
                return_type: Box<TypeInfo>,
            },
            TargetEnrichedTypeInfo::Class(Class {
                members: HashMap<String, Member>,
            }),
            TargetEnrichedTypeInfo::Var {
                type_info: Box<TypeInfo>,
            },
            TargetEnrichedTypeInfo::GenericType {
                name: String,
                constraint: Box<TypeInfo>,
            },*/
            TargetEnrichedTypeInfo::NamespaceImport(_) => panic!("namespace import in type info"),
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
