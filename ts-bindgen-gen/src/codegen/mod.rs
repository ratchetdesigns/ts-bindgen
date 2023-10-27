mod contextual;
mod funcs;
mod generics;
mod is_uninhabited;
mod named;
mod ns_path;
mod resolve_target_type;
mod serialization_type;
mod traits;
mod type_ref_like;

use crate::codegen::funcs::{
    fn_types, render_exposed_to_js_wrapper_closure, render_raw_return_to_js, AccessType,
    Constructor, FnPrototypeExt, HasFnPrototype, InternalFunc, PropertyAccessor, WrapperFunc,
};
use crate::codegen::generics::{
    apply_type_params, render_type_params, render_type_params_with_constraints,
    render_type_params_with_lifetimes, ResolveGeneric, TypeEnvImplying,
};
use crate::codegen::is_uninhabited::IsUninhabited;
use crate::codegen::named::{CasedTypeIdent, FnOverloadName, Named, SimpleNamed, UnionCaseName};
use crate::codegen::ns_path::ToNsPath;
use crate::codegen::resolve_target_type::ResolveTargetType;
use crate::codegen::serialization_type::{SerializationType, SerializationTypeGetter};
use crate::codegen::traits::{render_trait_defn, to_type_ref, IsTraitable, TraitName, Traitable};
use crate::codegen::type_ref_like::OwnedTypeRef;
use crate::fs::Fs;
use crate::identifier::{
    to_camel_case_ident, to_ident, to_snake_case_ident, to_unique_ident, Identifier,
};
use crate::ir::{
    Alias, Builtin, Class, Context, Ctor, Enum, EnumMember, EnumValue, Func, Indexer, Interface,
    Intersection, Member, NamespaceImport, Param, TargetEnrichedType, TargetEnrichedTypeInfo,
    Tuple, TypeIdent, TypeParamConfig, TypeRef, Union,
};
pub use crate::mod_def::ModDef;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::iter;
use std::path::{Path, PathBuf};
use syn::Token;

macro_rules! trait_impl_for_type_info {
    (match $matcher:ident,
     $invoker:path | $default:tt,
     $($case:pat => $res:expr),* $(,)?) => {
        match $matcher {
            $($case => $res),*,
            #[allow(unreachable_patterns)]
            TargetEnrichedTypeInfo::Alias(a) => a
                .resolve_target_type()
                .as_ref()
                .map($invoker)
                .unwrap_or($default),
            #[allow(unreachable_patterns)]
            TargetEnrichedTypeInfo::Ref(r) => match &r.referent {
                TypeIdent::GeneratedName { .. } => unreachable!(),
                _ => r
                    .resolve_target_type()
                    .as_ref()
                    .map($invoker)
                    .unwrap_or($default),
            },
            #[allow(unreachable_patterns)]
            TargetEnrichedTypeInfo::Optional { item_type } => $invoker(item_type.as_ref()),
            #[allow(unreachable_patterns)]
            TargetEnrichedTypeInfo::Array { item_type } => $invoker(item_type.as_ref()),
            #[allow(unreachable_patterns)]
            TargetEnrichedTypeInfo::Mapped { value_type } => $invoker(value_type.as_ref()),
            #[allow(unreachable_patterns)]
            TargetEnrichedTypeInfo::NamespaceImport(n) => n
                .resolve_target_type()
                .as_ref()
                .map($invoker)
                .unwrap_or($default),
            #[allow(unreachable_patterns)]
            TargetEnrichedTypeInfo::TypeQuery(tq) => tq
                .resolve_target_type()
                .as_ref()
                .map($invoker)
                .unwrap_or($default),
        }
    };
    (match $matcher:ident,
     $invoker:path | $default:tt,
     aggregate with $agg:ident,
     $($case:pat => $res:expr),* $(,)?) => {
        trait_impl_for_type_info!(
            match $matcher,
            $invoker | $default,
            TargetEnrichedTypeInfo::Interface(i) => i.fields.values()
                .map(ResolveTargetType::resolve_target_type)
                .chain(iter::once(
                    i.indexer.as_ref()
                        .and_then(|i| i.value_type.resolve_target_type())
                ))
                .$agg(|t| t.as_ref().map($invoker).unwrap_or($default)),
            TargetEnrichedTypeInfo::Union(Union { types, .. }) => types.iter().$agg($invoker),
            TargetEnrichedTypeInfo::Intersection(Intersection { types, .. }) => types.iter().$agg($invoker),
            TargetEnrichedTypeInfo::Tuple(Tuple { types, .. }) => types.iter().$agg($invoker),
            $($case => $res),*
        )
    };
}

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

pub struct WithFs<'a, T, FS: Fs + ?Sized> {
    pub data: &'a T,
    pub fs: &'a FS,
}

impl<'a, FS: Fs + ?Sized> ToTokens for WithFs<'a, ModDef, FS> {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let mod_def = self.data;
        let fs = self.fs;
        let mod_name = &mod_def.name;
        let types = mod_def.types.iter().map(|data| WithFs { data, fs });
        let children = mod_def.children.iter().map(|data| WithFs { data, fs });

        let imports = if mod_def.types.is_empty() {
            quote! {}
        } else {
            quote! {
                #[allow(unused)]
                use wasm_bindgen::prelude::*;

                #[allow(unused)]
                use super::*; // ts modules inherit their parent environment
            }
        };

        let our_toks = quote! {
            #[cfg(target_family = "wasm")]
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

fn get_recursive_fields(iface: &Interface) -> HashMap<String, TypeRef> {
    get_recursive_fields_with_type_params(iface, &Default::default())
}

fn get_recursive_fields_with_type_params(
    Interface {
        extends,
        fields,
        type_params: _, // TODO: should probably restrict resolve_generic_in_env to only fill in known generics
        ..
    }: &Interface,
    type_env: &HashMap<String, TypeRef>,
) -> HashMap<String, TypeRef> {
    let our_fields = fields
        .iter()
        .map(|(n, t)| (n.clone(), t.resolve_generic_in_env(type_env).into_owned()));
    let super_fields = extends
        .iter()
        .filter_map(|base| base.resolve_target_type().map(|t| (base, t)))
        .filter_map(|(base, resolved_base)| match resolved_base {
            // TODO: do we need to support non-interface super types?
            TargetEnrichedTypeInfo::Interface(iface) => Some((base, iface)),
            _ => None,
        })
        .flat_map(|(base, iface)| {
            let super_type_env = apply_type_params(base, &iface, type_env);
            get_recursive_fields_with_type_params(&iface, &super_type_env).into_iter()
        });

    our_fields.chain(super_fields).collect()
}

fn path_relative_to_cargo_toml<T: AsRef<Path>>(path: T) -> PathBuf {
    let mut best: Option<PathBuf> = None;
    let mut current_path: Option<PathBuf> = None;
    let path = path.as_ref();
    for component in path.components() {
        let p = current_path
            .map(|cp| cp.join(component))
            .unwrap_or_else(|| (component.as_ref() as &Path).to_path_buf());
        if p.is_dir() && p.join("Cargo.toml").exists() {
            best = Some(p.clone());
        }
        current_path = Some(p);
    }

    best.map(|p| path.components().skip(p.components().count()).collect())
        .unwrap_or_else(|| path.to_path_buf())
}

fn trim_after_dot(s: &str) -> &str {
    let idx = s.find('.');
    &s[0..idx.unwrap_or(s.len())]
}

fn get_field_count<T: FieldCountGetter>(t: &T) -> usize {
    t.get_field_count()
}

trait FieldCountGetter {
    fn get_field_count(&self) -> usize;
}

impl FieldCountGetter for TypeRef {
    fn get_field_count(&self) -> usize {
        self.resolve_target_type()
            .map(|t| t.get_field_count())
            .unwrap_or(0usize)
    }
}

impl FieldCountGetter for TargetEnrichedType {
    fn get_field_count(&self) -> usize {
        self.info.get_field_count()
    }
}

impl FieldCountGetter for TargetEnrichedTypeInfo {
    fn get_field_count(&self) -> usize {
        // we return the field count for things that have fields and, other than that, ensure that
        // undefined will always have the lowest field count
        let min = usize::MIN;
        trait_impl_for_type_info!(
            match self,
            get_field_count | min,
            TargetEnrichedTypeInfo::Interface(i) => {
                if i.indexer.is_some() {
                    // TODO: is this what we want????
                    usize::MAX
                } else {
                    i.fields.len()
                }
            },
            TargetEnrichedTypeInfo::Enum(e) => e.members.len(),
            TargetEnrichedTypeInfo::Ref(TypeRef {
                referent: TypeIdent::Builtin(_),
                ..
            }) => min,
            TargetEnrichedTypeInfo::Array { .. } => min,
            TargetEnrichedTypeInfo::Union(u) => {
                u.types.iter().map(get_field_count).max().unwrap_or(min)
            },
            TargetEnrichedTypeInfo::Intersection(i) => {
                i.types.iter().map(get_field_count).min().unwrap_or(min)
            },
            TargetEnrichedTypeInfo::Tuple(t) => t.types.len(),
            TargetEnrichedTypeInfo::Mapped { .. } => usize::MAX,
            TargetEnrichedTypeInfo::FuncGroup(_) => min,
            TargetEnrichedTypeInfo::Constructor(_) => min,
            TargetEnrichedTypeInfo::Class(c) => {
                c.members.len()
                    + c.super_class
                        .as_ref()
                        .and_then(|s| s.resolve_target_type())
                        .as_ref()
                        .map(|t| t.get_field_count())
                        .unwrap_or(0)
            },
            TargetEnrichedTypeInfo::Var { .. } => min,
        )
    }
}

trait MemberContainer {
    fn undefined_and_standard_members(&self) -> (Vec<&TypeRef>, Vec<&TypeRef>);

    fn has_undefined_member(&self) -> bool {
        !self.undefined_and_standard_members().0.is_empty()
    }
}

impl MemberContainer for Union {
    fn undefined_and_standard_members(&self) -> (Vec<&TypeRef>, Vec<&TypeRef>) {
        self.types.iter().partition(|t| {
            matches!(
                t.resolve_target_type(),
                Some(TargetEnrichedTypeInfo::Ref(t))
                    if t.referent == TypeIdent::Builtin(Builtin::PrimitiveUndefined)
            )
        })
    }
}

trait JsModulePath {
    fn js_module_path(&self) -> String;
}

impl JsModulePath for Context {
    fn js_module_path(&self) -> String {
        let path = &self.path;
        let path = path_relative_to_cargo_toml(path.with_file_name(
            trim_after_dot(&path.file_name().unwrap().to_string_lossy()).to_string() + ".js",
        ));
        path.to_string_lossy().to_string()
    }
}

fn to_internal_class_name(name: &Identifier) -> Identifier {
    name.suffix_name("_Class")
}

impl<'a, FS: Fs + ?Sized> ToTokens for WithFs<'a, TargetEnrichedType, FS> {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let WithFs { data: typ, fs } = self;
        let type_name = &typ.name;
        let is_exported = typ.is_exported;
        let cased_type_name = CasedTypeIdent {
            type_ident: type_name,
            type_info: &typ.info,
        };
        let (js_name, name) = cased_type_name.to_name();
        let vis = if is_exported {
            let vis = format_ident!("pub");
            quote! { #vis }
        } else {
            quote! {}
        };

        let our_toks = match &typ.info {
            TargetEnrichedTypeInfo::Interface(iface) => {
                let Interface {
                    indexer,
                    constructor: _, // nothing to render for an interface ctor
                    type_params,
                    ..
                } = iface;
                let extended_fields = get_recursive_fields(iface);

                let full_type_params = render_type_params(type_params);
                let mut field_toks = extended_fields
                    .iter()
                    .map(|(js_field_name, typ)| {
                        let field = FieldDefinition {
                            self_name: &name,
                            js_field_name,
                            typ,
                            type_params: &type_params.iter().map(|(k, v)| (k.clone(), v)).collect(),
                        };
                        quote! { #field }
                    })
                    .collect::<Vec<TokenStream2>>();

                let serializers: Vec<_> = extended_fields
                    .iter()
                    .filter_map(|(js_field_name, typ)| {
                        typ.resolve_target_type()
                            .map(|t| (to_snake_case_ident(js_field_name), t))
                    })
                    .filter_map(|(field_name, typ)| render_serialize_fn(&field_name, &typ))
                    .collect();
                let deserializers: Vec<_> = extended_fields
                    .iter()
                    .filter_map(|(js_field_name, typ)| {
                        typ.resolve_target_type()
                            .map(|t| (to_snake_case_ident(js_field_name), t))
                    })
                    .filter_map(|(field_name, typ)| render_deserialize_fn(&field_name, &typ))
                    .collect();
                let serializer_impl = if serializers.is_empty() && deserializers.is_empty() {
                    quote! {}
                } else {
                    quote! {
                        impl #name {
                            #(#serializers)*
                            #(#deserializers)*
                        }
                    }
                };

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
                let trait_defn = render_trait_defn(
                    &name,
                    type_name,
                    type_params,
                    is_exported,
                    iface,
                    &iface.context,
                );

                quote! {
                    #[derive(Clone, serde::Serialize, serde::Deserialize)]
                    pub struct #name #full_type_params {
                        #(#field_toks),*
                    }

                    #trait_defn

                    #serializer_impl
                }
            }
            TargetEnrichedTypeInfo::Enum(Enum { members, .. }) => {
                quote! {
                    #[wasm_bindgen]
                    #[derive(Clone, serde::Serialize, serde::Deserialize)]
                    #[serde(untagged)]
                    pub enum #name {
                        #(#members),*
                    }
                }
            }
            TargetEnrichedTypeInfo::Alias(Alias {
                target,
                type_params,
                ..
            }) => {
                let tps = render_type_params(type_params);

                let is_class = target
                    .resolve_target_type()
                    .map(|t| matches!(t, TargetEnrichedTypeInfo::Class(_)))
                    .unwrap_or(false);

                // would like to alias traits here if
                // target.resolve_target_type().map(IsTraitable::is_traitable)
                // but trait aliases are useless.

                let class_alias = if is_class {
                    let target_class_name = to_internal_class_name(&target.to_name().1);
                    let class_name = to_internal_class_name(&name);
                    quote! {
                        #[allow(dead_code, non_camel_case_types)]
                        #vis type #class_name = #target_class_name;
                    }
                } else {
                    quote! {}
                };

                if matches!(type_name, TypeIdent::DefaultExport(_)) {
                    // we don't emit default export aliases. we look them up
                    // when referenced.
                    quote! {}
                } else {
                    quote! {
                        #[allow(dead_code, non_camel_case_types)]
                        #vis type #name #tps = #target;

                        #class_alias
                    }
                }
            }
            TargetEnrichedTypeInfo::Union(u) => {
                let (undefined_members, mut not_undefined_members) =
                    u.undefined_and_standard_members();

                // members must be sorted in order of decreasing number of fields to ensure that we
                // deserialize unions into the "larger" variant in case of overlaps
                not_undefined_members.sort_by_key(|m| get_field_count(*m));
                not_undefined_members.reverse();
                let member_cases = not_undefined_members
                    .iter()
                    .map(|t| {
                        let case = t.union_case_name();
                        let attrs = match t.serialization_type() {
                            SerializationType::JsValue => {
                                quote! {
                                    #[serde(
                                        serialize_with = "ts_bindgen_rt::serialize_jsvalue",
                                        deserialize_with = "ts_bindgen_rt::deserialize_as_jsvalue")
                                    ]
                                }
                            },
                            SerializationType::Fn => {
                                let case = case.to_snake_case();
                                let serialize_name = serialize_field_name(&case).in_namespace(&name).to_string();
                                let deserialize_name = deserialize_field_name(&case).in_namespace(&name).to_string();
                                quote! {
                                    #[serde(
                                        serialize_with = #serialize_name,
                                        deserialize_with = #deserialize_name)
                                    ]
                                }
                            },
                            _ => Default::default(),
                        };

                        if t.is_uninhabited() {
                            (
                                case.clone(),
                                quote! {
                                    #attrs
                                    #case
                                },
                            )
                        } else {
                            let t = OwnedTypeInfo {
                                type_info: &TargetEnrichedTypeInfo::Ref((*t).clone()),
                            };
                            (
                                case.clone(),
                                quote! {
                                    #attrs
                                    #case(#t)
                                },
                            )
                        }
                    })
                    .chain(undefined_members.iter().map(|t| {
                        let case = t.union_case_name();

                        (
                            case.clone(),
                            quote! {
                                #[serde(serialize_with="ts_bindgen_rt::serialize_undefined", deserialize_with="ts_bindgen_rt::deserialize_undefined")]
                                #case
                            },
                        )
                    }))
                    .fold(
                        (HashSet::new(), Vec::new()),
                        |(mut names, mut cases), (name, case)| {
                            // de-dupe names
                            if names.insert(name) {
                                cases.push(case);
                            }
                            (names, cases)
                        }
                    ).1;

                let impl_fns: Vec<_> = not_undefined_members
                    .iter()
                    .filter_map(|t| {
                        let case = t.union_case_name().to_snake_case();
                        let typ = t.resolve_target_type()?;
                        let serialize_fn = render_serialize_fn(&case, &typ);
                        let deserialize_fn = render_deserialize_fn(&case, &typ);
                        serialize_fn.zip(deserialize_fn).map(|(s, d)| {
                            quote! {
                                #s
                                #d
                            }
                        })
                    })
                    .collect();
                let impls = if impl_fns.is_empty() {
                    Default::default()
                } else {
                    quote! {
                        impl #name {
                            #(#impl_fns)*
                        }
                    }
                };

                quote! {
                    #[derive(Clone, serde::Serialize, serde::Deserialize)]
                    #[serde(untagged)]
                    pub enum #name {
                        #(#member_cases),*
                    }

                    #impls
                }
            }
            TargetEnrichedTypeInfo::Tuple(Tuple { types, .. }) => {
                quote! {
                    #[derive(Clone, serde::Serialize, serde::Deserialize)]
                    pub struct #name(#(pub #types),*);
                }
            }
            TargetEnrichedTypeInfo::FuncGroup(func_group) => {
                let path = func_group.context.js_module_path();
                let common_attrs = vec![quote! { js_name = #js_name, catch }];
                let is_overloaded = func_group.overloads.len() > 1;
                let funcs = func_group.overloads.iter().map(|func| {
                    let attrs = {
                        let mut attrs = common_attrs.clone();
                        if func.is_variadic() {
                            attrs.push(quote! { variadic });
                        }
                        attrs
                    };
                    let internal_func = InternalFunc {
                        js_name,
                        func,
                        in_context: &None,
                    };
                    let wrapper_func = WrapperFunc {
                        js_name,
                        func,
                        is_overloaded,
                    };

                    quote! {
                        #[wasm_bindgen(module=#path)]
                        extern "C" {
                            #[allow(non_snake_case)]
                            #[wasm_bindgen(#(#attrs),*)]
                            #internal_func
                        }

                        #wrapper_func
                    }
                });

                quote! {
                    #(#funcs)*
                }
            }
            TargetEnrichedTypeInfo::Class(class) => {
                let Class {
                    super_class: _,
                    members,
                    context,
                    type_params,
                    implements: _,
                } = class;
                let internal_class_name = to_internal_class_name(&name);
                let full_type_params = render_type_params(type_params);
                let full_type_params_deserializable = render_type_params_with_constraints(
                    type_params,
                    &[
                        quote! { serde::ser::Serialize },
                        quote! { serde::de::DeserializeOwned },
                    ],
                );
                let wrapper_struct_members = if type_params.is_empty() {
                    vec![quote! { pub #internal_class_name }]
                } else {
                    vec![
                        quote! { pub #internal_class_name },
                        quote! { pub std::marker::PhantomData #full_type_params },
                    ]
                };
                let extra_args = if type_params.is_empty() {
                    Default::default()
                } else {
                    quote! { std::marker::PhantomData }
                };
                let wrapper_from_internal_args = vec![quote! { internal }, extra_args.clone()];
                let type_params_with_de_lifetime =
                    render_type_params_with_lifetimes(type_params, &["de"]);
                let type_params_with_a_lifetime =
                    render_type_params_with_lifetimes(type_params, &["a"]);
                let path = context.js_module_path();
                let class_ref = to_type_ref(type_name, type_params, &class.context);
                let parent_classes = || {
                    class
                        .recursive_super_traits(class_ref.clone(), &class_ref.type_env())
                        .map(|s| s.implementor)
                        .collect::<HashSet<_>>()
                        .into_iter()
                };
                let super_as_ref_impls = parent_classes()
                   .map(|super_ref| {
                        let ns = super_ref.to_ns_path(*fs, type_name);
                        let (_, super_name) = super_ref.to_name();
                        let super_name = if let TypeIdent::LocalName(_) = super_ref.referent {
                            // TODO: get rid of local names and make everything namespaced to avoid
                            // special handling like this
                            super_name
                        } else {
                            super_name.in_namespace_parts(&ns)
                        };
                        let super_name_without_tps = super_name.without_type_params();
                        let internal_super_name = to_internal_class_name(&super_name_without_tps);
                        let super_wrapper_from_src_args = if super_ref.type_params.is_empty() {
                            vec![quote! { src.clone() }]
                        } else {
                            vec![quote! { src.clone() }, quote! { std::marker::PhantomData }]
                        };
                        quote! {
                            impl #full_type_params std::convert::From<&#name #full_type_params> for #super_name {
                                fn from(src: &#name #full_type_params) -> #super_name {
                                    let src: &#internal_super_name = src.0.as_ref();
                                    #super_name_without_tps(#(#super_wrapper_from_src_args),*)
                                }
                            }

                            impl #full_type_params std::convert::From<&mut #name #full_type_params> for #super_name {
                                fn from(src: &mut #name #full_type_params) -> #super_name {
                                    let src: &#internal_super_name = src.0.as_ref();
                                    #super_name_without_tps(#(#super_wrapper_from_src_args),*)
                                }
                            }
                        }
                    });
                let attrs = iter::once(quote! { js_name = #js_name }).chain(
                    parent_classes()
                        .filter(|super_ref| {
                            // only get proper super classes
                            super_ref.referent != class_ref.referent
                        })
                        .map(|super_ref| {
                            let (_, super_name) = super_ref.to_name();
                            let super_name_without_tps = super_name.without_type_params();
                            let internal_super_name =
                                to_internal_class_name(&super_name_without_tps);

                            quote! {
                                extends = #internal_super_name
                            }
                        }),
                );
                let type_env: HashMap<_, _> = type_params
                    .iter()
                    .map(|(n, _)| {
                        (
                            n.clone(),
                            TypeRef {
                                referent: TypeIdent::Builtin(Builtin::PrimitiveAny),
                                type_params: Default::default(),
                                context: context.clone(),
                            },
                        )
                    })
                    .collect();

                let target = quote! { self.0 };
                let ctor_name = |overloads: &[Ctor], ctor: &Constructor| {
                    let is_overloaded = overloads.len() > 1;
                    let fn_group_name = to_snake_case_ident("new");
                    if is_overloaded {
                        ctor.overload_name(&fn_group_name)
                    } else {
                        fn_group_name
                    }
                };
                let default_ctor_name = members.iter().find_map(|(_, member)| {
                    if let Member::Constructor(ctor) = member {
                        ctor.overloads
                            .iter()
                            .find(|o| o.params.is_empty())
                            .map(|o| {
                                let c = Constructor::new(
                                    Cow::Borrowed(o),
                                    TypeIdent::LocalName(js_name.to_string()),
                                );
                                ctor_name(&ctor.overloads, &c)
                            })
                    } else {
                        None
                    }
                });
                let default_impl = default_ctor_name
                    .map(|default_ctor_name| {
                        quote! {
                            impl #full_type_params std::default::Default for #name #full_type_params {
                                fn default() -> Self {
                                    Self::#default_ctor_name()
                                }
                            }
                        }
                    })
                    .unwrap_or_default();
                let (member_defs, public_methods): (Vec<TokenStream2>, Vec<TokenStream2>) = members.iter()
                    .flat_map(|(member_js_name, member)| {
                        let member_js_ident = format_ident!("{}", member_js_name);
                        match member {
                            Member::Constructor(ctor) => {
                                let overloads = &ctor.overloads;
                                overloads
                                    .iter()
                                    .map(|ctor| {
                                        let ctor = ctor.resolve_generic_in_env(&type_env);
                                        let ctor = Constructor::new(
                                            ctor,
                                            TypeIdent::LocalName(js_name.to_string()),
                                        );
                                        let param_toks = ctor
                                            .params()
                                            .map(|p| p.as_exposed_to_js_named_param_list(None));

                                        let fn_name = ctor_name(overloads, &ctor);

                                        let member_def = quote! {
                                            #[wasm_bindgen(constructor, js_class = #js_name)]
                                            pub fn #fn_name(#(#param_toks),*) -> #internal_class_name;
                                        };
                                        let fq_internal_ctor = fn_name.in_namespace(&internal_class_name);

                                        let res_converter = |res: TokenStream2| -> TokenStream2 {
                                            let args = if type_params.is_empty() {
                                                vec![quote! { #res }]
                                            } else {
                                                vec![
                                                    quote! { #res },
                                                    quote! { std::marker::PhantomData },
                                                ]
                                            };
                                            quote! {
                                                #name(#(#args),*)
                                            }
                                        };
                                        let pub_fn = ctor.exposed_to_rust_generic_wrapper_fn(&fn_name, None, &fq_internal_ctor, false, Some(&res_converter), &type_env, None);

                                        (member_def, pub_fn)
                                    })
                                    .collect()
                            }
                            Member::Method(func) => {
                                let is_overloaded = func.overloads.len() > 1;
                                func.overloads
                                    .iter()
                                    .map(|func| {
                                        let func = {
                                            let mut func = func.clone();
                                            func.class_name = func.class_name.map(|_| TypeIdent::ExactName(internal_class_name.to_string()));
                                            func
                                        };
                                        let func = func.resolve_generic_in_env(&type_env);
                                        let in_context = None;
                                        let internal = InternalFunc {
                                            func: &func,
                                            js_name: member_js_name,
                                            in_context: &in_context,
                                        };
                                        let fn_name = internal.to_internal_rust_name();

                                        let f = func.exposed_to_js_fn_decl(fn_name, in_context);

                                        let mut attrs = vec![
                                            quote! {js_name = #member_js_ident},
                                            quote! {method},
                                            quote! {js_class = #js_name},
                                            quote! {catch},
                                        ];
                                        if func.is_variadic() {
                                            attrs.push(quote! { variadic });
                                        }

                                        let member_def = quote! {
                                            #[allow(non_snake_case)]
                                            #[wasm_bindgen(#(#attrs),*)]
                                            #f;
                                        };

                                        let rc: Option<&fn(TokenStream2) -> TokenStream2> = None;
                                        let in_context = None;
                                        let internal = InternalFunc {
                                            func: func.as_ref(),
                                            js_name: member_js_name,
                                            in_context: &in_context,
                                        };
                                        let internal_fn_name = internal.to_internal_rust_name();
                                        let fn_group_name = to_snake_case_ident(member_js_name);
                                        let fn_name = if is_overloaded {
                                            func.overload_name(&fn_group_name)
                                        } else {
                                            fn_group_name
                                        };
                                        let pub_fn = func.exposed_to_rust_generic_wrapper_fn(&fn_name, Some(&target), &internal_fn_name, true, rc, &type_env, in_context);

                                        (member_def, pub_fn)
                                    })
                                    .collect()
                            }
                            Member::Property(typ) => {
                                let resolved_type = typ.resolve_generic_in_env(&type_env).into_owned();
                                let member_name = to_snake_case_ident(member_js_name);
                                let setter_name = member_name.prefix_name("set_");

                                let (internal_getter_name, internal_setter_name) = if resolved_type.serialization_type() == SerializationType::Raw {
                                    (member_name.clone(), setter_name.clone())
                                } else {
                                    (
                                        InternalFunc::to_internal_rust_ident(&member_name),
                                        InternalFunc::to_internal_rust_ident(&setter_name),
                                    )
                                };

                                let internal_getter = PropertyAccessor {
                                    property_name: member_name.clone(),
                                    typ: resolved_type.clone(),
                                    class_name: TypeIdent::ExactName(internal_class_name.to_string()),
                                    access_type: AccessType::Getter,
                                };

                                let internal_setter = PropertyAccessor {
                                    property_name: member_name.clone(),
                                    typ: resolved_type,
                                    class_name: TypeIdent::ExactName(internal_class_name.to_string()),
                                    access_type: AccessType::Setter,
                                };


                                let member_getter = internal_getter.exposed_to_js_fn_decl(&internal_getter_name, None);
                                let member_setter = internal_setter.exposed_to_js_fn_decl(&internal_setter_name, None);
                                let member_def = quote! {
                                    #[wasm_bindgen(method, structural, catch, getter = #member_js_ident, js_class = #js_name)]
                                    #member_getter;

                                    #[wasm_bindgen(method, structural, catch, setter = #member_js_ident, js_class = #js_name)]
                                    #member_setter;
                                };

                                let getter = PropertyAccessor {
                                    property_name: member_name.clone(),
                                    typ: typ.clone(),
                                    class_name: type_name.clone(),
                                    access_type: AccessType::Getter,
                                }.getter_fn();

                                let setter = PropertyAccessor {
                                    property_name: member_name,
                                    typ: typ.clone(),
                                    class_name: type_name.clone(),
                                    access_type: AccessType::Setter,
                                }.setter_fn();

                                let rc: Option<&fn(TokenStream2) -> TokenStream2> = None;
                                let getter_fn = getter.exposed_to_rust_generic_wrapper_fn(&to_snake_case_ident(member_js_name), Some(&target), &internal_getter_name, true, rc, &type_env, None);
                                let setter_fn = setter.exposed_to_rust_generic_wrapper_fn(&setter_name, Some(&target), &internal_setter_name, true, rc, &type_env, None);

                                let pub_fn = quote! {
                                    #getter_fn

                                    #setter_fn
                                };

                                vec![(member_def, pub_fn)]
                            }
                        }
                    })
                    .unzip();

                let trait_defn = render_trait_defn(
                    &name,
                    type_name,
                    type_params,
                    is_exported,
                    class,
                    &class.context,
                );

                quote! {
                    #[wasm_bindgen(module = #path)]
                    extern "C" {
                        #[allow(non_camel_case_types)]
                        #[wasm_bindgen(#(#attrs),*)]
                        #vis type #internal_class_name;

                        #(#member_defs)*
                    }

                    #[derive(std::clone::Clone)]
                    #vis struct #name #full_type_params(#(#wrapper_struct_members),*);

                    #(#super_as_ref_impls)*

                    #default_impl

                    impl #full_type_params std::convert::From<#name #full_type_params> for JsValue {
                        fn from(src: #name #full_type_params) -> JsValue {
                            JsValue::from(src.0)
                        }
                    }

                    impl #full_type_params std::convert::AsRef<JsValue> for #name #full_type_params {
                        fn as_ref(&self) -> &JsValue {
                            self.0.as_ref()
                        }
                    }

                    impl #full_type_params wasm_bindgen::JsCast for #name #full_type_params {
                        fn instanceof(val: &JsValue) -> bool {
                            #internal_class_name::instanceof(val)
                        }
                        fn unchecked_from_js(val: JsValue) -> Self {
                            #name(#internal_class_name::unchecked_from_js(val), #extra_args)
                        }
                        fn unchecked_from_js_ref(val: &JsValue) -> &Self {
                            unsafe {
                                &*(#internal_class_name::unchecked_from_js_ref(val) as *const #internal_class_name as *const Self)
                            }
                        }
                    }

                    impl #full_type_params_deserializable #name #full_type_params {
                        #(#public_methods)*
                    }

                    impl #full_type_params wasm_bindgen::describe::WasmDescribe for #name #full_type_params {
                        fn describe() {
                            <#internal_class_name as wasm_bindgen::describe::WasmDescribe>::describe()
                        }
                    }

                    impl #full_type_params wasm_bindgen::convert::IntoWasmAbi for #name #full_type_params {
                        type Abi = <#internal_class_name as wasm_bindgen::convert::IntoWasmAbi>::Abi;
                        fn into_abi(self) -> Self::Abi {
                            wasm_bindgen::convert::IntoWasmAbi::into_abi(self.0)
                        }
                    }

                    impl #full_type_params wasm_bindgen::convert::FromWasmAbi for #name #full_type_params {
                        type Abi = <#internal_class_name as wasm_bindgen::convert::FromWasmAbi>::Abi;
                        unsafe fn from_abi(js: Self::Abi) -> Self {
                            #name(wasm_bindgen::convert::FromWasmAbi::from_abi(js), #extra_args)
                        }
                    }

                    impl #type_params_with_a_lifetime wasm_bindgen::convert::IntoWasmAbi for &'a #name #full_type_params {
                        type Abi = <&'a #internal_class_name as wasm_bindgen::convert::IntoWasmAbi>::Abi;
                        fn into_abi(self) -> Self::Abi {
                            wasm_bindgen::convert::IntoWasmAbi::into_abi(&self.0)
                        }
                    }

                    impl #full_type_params serde::ser::Serialize for #name #full_type_params {
                        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
                        where
                            S: serde::ser::Serializer,
                        {
                            serde::ser::Serialize::serialize(&self.0, serializer)
                        }
                    }

                    impl #type_params_with_de_lifetime serde::de::Deserialize<'de> for #name #full_type_params {
                        fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
                        where
                            D: serde::de::Deserializer<'de>,
                        {
                            let internal: #internal_class_name = <#internal_class_name as serde::de::Deserialize>::deserialize(deserializer)?;
                            std::result::Result::Ok(Self(#(#wrapper_from_internal_args),*))
                        }
                    }

                    #trait_defn

                    impl std::clone::Clone for #internal_class_name {
                        fn clone(&self) -> Self {
                            Self { obj: std::clone::Clone::clone(&self.obj) }
                        }
                    }

                    impl serde::ser::Serialize for #internal_class_name {
                        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
                        where
                            S: serde::ser::Serializer,
                        {
                            ts_bindgen_rt::serialize_as_jsvalue(serializer, self)
                        }
                    }

                    impl<'de> serde::de::Deserialize<'de> for #internal_class_name {
                        fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
                        where
                            D: serde::de::Deserializer<'de>,
                        {
                            ts_bindgen_rt::deserialize_as_jsvalue(deserializer)
                        }
                    }
                }
            }
            TargetEnrichedTypeInfo::Intersection(isect) => {
                if let Some(first_type) = isect.types.first().and_then(|t| t.resolve_target_type())
                {
                    // TODO: we should support classes too
                    if let TargetEnrichedTypeInfo::Interface(_) = first_type {
                        let interface_types = || {
                            isect
                                .types
                                .iter()
                                .filter_map(|t| t.resolve_target_type())
                                .filter_map(|t| {
                                    if let TargetEnrichedTypeInfo::Interface(iface) = t {
                                        Some(iface)
                                    } else {
                                        None
                                    }
                                })
                        };
                        let fields = interface_types()
                            .flat_map(|iface| get_recursive_fields(&iface))
                            .collect();

                        let indexer = interface_types().filter_map(|iface| iface.indexer).next();

                        let constructor = interface_types()
                            .filter_map(|iface| iface.constructor)
                            .next();

                        let typ = TargetEnrichedType {
                            name: type_name.clone(),
                            is_exported,
                            info: TargetEnrichedTypeInfo::Interface(Interface {
                                indexer,
                                fields,
                                constructor,
                                extends: Default::default(),
                                context: isect.context.clone(),
                                type_params: Default::default(), // TODO: copy over type params from isect
                            }),
                            context: isect.context.clone(),
                        };
                        let typ = WithFs {
                            data: &typ,
                            fs: *fs,
                        };

                        quote! {
                            #typ
                        }
                    } else {
                        // TODO: this is weird, do we ever run into trouble with this?
                        let mut typ = (*typ).clone();
                        typ.info = first_type;
                        let typ = WithFs {
                            data: &typ,
                            fs: *fs,
                        };
                        quote! {
                            #typ
                        }
                    }
                } else {
                    panic!("Intersections must not be empty");
                }
            }
            TargetEnrichedTypeInfo::NamespaceImport(NamespaceImport::All { src, .. }) => {
                let ns = src.as_path().to_ns_path(*fs, type_name);
                if ns.len() == 1 {
                    // we already have the module defined locally
                    //
                    // this doesn't work:
                    // mod abc;
                    // use abc;
                    quote! {}
                } else {
                    let name = to_snake_case_ident(js_name);

                    quote! {
                        #vis use #(#ns)::* as #name;
                    }
                }
            }
            TargetEnrichedTypeInfo::NamespaceImport(import) => {
                let (src, import_name) = match import {
                    NamespaceImport::Default { src, context } => {
                        // we generally can't rely on a type default = ...
                        // type alias because we can't use type aliases
                        // as a trait, struct, etc.
                        // so we need to check if our default name is an
                        // alias and resolve it ourselves
                        let import_name = context
                            .types_by_ident_by_path
                            .borrow()
                            .get(src)
                            .and_then(|types_by_ident| {
                                types_by_ident.get(&TypeIdent::DefaultExport(src.clone()))
                            })
                            .map(|t| {
                                if let TargetEnrichedTypeInfo::Alias(a) = &t.info {
                                    a.target.to_name().1
                                } else {
                                    to_ident("default")
                                }
                            })
                            .unwrap_or_else(|| to_ident("default"));
                        (src, import_name)
                    }
                    NamespaceImport::Named {
                        src,
                        name: item_name,
                        context,
                    } => {
                        let tr = TypeRef {
                            referent: TypeIdent::Name {
                                file: src.to_path_buf(),
                                name: item_name.clone(),
                            },
                            type_params: Default::default(),
                            context: context.clone(),
                        };
                        (src, tr.to_name().1)
                    }
                    NamespaceImport::All { .. } => {
                        // handled above
                        unreachable!();
                    }
                };

                let ns = src.as_path().to_ns_path(*fs, type_name);
                let vis = if is_exported {
                    let vis = format_ident!("pub");
                    quote! { #vis }
                } else {
                    quote! {}
                };

                let (is_traitable, is_class) = import
                    .resolve_target_type()
                    .map(|t| {
                        (
                            t.is_traitable(),
                            matches!(t, TargetEnrichedTypeInfo::Class(_)),
                        )
                    })
                    .unwrap_or((false, false));

                let trait_import = if is_traitable {
                    // traits can't be referenced through an alias so we need
                    // to get the actual trait name
                    let trait_name = import_name.trait_name();
                    let imported_trait_name = name.trait_name();
                    quote! {
                        #[allow(unused, non_camel_case_types)]
                        #vis use #(#ns)::* ::#trait_name as #imported_trait_name;
                    }
                } else {
                    quote! {}
                };

                let class_import = if is_class {
                    let cls_name = to_internal_class_name(&import_name);
                    let imported_cls_name = to_internal_class_name(&name);
                    quote! {
                        #[allow(unused, non_camel_case_types)]
                        #vis use #(#ns)::* ::#cls_name as #imported_cls_name;
                    }
                } else {
                    quote! {}
                };

                quote! {
                    #[allow(unused)]
                    #vis use #(#ns)::* ::#import_name as #name;
                    #trait_import
                    #class_import
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
            TargetEnrichedTypeInfo::Ref(type_ref) => {
                let local_name = type_ref.to_simple_name();
                quote! {
                    #local_name
                }
            }
            TargetEnrichedTypeInfo::Alias(Alias { target, .. }) => {
                // TODO: we should get the name of the alias, not the pointed-to name
                quote! { #target }
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
            TargetEnrichedTypeInfo::FuncGroup(fg) => {
                let f = &fg.widened_fn;

                quote! {
                    #f
                }
            }
            TargetEnrichedTypeInfo::NamespaceImport(_) => panic!("namespace import in type info"),
            _ => {
                quote! {}
            }
        };

        toks.append_all(our_toks);
    }
}

fn func_to_tokens(params: &[Param], return_type: &TypeRef) -> TokenStream2 {
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
        fn(#(#param_toks),*) -> #return_type
    }
}

impl ToTokens for Func {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        toks.append_all(func_to_tokens(&self.params, self.return_type.as_ref()));
    }
}

impl ToTokens for Constructor<'_> {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        toks.append_all(func_to_tokens(&self.ctor.params, self.class.as_ref()));
    }
}

impl ToTokens for TypeRef {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let our_toks = {
            if matches!(&self.referent, TypeIdent::Builtin(Builtin::Fn)) {
                let name = self.to_simple_name();
                let params = self
                    .params()
                    .map(|p| p.as_exposed_to_rust_unnamed_param_list(Some(&self.context)));
                let ret = fn_types::exposed_to_rust_return_type(
                    &self.return_type(),
                    true,
                    Some(&self.context),
                );
                quote! {
                    dyn #name(#(#params),*) -> #ret
                }
            } else if matches!(&self.referent, TypeIdent::Builtin(Builtin::PrimitiveVoid)) {
                quote! { () }
            } else {
                let (_, name) = self
                    .to_rel_qualified_name(self.context.fs.as_ref(), &self.context.base_namespace);
                quote! { #name }
            }
        };

        toks.append_all(our_toks);
    }
}

struct FieldDefinition<'a> {
    self_name: &'a Identifier,
    js_field_name: &'a str,
    typ: &'a TypeRef,
    type_params: &'a HashMap<String, &'a TypeParamConfig>,
}

fn serialize_field_name(field_name: &Identifier) -> Identifier {
    field_name.prefix_name("__TSB__serialize_")
}

fn deserialize_field_name(field_name: &Identifier) -> Identifier {
    field_name.prefix_name("__TSB__deserialize_")
}

impl<'a> ToTokens for FieldDefinition<'a> {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let js_field_name = self.js_field_name;
        let field_name = to_snake_case_ident(js_field_name);
        let typ = self.typ;
        let type_name = typ.to_simple_name().to_string();
        let type_param = self.type_params.get(&type_name);
        let mut serde_attrs = vec![quote! { rename = #js_field_name }];
        if type_param.is_some() {
            let bound = format!(
                "{}: Clone + serde::Serialize + serde::Deserialize<'de>",
                &type_name
            );
            let attr = quote! {
                bound(deserialize = #bound)
            };
            serde_attrs.push(attr);
        }
        let rendered_type = OwnedTypeRef(Cow::Borrowed(typ));

        if typ.serialization_type() == SerializationType::Fn {
            let serialize_fn = serialize_field_name(&field_name);
            let deserialize_fn = deserialize_field_name(&field_name);
            let serialize_fn = format!("{}::{}", self.self_name, serialize_fn);
            let deserialize_fn = format!("{}::{}", self.self_name, deserialize_fn);
            serde_attrs.push(quote! {
                serialize_with = #serialize_fn
            });
            serde_attrs.push(quote! {
                deserialize_with = #deserialize_fn
            });
        };

        let our_toks = quote! {
            #[serde(#(#serde_attrs),*)]
            pub #field_name: #rendered_type
        };

        toks.append_all(our_toks);
    }
}

struct OwnedTypeInfo<'a> {
    type_info: &'a TargetEnrichedTypeInfo,
}

impl<'a> ToTokens for OwnedTypeInfo<'a> {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        match self.type_info {
            TargetEnrichedTypeInfo::FuncGroup(f) => {
                let tr = OwnedTypeRef(Cow::Owned(f.widened_fn.clone().into()));
                tr.to_tokens(toks);
            }
            TargetEnrichedTypeInfo::Ref(r) => {
                let tr = OwnedTypeRef(Cow::Borrowed(r));
                tr.to_tokens(toks);
            }
            _ => {
                self.type_info.to_tokens(toks);
            }
        };
    }
}

impl From<Func> for TypeRef {
    fn from(src: Func) -> TypeRef {
        TypeRef {
            referent: TypeIdent::Builtin(Builtin::Fn),
            type_params: src
                .params
                .into_iter()
                .map(|p| {
                    if p.is_variadic {
                        TypeRef {
                            referent: TypeIdent::Builtin(Builtin::Variadic),
                            type_params: vec![p.type_info],
                            context: src.context.clone(),
                        }
                    } else {
                        p.type_info
                    }
                })
                .chain(std::iter::once(*src.return_type))
                .collect(),
            context: src.context.clone(),
        }
    }
}

impl ToTokens for EnumValue {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let our_toks = match self {
            EnumValue::Str(s) => quote! { #s },
            EnumValue::Num(n) => quote! { #n },
        };

        toks.append_all(our_toks);
    }
}

fn render_deserialize_fn(
    field_name: &Identifier,
    type_info: &TargetEnrichedTypeInfo,
) -> Option<TokenStream2> {
    let (f, rendered_type) = match type_info {
        TargetEnrichedTypeInfo::Ref(
            tr @ TypeRef {
                referent: TypeIdent::Builtin(Builtin::Fn),
                ..
            },
        ) => {
            let rendered_type = OwnedTypeRef(Cow::Borrowed(tr));
            (tr as &dyn HasFnPrototype, quote! { #rendered_type })
        }
        TargetEnrichedTypeInfo::FuncGroup(fg) => {
            let rendered_type = OwnedTypeInfo { type_info };
            (
                &fg.widened_fn as &dyn HasFnPrototype,
                quote! { #rendered_type },
            )
        }
        _ => {
            return None;
        }
    };
    let deserialize_fn_name = deserialize_field_name(field_name);
    let return_type = f.return_type();
    let return_value = quote! { ret };
    let ret = render_raw_return_to_js(&return_type, &return_value);
    let args = quote! { _Args };
    let params = f
        .params()
        .map(|p| p.as_exposed_to_rust_named_param_list(None));
    // TODO: need to render wrappers for fn params, used in rust_to_jsvalue_conversion
    let conversions = f.args().map(|p| {
        let name = p.rust_name();
        let conv = p.rust_to_jsvalue_conversion(None);
        quote! {
            let #name = #conv;
        }
    });
    let pushes = f.params().map(|p| {
        let name = p.rust_name();
        if p.is_variadic() {
            quote! {
                for #name in #name.into_iter(){
                    #args.push(&#name);
                }
            }
        } else {
            quote! {
                #args.push(&#name);
            }
        }
    });
    // TODO: do we need to handle member functions here (first arg to apply may be
    // non-null)
    Some(quote! {
        #[allow(non_snake_case)]
        fn #deserialize_fn_name<'de, D>(deserializer: D) -> std::result::Result<#rendered_type, D::Error>
        where
            D: serde::de::Deserializer<'de>,
        {
            let jsv: JsValue = ts_bindgen_rt::deserialize_as_jsvalue(deserializer)?;
            let #field_name: Option<&js_sys::Function> = wasm_bindgen::JsCast::dyn_ref(&jsv);
            #field_name.map(|f| {
                let f = f.clone();
                std::rc::Rc::new(move |#(#params),*| {
                    #(#conversions);*
                    let #args = js_sys::Array::new();
                    #(#pushes);*
                    let #return_value = f.apply(&JsValue::null(), &#args)?;
                    Ok(#ret)
                }) as #rendered_type
            })
            .ok_or_else(|| ts_bindgen_rt::jsvalue_serde::Error::InvalidType("expected function".to_string()))
            .map_err(serde::de::Error::custom)
        }
    })
}

fn render_serialize_fn(
    field_name: &Identifier,
    type_info: &TargetEnrichedTypeInfo,
) -> Option<TokenStream2> {
    let (f, rendered_type) = match type_info {
        TargetEnrichedTypeInfo::Ref(
            tr @ TypeRef {
                referent: TypeIdent::Builtin(Builtin::Fn),
                ..
            },
        ) => {
            let rendered_type = OwnedTypeRef(Cow::Borrowed(tr));
            (tr as &dyn HasFnPrototype, quote! { #rendered_type })
        }
        TargetEnrichedTypeInfo::FuncGroup(f) => {
            let rendered_type = OwnedTypeInfo { type_info };
            (
                &f.widened_fn as &dyn HasFnPrototype,
                quote! { #rendered_type },
            )
        }
        _ => {
            return None;
        }
    };
    let serialize_fn_name = serialize_field_name(field_name);
    let closure = render_exposed_to_js_wrapper_closure(f, field_name, None);
    let closure_name = field_name.suffix_name("_closure");
    Some(quote! {
        #[allow(non_snake_case)]
        fn #serialize_fn_name<S>(#field_name: &#rendered_type, serializer: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: serde::ser::Serializer,
        {
            let #field_name = #field_name.clone();
            let #closure_name = #closure;
            let jsv = ts_bindgen_rt::serialize_as_jsvalue(serializer, &#closure_name.into_js_value());
            //#closure_name.forget(); // TODO: how do we properly handle memory management?
            jsv
        }
    })
}

#[cfg(test)]
mod test {
    use crate::{generate_rust_for_typescript, Error, MemFs};
    use std::path::Path;

    fn ts_to_rust(code: &str) -> Result<String, Error> {
        let fs = {
            let mut fs: MemFs = Default::default();
            fs.set_cwd(Path::new("/"));
            fs.add_file_at(Path::new("/test.d.ts"), code.to_string());
            fs
        };

        Ok(generate_rust_for_typescript(fs, "/test")?.to_string())
    }

    #[test]
    fn test_fn_tuple_spread_args() -> Result<(), Error> {
        let rust = ts_to_rust(
            r#"
            export declare function foo(...args: [string, number]): void;
        "#,
        )?;

        assert!(rust.replace(' ', "").contains("vec![]"));
        assert!(rust.replace(' ', "").contains("JsValue::from"));
        assert!(rust.replace(' ', "").contains("into_boxed_slice"));
        Ok(())
    }

    #[test]
    fn test_fn_union_tuple_spread_args() -> Result<(), Error> {
        let rust = ts_to_rust(
            r#"
            export declare function foo(...args: [string, number] | [number, string]): void;
        "#,
        )?;

        assert!(rust
            .replace(' ', "")
            .contains(&"match args".replace(' ', "")));
        assert!(rust
            .replace(' ', "")
            .contains(&"FooParamsArgsUnion0(args) => {".replace(' ', "")));
        assert!(rust
            .replace(' ', "")
            .contains(&"FooParamsArgsUnion1(args) => {".replace(' ', "")));
        assert!(rust.replace(' ', "").contains("into_boxed_slice()"));
        Ok(())
    }

    #[test]
    fn test_closure_tuple_spread_args() -> Result<(), Error> {
        let rust = ts_to_rust(
            r#"
            export type foo = (...args: [string, number] | [number, string]) => void;
        "#,
        )?;

        assert!(rust.replace(' ', "").contains(
            &"pub type foo = dyn Fn(FooAliasedParamsArgs) -> std::result::Result<(), JsValue>;"
                .replace(' ', "")
        ));
        Ok(())
    }

    #[test]
    fn test_fn_spread_args() -> Result<(), Error> {
        let rust = ts_to_rust(
            r#"
            export declare function foo(...args: Array<string>): void;
        "#,
        )?;

        // aiming to ensure that we are creating an array to pass variadic args
        assert!(rust.replace(' ', "").contains("collect::<Vec<_>>"));
        assert!(rust.replace(' ', "").contains("into_boxed_slice"));
        Ok(())
    }

    #[test]
    fn test_closure_spread_args() -> Result<(), Error> {
        let rust = ts_to_rust(
            r#"
            export type foo = (...args: Array<string>) => void;
        "#,
        )?;

        assert!(rust.replace(' ', "").contains(
            &"dyn Fn (Vec<String>) -> std::result::Result<(), JsValue>".replace(' ', "")
        ));
        Ok(())
    }

    #[test]
    fn test_interface_function() -> Result<(), Error> {
        let rust = ts_to_rust(
            r#"
            export interface Foo {
                onSomething: Function;
            }
        "#,
        )?;

        assert!(rust.replace(' ', "").contains(
            &"fn on_something(&self) -> std::result::Result<std::rc::Rc<dyn Fn(Vec<JsValue>) -> std::result::Result<JsValue, JsValue>>, JsValue>".replace(' ', "")
        ));
        assert!(rust.replace(' ', "").contains(
            &"pub on_something: std::rc::Rc<dyn Fn(Vec<JsValue>) -> std::result::Result<JsValue, JsValue>>".replace(' ', "")
        ));
        assert!(rust
            .replace(' ', "")
            .contains(&"let _Args = js_sys::Array::new();".replace(' ', "")));
        assert!(rust
            .replace(' ', "")
            .contains(&"f.apply(&JsValue::null(), &_Args)".replace(' ', "")));
        Ok(())
    }

    #[test]
    fn test_interface_function_with_return() -> Result<(), Error> {
        let rust = ts_to_rust(
            r#"
            export interface FooRet {
                n: number;
            }
            export interface Foo {
                onSomething(...args: Array<number>): FooRet;
            }
        "#,
        )?;

        assert!(rust.replace(' ', "").contains(
            &"fn on_something(&self) -> std::result::Result<std::rc::Rc<dyn Fn(Vec<f64>) -> std::result::Result<FooRet, JsValue>>, JsValue>".replace(' ', "")
        ));
        assert!(rust.replace(' ', "").contains(
            &"pub on_something: std::rc::Rc<dyn Fn(Vec<f64>) -> std::result::Result<FooRet, JsValue>>".replace(' ', "")
        ));
        assert!(rust
            .replace(' ', "")
            .contains(&"let _Args = js_sys::Array::new()".replace(' ', "")));
        assert!(rust
            .replace(' ', "")
            .contains(&"f.apply(&JsValue::null(), &_Args)".replace(' ', "")));
        Ok(())
    }

    #[test]
    fn test_class_variadic_method() -> Result<(), Error> {
        let rust = ts_to_rust(
            r#"
            export declare class Foo {
                onSomething(...s: (string | number)[]): void;
            }
        "#,
        )?;

        assert!(rust
            .replace(' ', "")
            .contains(&"impl std::convert::From<Foo> for JsValue".replace(' ', "")));
        assert!(rust
            .replace(' ', "")
            .contains(&"impl std::convert::AsRef<JsValue> for Foo".replace(' ', "")));
        assert!(rust
            .replace(' ', "")
            .contains(&"impl wasm_bindgen::JsCast for Foo".replace(' ', "")));
        assert!(rust
            .replace(' ', "")
            .contains(&"impl wasm_bindgen::describe::WasmDescribe for Foo".replace(' ', "")));
        assert!(rust
            .replace(' ', "")
            .contains(&"impl wasm_bindgen::convert::IntoWasmAbi for Foo".replace(' ', "")));
        assert!(rust
            .replace(' ', "")
            .contains(&"impl wasm_bindgen::convert::FromWasmAbi for Foo".replace(' ', "")));
        assert!(rust
            .replace(' ', "")
            .contains(&"impl<'a> wasm_bindgen::convert::IntoWasmAbi for &'a Foo".replace(' ', "")));
        assert!(rust
            .replace(' ', "")
            .contains(&"impl serde::ser::Serialize for Foo".replace(' ', "")));
        assert!(rust
            .replace(' ', "")
            .contains(&"impl<'de> serde::de::Deserialize<'de> for Foo".replace(' ', "")));
        assert!(rust.replace(' ', "").contains(
            &"self.0.__TSB_on_something_FnVecOfFooOnSomethingParamsSTo(s.into_iter().map(|s_item| ts_bindgen_rt::to_jsvalue(&s_item).map_err(ts_bindgen_rt::Error::from).map_err(JsValue::from)).collect::<std::result::Result<Vec<_>, _>>().map_err(ts_bindgen_rt::Error::from).map_err(JsValue::from)?.into_boxed_slice())".replace(' ', "")
        ));
        Ok(())
    }

    #[test]
    fn test_class_callback_method() -> Result<(), Error> {
        let rust = ts_to_rust(
            r#"
            export declare class Foo {
                onSomething(f: Function): void;
            }
        "#,
        )?;

        assert!(rust
            .replace(' ', "")
            .contains(&"let arg0 = js_sys::Array::new()".replace(' ', "")));
        assert!(rust
            .replace(' ', "")
            .contains(&"arg0.push(&_Variadic0_arg0);".replace(' ', "")));
        assert!(rust.replace(' ', "").contains(
            &"let result = f({ let mut arg0_vec = vec![]; for arg0_item in arg0.iter() { arg0_vec.push(arg0_item); } arg0_vec })?".replace(' ', "")
        ));
        Ok(())
    }
}
