mod funcs;
mod generics;
mod named;
mod resolve_target_type;
mod serialization_type;
mod traits;
mod type_ref_like;

use crate::codegen::funcs::{
    fn_types, render_raw_return_to_js, AccessType, Constructor, FnPrototypeExt, HasFnPrototype,
    InternalFunc, PropertyAccessor, WrapperFunc,
};
use crate::codegen::generics::{
    apply_type_params, render_type_params, render_type_params_with_constraints,
    render_type_params_with_lifetimes, ResolveGeneric,
};
use crate::codegen::named::{CasedTypeIdent, Named, SimpleNamed};
use crate::codegen::resolve_target_type::ResolveTargetType;
use crate::codegen::serialization_type::{SerializationType, SerializationTypeGetter};
use crate::codegen::traits::{render_trait_defn, IsTraitable, TraitName};
use crate::codegen::type_ref_like::OwnedTypeRef;
use crate::fs::Fs;
use crate::identifier::{
    make_identifier, to_camel_case_ident, to_ident, to_snake_case_ident, to_unique_ident,
    Identifier,
};
use crate::ir::{
    Alias, Builtin, Class, Context, Enum, EnumMember, EnumValue, Func, Indexer, Interface,
    Intersection, Member, NamespaceImport, TargetEnrichedType, TargetEnrichedTypeInfo, Tuple,
    TypeIdent, TypeParamConfig, TypeRef, Union,
};
pub use crate::mod_def::ModDef;
use crate::mod_def::ToModPathIter;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use std::borrow::Cow;
use std::collections::HashMap;
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

trait ToNsPath<T: ?Sized> {
    fn to_ns_path<FS: Fs + ?Sized>(&self, fs: &FS, current_mod: &T) -> Vec<Identifier>;
}

impl<T, U> ToNsPath<T> for U
where
    T: ToModPathIter + ?Sized + std::fmt::Debug,
    U: ToModPathIter + ?Sized + std::fmt::Debug,
{
    fn to_ns_path<FS: Fs + ?Sized>(&self, fs: &FS, current_mod: &T) -> Vec<Identifier> {
        let mut cur_mod_path = current_mod.to_mod_path_iter(fs);
        let mut target_mod_path = self.to_mod_path_iter(fs);

        let mut ns: Vec<Identifier> = Default::default();

        // we skip all of the common elements of cur and target
        // then, we add in a "super" for each remaining element of cur
        // then, we add in all of the remaining elements of target

        loop {
            let cur = cur_mod_path.next();
            let tar = target_mod_path.next();

            if tar.is_none() {
                ns.push(make_identifier!(super));
                for _ in cur_mod_path {
                    ns.push(make_identifier!(super));
                }
                break;
            }

            let tar = tar.unwrap();

            if cur.is_none() {
                ns.push(tar);
                ns.extend(&mut target_mod_path);
                break;
            }

            let cur = cur.unwrap();

            if cur == tar {
                // skip any shared prefix
                continue;
            }

            // from this point on, current and target vary.
            // add supers for all remaining currents to get us up to the fork
            // and then append target
            ns.push(make_identifier!(super));
            for _ in cur_mod_path {
                ns.push(make_identifier!(super));
            }

            ns.push(tar);
            ns.extend(&mut target_mod_path);
            break;
        }

        ns
    }
}

#[cfg(test)]
mod test {
    use super::ToNsPath;
    use crate::fs::MemFs;
    use crate::generators::*;
    use crate::identifier::{make_identifier, to_ns_name};
    use crate::ir::TypeIdent;
    use proptest::prelude::*;
    use std::path::Path;

    proptest! {
        #[test]
        fn test_to_ns_path_for_sub_mod(
            prefix in arb_abs_path(),
            ns_rest in arb_rel_path(),
        ) {
            let fs = {
                let mut fs: MemFs = Default::default();
                fs.set_cwd(Path::new("/"));
                fs
            };
            let prefix_path = Path::new(&prefix);
            let cur = TypeIdent::Name {
                file: prefix_path.join(&ns_rest).to_path_buf(),
                name: "Hi".to_string(),
            };

            let expected: Vec<_> = ns_rest.split("/")
                .map(|p| to_ns_name(&p))
                .collect();

            prop_assert_eq!(
                cur.to_ns_path(&fs, prefix_path),
                expected
            );
        }
    }

    proptest! {
        #[test]
        fn test_to_ns_path_for_super_mod(
            prefix in arb_abs_path(),
            ns_rest in arb_rel_path(),
        ) {
            let fs = {
                let mut fs: MemFs = Default::default();
                fs.set_cwd(Path::new("/"));
                fs
            };
            let prefix_path = Path::new(&prefix);
            let full_path = prefix_path.join(&ns_rest);
            let cur = TypeIdent::Name {
                file: prefix_path.to_path_buf(),
                name: "Hi".to_string(),
            };

            let expected: Vec<_> = ns_rest.split("/")
                .map(|_| make_identifier!(super))
                .collect();

            prop_assert_eq!(
                cur.to_ns_path(&fs, full_path.as_path()),
                expected
            );
        }
    }

    proptest! {
        #[test]
        fn test_to_ns_path_for_branch_mod(
            prefix in arb_abs_path(),
            cur_rest in arb_rel_path(),
            target_rest in arb_rel_path(),
        ) {
            let fs = {
                let mut fs: MemFs = Default::default();
                fs.set_cwd(Path::new("/"));
                fs
            };
            let prefix_path = Path::new(&prefix);
            let cur = TypeIdent::Name {
                file: prefix_path.join(&cur_rest).to_path_buf(),
                name: "Hi".to_string(),
            };
            let full_path = prefix_path.join(&target_rest);

            let cur_path = Path::new(&cur_rest);
            let tar_path = Path::new(&target_rest);

            let expected = cur_path.to_ns_path(&fs, tar_path);

            prop_assert_eq!(
                cur.to_ns_path(&fs, full_path.as_path()),
                expected
            );
        }
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
        matches!(
            self.referent,
            TypeIdent::Builtin(Builtin::PrimitiveNull)
                | TypeIdent::Builtin(Builtin::PrimitiveUndefined)
                | TypeIdent::Builtin(Builtin::PrimitiveVoid),
        )
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
    &s[0..idx.unwrap_or_else(|| s.len())]
}

fn get_field_count<T: FieldCountGetter>(t: &T) -> usize {
    t.get_field_count()
}

trait FieldCountGetter {
    fn get_field_count(&self) -> usize;
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
            TargetEnrichedTypeInfo::Func(_) => min,
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
    fn undefined_and_standard_members(
        &self,
    ) -> (Vec<&TargetEnrichedTypeInfo>, Vec<&TargetEnrichedTypeInfo>);

    fn has_undefined_member(&self) -> bool {
        !self.undefined_and_standard_members().0.is_empty()
    }
}

impl MemberContainer for Union {
    fn undefined_and_standard_members(
        &self,
    ) -> (Vec<&TargetEnrichedTypeInfo>, Vec<&TargetEnrichedTypeInfo>) {
        self.types.iter().partition(|t| match t {
            TargetEnrichedTypeInfo::Ref(t)
                if t.referent == TypeIdent::Builtin(Builtin::PrimitiveUndefined) =>
            {
                true
            }
            _ => false,
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
            trim_after_dot(&*path.file_name().unwrap().to_string_lossy()).to_string() + ".js",
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
                    js_name,
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
                    })
                    .chain(undefined_members.iter().map(|t| {
                        let case = type_to_union_case_name(t);

                        quote! {
                            #[serde(serialize_with="ts_bindgen_rt::serialize_undefined", deserialize_with="ts_bindgen_rt::deserialize_undefined")]
                            #case
                        }
                    }));

                quote! {
                    #[derive(Clone, serde::Serialize, serde::Deserialize)]
                    #[serde(untagged)]
                    pub enum #name {
                        #(#member_cases),*
                    }
                }
            }
            TargetEnrichedTypeInfo::Tuple(Tuple { types, .. }) => {
                quote! {
                    #[derive(Clone, serde::Serialize, serde::Deserialize)]
                    pub struct #name(#(pub #types),*);
                }
            }
            TargetEnrichedTypeInfo::Func(func) => {
                let path = func.context.js_module_path();
                let attrs = {
                    let mut attrs = vec![quote! { js_name = #js_name, catch }];
                    if func.is_variadic() {
                        attrs.push(quote! { variadic });
                    }
                    attrs
                };
                let internal_func = InternalFunc { js_name, func };
                let wrapper_func = WrapperFunc { js_name, func };

                quote! {
                    #[wasm_bindgen(module=#path)]
                    extern "C" {
                        #[allow(non_snake_case)]
                        #[wasm_bindgen(#(#attrs),*)]
                        #internal_func
                    }

                    #wrapper_func
                }
            }
            TargetEnrichedTypeInfo::Class(class) => {
                let Class {
                    super_class,
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
                let wrapper_from_internal_args = if type_params.is_empty() {
                    vec![quote! { internal }]
                } else {
                    vec![quote! { internal }, quote! { std::marker::PhantomData }]
                };
                let type_params_with_de_lifetime =
                    render_type_params_with_lifetimes(type_params, &["de"]);
                let type_params_with_a_lifetime =
                    render_type_params_with_lifetimes(type_params, &["a"]);
                let path = context.js_module_path();
                let mut super_as_ref_impls: Vec<TokenStream2> = Default::default();
                let mut attrs = vec![quote! { js_name = #js_name }];
                if let Some(super_ref) = super_class.as_ref() {
                    let (_, super_name) = super_ref.to_name();
                    let super_name_without_tps = super_name.without_type_params();
                    let internal_super_name = to_internal_class_name(&super_name_without_tps);
                    let super_wrapper_from_src_args = if super_ref.type_params.is_empty() {
                        vec![quote! { src.clone() }]
                    } else {
                        vec![quote! { src.clone() }, quote! { std::marker::PhantomData }]
                    };

                    attrs.push(quote! {
                        extends = #internal_super_name
                    });

                    super_as_ref_impls.push(quote! {
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
                    });
                }
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
                let (member_defs, public_methods): (Vec<TokenStream2>, Vec<TokenStream2>) = members.iter()
                    .filter(|(_, member)| {
                        if let Member::Property(typ) = member {
                            // if we have a javascript type that acts as a namespace, containing
                            // other types, we just ignore those entries.
                            // this is fine IF those classes are exported elsewhere. if they
                            // aren't, we may need to find a way to force them to be exported
                            // (reasonable since they are de facto exported by virtue of being a property on
                            // an exported class)
                            // TODO: maybe add a level property to TypeRef indicating TermLevel or TypeLevel
                            true
                            // !matches!(typ, TargetEnrichedTypeInfo::TypeQuery(_))
                        } else {
                            true
                        }
                    })
                    .map(|(member_js_name, member)| {
                        let member_js_ident = format_ident!("{}", member_js_name);
                        let internal_fn_name = InternalFunc::to_internal_rust_name(member_js_name);
                        match member {
                            Member::Constructor(ctor) => {
                                let ctor = ctor.resolve_generic_in_env(&type_env);
                                let ctor = Constructor::new(
                                    ctor,
                                    TypeIdent::LocalName(js_name.to_string()),
                                );
                                let param_toks = ctor
                                    .params()
                                    .map(|p| p.as_exposed_to_rust_named_param_list());

                                let member_def = quote! {
                                    #[wasm_bindgen(constructor, js_class = #js_name)]
                                    pub fn new(#(#param_toks),*) -> #internal_class_name;
                                };
                                let fq_internal_ctor = to_snake_case_ident("new").in_namespace(&internal_class_name);

                                let res_converter = |res: TokenStream2| -> TokenStream2 {
                                    let args = if type_params.is_empty() {
                                        vec![quote! { #res }]
                                    } else {
                                        vec![
                                            quote! { #res },
                                            quote! { std::marker::PhantomData #full_type_params },
                                        ]
                                    };
                                    quote! {
                                        #name(#(#args),*)
                                    }
                                };
                                let pub_fn = ctor.exposed_to_rust_generic_wrapper_fn(&make_identifier!(new), None, &fq_internal_ctor, false, Some(&res_converter), &type_env);

                                (member_def, pub_fn)
                            }
                            Member::Method(func) => {
                                let func = {
                                    // func.class_name refers to our wrapper, which is what we want
                                    // other than when we're defining our actual methods
                                    let mut func = func.clone();
                                    func.class_name = func.class_name.map(|_| TypeIdent::ExactName(internal_class_name.to_string()));
                                    func
                                };
                                let func = func.resolve_generic_in_env(&type_env);
                                let fn_name = InternalFunc::to_internal_rust_name(member_js_name);

                                let f = func.exposed_to_js_fn_decl(fn_name);

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
                                let pub_fn = func.exposed_to_rust_generic_wrapper_fn(&to_snake_case_ident(&member_js_name), Some(&target), &internal_fn_name, true, rc, &type_env);

                                (member_def, pub_fn)
                            }
                            Member::Property(typ) => {
                                let resolved_type = typ.resolve_generic_in_env(&type_env);
                                let member_name = to_snake_case_ident(member_js_name);
                                let setter_name = format_ident!("set_{}", member_name.to_string());
                                // TODO: don't add structural if the property is actually a
                                // javascript getter/setter
                                let member_def = quote! {
                                    #[wasm_bindgen(method, structural, getter = #member_js_ident, js_class = #js_name)]
                                    fn #member_name(this: &#internal_class_name) -> #resolved_type;

                                    #[wasm_bindgen(method, structural, setter = #member_js_ident, js_class = #js_name)]
                                    fn #setter_name(this: &#internal_class_name, value: #resolved_type);
                                };

                                let getter = PropertyAccessor {
                                    property_name: member_name.clone(),
                                    typ: typ.clone(),
                                    class_name: type_name.clone(),
                                    access_type: AccessType::Getter,
                                }.getter_fn();

                                let setter = PropertyAccessor {
                                    property_name: member_name.clone(),
                                    typ: typ.clone(),
                                    class_name: type_name.clone(),
                                    access_type: AccessType::Setter,
                                }.setter_fn();

                                let rc: Option<&fn(TokenStream2) -> TokenStream2> = None;
                                let getter_fn = getter.exposed_to_rust_generic_wrapper_fn(&to_snake_case_ident(&member_js_name), Some(&target), &member_name, false, rc, &type_env);
                                let setter_ident = Identifier::new_ident(setter_name);
                                let setter_fn = setter.exposed_to_rust_generic_wrapper_fn(&setter_ident, Some(&target), &setter_ident, false, rc, &type_env);

                                let pub_fn = quote! {
                                    #getter_fn

                                    #setter_fn
                                };

                                (member_def, pub_fn)
                            }
                        }
                    })
                    .unzip();

                let trait_defn = render_trait_defn(
                    &name,
                    js_name,
                    type_params,
                    is_exported,
                    class,
                    &class.context,
                );

                quote! {
                    #[wasm_bindgen(module = #path)]
                    extern "C" {
                        #[wasm_bindgen(#(#attrs),*)]
                        #vis type #internal_class_name;

                        #(#member_defs)*
                    }

                    #[derive(std::clone::Clone)]
                    #vis struct #name #full_type_params(#(#wrapper_struct_members),*);

                    #(#super_as_ref_impls)*

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
            /*TypeInfo::Mapped {
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
            TypeInfo::Constructor {
                params: Vec<Param>,
                return_type: Box<TypeInfo>,
            },
            TypeInfo::Var {
                type_info: Box<TypeInfo>,
            },*/
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
                    fn(#(#param_toks),*) -> #return_type
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
            if matches!(&self.referent, TypeIdent::Builtin(Builtin::Fn)) {
                let name = self.to_simple_name();
                let params = self
                    .params()
                    .map(|p| p.as_exposed_to_rust_unnamed_param_list());
                let ret = fn_types::exposed_to_rust_return_type(&self.return_type(), true);
                quote! {
                    dyn #name(#(#params),*) -> #ret
                }
            } else if matches!(&self.referent, TypeIdent::Builtin(Builtin::PrimitiveVoid)) {
                quote! { () }
            } else {
                let (_, name) = self.to_name();
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
            let serialize_fn = field_name.prefix_name("__tsb__serialize_");
            let deserialize_fn = field_name.prefix_name("__tsb__deserialize_");
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
    if let TargetEnrichedTypeInfo::Ref(
        tr
        @ TypeRef {
            referent: TypeIdent::Builtin(Builtin::Fn),
            ..
        },
    ) = type_info
    {
        let deserialize_fn_name = field_name.prefix_name("__tsb__deserialize_");
        let return_type = tr.return_type();
        let return_value = quote! { ret };
        let ret = render_raw_return_to_js(&return_type, &return_value);
        let args = quote! { args };
        let params = tr.params().map(|p| p.as_exposed_to_rust_named_param_list());
        let rendered_type = OwnedTypeRef(Cow::Borrowed(tr));
        // TODO: need to render wrappers for fn params, used in rust_to_jsvalue_conversion
        let conversions = tr.args().map(|p| {
            let name = p.rust_name();
            let conv = p.rust_to_jsvalue_conversion();
            quote! {
                let #name = #conv;
            }
        });
        let pushes = tr.params().map(|p| {
            let name = p.rust_name();
            quote! {
                #args.push(&#name);
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
                Ok(#field_name.map(|f| {
                    let f = f.clone();
                    std::rc::Rc::new(move |#(#params),*| {
                        #(#conversions);*
                        let args = js_sys::Array::new();
                        #(#pushes);*
                        let #return_value = f.apply(&JsValue::null(), &args)?;
                        Ok(#ret)
                    }) as #rendered_type
                })
                .ok_or_else(|| ts_bindgen_rt::jsvalue_serde::Error::InvalidType("expected function".to_string()))
                .map_err(serde::de::Error::custom)?)
            }
        })
    } else {
        None
    }
}

fn render_serialize_fn(
    field_name: &Identifier,
    type_info: &TargetEnrichedTypeInfo,
) -> Option<TokenStream2> {
    if let TargetEnrichedTypeInfo::Ref(
        tr
        @ TypeRef {
            referent: TypeIdent::Builtin(Builtin::Fn),
            ..
        },
    ) = type_info
    {
        let serialize_fn_name = field_name.prefix_name("__tsb__serialize_");
        let invocation = tr.invoke_with_name(field_name);
        let closure = tr.exposed_to_js_wrapped_closure(invocation);
        let rendered_type = OwnedTypeRef(Cow::Borrowed(tr));
        Some(quote! {
            #[allow(non_snake_case)]
            fn #serialize_fn_name<S>(#field_name: &#rendered_type, serializer: S) -> std::result::Result<S::Ok, S::Error>
            where
                S: serde::ser::Serializer,
            {
                let #field_name = #field_name.clone();
                let #field_name = #closure;
                let jsv = ts_bindgen_rt::serialize_as_jsvalue(serializer, &#field_name.into_js_value());
                //#field_name.forget(); // TODO: how do we properly handle memory management?
                jsv
            }
        })
    } else {
        None
    }
}
