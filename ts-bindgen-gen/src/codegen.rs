use crate::identifier::{
    make_identifier, to_camel_case_ident, to_ident, to_snake_case_ident, to_unique_ident,
    Identifier,
};
pub use crate::mod_def::ModDef;
use crate::mod_def::ToModPathIter;
use crate::target_enriched_ir::{
    Alias, Builtin, Enum, EnumMember, Func, Indexer, Interface, Intersection, NamespaceImport,
    Param, TargetEnrichedType, TargetEnrichedTypeInfo, Tuple, TypeIdent, TypeRef,
    TypesByIdentByPath, Union,
};
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use std::cell::RefCell;
use std::collections::HashMap;
use std::iter;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use syn::Token;

macro_rules! if_requires_resolution {
    ($matcher:ident, $id:ident then $then:tt else $else:tt) => {
        match $matcher {
            TargetEnrichedTypeInfo::Ref($id) => $then,
            TargetEnrichedTypeInfo::NamespaceImport($id) => $then,
            TargetEnrichedTypeInfo::Alias($id) => $then,
            _ => $else,
        }
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
            let resolved_type = base
                .resolve_target_type()
                .expect("cannot resolve base type for interface");
            if let TargetEnrichedTypeInfo::Interface(i) = resolved_type {
                get_recursive_fields(&i).into_iter()
            } else {
                panic!("expected an interface as the base type for an interface");
            }
        }))
        .collect()
}

struct TransformedParam<'a, T: Fn(&TypeRef) -> TokenStream2>(&'a Param, T);

impl<'a, T: Fn(&TypeRef) -> TokenStream2> ToTokens for TransformedParam<'a, T> {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let param = self.0;
        let xform = &self.1;
        let param_name = to_snake_case_ident(&param.name);
        let typ = xform(&param.type_info);
        let full_type = if param.is_variadic {
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

struct InternalFunc<'a> {
    func: &'a Func,
    js_name: &'a str,
}

impl<'a> InternalFunc<'a> {
    fn to_internal_rust_name(js_name: &str) -> Identifier {
        to_snake_case_ident(format!("__tsb_{}", js_name))
    }

    fn to_serialized_type(typ: &TypeRef) -> TokenStream2 {
        let serialization_type = typ.serialization_type();
        match serialization_type {
            SerializationType::Raw => quote! { #typ },
            SerializationType::Ref => quote! { &#typ },
            SerializationType::SerdeJson => quote! { JsValue },
            SerializationType::Fn => {
                let target = typ.resolve_target_type();
                match target {
                    // TODO: lots of similar code
                    Some(TargetEnrichedTypeInfo::Ref(typ))
                        if matches!(&typ.referent, TypeIdent::Builtin(Builtin::Fn)) =>
                    {
                        let params = typ
                            .type_params
                            .iter()
                            .map(InternalFunc::to_serialized_type)
                            .take(typ.type_params.len() - 1);
                        let ret = typ
                            .type_params
                            .last()
                            .map(InternalFunc::to_serialized_type)
                            .unwrap_or_else(|| quote! { () });
                        quote! {
                            &Closure<dyn Fn(#(#params),*) -> #ret>
                        }
                    }
                    _ => {
                        unreachable!();
                    }
                }
            }
        }
    }
}

impl<'a> ToTokens for InternalFunc<'a> {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let fn_name = Self::to_internal_rust_name(self.js_name);

        let param_toks: Vec<TokenStream2> = self
            .func
            .params
            .iter()
            .map(|p| {
                let p = TransformedParam(p, InternalFunc::to_serialized_type);
                quote! { #p }
            })
            .collect();

        let return_type = InternalFunc::to_serialized_type(&self.func.return_type);

        let our_toks = quote! {
            pub fn #fn_name(#(#param_toks),*) -> std::result::Result<#return_type, JsValue>;
        };

        toks.extend(our_toks);
    }
}

struct WrapperFunc<'a> {
    func: &'a Func,
    js_name: &'a str,
}

impl<'a> WrapperFunc<'a> {
    fn to_rust_name(js_name: &str) -> Identifier {
        to_snake_case_ident(js_name)
    }

    fn to_local_fn_name(name: &str) -> Identifier {
        to_snake_case_ident(format!("__tsb_local_{}", name))
    }

    fn to_serialized_type(typ: &TypeRef) -> TokenStream2 {
        let serialization_type = typ.serialization_type();
        match serialization_type {
            SerializationType::Raw | SerializationType::SerdeJson => quote! { #typ },
            SerializationType::Ref | SerializationType::Fn => quote! { &#typ },
        }
    }
}

impl<'a> ToTokens for WrapperFunc<'a> {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let fn_name = Self::to_rust_name(self.js_name);

        let param_toks: Vec<TokenStream2> = self
            .func
            .params
            .iter()
            .map(|p| {
                let p = TransformedParam(p, WrapperFunc::to_serialized_type);
                quote! { #p }
            })
            .collect();

        let return_type = &self.func.return_type;

        let internal_fn_name = InternalFunc::to_internal_rust_name(self.js_name);
        let args: Vec<TokenStream2> = self
            .func
            .params
            .iter()
            .map(|p| {
                let serialization_type = p.type_info.serialization_type();
                let param_name = to_snake_case_ident(&p.name);
                match serialization_type {
                    SerializationType::Raw | SerializationType::Ref => quote! { #param_name },
                    SerializationType::SerdeJson => quote! { JsValue::from_serde(&#param_name) },
                    SerializationType::Fn => {
                        let local_fn_name = Self::to_local_fn_name(&p.name);
                        quote! { &#local_fn_name }
                    }
                }
            })
            .collect();
        let unwrapper = {
            let serialization_type = return_type.serialization_type();
            match serialization_type {
                SerializationType::Raw | SerializationType::Ref => quote! {},
                SerializationType::SerdeJson => quote! { .into_serde().unwrap() },
                SerializationType::Fn => {
                    // TODO - should be a js_sys::Function that we wrap
                    quote! { .into_serde().unwrap() }
                }
            }
        };
        let wrapper_fns = self.func.params.iter()
            .filter_map(|p| p.type_info.resolve_target_type().map(|t| (p, t)))
            .filter(|(_p, t)| t.serialization_type() == SerializationType::Fn)
            .filter_map(|(p, t)| {
                // TODO: lots of similar code
                let orig_name = to_snake_case_ident(&p.name);
                let name = Self::to_local_fn_name(&p.name);
                match t {
                    TargetEnrichedTypeInfo::Ref(typ) if matches!(&typ.referent, TypeIdent::Builtin(Builtin::Fn)) => {
                        let params = typ
                            .type_params
                            .iter()
                            .enumerate()
                            .map(|(i, t)| {
                                let typ = InternalFunc::to_serialized_type(t);
                                let n = to_snake_case_ident(format!("arg{}", i));
                                quote! { #n: #typ }
                            })
                            .take(typ.type_params.len() - 1);
                        let param_types = typ
                            .type_params
                            .iter()
                            .map(|t| {
                                let typ = InternalFunc::to_serialized_type(t);
                                quote! { #typ }
                            })
                            .take(typ.type_params.len() - 1);
                        let ret = typ
                            .type_params
                            .last()
                            .map(InternalFunc::to_serialized_type)
                            .unwrap_or_else(|| quote! { () });
                        let args = typ
                            .type_params
                            .iter()
                            .enumerate()
                            .map(|(i, t)| {
                                let n = to_snake_case_ident(format!("arg{}", i));
                                let serialization_type = t.serialization_type();
                                match serialization_type {
                                    SerializationType::Raw | SerializationType::Ref => quote! { #n },
                                    SerializationType::SerdeJson => quote! { #n.into_serde().map_err(ts_bindgen_rt::Error::from)? },
                                    SerializationType::Fn => {
                                        // TODO: we're not recursive yet
                                        unimplemented!();
                                    }
                                }
                            })
                            .take(typ.type_params.len() - 1);
                        let ret_type = typ.type_params.last()
                            .map(SerializationTypeGetter::serialization_type)
                            .unwrap_or_else(|| SerializationType::Raw);
                        let full_ret = quote! {
                            std::result::Result<#ret, JsValue>
                        };
                        let box_fn_type = quote! {
                            Box<dyn Fn(#(#param_types),*) -> #ret>
                        };
                        let invocation = if ret_type == SerializationType::SerdeJson {
                            quote! {
                                Ok(JsValue::from_serde(&#orig_name(#(#args),*)).map_err(ts_bindgen_rt::Error::from)?)
                            }
                        } else {
                            quote! {
                                #orig_name(#(#args),*)
                            }
                        };
                        Some(quote! {
                            let #name = Closure::wrap(Box::new(
                                |#(#params),*| -> #full_ret {
                                    #invocation
                                }
                            ) as #box_fn_type);
                        })
                    },
                    _ => None,
                }
            });

        let our_toks = quote! {
            pub fn #fn_name(#(#param_toks),*) -> std::result::Result<#return_type, JsValue> {
                #(#wrapper_fns);*
                Ok(#internal_fn_name(#(#args),*)?#unwrapper)
            }
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
            Builtin::Map => (
                "std::collections::HashMap",
                make_identifier!(std::collections::HashMap),
            ),
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

#[derive(Debug, Clone, PartialEq, Eq)]
enum SerializationType {
    Raw,
    SerdeJson,
    Ref,
    Fn,
}

trait SerializationTypeGetter {
    fn serialization_type(&self) -> SerializationType;
}

impl SerializationTypeGetter for TypeRef {
    fn serialization_type(&self) -> SerializationType {
        let resolved_type = self.resolve_target_type();
        resolved_type.map(|ti| ti.serialization_type()).unwrap()
    }
}

impl SerializationTypeGetter for TargetEnrichedTypeInfo {
    fn serialization_type(&self) -> SerializationType {
        match self {
            TargetEnrichedTypeInfo::Func(_) => SerializationType::Fn,
            TargetEnrichedTypeInfo::Enum(_) => SerializationType::Raw,
            TargetEnrichedTypeInfo::Class(_) => SerializationType::Raw,
            TargetEnrichedTypeInfo::Ref(t) => match &t.referent {
                TypeIdent::Builtin(Builtin::Fn) => SerializationType::Fn,
                TypeIdent::Builtin(_) => SerializationType::Raw,
                _ => SerializationType::SerdeJson,
            },
            _ => SerializationType::SerdeJson,
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

trait ResolveTargetType {
    fn resolve_target_type(&self) -> Option<TargetEnrichedTypeInfo>;
}

impl ResolveTargetType for TargetEnrichedType {
    fn resolve_target_type(&self) -> Option<TargetEnrichedTypeInfo> {
        self.info.resolve_target_type()
    }
}

fn resolve_type(
    types_by_ident_by_path: &Rc<RefCell<TypesByIdentByPath>>,
    path: &PathBuf,
    id: &TypeIdent,
) -> Option<TargetEnrichedTypeInfo> {
    // TODO: need to look for TypeIdent::Name or TypeIdent::Local interchangeably
    let ti = RefCell::borrow(types_by_ident_by_path)
        .get(path)
        .and_then(|t_by_id| t_by_id.get(id))
        .map(|t| t.info.clone());
    match ti {
        None => return None,
        Some(t) => if_requires_resolution!(
            t,
            x
            then (x.resolve_target_type())
            else (Some(t))
        ),
    }
}

impl ResolveTargetType for TypeRef {
    fn resolve_target_type(&self) -> Option<TargetEnrichedTypeInfo> {
        match &self.referent {
            TypeIdent::LocalName(_) => resolve_type(
                &self.context.types_by_ident_by_path,
                &self.context.path,
                &self.referent,
            ),
            TypeIdent::Name { file, name: _ } => {
                resolve_type(&self.context.types_by_ident_by_path, &file, &self.referent)
            }
            TypeIdent::DefaultExport(path) => {
                resolve_type(&self.context.types_by_ident_by_path, &path, &self.referent)
            }
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
                        if let Some(target_type) = ty.resolve_target_type() {
                            // TODO: silly to clone on every iteration but i need to figure out how
                            // to get resolved_type to live long enough to just pass along the ref
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
                    resolve_type(
                        &self.context.types_by_ident_by_path,
                        &final_file,
                        final_name,
                    )
                })
            }
            _ => Some(TargetEnrichedTypeInfo::Ref(self.clone())),
        }
    }
}

impl ResolveTargetType for NamespaceImport {
    fn resolve_target_type(&self) -> Option<TargetEnrichedTypeInfo> {
        match self {
            NamespaceImport::Default { src, context } => resolve_type(
                &context.types_by_ident_by_path,
                src,
                &TypeIdent::DefaultExport(src.clone()),
            ),
            NamespaceImport::Named { src, name, context } => resolve_type(
                &context.types_by_ident_by_path,
                src,
                &TypeIdent::Name {
                    file: src.clone(),
                    name: name.clone(),
                },
            ),
            NamespaceImport::All { src: _, context: _ } => {
                // TODO
                unimplemented!()
            }
        }
    }
}

impl ResolveTargetType for Alias {
    fn resolve_target_type(&self) -> Option<TargetEnrichedTypeInfo> {
        self.target.resolve_target_type()
    }
}

impl ResolveTargetType for TargetEnrichedTypeInfo {
    fn resolve_target_type(&self) -> Option<TargetEnrichedTypeInfo> {
        if_requires_resolution!(
            self,
            x
            then (x.resolve_target_type())
            else (Some(self.clone()))
        )
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

fn path_relative_to_cargo_toml<T: AsRef<Path>>(path: T) -> PathBuf {
    let mut best: Option<PathBuf> = None;
    let mut current_path: Option<PathBuf> = None;
    let path = path.as_ref();
    for component in path.components() {
        let p = current_path
            .map(|cp| cp.join(component))
            .unwrap_or_else(|| (component.as_ref() as &Path).to_path_buf());
        if p.is_dir() {
            if p.join("Cargo.toml").exists() {
                best = Some(p.clone());
            }
        }
        current_path = Some(p);
    }

    best.map(|p| path.components().skip(p.components().count()).collect())
        .unwrap_or_else(|| path.to_path_buf())
}

fn trim_after_dot<'a>(s: &'a str) -> &'a str {
    let idx = s.find('.');
    &s[0..idx.unwrap_or_else(|| s.len())]
}

impl ToTokens for TargetEnrichedType {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let (js_name, name) = self.name.to_name();
        let vis = if self.is_exported {
            let vis = format_ident!("pub");
            quote! { #vis }
        } else {
            quote! {}
        };

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
                quote! {
                    #vis type #name = #target;
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
            TargetEnrichedTypeInfo::Tuple(Tuple { types, .. }) => {
                quote! {
                    #[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
                    pub struct #name(#(pub #types),*);
                }
            }
            TargetEnrichedTypeInfo::Func(func) => {
                let path = &func.context.path;
                let path = path_relative_to_cargo_toml(path.with_file_name(
                    trim_after_dot(&*path.file_name().unwrap().to_string_lossy()).to_string()
                        + ".js",
                ));
                let path = path.to_string_lossy();
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
                        #[wasm_bindgen(#(#attrs),*)]
                        #internal_func
                    }

                    #wrapper_func
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
                            let f = InternalFunc {
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
            TargetEnrichedTypeInfo::NamespaceImport(NamespaceImport::All { src, .. }) => {
                let ns = src.as_path().to_ns_path(&self.name);
                let name = to_snake_case_ident(js_name);

                quote! {
                    #vis use #ns as #name;
                }
            }
            TargetEnrichedTypeInfo::NamespaceImport(NamespaceImport::Default { src, .. }) => {
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
                ..
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
                // TODO: we should get the name of the alias, not the ponited-to name
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
            let (_, name) = self.referent.to_name();
            if matches!(&self.referent, TypeIdent::Builtin(Builtin::Fn)) {
                let params = self
                    .type_params
                    .iter()
                    .map(|p| quote! { #p })
                    .take(self.type_params.len() - 1);
                let ret = self
                    .type_params
                    .last()
                    .map(|p| quote! { #p })
                    .unwrap_or_else(|| quote! {()});
                quote! {
                    Box<dyn #name(#(#params),*) -> Result<#ret, JsValue>>
                }
            } else if self.type_params.is_empty() {
                quote! { #name }
            } else {
                let type_params = self.type_params.iter().map(|p| quote! { #p });
                quote! { #name<#(#type_params),*> }
            }
        };

        toks.append_all(our_toks);
    }
}
