use crate::identifier::{
    make_identifier, to_camel_case_ident, to_ident, to_snake_case_ident, to_unique_ident,
    Identifier,
};
pub use crate::mod_def::ModDef;
use crate::mod_def::ToModPathIter;
use crate::target_enriched_ir::{
    Alias, Builtin, Class, Context, Ctor, Enum, EnumMember, Func, Indexer, Interface, Intersection,
    Member, NamespaceImport, Param, TargetEnrichedType, TargetEnrichedTypeInfo, Tuple, TypeIdent,
    TypeParamConfig, TypeRef, TypesByIdentByPath, Union,
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

trait ProvidedTypeParams {
    /// Get the provided type parameters for this reference
    fn provided_type_params<'a>(&'a self) -> &'a [TypeRef];
}

impl ProvidedTypeParams for TypeRef {
    fn provided_type_params<'a>(&'a self) -> &'a [TypeRef] {
        self.type_params.as_slice()
    }
}

trait RequiredTypeParams {
    /// Get the required type parameters for instantiations of self
    fn required_type_params<'a>(&'a self) -> Option<&'a [(String, TypeParamConfig)]>;
}

impl RequiredTypeParams for TargetEnrichedTypeInfo {
    fn required_type_params<'a>(&'a self) -> Option<&'a [(String, TypeParamConfig)]> {
        match self {
            TargetEnrichedTypeInfo::Func(f) => Some(f.type_params.as_slice()),
            // TODO: do refs need provided and required type params?
            //TargetEnrichedTypeInfo::Ref(f) =>  Some(f.type_params.as_slice()),
            TargetEnrichedTypeInfo::Interface(i) => Some(i.type_params.as_slice()),
            TargetEnrichedTypeInfo::Class(c) => Some(c.type_params.as_slice()),
            TargetEnrichedTypeInfo::Alias(a) => Some(a.type_params.as_slice()),
            _ => None,
        }
    }
}

impl RequiredTypeParams for Interface {
    fn required_type_params<'a>(&'a self) -> Option<&'a [(String, TypeParamConfig)]> {
        Some(self.type_params.as_slice())
    }
}

trait ResolveGeneric {
    /// resolve this possibly-generic-referencing type into a TypeRef
    /// in the given type_env.
    fn resolve_generic_in_env<'a>(&'a self, type_env: &'a HashMap<String, TypeRef>) -> &'a TypeRef;
}

impl ResolveGeneric for TypeRef {
    fn resolve_generic_in_env<'a>(&'a self, type_env: &'a HashMap<String, TypeRef>) -> &'a TypeRef {
        // TODO: type_envs and type_params should key off of something better than String...
        type_env
            .get(&self.referent.to_name().1.to_string())
            .unwrap_or(self)
    }
}

fn apply_type_params<P, R>(
    provided: &P,
    required: &R,
    type_env: &HashMap<String, TypeRef>,
) -> HashMap<String, TypeRef>
where
    P: ProvidedTypeParams,
    R: RequiredTypeParams,
{
    let provided = provided.provided_type_params();
    let required: Vec<_> = required
        .required_type_params()
        .map(|s| s.into())
        .unwrap_or_default();

    let mut result: HashMap<String, TypeRef> = Default::default();

    for (i, (type_param, cfg)) in required.into_iter().enumerate() {
        let type_value = provided
            .get(i)
            .map(|p| p.clone())
            .or(cfg.default_type_arg)
            .unwrap()
            .resolve_generic_in_env(type_env)
            .clone();
        result.insert(type_param, type_value);
    }

    result
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
        .map(|(n, t)| (n.clone(), t.resolve_generic_in_env(type_env).clone()));
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

trait HasFnPrototype {
    fn return_type(&self) -> TypeRef;
    fn params<'a>(&'a self) -> Box<dyn Iterator<Item = Box<dyn ParamExt>> + 'a>;
    fn args<'a>(&'a self) -> Box<dyn Iterator<Item = Box<dyn ParamExt>> + 'a>;
    fn is_member(&self) -> bool;
    fn is_variadic(&self) -> bool {
        self.params().any(|p| p.is_variadic())
    }
}

impl HasFnPrototype for TypeRef {
    fn return_type(&self) -> TypeRef {
        self.type_params
            .last()
            .map(Clone::clone)
            .unwrap_or_else(|| TypeRef {
                referent: TypeIdent::Builtin(Builtin::PrimitiveVoid),
                type_params: vec![],
                context: self.context.clone(),
            })
    }

    fn params<'a>(&'a self) -> Box<dyn Iterator<Item = Box<dyn ParamExt>> + 'a> {
        Box::new(
            self.type_params
                .iter()
                .enumerate()
                .map(|(i, t)| {
                    Box::new(Param {
                        name: format!("arg{}", i),
                        type_info: t.clone(),
                        is_variadic: false, // TODO
                        context: t.context.clone(),
                    }) as Box<dyn ParamExt>
                })
                .take(self.type_params.len() - 1),
        )
    }

    fn args<'a>(&'a self) -> Box<dyn Iterator<Item = Box<dyn ParamExt>> + 'a> {
        // args and params are the same for non-members
        self.params()
    }

    fn is_member(&self) -> bool {
        false
    }
}

#[derive(Debug, Clone)]
struct SelfParam {
    class_name: TypeIdent,
}

impl HasFnPrototype for Func {
    fn return_type(&self) -> TypeRef {
        (*self.return_type).clone()
    }

    fn params<'a>(&'a self) -> Box<dyn Iterator<Item = Box<dyn ParamExt>> + 'a> {
        let reg_params = self.args();

        if let Some(class_name) = &self.class_name {
            Box::new(
                iter::once(Box::new(SelfParam {
                    class_name: class_name.clone(),
                }) as Box<dyn ParamExt>)
                .chain(reg_params),
            )
        } else {
            Box::new(reg_params)
        }
    }

    fn args<'a>(&'a self) -> Box<dyn Iterator<Item = Box<dyn ParamExt>> + 'a> {
        Box::new(
            self.params
                .iter()
                .map(|p| Box::new(p.clone()) as Box<dyn ParamExt>),
        )
    }

    fn is_member(&self) -> bool {
        self.class_name.is_some()
    }
}

impl HasFnPrototype for Ctor {
    fn return_type(&self) -> TypeRef {
        // TODO
        unimplemented!()
    }

    fn params<'a>(&'a self) -> Box<dyn Iterator<Item = Box<dyn ParamExt>> + 'a> {
        Box::new(
            self.params
                .iter()
                .map(|p| Box::new(p.clone()) as Box<dyn ParamExt>),
        )
    }

    fn args<'a>(&'a self) -> Box<dyn Iterator<Item = Box<dyn ParamExt>> + 'a> {
        self.params()
    }

    fn is_member(&self) -> bool {
        true
    }
}

mod fn_types {
    use super::{
        quote, Builtin, HasFnPrototype, ResolveTargetType, SerializationType,
        SerializationTypeGetter, TargetEnrichedTypeInfo, TokenStream2, TypeIdent, TypeRef,
    };

    fn exposed_to_js_type(typ: &TypeRef) -> TokenStream2 {
        let serialization_type = typ.serialization_type();
        match serialization_type {
            SerializationType::Raw => quote! { #typ },
            SerializationType::Ref => quote! { &#typ },
            SerializationType::SerdeJson => quote! { JsValue },
            SerializationType::Fn => {
                let target = typ.resolve_target_type();
                match target {
                    Some(TargetEnrichedTypeInfo::Ref(typ))
                        if matches!(&typ.referent, TypeIdent::Builtin(Builtin::Fn)) =>
                    {
                        let params = typ
                            .params()
                            .map(|p| p.as_exposed_to_js_unnamed_param_list());
                        let ret = exposed_to_js_param_type(&typ.return_type());
                        quote! {
                            &Closure<dyn Fn(#(#params),*) -> std::result::Result<#ret, JsValue>>
                        }
                    }
                    _ => {
                        unreachable!();
                    }
                }
            }
        }
    }

    pub fn exposed_to_js_param_type(typ: &TypeRef) -> TokenStream2 {
        exposed_to_js_type(typ)
    }

    pub fn exposed_to_js_return_type(typ: &TypeRef) -> TokenStream2 {
        let t = exposed_to_js_param_type(typ);
        quote! {
            std::result::Result<#t, JsValue>
        }
    }

    fn exposed_to_rust_type(typ: &TypeRef) -> TokenStream2 {
        let serialization_type = typ.serialization_type();
        match serialization_type {
            SerializationType::Raw | SerializationType::SerdeJson => quote! { #typ },
            SerializationType::Ref => quote! { &#typ },
            SerializationType::Fn => quote! { &'static #typ },
        }
    }

    pub fn exposed_to_rust_param_type(typ: &TypeRef) -> TokenStream2 {
        exposed_to_rust_type(typ)
    }

    pub fn exposed_to_rust_return_type(typ: &TypeRef) -> TokenStream2 {
        quote! { #typ }
    }
}

trait FnPrototypeExt {
    fn exposed_to_js_closure<Body: ToTokens>(&self, body: Body) -> TokenStream2;
    fn exposed_to_js_fn_type(&self) -> TokenStream2;

    fn exposed_to_js_boxed_fn_type(&self) -> TokenStream2;

    fn exposed_to_js_wrapped_closure<Body: ToTokens>(&self, body: Body) -> TokenStream2;

    fn exposed_to_js_fn_decl<Name: ToTokens>(&self, name: Name) -> TokenStream2;

    fn invoke_with_name<Name: ToTokens>(&self, name: Name) -> TokenStream2;

    fn exposed_to_rust_fn_decl<Name: ToTokens>(&self, name: Name) -> TokenStream2;

    /// Returns a token stream defining local closures for any parameters
    /// such that the closures may be invoked by js, wrapping rust closures.
    /// This is named exposed_to_rust because it is used when constructing
    /// a function that may be called from rust (and, hence, might have
    /// rust closures in need of wrapping).
    fn exposed_to_rust_param_wrappers(&self) -> TokenStream2;

    /// Renders a wrapper function intended to be called idiomatically from
    /// rust, which wraps an invocation of a corresponding underlying js
    /// function.
    fn exposed_to_rust_wrapper_fn(
        &self,
        fn_name: &Identifier,
        internal_fn_name: &Identifier,
    ) -> TokenStream2;
}

impl<T: HasFnPrototype> FnPrototypeExt for T {
    fn exposed_to_js_closure<Body: ToTokens>(&self, body: Body) -> TokenStream2 {
        let params = self.params().map(|p| p.as_exposed_to_js_named_param_list());
        let ret = fn_types::exposed_to_js_return_type(&self.return_type());

        quote! {
            move |#(#params),*| -> #ret {
                #body
            }
        }
    }

    fn exposed_to_js_fn_type(&self) -> TokenStream2 {
        let params = self
            .params()
            .map(|p| p.as_exposed_to_js_unnamed_param_list());
        let ret = fn_types::exposed_to_js_return_type(&self.return_type());
        quote! {
            dyn Fn(#(#params),*) -> #ret
        }
    }

    fn exposed_to_js_boxed_fn_type(&self) -> TokenStream2 {
        let fn_type = self.exposed_to_js_fn_type();
        quote! {
            Box<#fn_type>
        }
    }

    fn exposed_to_js_wrapped_closure<Body: ToTokens>(&self, body: Body) -> TokenStream2 {
        let closure = self.exposed_to_js_closure(body);
        let boxed_fn_type = self.exposed_to_js_boxed_fn_type();
        quote! {
            Closure::wrap(Box::new(
                #closure
            ) as #boxed_fn_type)
        }
    }

    fn exposed_to_js_fn_decl<Name: ToTokens>(&self, name: Name) -> TokenStream2 {
        let params = self.params().map(|p| p.as_exposed_to_js_named_param_list());
        let ret = fn_types::exposed_to_js_return_type(&self.return_type());
        quote! {
            fn #name(#(#params),*) -> #ret
        }
    }

    fn invoke_with_name<Name: ToTokens>(&self, name: Name) -> TokenStream2 {
        let args = self.args().map(|p| p.rust_name());
        quote! {
            #name(#(#args),*)
        }
    }

    fn exposed_to_rust_fn_decl<Name: ToTokens>(&self, name: Name) -> TokenStream2 {
        let params = self
            .params()
            .map(|p| p.as_exposed_to_rust_named_param_list());
        let ret = fn_types::exposed_to_rust_return_type(&self.return_type());
        quote! {
            fn #name(#(#params),*) -> std::result::Result<#ret, JsValue>
        }
    }

    fn exposed_to_rust_param_wrappers(&self) -> TokenStream2 {
        let wrapper_fns = self.params().filter_map(|p| {
            let name = p.local_fn_name();
            let f = p.js_wrapper_fn();
            f.map(|f| {
                quote! {
                    let #name = #f;
                }
            })
        });

        quote! {
            #(#wrapper_fns)*
        }
    }

    fn exposed_to_rust_wrapper_fn(
        &self,
        fn_name: &Identifier,
        internal_fn_name: &Identifier,
    ) -> TokenStream2 {
        let args = self.args().map(|p| p.rust_to_js_conversion());
        let self_access = if self.is_member() {
            quote! { self. }
        } else {
            quote! {}
        };
        let return_value = quote! {
            #self_access #internal_fn_name(#(#args),*)?
        };
        let ret = render_wasm_bindgen_return_to_js(&self.return_type(), &return_value);
        let wrapper_fns = self.exposed_to_rust_param_wrappers();

        let f = self.exposed_to_rust_fn_decl(fn_name);

        quote! {
            #[allow(dead_code)]
            pub #f {
                #[allow(unused_imports)]
                use ts_bindgen_rt::IntoSerdeOrDefault;

                #wrapper_fns

                Ok(#ret)
            }
        }
    }
}

trait ParamExt {
    /// The rust name for this parameter.
    fn rust_name(&self) -> Identifier;

    /// Returns a token stream suitable for inclusion in a named parameter
    /// list, such as that of a function definition, where the param type
    /// is able to be exposed to javascript.
    fn as_exposed_to_js_named_param_list(&self) -> TokenStream2;

    /// Returns a token stream suitable for inclusing in an un-named parameter
    /// list, such as that of a function type, where the param type is able
    /// to be exposed to javascript
    fn as_exposed_to_js_unnamed_param_list(&self) -> TokenStream2;

    /// Returns a token stream suitable for inclusion in a named parameter
    /// list, such as that of a function definition, where the param type
    /// is idiomatic to be exposed to rust callers.
    fn as_exposed_to_rust_named_param_list(&self) -> TokenStream2;

    /// Returns a token stream suitable for inclusion in an un-named parameter
    /// list, such as that of a function type, where the param type
    /// is idiomatic to be exposed to rust callers.
    fn as_exposed_to_rust_unnamed_param_list(&self) -> TokenStream2;

    /// Renders a conversion from a local rust type with the same name as the
    /// parameter to a js type.
    fn rust_to_js_conversion(&self) -> TokenStream2;

    /// Renders a conversion from a local rust type with the same name as the
    /// parameter to a JsValue.
    fn rust_to_jsvalue_conversion(&self) -> TokenStream2;

    /// Returns an Identifier representing the name of the local wrapper
    /// function corresponding to this parameter if this parameter is of
    /// function type.
    fn local_fn_name(&self) -> Identifier;

    /// Renders a conversion from a local JsValue with the same name as the
    /// parameter to a rust type.
    fn js_to_rust_conversion(&self) -> TokenStream2;

    /// If this parameter is a function, return a Some(TokenStream) where the
    /// TokenStream defines an exposed-to-js closure that will proxy calls
    /// to the underlying param.
    fn js_wrapper_fn(&self) -> Option<TokenStream2>;

    /// Is the parameter the final parameter of a variadic function?
    fn is_variadic(&self) -> bool;
}

impl ParamExt for Param {
    fn rust_name(&self) -> Identifier {
        to_snake_case_ident(&self.name)
    }

    fn as_exposed_to_js_named_param_list(&self) -> TokenStream2 {
        let full_type = self.as_exposed_to_js_unnamed_param_list();
        let n = self.rust_name();
        quote! { #n: #full_type }
    }

    fn as_exposed_to_js_unnamed_param_list(&self) -> TokenStream2 {
        let typ = fn_types::exposed_to_js_param_type(&self.type_info);
        if self.is_variadic {
            quote! { &[#typ] }
        } else {
            quote! { #typ }
        }
    }

    fn as_exposed_to_rust_named_param_list(&self) -> TokenStream2 {
        let full_type = self.as_exposed_to_rust_unnamed_param_list();
        let n = self.rust_name();
        quote! { #n: #full_type }
    }

    fn as_exposed_to_rust_unnamed_param_list(&self) -> TokenStream2 {
        let typ = fn_types::exposed_to_rust_param_type(&self.type_info);
        if self.is_variadic {
            quote! { &[#typ] }
        } else {
            quote! { #typ }
        }
    }

    fn js_to_rust_conversion(&self) -> TokenStream2 {
        let serialization_type = self.type_info.serialization_type();
        let name = self.rust_name();

        match serialization_type {
            SerializationType::Raw | SerializationType::Ref => quote! { #name },
            SerializationType::SerdeJson => {
                quote! {
                    ts_bindgen_rt::IntoSerdeOrDefault::into_serde_or_default(&#name).map_err(ts_bindgen_rt::Error::from)?
                }
            }
            SerializationType::Fn => {
                // TODO: we're not recursive yet
                unimplemented!();
            }
        }
    }

    fn local_fn_name(&self) -> Identifier {
        to_snake_case_ident(format!("__tsb_local_{}", self.name))
    }

    fn rust_to_js_conversion(&self) -> TokenStream2 {
        let name = self.rust_name();
        let fn_name = self.local_fn_name();
        render_rust_to_js_conversion(&name, &fn_name, &self.type_info, quote! {})
    }

    fn rust_to_jsvalue_conversion(&self) -> TokenStream2 {
        let name = self.rust_name();
        let fn_name = self.local_fn_name();
        render_rust_to_jsvalue_conversion(&name, &fn_name, &self.type_info, quote! {})
    }

    fn js_wrapper_fn(&self) -> Option<TokenStream2> {
        let type_info = self.type_info.resolve_target_type();
        if type_info
            .as_ref()
            .map(SerializationTypeGetter::serialization_type)
            != Some(SerializationType::Fn)
        {
            return None;
        }

        if let Some(TargetEnrichedTypeInfo::Ref(typ)) = type_info {
            if !matches!(&typ.referent, TypeIdent::Builtin(Builtin::Fn)) {
                return None;
            }

            // TODO: needs to render wrappers for typ.params() that are
            // functions
            let args = typ.args().map(|p| p.js_to_rust_conversion());
            let result = to_snake_case_ident("result");
            let fn_name = to_snake_case_ident("result_adapter");
            let conversion = render_rust_to_js_conversion(
                &result,
                &fn_name,
                &typ.return_type(),
                quote! { .map_err(ts_bindgen_rt::Error::from)? },
            );
            let name = self.rust_name();
            let invocation = quote! {
                let #result = #name(#(#args),*)?;
                Ok(#conversion)
            };
            Some(typ.exposed_to_js_wrapped_closure(invocation))
        } else {
            None
        }
    }

    fn is_variadic(&self) -> bool {
        self.is_variadic
    }
}

impl ParamExt for SelfParam {
    fn rust_name(&self) -> Identifier {
        to_snake_case_ident("self")
    }

    fn as_exposed_to_js_named_param_list(&self) -> TokenStream2 {
        let (_, class_name) = self.class_name.to_name();
        quote! { this: &#class_name }
    }

    fn as_exposed_to_js_unnamed_param_list(&self) -> TokenStream2 {
        let (_, class_name) = self.class_name.to_name();
        quote! { &#class_name}
    }

    fn as_exposed_to_rust_named_param_list(&self) -> TokenStream2 {
        quote! { &self }
    }

    fn as_exposed_to_rust_unnamed_param_list(&self) -> TokenStream2 {
        quote! { &self }
    }

    fn js_to_rust_conversion(&self) -> TokenStream2 {
        quote! { &self }
    }

    fn local_fn_name(&self) -> Identifier {
        // this is never used...
        self.rust_name()
    }

    fn rust_to_js_conversion(&self) -> TokenStream2 {
        quote! { &self }
    }

    fn rust_to_jsvalue_conversion(&self) -> TokenStream2 {
        quote! { &self }
    }

    fn js_wrapper_fn(&self) -> Option<TokenStream2> {
        None
    }

    fn is_variadic(&self) -> bool {
        false
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
}

impl<'a> ToTokens for InternalFunc<'a> {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let fn_name = Self::to_internal_rust_name(self.js_name);

        let f = self.func.exposed_to_js_fn_decl(fn_name);

        let our_toks = quote! {
            pub #f;
        };

        toks.extend(our_toks);
    }
}

fn render_wasm_bindgen_return_to_js(
    return_type: &TypeRef,
    return_value: &TokenStream2,
) -> TokenStream2 {
    let serialization_type = return_type.serialization_type();
    match serialization_type {
        SerializationType::Raw | SerializationType::Ref => return_value.clone(),
        SerializationType::SerdeJson => {
            quote! {
                ts_bindgen_rt::IntoSerdeOrDefault::into_serde_or_default(&#return_value).unwrap()
            }
        }
        SerializationType::Fn => {
            // TODO - should be a js_sys::Function that we wrap
            quote! { #return_value.into_serde().unwrap() }
        }
    }
}

fn render_raw_return_to_js(return_type: &TypeRef, return_value: &TokenStream2) -> TokenStream2 {
    let serialization_type = return_type.serialization_type();
    match serialization_type {
        SerializationType::Raw | SerializationType::Ref => quote! {
            #return_value.into_serde().unwrap()
        },
        SerializationType::SerdeJson => {
            quote! {
                ts_bindgen_rt::IntoSerdeOrDefault::into_serde_or_default(&#return_value).unwrap()
            }
        }
        SerializationType::Fn => {
            // TODO - should be a js_sys::Function that we wrap
            quote! { #return_value.into_serde().unwrap() }
        }
    }
}

fn render_rust_to_js_conversion(
    name: &Identifier,
    fn_name: &Identifier,
    typ: &TypeRef,
    error_mapper: TokenStream2,
) -> TokenStream2 {
    let serialization_type = typ.serialization_type();
    match serialization_type {
        SerializationType::Raw | SerializationType::Ref => quote! { #name },
        SerializationType::SerdeJson => {
            quote! { ts_bindgen_rt::from_serde_or_undefined(#name)#error_mapper }
        }
        SerializationType::Fn => {
            quote! { &#fn_name }
        }
    }
}

fn render_rust_to_jsvalue_conversion(
    name: &Identifier,
    fn_name: &Identifier,
    typ: &TypeRef,
    error_mapper: TokenStream2,
) -> TokenStream2 {
    let serialization_type = typ.serialization_type();
    match serialization_type {
        SerializationType::Raw | SerializationType::Ref => quote! { JsValue::from(#name) },
        SerializationType::SerdeJson => {
            quote! { ts_bindgen_rt::from_serde_or_undefined(#name)#error_mapper }
        }
        SerializationType::Fn => {
            quote! { &#fn_name }
        }
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
}

impl<'a> ToTokens for WrapperFunc<'a> {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let fn_name = Self::to_rust_name(self.js_name);

        let internal_fn_name = InternalFunc::to_internal_rust_name(self.js_name);
        let our_toks = self
            .func
            .exposed_to_rust_wrapper_fn(&fn_name, &internal_fn_name);

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

impl Named for TypeRef {
    fn to_name(&self) -> (&str, Identifier) {
        let (n, mut id) = self.referent.to_name();
        id.type_params = self.type_params.iter().map(|t| t.to_name().1).collect();
        (n, id)
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
    let ti = RefCell::borrow(types_by_ident_by_path)
        .get(path)
        .and_then(|t_by_id| {
            t_by_id.get(id).or_else(|| match id {
                // TODO: Name and Local are interchangable. should be resolved as part of ir
                // transformation...
                TypeIdent::LocalName(n) => t_by_id.get(&TypeIdent::Name {
                    file: path.clone(),
                    name: n.clone(),
                }),
                TypeIdent::Name { file: _, name } => {
                    t_by_id.get(&TypeIdent::LocalName(name.clone()))
                }
                _ => None,
            })
        })
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

fn is_potentially_undefined<T: UndefinedHandler>(t: &T) -> bool {
    t.is_potentially_undefined()
}

trait UndefinedHandler {
    /// Is self potentially undefined if the type were living on its own.
    /// That is, we ignore the possibility of an optional field of this type being undefined
    /// because that is a property of the field and not the type.
    fn is_potentially_undefined(&self) -> bool;
}

impl UndefinedHandler for Union {
    fn is_potentially_undefined(&self) -> bool {
        let (und, std) = self.undefined_and_standard_members();
        let has_direct_undefined_member = !und.is_empty();
        has_direct_undefined_member || std.iter().any(|t| t.is_potentially_undefined())
    }
}

impl UndefinedHandler for TypeRef {
    fn is_potentially_undefined(&self) -> bool {
        self.resolve_target_type()
            .as_ref()
            .map(is_potentially_undefined)
            .unwrap_or(false)
    }
}

impl UndefinedHandler for TargetEnrichedTypeInfo {
    fn is_potentially_undefined(&self) -> bool {
        trait_impl_for_type_info!(
            match self,
            is_potentially_undefined | false,
            TargetEnrichedTypeInfo::Interface(_) => false,
            TargetEnrichedTypeInfo::Enum(_) => false,
            TargetEnrichedTypeInfo::Ref(TypeRef {
                referent: TypeIdent::Builtin(
                    Builtin::PrimitiveUndefined
                    | Builtin::PrimitiveAny
                    | Builtin::PrimitiveObject
                    | Builtin::PrimitiveVoid,
                ),
                ..
            }) => true,
            TargetEnrichedTypeInfo::Ref(TypeRef {
                referent: TypeIdent::Builtin(_),
                ..
            }) => false,
            TargetEnrichedTypeInfo::Array { .. } => false,
            TargetEnrichedTypeInfo::Optional { .. } => true,
            TargetEnrichedTypeInfo::Union(u) => u.is_potentially_undefined(),
            TargetEnrichedTypeInfo::Intersection(i) => i.types.iter().any(is_potentially_undefined),
            TargetEnrichedTypeInfo::Tuple(_) => false,
            TargetEnrichedTypeInfo::Mapped { .. } => false,
            TargetEnrichedTypeInfo::Func(_) => false,
            TargetEnrichedTypeInfo::Constructor(_) => false,
            TargetEnrichedTypeInfo::Class(_) => false,
            TargetEnrichedTypeInfo::Var { .. } => false,
        )
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
                let Interface {
                    indexer,
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
                let trait_defn = render_trait_defn(&name, type_params, iface, &iface.context);

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

                quote! {
                    #[allow(dead_code)]
                    #vis type #name #tps = #target;
                }
            }
            //TargetEnrichedTypeInfo::Ref(_) => panic!("ref isn't a top-level type"),
            //TargetEnrichedTypeInfo::Array { .. } => panic!("Array isn't a top-level type"),
            //TargetEnrichedTypeInfo::Optional { .. } => panic!("Optional isn't a top-level type"),
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
                let path = context.js_module_path();
                let mut attrs = vec![quote! { js_name = #js_name }];
                if let Some(TypeRef {
                    referent,
                    type_params,
                    ..
                }) = super_class.as_ref()
                {
                    let (_, super_name) = referent.to_name();

                    attrs.push(quote! {
                        extends = #super_name
                    });
                }

                let member_defs = members.iter().map(|(member_js_name, member)| {
                    let member_js_ident = format_ident!("{}", member_js_name);
                    match member {
                        Member::Constructor(ctor) => {
                            let param_toks = ctor
                                .params()
                                .map(|p| p.as_exposed_to_rust_named_param_list());

                            quote! {
                                #[wasm_bindgen(constructor)]
                                pub fn new(#(#param_toks),*) -> #name;
                            }
                        }
                        Member::Method(func) => {
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

                            quote! {
                                #[wasm_bindgen(#(#attrs),*)]
                                #f;
                            }
                        }
                        Member::Property(typ) => {
                            let member_name = to_snake_case_ident(member_js_name);
                            let setter_name = format_ident!("set_{}", member_name.to_string());
                            // TODO: don't add structural if the property is actually a
                            // javascript getter/setter
                            quote! {
                                #[wasm_bindgen(method, structural, getter = #member_js_ident)]
                                fn #member_name(this: &#name) -> #typ;

                                #[wasm_bindgen(method, structural, setter = #member_js_ident)]
                                fn #setter_name(this: &#name, value: #typ);
                            }
                        }
                    }
                });

                let public_methods = members
                    .iter()
                    .filter_map(|(js_name, member)| match member {
                        Member::Method(m) => Some((js_name, m)),
                        _ => None,
                    })
                    .map(|(js_name, method)| {
                        let fn_name = to_snake_case_ident(js_name);
                        let internal_fn_name = InternalFunc::to_internal_rust_name(js_name);
                        method.exposed_to_rust_wrapper_fn(&fn_name, &internal_fn_name)
                    });

                let trait_defn = render_trait_defn(&name, &type_params, class, &class.context);

                quote! {
                    #[wasm_bindgen(module = #path)]
                    extern "C" {
                        #[wasm_bindgen(#(#attrs),*)]
                        #vis type #name;

                        #(#member_defs)*
                    }

                    impl #name {
                        #(#public_methods)*
                    }

                    #trait_defn

                    impl Clone for #name {
                        fn clone(&self) -> Self {
                            Self { obj: self.obj.clone() }
                        }
                    }

                    impl serde::ser::Serialize for #name {
                        fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
                        where
                            S: serde::ser::Serializer,
                        {
                            ts_bindgen_rt::serialize_as_jsvalue(serializer, self)
                        }
                    }

                    impl<'de> serde::de::Deserialize<'de> for #name {
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
                        let fields = isect
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
                            .flat_map(|iface| get_recursive_fields(&iface))
                            .collect();

                        let indexer = isect
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
                            .filter_map(|iface| iface.indexer)
                            .next();

                        let typ = TargetEnrichedType {
                            name: self.name.clone(),
                            is_exported: self.is_exported,
                            info: TargetEnrichedTypeInfo::Interface(Interface {
                                indexer,
                                fields,
                                extends: Default::default(),
                                context: isect.context.clone(),
                                type_params: Default::default(), // TODO: copy over type params from isect
                            }),
                            context: isect.context.clone(),
                        };

                        quote! {
                            #typ
                        }
                    } else {
                        // TODO: this is weird, do we ever run into trouble with this?
                        let mut typ = self.clone();
                        typ.info = first_type.clone();
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
                    .params()
                    .map(|p| p.as_exposed_to_rust_unnamed_param_list());
                let ret = fn_types::exposed_to_rust_return_type(&self.return_type());
                quote! {
                    dyn #name(#(#params),*) -> Result<#ret, JsValue>
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
        let (_, type_name) = typ.referent.to_name();
        let type_name = type_name.to_string();
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
        let rendered_type = if matches!(
            typ,
            TypeRef {
                referent: TypeIdent::Builtin(Builtin::Fn),
                ..
            }
        ) {
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
            quote! {
                std::rc::Rc<#typ>
            }
        } else {
            quote! { #typ }
        };

        let our_toks = quote! {
            #[serde(#(#serde_attrs),*)]
            pub #field_name: #rendered_type
        };

        toks.append_all(our_toks);
    }
}

trait TraitName {
    fn trait_name(&self) -> Identifier;
}

impl TraitName for Identifier {
    fn trait_name(&self) -> Identifier {
        self.suffix_name("Trait")
    }
}

impl TraitName for TypeRef {
    fn trait_name(&self) -> Identifier {
        self.to_name().1.trait_name()
    }
}

type BoxedTypeRefIter<'a> = Box<dyn Iterator<Item = TypeRef> + 'a>;

trait Traitable {
    fn has_super_traits(&self) -> bool;

    fn super_traits<'a>(&'a self) -> BoxedTypeRefIter<'a>;

    fn recursive_super_traits<'a>(&'a self) -> BoxedTypeRefIter<'a> {
        Box::new(self.super_traits().fold(
            Box::new(iter::empty()) as BoxedTypeRefIter<'a>,
            |cur, s| {
                let supers = s
                    .resolve_target_type()
                    .map(|t| {
                        Box::new(
                            t.recursive_super_traits()
                                .collect::<Vec<TypeRef>>()
                                .into_iter(),
                        ) as BoxedTypeRefIter<'a>
                    })
                    .unwrap_or_else(|| Box::new(iter::empty()) as BoxedTypeRefIter<'a>);
                Box::new(
                    cur.chain(Box::new(iter::once(s)) as BoxedTypeRefIter<'a>)
                        .chain(supers),
                ) as BoxedTypeRefIter<'a>
            },
        )) as BoxedTypeRefIter<'a>
    }
}

impl Traitable for Interface {
    fn has_super_traits(&self) -> bool {
        !self.extends.is_empty()
    }

    fn super_traits<'a>(&'a self) -> BoxedTypeRefIter<'a> {
        Box::new(self.extends.iter().cloned()) as BoxedTypeRefIter<'a>
    }
}

impl Traitable for Class {
    fn has_super_traits(&self) -> bool {
        self.super_class.is_some() || !self.implements.is_empty()
    }

    fn super_traits<'a>(&'a self) -> BoxedTypeRefIter<'a> {
        let super_class = self
            .super_class
            .as_ref()
            .map(|s| Box::new(iter::once(s).cloned()) as BoxedTypeRefIter<'a>)
            .unwrap_or_else(|| Box::new(iter::empty()) as BoxedTypeRefIter<'a>);
        let implements = self.implements.iter().cloned();

        Box::new(super_class.chain(implements))
    }
}

impl Traitable for TargetEnrichedTypeInfo {
    fn has_super_traits(&self) -> bool {
        match self {
            TargetEnrichedTypeInfo::Class(c) => c.has_super_traits(),
            TargetEnrichedTypeInfo::Interface(i) => i.has_super_traits(),
            _ => false,
        }
    }

    fn super_traits<'a>(&'a self) -> BoxedTypeRefIter<'a> {
        match self {
            TargetEnrichedTypeInfo::Class(c) => c.super_traits(),
            TargetEnrichedTypeInfo::Interface(i) => i.super_traits(),
            _ => Box::new(iter::empty()) as BoxedTypeRefIter<'a>,
        }
    }
}

impl Traitable for TypeRef {
    fn has_super_traits(&self) -> bool {
        self.resolve_target_type()
            .map(|t| t.has_super_traits())
            .unwrap_or(false)
    }

    fn super_traits<'a>(&'a self) -> BoxedTypeRefIter<'a> {
        self.resolve_target_type()
            .map(|t| {
                Box::new(t.super_traits().collect::<Vec<_>>().into_iter()) as BoxedTypeRefIter<'a>
            })
            .unwrap_or_else(|| Box::new(iter::empty()) as BoxedTypeRefIter<'a>)
    }
}

trait NameWithGenericEnv {
    fn name_with_generic_env(&self, type_env: &[TypeRef]) -> Identifier;
}

impl NameWithGenericEnv for Identifier {
    fn name_with_generic_env(&self, type_env: &[TypeRef]) -> Identifier {
        if self.type_params.len() != type_env.len() {
            // TODO: panic
        }

        let tps: Vec<_> = type_env.iter().map(|t| t.to_name().1).collect();
        self.with_type_params(&tps)
    }
}

fn render_trait_defn<T>(
    name: &Identifier,
    type_params: &[(String, TypeParamConfig)],
    item: &T,
    ctx: &Context,
) -> TokenStream2
where
    T: Traitable,
{
    let tps = render_type_params(type_params);
    let full_name = quote! {
        #name #tps
    };
    let (super_decl, super_impls) = if item.has_super_traits() {
        // TODO: would be nice to mark Identifiers with the type of identifier they are
        // e.g. mark anything coming out of a TraitName::trait_name as a trait
        // identifier
        let supers = item.super_traits().map(|s| s.trait_name());
        let super_decl = quote! {
            : #(#supers)+*
        };
        let make_impl = |tr: TypeRef| {
            // TODO: tps needs to also include the trait generics
            let trait_name = tr.trait_name();
            let supers: Vec<_> = tr
                .super_traits()
                .map(|s| {
                    s.trait_name()
                        .name_with_generic_env(tr.type_params.as_slice())
                })
                .collect();
            let preds = if supers.is_empty() {
                quote! {}
            } else {
                quote! {
                    where #full_name: #(#supers)+*
                }
            };
            quote! {
                impl #tps #trait_name for #full_name #preds {}
            }
        };
        let super_impls = item
            .recursive_super_traits()
            .chain(Box::new(iter::once(TypeRef {
                referent: TypeIdent::LocalName(name.to_string()),
                type_params: type_params
                    .iter()
                    .map(|(n, _)| TypeRef {
                        referent: TypeIdent::LocalName(n.clone()),
                        type_params: Default::default(),
                        context: ctx.clone(),
                    })
                    .collect(),
                context: ctx.clone(),
            })))
            .map(&make_impl);
        let super_impls = quote! {
            #(#super_impls)*
        };

        (super_decl, super_impls)
    } else {
        (quote! {}, quote! {})
    };

    // TODO: add field accessors and setters

    let trait_name = name.trait_name();

    quote! {
        trait #trait_name #tps #super_decl {
        }

        #super_impls
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
            fn #deserialize_fn_name<'de, D>(deserializer: D) -> std::result::Result<std::rc::Rc<#tr>, D::Error>
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
                    }) as std::rc::Rc<#tr>
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
        Some(quote! {
            #[allow(non_snake_case)]
            fn #serialize_fn_name<S>(#field_name: &std::rc::Rc<#tr>, serializer: S) -> std::result::Result<S::Ok, S::Error>
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

fn render_type_params(type_params: &[(String, TypeParamConfig)]) -> TokenStream2 {
    let type_param_toks = type_params.iter().map(|(n, _t)| {
        let n = to_camel_case_ident(n);
        quote! {
            #n
        }
    });
    if type_params.is_empty() {
        quote! {}
    } else {
        quote! {
            <#(#type_param_toks),*>
        }
    }
}
