use crate::codegen::named::SimpleNamed;
use crate::codegen::type_ref_like::{OwnedTypeRef, TypeRefLike};
use crate::codegen::ResolveTargetType;
use crate::identifier::to_camel_case_ident;
use crate::ir::{Ctor, Func, Interface, Param, TargetEnrichedTypeInfo, TypeParamConfig, TypeRef};
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use std::borrow::Cow;
use std::collections::HashMap;

pub trait ResolveGeneric: Clone {
    /// resolve this possibly-generic-referencing type into a TypeRef
    /// in the given type_env.
    fn resolve_generic_in_env<'a>(
        &'a self,
        type_env: &'a HashMap<String, TypeRef>,
    ) -> Cow<'a, Self>;
}

impl ResolveGeneric for TypeRef {
    fn resolve_generic_in_env<'a>(
        &'a self,
        type_env: &'a HashMap<String, TypeRef>,
    ) -> Cow<'a, Self> {
        // TODO: type_envs and type_params should key off of something better than String...
        let mut resolved = type_env
            .get(&self.to_simple_name().to_string())
            .unwrap_or(self)
            .clone();
        resolved.type_params = resolved
            .type_params
            .into_iter()
            .map(|tp| tp.resolve_generic_in_env(type_env).into_owned())
            .collect();
        Cow::Owned(resolved)
    }
}

impl<'b> ResolveGeneric for TypeRefLike<'b> {
    fn resolve_generic_in_env<'a>(
        &'a self,
        type_env: &'a HashMap<String, TypeRef>,
    ) -> Cow<'a, Self> {
        match self {
            TypeRefLike::TypeRef(tr) => Cow::Owned(TypeRefLike::TypeRef(Cow::Owned(
                tr.resolve_generic_in_env(type_env).into_owned(),
            ))),
            TypeRefLike::OwnedTypeRef(tr) => Cow::Owned(TypeRefLike::OwnedTypeRef(Cow::Owned(
                tr.resolve_generic_in_env(type_env).into_owned(),
            ))),
        }
    }
}

impl<'b> ResolveGeneric for OwnedTypeRef<'b> {
    fn resolve_generic_in_env<'a>(
        &'a self,
        type_env: &'a HashMap<String, TypeRef>,
    ) -> Cow<'a, Self> {
        Cow::Owned(OwnedTypeRef(Cow::Owned(
            self.0.resolve_generic_in_env(type_env).into_owned(),
        )))
    }
}

impl ResolveGeneric for Func {
    fn resolve_generic_in_env<'a>(
        &'a self,
        type_env: &'a HashMap<String, TypeRef>,
    ) -> Cow<'a, Self> {
        Cow::Owned(Func {
            type_params: self.type_params.clone(),
            params: self
                .params
                .iter()
                .map(|p| p.resolve_generic_in_env(type_env).into_owned())
                .collect(),
            return_type: Box::new(
                self.return_type
                    .resolve_generic_in_env(type_env)
                    .into_owned(),
            ),
            class_name: self.class_name.clone(),
            context: self.context.clone(),
        })
    }
}

impl ResolveGeneric for Ctor {
    fn resolve_generic_in_env<'a>(
        &'a self,
        type_env: &'a HashMap<String, TypeRef>,
    ) -> Cow<'a, Self> {
        Cow::Owned(Ctor {
            params: self
                .params
                .iter()
                .map(|p| p.resolve_generic_in_env(type_env).into_owned())
                .collect(),
            context: self.context.clone(),
        })
    }
}

impl ResolveGeneric for Param {
    fn resolve_generic_in_env<'a>(
        &'a self,
        type_env: &'a HashMap<String, TypeRef>,
    ) -> Cow<'a, Self> {
        Cow::Owned(Param {
            name: self.name.clone(),
            type_info: self.type_info.resolve_generic_in_env(type_env).into_owned(),
            is_variadic: self.is_variadic,
            context: self.context.clone(),
        })
    }
}

pub trait TypeEnvImplying {
    /// type params are implied by mapping the type_params of self
    /// to the type_params of the resolved type of self. So, if self
    /// is a TypeRef of A<string> and self.resolve_target_type() yields
    /// A<T>, we have the implied mapping of {T => string}.
    fn type_env(&self) -> HashMap<String, TypeRef>;
}

impl TypeEnvImplying for TypeRef {
    fn type_env(&self) -> HashMap<String, TypeRef> {
        self.resolve_target_type()
            .and_then(|t| {
                t.required_type_params().map(|tps| {
                    tps.iter()
                        .zip(self.type_params.iter())
                        .map(|((n, _), t)| (n.clone(), t.clone()))
                        .collect()
                })
            })
            .unwrap_or_default()
    }
}

pub fn apply_type_params<P, R>(
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
            .cloned()
            .or(cfg.default_type_arg)
            .unwrap()
            .resolve_generic_in_env(type_env)
            .into_owned();
        result.insert(type_param, type_value);
    }

    result
}

pub trait ProvidedTypeParams {
    /// Get the provided type parameters for this reference
    fn provided_type_params(&self) -> &'_ [TypeRef];
}

impl ProvidedTypeParams for TypeRef {
    fn provided_type_params(&self) -> &'_ [TypeRef] {
        self.type_params.as_slice()
    }
}

pub trait RequiredTypeParams {
    /// Get the required type parameters for instantiations of self
    fn required_type_params(&self) -> Option<&'_ [(String, TypeParamConfig)]>;
}

impl RequiredTypeParams for TargetEnrichedTypeInfo {
    fn required_type_params(&self) -> Option<&'_ [(String, TypeParamConfig)]> {
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
    fn required_type_params(&self) -> Option<&'_ [(String, TypeParamConfig)]> {
        Some(self.type_params.as_slice())
    }
}

pub fn render_type_params(type_params: &[(String, TypeParamConfig)]) -> TokenStream2 {
    render_type_params_with_lifetimes(type_params, &[])
}

pub fn render_type_params_with_lifetimes(
    type_params: &[(String, TypeParamConfig)],
    lifetimes: &[&str],
) -> TokenStream2 {
    if lifetimes.is_empty() && type_params.is_empty() {
        quote! {}
    } else {
        let type_param_toks = lifetimes
            .iter()
            .map(|l| {
                let l = format!("'{}", l);
                let l = syn::Lifetime::new(&l, proc_macro2::Span::call_site());
                quote! { #l }
            })
            .chain(type_params.iter().map(|(n, _t)| {
                let n = to_camel_case_ident(n);
                quote! {
                    #n
                }
            }));
        quote! {
            <#(#type_param_toks),*>
        }
    }
}

pub fn render_type_params_with_constraints(
    type_params: &[(String, TypeParamConfig)],
    extra_constraints: &[TokenStream2],
) -> TokenStream2 {
    render_type_params_with_lifetimes_and_constraints(type_params, &[], extra_constraints)
}

pub fn render_type_params_with_lifetimes_and_constraints(
    type_params: &[(String, TypeParamConfig)],
    lifetimes: &[&str],
    extra_constraints: &[TokenStream2],
) -> TokenStream2 {
    if lifetimes.is_empty() && type_params.is_empty() {
        quote! {}
    } else {
        let type_param_toks = lifetimes
            .iter()
            .map(|l| {
                let l = format!("'{}", l);
                let l = syn::Lifetime::new(&l, proc_macro2::Span::call_site());
                quote! { #l }
            })
            .chain(type_params.iter().map(|(n, _t)| {
                let constraints = if extra_constraints.is_empty() {
                    quote! {}
                } else {
                    quote! {
                        : #(#extra_constraints)+*
                    }
                };
                let n = to_camel_case_ident(n);
                quote! {
                    #n #constraints
                }
            }));
        quote! {
            <#(#type_param_toks),*>
        }
    }
}
