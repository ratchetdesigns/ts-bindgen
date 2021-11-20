use crate::codegen::named::Named;
use crate::codegen::ResolveTargetType;
use crate::identifier::{to_camel_case_ident, Identifier};
use crate::ir::{Interface, TargetEnrichedTypeInfo, TypeParamConfig, TypeRef};
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use std::collections::HashMap;

pub trait ResolveGeneric {
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

pub trait TypeEnvImplying {
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
            .unwrap_or_else(|| Default::default())
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
            .map(|p| p.clone())
            .or(cfg.default_type_arg)
            .unwrap()
            .resolve_generic_in_env(type_env)
            .clone();
        result.insert(type_param, type_value);
    }

    result
}

pub trait ProvidedTypeParams {
    /// Get the provided type parameters for this reference
    fn provided_type_params<'a>(&'a self) -> &'a [TypeRef];
}

impl ProvidedTypeParams for TypeRef {
    fn provided_type_params<'a>(&'a self) -> &'a [TypeRef] {
        self.type_params.as_slice()
    }
}

pub trait RequiredTypeParams {
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

pub trait WithTypeEnv {
    fn with_type_env(&self, type_env: &[TypeRef]) -> Self;
}

impl WithTypeEnv for TypeRef {
    fn with_type_env(&self, type_env: &[TypeRef]) -> Self {
        let mut t = self.clone();
        t.type_params = type_env
            .iter()
            .chain(t.type_params.iter().skip(type_env.len()))
            .cloned()
            .collect();
        t
    }
}

pub trait NameWithGenericEnv {
    fn name_with_generic_env(&self, type_env: &[TypeRef]) -> Identifier;
}

impl NameWithGenericEnv for Identifier {
    fn name_with_generic_env(&self, type_env: &[TypeRef]) -> Identifier {
        let tps: Vec<_> = type_env.iter().map(|t| t.to_name().1).collect();
        self.with_type_params(&tps)
    }
}

pub fn render_type_params(type_params: &[(String, TypeParamConfig)]) -> TokenStream2 {
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

pub fn render_type_params_with_constraints(
    type_params: &[(String, TypeParamConfig)],
    extra_constraints: Vec<TokenStream2>,
) -> TokenStream2 {
    let type_param_toks = type_params.iter().map(|(n, _t)| {
        // TODO: deal with t.constraint
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
    });
    if type_params.is_empty() {
        quote! {}
    } else {
        quote! {
            <#(#type_param_toks),*>
        }
    }
}
