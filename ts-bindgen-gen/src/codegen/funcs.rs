use crate::codegen::named::{CasedTypeIdent, Named};
use crate::codegen::resolve_target_type::ResolveTargetType;
use crate::codegen::serialization_type::{SerializationType, SerializationTypeGetter};
use crate::codegen::traits::TraitMember;
use crate::codegen::type_ref_like::{OwnedTypeRef, TypeRefLike};
use crate::identifier::{to_snake_case_ident, Identifier};
use crate::ir::{
    Builtin, Class, Context, Ctor, Func, Param, TargetEnrichedTypeInfo, TypeIdent, TypeRef,
};
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use std::borrow::Cow;
use std::collections::HashMap;
use std::iter;
use std::path::{Path, PathBuf};

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

type BoxedParamExtIter<'a> = Box<dyn Iterator<Item = Box<dyn ParamExt>> + 'a>;

pub trait HasFnPrototype {
    fn return_type(&self) -> TypeRef;
    fn params(&self) -> BoxedParamExtIter<'_>;
    fn args(&self) -> BoxedParamExtIter<'_>;
    fn is_member(&self) -> bool;
    fn is_variadic(&self) -> bool {
        self.params().any(|p| p.is_variadic())
    }
}

// it's a bit of a stretch to impl HasFnPrototype for TypeRef since this is
// only valid if TypeRef::referent == TypeIdent::Builtin(Builtin::Fn) but
// this is quite useful
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

    fn params(&self) -> BoxedParamExtIter<'_> {
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

    fn args(&self) -> BoxedParamExtIter<'_> {
        // args and params are the same for non-members
        self.params()
    }

    fn is_member(&self) -> bool {
        false
    }
}

impl<'b> HasFnPrototype for OwnedTypeRef<'b> {
    fn return_type(&self) -> TypeRef {
        self.0.return_type()
    }

    fn params(&self) -> BoxedParamExtIter<'_> {
        self.0.params()
    }

    fn args(&self) -> BoxedParamExtIter<'_> {
        self.0.args()
    }

    fn is_member(&self) -> bool {
        self.0.is_member()
    }
}

#[derive(Debug, Clone)]
struct SelfParam {
    class_name: TypeIdent,
    is_mut: bool,
}

/// Represents an owned parameter. Needed to render the type of the Param
/// as owned (sized).
#[derive(Debug, Clone)]
struct OwnedParam<'a> {
    name: &'a str,
    type_ref: OwnedTypeRef<'a>,
    is_variadic: bool,
}

impl HasFnPrototype for Func {
    fn return_type(&self) -> TypeRef {
        (*self.return_type).clone()
    }

    fn params(&self) -> BoxedParamExtIter<'_> {
        let reg_params = self.args();

        if let Some(class_name) = &self.class_name {
            Box::new(
                iter::once(Box::new(SelfParam {
                    class_name: class_name.clone(),
                    is_mut: false,
                }) as Box<dyn ParamExt>)
                .chain(reg_params),
            )
        } else {
            Box::new(reg_params)
        }
    }

    fn args(&self) -> BoxedParamExtIter<'_> {
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

impl<'b> HasFnPrototype for Constructor<'b> {
    fn return_type(&self) -> TypeRef {
        (*self.class).clone()
    }

    fn params(&self) -> BoxedParamExtIter<'_> {
        Box::new(
            self.ctor
                .params
                .iter()
                .map(|p| Box::new(p.clone()) as Box<dyn ParamExt>),
        )
    }

    fn args(&self) -> BoxedParamExtIter<'_> {
        self.params()
    }

    fn is_member(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone)]
pub struct PropertyAccessor {
    pub property_name: Identifier,
    pub typ: TypeRef,
    pub class_name: TypeIdent,
    pub access_type: AccessType,
}

impl PropertyAccessor {
    pub fn getter_fn(&self) -> Func {
        Func {
            type_params: Default::default(),
            params: Default::default(),
            return_type: Box::new(self.typ.clone()),
            class_name: Some(self.class_name.clone()),
            context: self.typ.context.clone(),
        }
    }

    pub fn setter_fn(&self) -> Func {
        Func {
            type_params: Default::default(),
            params: vec![Param {
                name: "value".to_string(),
                type_info: self.typ.clone(),
                is_variadic: false,
                context: self.typ.context.clone(),
            }],
            return_type: Box::new(TypeRef {
                referent: TypeIdent::Builtin(Builtin::PrimitiveVoid),
                type_params: Default::default(),
                context: self.typ.context.clone(),
            }),
            class_name: Some(self.class_name.clone()),
            context: self.typ.context.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AccessType {
    Getter,
    Setter,
}

impl HasFnPrototype for PropertyAccessor {
    fn return_type(&self) -> TypeRef {
        match self.access_type {
            AccessType::Getter => self.typ.clone(),
            AccessType::Setter => TypeRef {
                referent: TypeIdent::Builtin(Builtin::PrimitiveVoid),
                type_params: Default::default(),
                context: self.typ.context.clone(),
            },
        }
    }

    fn params(&'_ self) -> BoxedParamExtIter<'_> {
        let self_param = Box::new(iter::once(Box::new(SelfParam {
            class_name: self.class_name.clone(),
            is_mut: self.access_type == AccessType::Setter,
        }) as Box<dyn ParamExt>)) as BoxedParamExtIter<'_>;

        match self.access_type {
            AccessType::Getter => self_param,
            AccessType::Setter => Box::new(self_param.chain(iter::once(Box::new(OwnedParam {
                name: "value",
                type_ref: OwnedTypeRef(Cow::Owned(self.typ.clone())),
                is_variadic: false,
            })
                as Box<dyn ParamExt>))) as BoxedParamExtIter<'_>,
        }
    }

    fn args(&self) -> BoxedParamExtIter<'_> {
        self.params()
    }

    fn is_member(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone)]
pub struct Constructor<'a> {
    pub class: Cow<'a, TypeRef>,
    pub ctor: Cow<'a, Ctor>,
}

impl<'a> Constructor<'a> {
    pub fn new(ctor: Cow<'a, Ctor>, class_name: TypeIdent) -> Constructor<'a> {
        let class_ref = TypeRef {
            referent: class_name,
            type_params: Default::default(),
            context: (*ctor).context.clone(),
        };
        Constructor {
            class: Cow::Owned(class_ref),
            ctor,
        }
    }
}

impl<'b> HasFnPrototype for TypeRefLike<'b> {
    fn return_type(&self) -> TypeRef {
        match self {
            TypeRefLike::TypeRef(t) => t.return_type(),
            TypeRefLike::OwnedTypeRef(t) => t.return_type(),
        }
    }

    fn params(&self) -> BoxedParamExtIter<'_> {
        match self {
            TypeRefLike::TypeRef(t) => t.params(),
            TypeRefLike::OwnedTypeRef(t) => t.params(),
        }
    }

    fn args(&self) -> BoxedParamExtIter<'_> {
        match self {
            TypeRefLike::TypeRef(t) => t.args(),
            TypeRefLike::OwnedTypeRef(t) => t.args(),
        }
    }

    fn is_member(&self) -> bool {
        match self {
            TypeRefLike::TypeRef(t) => t.is_member(),
            TypeRefLike::OwnedTypeRef(t) => t.is_member(),
        }
    }
}

pub mod fn_types {
    use super::{
        Builtin, HasFnPrototype, OwnedTypeRef, ResolveTargetType, SerializationType,
        SerializationTypeGetter, TargetEnrichedTypeInfo, TokenStream2, TypeIdent, TypeRef,
        TypeRefLike,
    };
    use quote::quote;
    use std::borrow::Cow;

    fn exposed_to_js_type(typ: &TypeRefLike) -> TokenStream2 {
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
                        let ret = exposed_to_js_param_type(&typ.return_type().into());
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

    pub fn exposed_to_js_param_type(typ: &TypeRefLike) -> TokenStream2 {
        exposed_to_js_type(typ)
    }

    pub fn exposed_to_js_return_type(typ: &TypeRef) -> TokenStream2 {
        let t = exposed_to_js_param_type(&typ.into());
        quote! {
            std::result::Result<#t, JsValue>
        }
    }

    fn exposed_to_rust_type(typ: &TypeRefLike) -> TokenStream2 {
        let serialization_type = typ.serialization_type();
        match serialization_type {
            SerializationType::Raw | SerializationType::SerdeJson => quote! { #typ },
            SerializationType::Ref => quote! { &#typ },
            SerializationType::Fn => match typ {
                // TODO: fix this leaky abstraction...
                TypeRefLike::TypeRef(_) => quote! { &'static #typ },
                TypeRefLike::OwnedTypeRef(_) => quote! { #typ },
            },
        }
    }

    pub fn exposed_to_rust_param_type(typ: &TypeRefLike) -> TokenStream2 {
        exposed_to_rust_type(typ)
    }

    pub fn exposed_to_rust_return_type(typ: &TypeRef, is_fallible: bool) -> TokenStream2 {
        let rendered_type = OwnedTypeRef(Cow::Borrowed(typ));
        if is_fallible {
            quote! {
                std::result::Result<#rendered_type, JsValue>
            }
        } else {
            quote! {
                #rendered_type
            }
        }
    }
}

pub trait FnPrototypeExt {
    fn exposed_to_js_closure<Body: ToTokens>(&self, body: Body) -> TokenStream2;
    fn exposed_to_js_fn_type(&self) -> TokenStream2;

    fn exposed_to_js_boxed_fn_type(&self) -> TokenStream2;

    fn exposed_to_js_wrapped_closure<Body: ToTokens>(&self, body: Body) -> TokenStream2;

    fn exposed_to_js_fn_decl<Name: ToTokens>(&self, name: Name) -> TokenStream2;

    fn invoke_with_name<Name: ToTokens>(&self, name: Name) -> TokenStream2;

    fn fully_qualified_invoke_with_name<SelfArg: ToTokens>(
        &self,
        name: &Identifier,
        self_arg: Option<SelfArg>,
    ) -> TokenStream2;

    fn exposed_to_rust_fn_decl<Name: ToTokens>(
        &self,
        name: Name,
        is_fallible: bool,
    ) -> TokenStream2;

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

    fn exposed_to_rust_generic_wrapper_fn<ResConverter>(
        &self,
        fn_name: &Identifier,
        internal_fn_target: Option<&TokenStream2>,
        internal_fn_name: &Identifier,
        is_fallible: bool,
        result_converter: Option<&ResConverter>,
        type_env: &HashMap<String, TypeRef>,
    ) -> TokenStream2
    where
        ResConverter: Fn(TokenStream2) -> TokenStream2;
}

fn is_generic_type<T: ResolveTargetType>(t: &T) -> bool {
    // TODO: this is obviously wrong...
    t.resolve_target_type().is_none()
}

impl<T: HasFnPrototype + ?Sized> FnPrototypeExt for T {
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

    fn fully_qualified_invoke_with_name<SelfArg: ToTokens>(
        &self,
        name: &Identifier,
        self_arg: Option<SelfArg>,
    ) -> TokenStream2 {
        let name = name.without_type_params(); // we want A::whatever(), not A<T>::whatever() in rust
        let args = self.args().map(|p| p.rust_name()).map(|a| quote! { #a });
        let args: Box<dyn Iterator<Item = TokenStream2>> = if let Some(s) = self_arg {
            Box::new(iter::once(quote! { #s }).chain(args))
        } else {
            Box::new(args)
        };
        quote! {
            #name(#(#args),*)
        }
    }

    fn exposed_to_rust_fn_decl<Name: ToTokens>(
        &self,
        name: Name,
        is_fallible: bool,
    ) -> TokenStream2 {
        let params = self
            .params()
            .map(|p| p.as_exposed_to_rust_named_param_list());
        let ret = fn_types::exposed_to_rust_return_type(&self.return_type(), is_fallible);
        quote! {
            fn #name(#(#params),*) -> #ret
        }
    }

    fn exposed_to_rust_param_wrappers(&self) -> TokenStream2 {
        let wrapper_fns = self.params().filter_map(|p| {
            let name = p.local_fn_name();
            let f = p.js_wrapper_fn();
            f.map(|f| {
                quote! {
                    #[allow(non_snake_case)]
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
        let ret = render_wasm_bindgen_return_to_js(&self.return_type(), &return_value, true);
        let wrapper_fns = self.exposed_to_rust_param_wrappers();

        let f = self.exposed_to_rust_fn_decl(fn_name, true);

        quote! {
            #[allow(dead_code)]
            pub #f {
                #wrapper_fns

                #ret
            }
        }
    }

    fn exposed_to_rust_generic_wrapper_fn<ResConverter>(
        &self,
        fn_name: &Identifier,
        internal_fn_target: Option<&TokenStream2>,
        internal_fn_name: &Identifier,
        is_fallible: bool,
        result_converter: Option<&ResConverter>,
        type_env: &HashMap<String, TypeRef>,
    ) -> TokenStream2
    where
        ResConverter: Fn(TokenStream2) -> TokenStream2,
    {
        let arg_converters = self.args().filter_map(|arg| {
            let tr = arg.type_ref();
            if is_generic_type(&tr) {
                let a_name = arg.rust_name();
                Some(quote! {
                    let #a_name = ts_bindgen_rt::jsvalue_serde::to_jsvalue(&#a_name).unwrap();
                })
            } else {
                None
            }
        });
        let ret_type = self.return_type();
        let ret_converter = if is_generic_type(&ret_type) {
            quote! {
                let result = ts_bindgen_rt::jsvalue_serde::from_jsvalue(&result).unwrap();
            }
        } else {
            quote! {}
        };
        let final_ret_converter = result_converter
            .map(|conv| conv(quote! { result }))
            .unwrap_or_else(|| quote! { result });
        let args = self.args().map(|p| p.rust_to_js_conversion());
        let internal_fn_target = internal_fn_target
            .map(|t| quote! { #t.})
            .unwrap_or_else(|| quote! {});
        let return_value = quote! {
            #internal_fn_target #internal_fn_name(#(#args),*)
        };
        let return_value = if is_fallible {
            quote! {
                #return_value?
            }
        } else {
            return_value
        };
        let ret = render_wasm_bindgen_return_to_js(&ret_type, &return_value, is_fallible);
        let wrapper_fns = self.exposed_to_rust_param_wrappers();

        let f = self.exposed_to_rust_fn_decl(fn_name, is_fallible);

        quote! {
            #[allow(dead_code)]
            pub #f {
                #(#arg_converters)*

                #wrapper_fns

                let result = #ret;
                #ret_converter
                #final_ret_converter
            }
        }
    }
}

pub trait ParamExt {
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

    /// The TypeRef for this param
    fn type_ref(&self) -> TypeRefLike<'_>;
}

pub trait WrappedParam {
    fn wrapped_type(&self) -> TypeRefLike<'_>;

    fn name(&self) -> &str;

    fn is_variadic(&self) -> bool;
}

impl WrappedParam for Param {
    fn wrapped_type(&self) -> TypeRefLike<'_> {
        (&self.type_info).into()
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn is_variadic(&self) -> bool {
        self.is_variadic
    }
}

impl<'a> WrappedParam for OwnedParam<'a> {
    fn wrapped_type(&self) -> TypeRefLike<'_> {
        (&self.type_ref).into()
    }

    fn name(&self) -> &str {
        self.name
    }

    fn is_variadic(&self) -> bool {
        self.is_variadic
    }
}

impl<T: WrappedParam> ParamExt for T {
    fn rust_name(&self) -> Identifier {
        to_snake_case_ident(self.name())
    }

    fn as_exposed_to_js_named_param_list(&self) -> TokenStream2 {
        let full_type = self.as_exposed_to_js_unnamed_param_list();
        let n = self.rust_name();
        quote! { #n: #full_type }
    }

    fn as_exposed_to_js_unnamed_param_list(&self) -> TokenStream2 {
        let wrapped = self.wrapped_type();
        let typ = fn_types::exposed_to_js_param_type(&wrapped);
        if self.is_variadic() {
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
        let wrapped = self.wrapped_type();
        let typ = fn_types::exposed_to_rust_param_type(&wrapped);
        if self.is_variadic() {
            quote! { &[#typ] }
        } else {
            quote! { #typ }
        }
    }

    fn js_to_rust_conversion(&self) -> TokenStream2 {
        let wrapped = self.wrapped_type();
        let serialization_type = wrapped.serialization_type();
        let name = self.rust_name();

        match serialization_type {
            SerializationType::Raw | SerializationType::Ref => quote! { #name },
            SerializationType::SerdeJson => {
                quote! {
                    ts_bindgen_rt::from_jsvalue(&#name).map_err(ts_bindgen_rt::Error::from)?
                }
            }
            SerializationType::Fn => {
                // TODO: we're not recursive yet
                unimplemented!();
            }
        }
    }

    fn local_fn_name(&self) -> Identifier {
        to_snake_case_ident(self.name()).prefix_name("__TSB_Local_")
    }

    fn rust_to_js_conversion(&self) -> TokenStream2 {
        let name = self.rust_name();
        let fn_name = self.local_fn_name();
        let wrapped = self.wrapped_type();
        render_rust_to_js_conversion(
            &name,
            &fn_name,
            &wrapped,
            quote! { .map_err(ts_bindgen_rt::Error::from)? },
        )
    }

    fn rust_to_jsvalue_conversion(&self) -> TokenStream2 {
        let name = self.rust_name();
        let fn_name = self.local_fn_name();
        let wrapped = self.wrapped_type();
        render_rust_to_jsvalue_conversion(&name, &fn_name, &wrapped, quote! {})
    }

    fn js_wrapper_fn(&self) -> Option<TokenStream2> {
        let wrapped = self.wrapped_type();
        let type_info = wrapped.resolve_target_type();
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
                &typ.return_type().into(),
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
        WrappedParam::is_variadic(self)
    }

    fn type_ref(&self) -> TypeRefLike<'_> {
        self.wrapped_type()
    }
}

impl ParamExt for SelfParam {
    fn rust_name(&self) -> Identifier {
        to_snake_case_ident("self")
    }

    fn as_exposed_to_js_named_param_list(&self) -> TokenStream2 {
        let class_name = self.as_exposed_to_js_unnamed_param_list();
        quote! { this: #class_name }
    }

    fn as_exposed_to_js_unnamed_param_list(&self) -> TokenStream2 {
        // TODO: this is very ugly... would love to just pass in the "type"
        // instead of an instance of type info.
        let class_name = CasedTypeIdent {
            type_ident: &self.class_name,
            type_info: &TargetEnrichedTypeInfo::Class(Class {
                super_class: None,
                members: Default::default(),
                type_params: Default::default(),
                implements: Default::default(),
                context: Context {
                    types_by_ident_by_path: Default::default(),
                    path: Path::new("/").to_path_buf(),
                },
            }),
        };
        let class_name = class_name.to_name().1;
        quote! { &#class_name}
    }

    fn as_exposed_to_rust_named_param_list(&self) -> TokenStream2 {
        if self.is_mut {
            quote! { &mut self }
        } else {
            quote! { &self }
        }
    }

    fn as_exposed_to_rust_unnamed_param_list(&self) -> TokenStream2 {
        self.as_exposed_to_rust_named_param_list()
    }

    fn js_to_rust_conversion(&self) -> TokenStream2 {
        self.as_exposed_to_rust_named_param_list()
    }

    fn local_fn_name(&self) -> Identifier {
        // this is never used...
        self.rust_name()
    }

    fn rust_to_js_conversion(&self) -> TokenStream2 {
        self.as_exposed_to_rust_named_param_list()
    }

    fn rust_to_jsvalue_conversion(&self) -> TokenStream2 {
        self.as_exposed_to_rust_named_param_list()
    }

    fn js_wrapper_fn(&self) -> Option<TokenStream2> {
        None
    }

    fn is_variadic(&self) -> bool {
        false
    }

    fn type_ref(&self) -> TypeRefLike<'_> {
        TypeRefLike::TypeRef(Cow::Owned(TypeRef {
            referent: self.class_name.clone(),
            type_params: Default::default(),
            context: Context {
                // we really have to get rid of this context bs
                types_by_ident_by_path: Default::default(),
                path: PathBuf::new(),
            },
        }))
    }
}

pub struct InternalFunc<'a> {
    pub func: &'a Func,
    pub js_name: &'a str,
}

impl<'a> InternalFunc<'a> {
    pub fn to_internal_rust_name(js_name: &str) -> Identifier {
        to_snake_case_ident(js_name).prefix_name("__TSB_")
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
    is_fallible: bool,
) -> TokenStream2 {
    let serialization_type = return_type.serialization_type();
    let res = match serialization_type {
        SerializationType::Raw | SerializationType::Ref => return_value.clone(),
        SerializationType::SerdeJson => {
            quote! {
                ts_bindgen_rt::from_jsvalue(&#return_value).unwrap()
            }
        }
        SerializationType::Fn => {
            // TODO - should be a js_sys::Function that we wrap
            quote! { #return_value.into_serde().unwrap() }
        }
    };
    if is_fallible {
        quote! { std::result::Result::Ok(#res) }
    } else {
        quote! { #res }
    }
}

pub fn render_raw_return_to_js(return_type: &TypeRef, return_value: &TokenStream2) -> TokenStream2 {
    let serialization_type = return_type.serialization_type();
    match serialization_type {
        SerializationType::Raw | SerializationType::Ref => quote! {
            #return_value.into_serde().unwrap()
        },
        SerializationType::SerdeJson => {
            quote! {
                ts_bindgen_rt::from_jsvalue(&#return_value).unwrap()
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
    typ: &TypeRefLike,
    error_mapper: TokenStream2,
) -> TokenStream2 {
    let serialization_type = typ.serialization_type();
    match serialization_type {
        SerializationType::Raw | SerializationType::Ref => quote! { #name },
        SerializationType::SerdeJson => {
            quote! { ts_bindgen_rt::to_jsvalue(&#name)#error_mapper }
        }
        SerializationType::Fn => {
            quote! { &#fn_name }
        }
    }
}

fn render_rust_to_jsvalue_conversion(
    name: &Identifier,
    fn_name: &Identifier,
    typ: &TypeRefLike,
    error_mapper: TokenStream2,
) -> TokenStream2 {
    let serialization_type = typ.serialization_type();
    match serialization_type {
        SerializationType::Raw | SerializationType::Ref => quote! { JsValue::from(#name) },
        SerializationType::SerdeJson => {
            quote! { ts_bindgen_rt::to_jsvalue(&#name)#error_mapper }
        }
        SerializationType::Fn => {
            quote! { &#fn_name }
        }
    }
}

pub struct WrapperFunc<'a> {
    pub func: &'a Func,
    pub js_name: &'a str,
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

macro_rules! impl_fn_proto_for_trait_member {
    ($slf:ident, $f:ident) => {
        match $slf {
            TraitMember::Constructor { ctor, .. } => ctor.$f(),
            TraitMember::Method { method, .. } => method.$f(),
            TraitMember::Getter { prop, .. } => prop.$f(),
            TraitMember::Setter { prop, .. } => prop.$f(),
        }
    };
}

impl<'b> HasFnPrototype for TraitMember<'b> {
    fn return_type(&self) -> TypeRef {
        impl_fn_proto_for_trait_member!(self, return_type)
    }

    fn params(&self) -> BoxedParamExtIter<'_> {
        impl_fn_proto_for_trait_member!(self, params)
    }

    fn args(&self) -> BoxedParamExtIter<'_> {
        impl_fn_proto_for_trait_member!(self, args)
    }

    fn is_member(&self) -> bool {
        impl_fn_proto_for_trait_member!(self, is_member)
    }
}
