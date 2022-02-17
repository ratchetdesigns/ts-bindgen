use crate::ir::{Builtin, TypeIdent, TypeRef};
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens, TokenStreamExt};
use std::borrow::Cow;

#[derive(Debug, Clone)]
pub enum TypeRefLike<'a> {
    TypeRef(Cow<'a, TypeRef>),
    OwnedTypeRef(Cow<'a, OwnedTypeRef<'a>>),
}

impl<'a> TypeRefLike<'a> {
    pub fn similarly_wrap<'b>(&self, tr: &'b TypeRef) -> TypeRefLike<'b> {
        match self {
            Self::TypeRef(_) => TypeRefLike::TypeRef(Cow::Borrowed(tr)),
            Self::OwnedTypeRef(_) => {
                TypeRefLike::OwnedTypeRef(Cow::Owned(OwnedTypeRef(Cow::Borrowed(tr))))
            }
        }
    }
}

impl<'a> AsRef<TypeRef> for TypeRefLike<'a> {
    fn as_ref(&self) -> &TypeRef {
        match self {
            Self::TypeRef(tr) => tr.as_ref(),
            Self::OwnedTypeRef(tr) => tr.as_ref().as_ref(),
        }
    }
}

impl<'a> From<&'a TypeRef> for TypeRefLike<'a> {
    fn from(src: &'a TypeRef) -> TypeRefLike<'a> {
        TypeRefLike::TypeRef(Cow::Borrowed(src))
    }
}

impl<'a> From<TypeRef> for TypeRefLike<'a> {
    fn from(src: TypeRef) -> TypeRefLike<'a> {
        TypeRefLike::TypeRef(Cow::Owned(src))
    }
}

impl<'a> From<OwnedTypeRef<'a>> for TypeRefLike<'a> {
    fn from(src: OwnedTypeRef<'a>) -> TypeRefLike<'a> {
        TypeRefLike::OwnedTypeRef(Cow::Owned(src))
    }
}

impl<'a> From<&'a OwnedTypeRef<'a>> for TypeRefLike<'a> {
    fn from(src: &'a OwnedTypeRef<'a>) -> TypeRefLike<'a> {
        TypeRefLike::OwnedTypeRef(Cow::Borrowed(src))
    }
}

impl<'a> ToTokens for TypeRefLike<'a> {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        match self {
            TypeRefLike::TypeRef(t) => t.to_tokens(toks),
            TypeRefLike::OwnedTypeRef(t) => t.to_tokens(toks),
        };
    }
}

/// Represents an owned TypeRef. Needed to render the type as owned (sized).
#[derive(Debug, Clone)]
pub struct OwnedTypeRef<'a>(pub Cow<'a, TypeRef>);

impl<'a> AsRef<TypeRef> for OwnedTypeRef<'a> {
    fn as_ref(&self) -> &TypeRef {
        self.0.as_ref()
    }
}

impl<'a> ToTokens for OwnedTypeRef<'a> {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let typ = &*self.0;
        let our_toks = if TypeIdent::Builtin(Builtin::Fn) == self.0.referent {
            quote! {
                std::rc::Rc<#typ>
            }
        } else {
            quote! {
                #typ
            }
        };

        toks.append_all(our_toks);
    }
}
