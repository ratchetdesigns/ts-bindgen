use crate::codegen::type_ref_like::{OwnedTypeRef, TypeRefLike};
use crate::ir::{Context, TypeRef};
use std::borrow::Cow;

pub trait Contextual {
    fn with_context(&self, ctx: &Context) -> Self;
}

impl<'a> Contextual for OwnedTypeRef<'a> {
    fn with_context(&self, ctx: &Context) -> OwnedTypeRef<'a> {
        OwnedTypeRef(Cow::Owned(self.0.with_context(ctx)))
    }
}

impl<'a> Contextual for TypeRefLike<'a> {
    fn with_context(&self, ctx: &Context) -> TypeRefLike<'a> {
        match self {
            TypeRefLike::TypeRef(t) => TypeRefLike::TypeRef(Cow::Owned(t.with_context(ctx))),
            TypeRefLike::OwnedTypeRef(t) => {
                TypeRefLike::OwnedTypeRef(Cow::Owned(t.with_context(ctx)))
            }
        }
    }
}

impl Contextual for TypeRef {
    fn with_context(&self, ctx: &Context) -> Self {
        TypeRef {
            referent: self.referent.clone(),
            type_params: self
                .type_params
                .iter()
                .map(|t| t.with_context(ctx))
                .collect(),
            context: ctx.clone(),
        }
    }
}
