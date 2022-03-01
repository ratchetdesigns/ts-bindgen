use crate::codegen::resolve_target_type::ResolveTargetType;
use crate::codegen::type_ref_like::{OwnedTypeRef, TypeRefLike};
use crate::ir::{Builtin, TargetEnrichedTypeInfo, TypeIdent, TypeRef, Union};
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SerializationType {
    Raw,
    SerdeJson,
    Array,
    Fn,
    JsValue,
}

pub trait SerializationTypeGetter {
    fn serialization_type(&self) -> SerializationType;
}

impl SerializationTypeGetter for TypeRef {
    fn serialization_type(&self) -> SerializationType {
        let resolved_type = self.resolve_target_type();
        resolved_type
            .map(|ti| ti.serialization_type())
            .unwrap_or(SerializationType::Raw) // TODO: we should only ever fall through for generic parameters (really need a different TypeIdent for them)
    }
}

impl SerializationTypeGetter for TargetEnrichedTypeInfo {
    fn serialization_type(&self) -> SerializationType {
        match self {
            TargetEnrichedTypeInfo::FuncGroup(_) => SerializationType::Fn,
            TargetEnrichedTypeInfo::Class(_) => SerializationType::Raw,
            TargetEnrichedTypeInfo::Tuple(_) => SerializationType::Array,
            TargetEnrichedTypeInfo::Union(Union { types, .. }) => {
                if types
                    .iter()
                    .all(|t| t.serialization_type() == SerializationType::Array)
                {
                    SerializationType::Array
                } else {
                    SerializationType::SerdeJson
                }
            }
            TargetEnrichedTypeInfo::Ref(t) => match &t.referent {
                TypeIdent::Builtin(Builtin::Fn) => SerializationType::Fn,
                TypeIdent::Builtin(Builtin::Array) => SerializationType::Array,
                TypeIdent::Builtin(
                    Builtin::PrimitiveAny | Builtin::PrimitiveObject | Builtin::Named(_),
                ) => SerializationType::JsValue,
                TypeIdent::Builtin(_) => SerializationType::Raw,
                _ => SerializationType::SerdeJson,
            },
            TargetEnrichedTypeInfo::Array { .. } => SerializationType::Array,
            _ => SerializationType::SerdeJson,
        }
    }
}

impl<'a> SerializationTypeGetter for OwnedTypeRef<'a> {
    fn serialization_type(&self) -> SerializationType {
        self.0.serialization_type()
    }
}

impl<'a> SerializationTypeGetter for TypeRefLike<'a> {
    fn serialization_type(&self) -> SerializationType {
        match self {
            TypeRefLike::TypeRef(t) => t.serialization_type(),
            TypeRefLike::OwnedTypeRef(t) => t.serialization_type(),
        }
    }
}

trait IsCopy {
    fn is_copy(&self) -> bool;
}

impl IsCopy for TargetEnrichedTypeInfo {
    fn is_copy(&self) -> bool {
        match self {
            TargetEnrichedTypeInfo::Ref(t) => {
                matches!(
                    &t.referent,
                    TypeIdent::Builtin(
                        Builtin::PrimitiveNumber
                            | Builtin::PrimitiveBoolean
                            | Builtin::LitNumber
                            | Builtin::LitBoolean
                    )
                )
            }
            _ => false,
        }
    }
}

pub fn clone_item_of_type(item: TokenStream2, typ: &TargetEnrichedTypeInfo) -> TokenStream2 {
    if typ.is_copy() {
        item
    } else {
        quote! {
            #item.clone()
        }
    }
}
