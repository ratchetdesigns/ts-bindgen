use crate::codegen::resolve_target_type::ResolveTargetType;
use crate::codegen::type_ref_like::{OwnedTypeRef, TypeRefLike};
use crate::ir::{Builtin, TargetEnrichedTypeInfo, TypeIdent, TypeRef};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SerializationType {
    Raw,
    SerdeJson,
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
            TargetEnrichedTypeInfo::Ref(t) => match &t.referent {
                TypeIdent::Builtin(Builtin::Fn) => SerializationType::Fn,
                TypeIdent::Builtin(Builtin::Array) => SerializationType::SerdeJson,
                TypeIdent::Builtin(
                    Builtin::PrimitiveAny | Builtin::PrimitiveObject | Builtin::Named(_),
                ) => SerializationType::JsValue,
                TypeIdent::Builtin(_) => SerializationType::Raw,
                _ => SerializationType::SerdeJson,
            },
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
