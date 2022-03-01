use crate::ir::{Builtin, TargetEnrichedTypeInfo, TypeIdent, TypeRef, Union};

pub trait IsUninhabited {
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
