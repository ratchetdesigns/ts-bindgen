use crate::codegen::type_ref_like::{OwnedTypeRef, TypeRefLike};
use crate::ir::{
    Alias, NamespaceImport, TargetEnrichedType, TargetEnrichedTypeInfo, TypeIdent, TypeRef,
    TypesByIdentByPath,
};
use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

pub trait ResolveTargetType {
    fn resolve_target_type(&self) -> Option<TargetEnrichedTypeInfo>;
}

impl ResolveTargetType for TargetEnrichedType {
    fn resolve_target_type(&self) -> Option<TargetEnrichedTypeInfo> {
        self.info.resolve_target_type()
    }
}

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

fn resolve_type(
    types_by_ident_by_path: &Rc<RefCell<TypesByIdentByPath>>,
    path: &Path,
    id: &TypeIdent,
) -> Option<TargetEnrichedTypeInfo> {
    let ti = RefCell::borrow(types_by_ident_by_path)
        .get(path)
        .and_then(|t_by_id| {
            t_by_id.get(id).or_else(|| match id {
                // TODO: Name and Local are interchangable. should be resolved as part of ir
                // transformation...
                TypeIdent::LocalName(n) => t_by_id.get(&TypeIdent::Name {
                    file: path.to_path_buf(),
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
        None => None,
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
        let file = match &self.referent {
            TypeIdent::LocalName(_) => &self.context.path,
            TypeIdent::Name { file, name: _ } => file,
            TypeIdent::DefaultExport(path) => path,
            TypeIdent::QualifiedName {
                file,
                name_parts: _,
            } => file,
            _ => return Some(TargetEnrichedTypeInfo::Ref(self.clone())),
        };

        resolve_type(&self.context.types_by_ident_by_path, file, &self.referent)
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
                None
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

impl<'a> ResolveTargetType for TypeRefLike<'a> {
    fn resolve_target_type(&self) -> Option<TargetEnrichedTypeInfo> {
        match self {
            TypeRefLike::TypeRef(t) => t.resolve_target_type(),
            TypeRefLike::OwnedTypeRef(t) => t.resolve_target_type(),
        }
    }
}

impl<'a> ResolveTargetType for OwnedTypeRef<'a> {
    fn resolve_target_type(&self) -> Option<TargetEnrichedTypeInfo> {
        self.0.resolve_target_type()
    }
}
