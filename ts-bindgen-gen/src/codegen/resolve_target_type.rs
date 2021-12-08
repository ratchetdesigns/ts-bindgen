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
        match &self.referent {
            TypeIdent::LocalName(_) => resolve_type(
                &self.context.types_by_ident_by_path,
                &self.context.path,
                &self.referent,
            ),
            TypeIdent::Name { file, name: _ } => {
                resolve_type(&self.context.types_by_ident_by_path, file, &self.referent)
            }
            TypeIdent::DefaultExport(path) => {
                resolve_type(&self.context.types_by_ident_by_path, path, &self.referent)
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
