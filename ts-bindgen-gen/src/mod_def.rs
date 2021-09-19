use crate::identifier::{to_ns_name, to_snake_case_ident, Identifier};
use crate::target_enriched_ir::{TargetEnrichedType, TypeIdent, TypeRef};
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::{Component, Path, PathBuf};
use std::rc::Rc;

#[derive(Debug, Clone)]
struct MutModDef {
    name: Identifier,
    types: Vec<TargetEnrichedType>,
    children: Vec<Rc<RefCell<MutModDef>>>,
}

impl MutModDef {
    fn into_mod_def(self) -> ModDef {
        ModDef {
            name: self.name,
            types: self.types,
            children: self
                .children
                .into_iter()
                .map(move |c| {
                    Rc::try_unwrap(c)
                        .expect("Rc still borrowed")
                        .into_inner()
                        .into_mod_def()
                })
                .collect(),
        }
    }

    fn add_child_mod(
        &mut self,
        mod_name: Identifier,
        types: Vec<TargetEnrichedType>,
    ) -> Rc<RefCell<MutModDef>> {
        if let Some(child) = self.children.iter().find(|c| c.borrow().name == mod_name) {
            let child = child.clone();
            child.borrow_mut().types.extend(types);
            child
        } else {
            let child = Rc::new(RefCell::new(MutModDef {
                name: mod_name,
                types,
                children: Default::default(),
            }));
            self.children.push(child.clone());
            child
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModDef {
    pub name: Identifier,
    pub types: Vec<TargetEnrichedType>,
    pub children: Vec<ModDef>,
}

pub trait ToModPathIter {
    fn to_mod_path_iter(&self) -> Box<dyn Iterator<Item = Identifier>>;
}

impl ToModPathIter for Path {
    fn to_mod_path_iter(&self) -> Box<dyn Iterator<Item = Identifier>> {
        Box::new(
            self.canonicalize()
                .expect("canonicalize failed")
                .components()
                .filter_map(|c| match c {
                    Component::Normal(s) => Some(s.to_string_lossy()),
                    _ => None,
                })
                .rev()
                .take_while(|p| p != "node_modules")
                .map(|p| p.as_ref().to_string())
                .collect::<Vec<String>>()
                .into_iter()
                .rev()
                .map(|n| to_ns_name(&n)),
        )
    }
}

impl ToModPathIter for TypeIdent {
    fn to_mod_path_iter(&self) -> Box<dyn Iterator<Item = Identifier>> {
        match self {
            TypeIdent::QualifiedName { file, name_parts } => Box::new(
                file.to_mod_path_iter().chain(
                    (&name_parts[..name_parts.len() - 1])
                        .to_vec()
                        .into_iter()
                        .map(|n| to_snake_case_ident(&n)),
                ),
            ),
            TypeIdent::Name { file, .. } => file.to_mod_path_iter(),
            _ => Box::new((vec![]).into_iter()),
        }
    }
}

impl ToModPathIter for TypeRef {
    fn to_mod_path_iter(&self) -> Box<dyn Iterator<Item = Identifier>> {
        self.referent.to_mod_path_iter()
    }
}

// TODO: maybe don't make "index" namespaces and put their types in the parent
impl From<&HashMap<PathBuf, HashMap<TypeIdent, TargetEnrichedType>>> for ModDef {
    fn from(
        types_by_name_by_file: &HashMap<PathBuf, HashMap<TypeIdent, TargetEnrichedType>>,
    ) -> Self {
        let root = Rc::new(RefCell::new(MutModDef {
            name: to_ns_name("root"),
            types: Default::default(),
            children: Default::default(),
        }));

        types_by_name_by_file
            .iter()
            .for_each(|(path, types_by_name)| {
                // given a path like /.../node_modules/a/b/c, we fold over
                // [a, b, c].
                // given a path like /a/b/c (without a node_modules), we fold
                // over [a, b, c].
                let mod_path = path.to_mod_path_iter().collect::<Vec<Identifier>>();
                let last_idx = mod_path.len() - 1;

                mod_path
                    .iter()
                    .enumerate()
                    .fold(root.clone(), move |parent, (i, mod_name)| {
                        let mut parent = parent.borrow_mut();
                        let types = if i == last_idx {
                            types_by_name
                                .values()
                                .cloned()
                                .collect::<Vec<TargetEnrichedType>>()
                        } else {
                            Default::default()
                        };
                        parent.add_child_mod(mod_name.clone(), types)
                    });

                types_by_name
                    .iter()
                    .filter_map(|(name, typ)| {
                        if let TypeIdent::QualifiedName { .. } = name {
                            Some((name.to_mod_path_iter().collect::<Vec<Identifier>>(), typ))
                        } else {
                            None
                        }
                    })
                    .for_each(|(names, typ)| {
                        let last_idx = mod_path.len() + names.len() - 1;
                        mod_path.iter().chain(names.iter()).enumerate().fold(
                            root.clone(),
                            move |parent, (i, mod_name)| {
                                let mut parent = parent.borrow_mut();
                                let types = if i == last_idx {
                                    vec![typ.clone()]
                                } else {
                                    Default::default()
                                };
                                parent.add_child_mod(mod_name.clone(), types)
                            },
                        );
                    });
            });

        Rc::try_unwrap(root).unwrap().into_inner().into_mod_def()
    }
}

#[cfg(test)]
mod mod_def_tests {
    use super::*;
    use crate::identifier::to_ident;
    use crate::target_enriched_ir::{Builtin, Context, TargetEnrichedTypeInfo};
    use std::cell::RefCell;
    use std::fs::{DirBuilder, File};
    use std::rc::Rc;

    #[test]
    fn mod_def_from_types_by_name_by_file() -> std::io::Result<()> {
        let mut tbnbf: HashMap<PathBuf, HashMap<TypeIdent, TargetEnrichedType>> = HashMap::new();
        let b_c = PathBuf::from("/tmp/a/node_modules/b/c");
        DirBuilder::new()
            .recursive(true)
            .create(b_c.parent().unwrap())?;
        let context = Context {
            types_by_ident_by_path: Rc::new(RefCell::new(Default::default())),
            path: b_c.clone(),
        };
        File::create(&b_c)?;

        tbnbf.insert(b_c.clone(), {
            let mut tbn = HashMap::new();
            tbn.insert(
                TypeIdent::Name {
                    file: b_c.clone(),
                    name: "my_mod".to_string(),
                },
                TargetEnrichedType {
                    name: TypeIdent::Name {
                        file: b_c.clone(),
                        name: "my_mod".to_string(),
                    },
                    is_exported: true,
                    info: TargetEnrichedTypeInfo::Ref(TypeRef {
                        referent: TypeIdent::Builtin(Builtin::PrimitiveAny),
                        type_params: Default::default(),
                        context: context.clone(),
                    }),
                    context: context.clone(),
                },
            );
            tbn
        });

        let mods: ModDef = (&tbnbf).into();
        assert_eq!(
            mods,
            ModDef {
                name: to_ident("root"),
                types: Default::default(),
                children: vec![ModDef {
                    name: to_ident("b"),
                    types: Default::default(),
                    children: vec![ModDef {
                        name: to_ident("c"),
                        types: vec![TargetEnrichedType {
                            name: TypeIdent::Name {
                                file: b_c,
                                name: "my_mod".to_string(),
                            },
                            is_exported: true,
                            info: TargetEnrichedTypeInfo::Ref(TypeRef {
                                referent: TypeIdent::Builtin(Builtin::PrimitiveAny),
                                type_params: Default::default(),
                                context: context.clone(),
                            }),
                            context: context.clone(),
                        }],
                        children: Default::default(),
                    }]
                }]
            }
        );

        Ok(())
    }
}
