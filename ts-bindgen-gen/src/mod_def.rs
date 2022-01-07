use crate::fs::Fs;
use crate::identifier::{make_identifier, to_ns_name, to_snake_case_ident, Identifier};
use crate::ir::{TargetEnrichedType, TypeIdent, TypeRef};
use std::cell::RefCell;
use std::collections::HashMap;
use std::iter;
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
    fn to_mod_path_iter<FS: Fs + ?Sized>(&self, fs: &FS) -> Box<dyn Iterator<Item = Identifier>>;
}

impl ToModPathIter for Path {
    fn to_mod_path_iter<FS: Fs + ?Sized>(&self, fs: &FS) -> Box<dyn Iterator<Item = Identifier>> {
        let cwd = fs.cwd().unwrap();
        let cwd_components: Vec<_> = cwd.components().collect();
        let mut rev_components = self
            .components() // components does some normalization for us
            .enumerate()
            .skip_while(|(i, c)| {
                cwd_components
                    .get(*i)
                    .map(|cwd_part| cwd_part == c)
                    .unwrap_or(false)
            })
            .map(|(_, c)| c)
            .filter_map(|c| match c {
                Component::Normal(s) => Some(s.to_string_lossy()),
                _ => None,
            })
            .collect::<Vec<_>>()
            .iter()
            .rev()
            .take_while(|p| *p != "node_modules")
            .map(|n| to_ns_name(&n))
            .collect::<Vec<_>>();

        let mut components = {
            rev_components.reverse();
            rev_components
        };

        // get rid of a final index component (e.g. a/b/index means a/b in js-land)
        if components
            .last()
            .map(|l| *l == make_identifier!(index))
            .unwrap_or(false)
        {
            components.pop();
        }

        Box::new(components.into_iter())
    }
}

impl ToModPathIter for TypeIdent {
    fn to_mod_path_iter<FS: Fs + ?Sized>(&self, fs: &FS) -> Box<dyn Iterator<Item = Identifier>> {
        match self {
            TypeIdent::QualifiedName { file, name_parts } => Box::new(
                file.to_mod_path_iter(fs).chain(
                    (&name_parts[..name_parts.len() - 1])
                        .to_vec()
                        .into_iter()
                        .map(|n| to_snake_case_ident(&n)),
                ),
            ),
            TypeIdent::Name { file, .. } => file.to_mod_path_iter(fs),
            _ => Box::new(iter::empty()),
        }
    }
}

impl ToModPathIter for TypeRef {
    fn to_mod_path_iter<FS: Fs + ?Sized>(&self, fs: &FS) -> Box<dyn Iterator<Item = Identifier>> {
        self.referent.to_mod_path_iter(fs)
    }
}

impl ModDef {
    pub fn new<FS: Fs + ?Sized>(
        fs: &FS,
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
                let mod_path = path.to_mod_path_iter(fs).collect::<Vec<Identifier>>();
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
                            Some((name.to_mod_path_iter(fs).collect::<Vec<Identifier>>(), typ))
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
    use crate::error::Error;
    use crate::fs::{MemFs, StdFs};
    use crate::identifier::{make_identifier, to_ident};
    use crate::ir::{to_final_ir, Builtin, Context, TargetEnrichedTypeInfo};
    use crate::parse::{ArcFs, TsTypes};
    use std::cell::RefCell;
    use std::fs::{DirBuilder, File};
    use std::rc::Rc;
    use std::sync::Arc;

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

        let mods = ModDef::new(&StdFs, &tbnbf);
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

    #[test]
    fn mod_def_with_index_removed() -> Result<(), Error> {
        let mut fs: MemFs = Default::default();
        fs.set_cwd(Path::new("/"));
        fs.add_dir_at(Path::new("/test"));
        fs.add_file_at(
            Path::new("/test/index.d.ts"),
            r#"export type Test = number | string | null;"#.to_string(),
        );

        let arc_fs = Arc::new(fs) as ArcFs;
        let tbnbf = TsTypes::parse(arc_fs.clone(), "/test")?;
        let ir = to_final_ir(tbnbf);
        let mods = ModDef::new(&*arc_fs, &*ir.borrow());

        assert_eq!(mods.children.len(), 1);

        let test_mod = mods.children.first().unwrap();
        assert_eq!(test_mod.name, make_identifier!(test));

        assert!(test_mod.children.is_empty());
        assert!(!test_mod.types.is_empty());

        Ok(())
    }

    #[test]
    fn mod_def_with_cwd_prefix_removed() -> Result<(), Error> {
        let mut fs: MemFs = Default::default();
        fs.set_cwd(Path::new("/abc/def"));
        fs.add_dir_at(Path::new("/abc/def/test"));
        fs.add_file_at(
            Path::new("/abc/def/test/index.d.ts"),
            r#"export type Test = number | string | null;"#.to_string(),
        );

        let arc_fs = Arc::new(fs) as ArcFs;
        let tbnbf = TsTypes::parse(arc_fs.clone(), "/abc/def/test")?;
        let ir = to_final_ir(tbnbf);
        let mods = ModDef::new(&*arc_fs, &*ir.borrow());

        assert_eq!(mods.children.len(), 1);

        let test_mod = mods.children.first().unwrap();
        assert_eq!(test_mod.name, make_identifier!(test));

        assert!(test_mod.children.is_empty());
        assert!(!test_mod.types.is_empty());

        Ok(())
    }
}
