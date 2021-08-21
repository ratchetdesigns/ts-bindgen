use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeIdent {
    Name(String),
    DefaultExport(),
    QualifiedName(Vec<String>),
}

#[derive(Debug, Clone)]
pub struct TypeName {
    pub file: PathBuf,
    pub name: TypeIdent,
}

impl TypeName {
    pub fn default_export_for(file: PathBuf) -> TypeName {
        TypeName {
            file,
            name: TypeIdent::DefaultExport(),
        }
    }

    pub fn for_name<T: Into<PathBuf>>(file: T, name: &str) -> TypeName {
        TypeName {
            file: file.into(),
            name: TypeIdent::Name(name.to_string()),
        }
    }

    pub fn for_qualified_name(file: PathBuf, names: Vec<String>) -> TypeName {
        TypeName {
            file,
            name: TypeIdent::QualifiedName(names),
        }
    }

    pub fn to_name(&self) -> &str {
        match &self.name {
            TypeIdent::Name(n) => n,
            TypeIdent::QualifiedName(n) => n.last().expect("bad qualified name"),
            TypeIdent::DefaultExport() => "default",
        }
    }
}

#[derive(Debug, Clone)]
pub struct EnumMember {
    pub id: String,
    pub value: Option<String>, // TODO: really a string | number
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub type_info: TypeInfo,
    pub is_variadic: bool,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub type_params: HashMap<String, TypeInfo>,
    pub params: Vec<Param>,
    pub return_type: Box<TypeInfo>,
}

#[derive(Debug, Clone)]
pub enum Member {
    Constructor(),
    Method(),
    Property(),
}

impl Member {
    fn resolve_names(
        &self,
        _types_by_name_by_file: &HashMap<PathBuf, HashMap<TypeIdent, Type>>,
        _type_params: &HashMap<String, TypeInfo>,
    ) -> Self {
        self.clone() // TODO
    }
}

#[derive(Debug, Clone)]
pub struct Indexer {
    pub readonly: bool,
    pub type_info: Box<TypeInfo>,
}

impl Indexer {
    fn resolve_names(
        &self,
        types_by_name_by_file: &HashMap<PathBuf, HashMap<TypeIdent, Type>>,
        type_params: &HashMap<String, TypeInfo>,
    ) -> Self {
        Indexer {
            readonly: self.readonly,
            type_info: Box::new(
                self.type_info
                    .resolve_names(&types_by_name_by_file, &type_params),
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeInfo {
    Interface {
        indexer: Option<Indexer>,
        fields: HashMap<String, TypeInfo>,
    },
    Enum {
        members: Vec<EnumMember>,
    },
    Ref {
        referent: TypeName,
        type_params: Vec<TypeInfo>,
    },
    Alias {
        target: TypeName,
    },
    PrimitiveAny {},
    PrimitiveNumber {},
    PrimitiveObject {},
    PrimitiveBoolean {},
    PrimitiveBigInt {},
    PrimitiveString {},
    PrimitiveSymbol {},
    PrimitiveVoid {},
    PrimitiveUndefined {},
    PrimitiveNull {},
    BuiltinPromise {
        value_type: Box<TypeInfo>,
    },
    BuiltinDate {},
    Array {
        item_type: Box<TypeInfo>,
    },
    Optional {
        item_type: Box<TypeInfo>,
    },
    Union {
        types: Vec<TypeInfo>,
    },
    Intersection {
        types: Vec<TypeInfo>,
    },
    Mapped {
        value_type: Box<TypeInfo>,
    },
    LitNumber {
        n: f64,
    },
    LitString {
        s: String,
    },
    LitBoolean {
        b: bool,
    },
    Func(Func),
    Constructor {
        params: Vec<Param>,
        return_type: Box<TypeInfo>,
    },
    Class {
        members: HashMap<String, Member>,
    },
    Var {
        type_info: Box<TypeInfo>,
    },
    GenericType {
        name: String,
        constraint: Box<TypeInfo>,
    },
    NamespaceImport(NamespaceImport),
}

#[derive(Debug, Clone)]
pub enum NamespaceImport {
    Default { src: PathBuf },
    All { src: PathBuf },
    Named { src: PathBuf, name: String },
}

impl TypeInfo {
    fn resolve_builtin(
        &self,
        referent: &TypeName,
        alias_type_params: &Vec<TypeInfo>,
        types_by_name_by_file: &HashMap<PathBuf, HashMap<TypeIdent, Type>>,
        type_params: &HashMap<String, TypeInfo>,
    ) -> Option<TypeInfo> {
        if referent.name == TypeIdent::Name("Array".to_string()) {
            assert_eq!(
                alias_type_params.len(),
                1,
                "expected 1 type param for Array"
            );
            return Some(TypeInfo::Array {
                item_type: Box::new(
                    alias_type_params
                        .first()
                        .as_ref()
                        .unwrap()
                        .resolve_names(&types_by_name_by_file, &type_params),
                ),
            });
        }

        if referent.name == TypeIdent::Name("Record".to_string()) {
            assert_eq!(
                alias_type_params.len(),
                2,
                "expected 2 type params for Record"
            );
            // TODO: do we care about key type?
            return Some(TypeInfo::Mapped {
                value_type: Box::new(
                    alias_type_params
                        .get(1)
                        .as_ref()
                        .unwrap()
                        .resolve_names(&types_by_name_by_file, &type_params),
                ),
            });
        }

        if referent.name == TypeIdent::Name("Date".to_string()) {
            return Some(TypeInfo::BuiltinDate {});
        }

        if referent.name == TypeIdent::Name("Function".to_string()) {
            return Some(TypeInfo::Func(Func {
                type_params: Default::default(),
                return_type: Box::new(TypeInfo::PrimitiveAny {}),
                params: vec![Param {
                    name: "args".to_string(),
                    type_info: TypeInfo::PrimitiveAny {},
                    is_variadic: true,
                }],
            }));
        }

        if referent.name == TypeIdent::Name("Object".to_string()) {
            return Some(TypeInfo::Mapped {
                value_type: Box::new(TypeInfo::PrimitiveAny {}),
            });
        }

        if referent.name == TypeIdent::Name("Promise".to_string()) {
            return Some(TypeInfo::BuiltinPromise {
                value_type: Box::new(
                    alias_type_params
                        .first()
                        .as_ref()
                        .map(|p| p.resolve_names(&types_by_name_by_file, &type_params))
                        .unwrap_or(TypeInfo::PrimitiveAny {}),
                ),
            });
        }

        None
    }

    fn resolve_names(
        &self,
        types_by_name_by_file: &HashMap<PathBuf, HashMap<TypeIdent, Type>>,
        type_params: &HashMap<String, TypeInfo>,
    ) -> Self {
        match self {
            Self::Interface { indexer, fields } => Self::Interface {
                indexer: indexer
                    .as_ref()
                    .map(|i| i.resolve_names(&types_by_name_by_file, &type_params)),
                fields: fields
                    .iter()
                    .map(|(n, t)| {
                        (
                            n.to_string(),
                            t.resolve_names(&types_by_name_by_file, &type_params),
                        )
                    })
                    .collect(),
            },
            Self::Ref {
                referent,
                type_params: alias_type_params,
            } => {
                if let TypeIdent::Name(ref name) = &referent.name {
                    if let Some(constraint) = type_params.get(name) {
                        return Self::GenericType {
                            name: name.to_string(),
                            constraint: Box::new(constraint.clone()),
                        };
                    }
                }

                types_by_name_by_file
                    .get(&referent.file)
                    .and_then(|types_by_name| {
                        let n = if let TypeIdent::QualifiedName(qn) = &referent.name {
                            TypeIdent::Name(
                                qn.first()
                                    .expect("must have a name in a qualified name")
                                    .to_string(),
                            )
                        } else {
                            referent.name.clone()
                        };

                        types_by_name.get(&n).map(|_| TypeInfo::Alias {
                            target: referent.clone(),
                        })
                    })
                    .or_else(|| {
                        self.resolve_builtin(
                            &referent,
                            &alias_type_params,
                            &types_by_name_by_file,
                            &type_params,
                        )
                    })
                    .or_else(|| {
                        println!(
                            "can't resolve, {:?}, {:?}",
                            self,
                            types_by_name_by_file.get(&referent.file)
                        );
                        None
                    })
                    .expect("can't resolve alias")
            }
            Self::Alias { target } => Self::Alias {
                target: target.clone(),
            },
            Self::Array { item_type } => Self::Array {
                item_type: Box::new(item_type.resolve_names(&types_by_name_by_file, &type_params)),
            },
            Self::Optional { item_type } => Self::Optional {
                item_type: Box::new(item_type.resolve_names(&types_by_name_by_file, &type_params)),
            },
            Self::Union { types } => Self::Union {
                types: types
                    .iter()
                    .map(|t| t.resolve_names(&types_by_name_by_file, &type_params))
                    .collect(),
            },
            Self::Intersection { types } => Self::Intersection {
                types: types
                    .iter()
                    .map(|t| t.resolve_names(&types_by_name_by_file, &type_params))
                    .collect(),
            },
            Self::Mapped { value_type } => Self::Mapped {
                value_type: Box::new(
                    value_type.resolve_names(&types_by_name_by_file, &type_params),
                ),
            },
            Self::Func(Func {
                params,
                type_params: fn_type_params,
                return_type,
            }) => {
                let tps = {
                    let mut tps = type_params.clone();
                    tps.extend(fn_type_params.clone().into_iter());
                    tps
                };
                Self::Func(Func {
                    type_params: fn_type_params.clone(),
                    params: params
                        .iter()
                        .map(|p| Param {
                            name: p.name.to_string(),
                            is_variadic: p.is_variadic.clone(),
                            type_info: p.type_info.resolve_names(&types_by_name_by_file, &tps),
                        })
                        .collect(),
                    return_type: Box::new(
                        return_type.resolve_names(&types_by_name_by_file, &type_params),
                    ),
                })
            }
            Self::Constructor {
                params,
                return_type,
            } => Self::Constructor {
                params: params
                    .iter()
                    .map(|p| Param {
                        name: p.name.to_string(),
                        is_variadic: p.is_variadic.clone(),
                        type_info: p
                            .type_info
                            .resolve_names(&types_by_name_by_file, &type_params),
                    })
                    .collect(),
                return_type: Box::new(
                    return_type.resolve_names(&types_by_name_by_file, &type_params),
                ),
            },
            Self::Class { members } => Self::Class {
                members: members
                    .iter()
                    .map(|(n, m)| {
                        (
                            n.to_string(),
                            m.resolve_names(&types_by_name_by_file, &type_params),
                        )
                    })
                    .collect(),
            },
            Self::Var { type_info } => Self::Var {
                type_info: Box::new(type_info.resolve_names(&types_by_name_by_file, &type_params)),
            },
            Self::GenericType { name, constraint } => Self::GenericType {
                name: name.to_string(),
                constraint: Box::new(
                    constraint.resolve_names(&types_by_name_by_file, &type_params),
                ),
            },
            Self::Enum { .. } => self.clone(),
            Self::PrimitiveAny {} => self.clone(),
            Self::PrimitiveNumber {} => self.clone(),
            Self::PrimitiveObject {} => self.clone(),
            Self::PrimitiveBoolean {} => self.clone(),
            Self::PrimitiveBigInt {} => self.clone(),
            Self::PrimitiveString {} => self.clone(),
            Self::PrimitiveSymbol {} => self.clone(),
            Self::PrimitiveVoid {} => self.clone(),
            Self::PrimitiveUndefined {} => self.clone(),
            Self::PrimitiveNull {} => self.clone(),
            Self::LitNumber { .. } => self.clone(),
            Self::LitString { .. } => self.clone(),
            Self::LitBoolean { .. } => self.clone(),
            Self::BuiltinDate {} => self.clone(),
            Self::BuiltinPromise { .. } => self.clone(),
            Self::NamespaceImport { .. } => self.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    pub name: TypeName,
    pub is_exported: bool,
    pub info: TypeInfo,
}

impl Type {
    pub fn resolve_names(
        &self,
        types_by_name_by_file: &HashMap<PathBuf, HashMap<TypeIdent, Type>>,
    ) -> Self {
        Self {
            name: self.name.clone(),
            is_exported: self.is_exported,
            info: self
                .info
                .resolve_names(&types_by_name_by_file, &Default::default()),
        }
    }
}
