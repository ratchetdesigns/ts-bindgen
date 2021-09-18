use crate::flattened_ir::{
    Alias as FlattenedAlias, Class as FlattenedClass, Ctor as FlattenedCtor, Enum as FlattenedEnum,
    EnumMember as FlattenedEnumMember, FlatType, FlattenedTypeInfo, Func as FlattenedFunc,
    Indexer as FlattenedIndexer, Interface as FlattenedInterface,
    Intersection as FlattenedIntersection, Member as FlattenedMember, Param as FlattenedParam,
    TypeRef as FlattenedTypeRef, Union as FlattenedUnion,
};
pub use crate::flattened_ir::{Builtin, NamespaceImport, TypeIdent};
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

type SourceTypesByIdentByPath = HashMap<PathBuf, HashMap<TypeIdent, FlatType>>;
type TypesByIdentByPath = HashMap<PathBuf, HashMap<TypeIdent, TargetEnrichedType>>;
type WrappedTypesByIdentByPath = Rc<RefCell<TypesByIdentByPath>>;

macro_rules! from_field {
    ($value:ident, $types_by_ident_by_path:ident, $field:ident, .) => {
        WithContext {
            value: $value.$field,
            types_by_ident_by_path: Rc::clone(&$types_by_ident_by_path),
        }
        .into()
    };
    ($value:ident, $types_by_ident_by_path:ident, $field:ident, nc) => {
        $value.$field.into()
    };
    ($value:ident, $types_by_ident_by_path:ident, $field:ident, box) => {
        Box::new(
            WithContext {
                value: (*$value.$field),
                types_by_ident_by_path: Rc::clone(&$types_by_ident_by_path),
            }
            .into(),
        )
    };
    ($value:ident, $types_by_ident_by_path:ident, $field:ident, []) => {
        $value
            .$field
            .into_iter()
            .map(|value| WithContext {
                value,
                types_by_ident_by_path: Rc::clone(&$types_by_ident_by_path),
            })
            .map(Into::into)
            .collect()
    };
    ($value:ident, $types_by_ident_by_path:ident, $field:ident, Option) => {
        $value
            .$field
            .map(|value| WithContext {
                value,
                types_by_ident_by_path: Rc::clone(&$types_by_ident_by_path),
            })
            .map(Into::into)
    };
    ($value:ident, $types_by_ident_by_path:ident, $field:ident, {}) => {
        $value
            .$field
            .into_iter()
            .map(|(k, v)| {
                (
                    k.into(),
                    WithContext {
                        value: v,
                        types_by_ident_by_path: Rc::clone(&$types_by_ident_by_path),
                    }
                    .into(),
                )
            })
            .collect()
    };
}

/// from_struct generates a From implementation for a struct with same-named fields.
///
/// The format is:
///     SrcStruct => DestStruct;
/// Followed by
///     field => field_type
/// Where field_type is
///     . for a scalar with context (wrap in WithContext and call .into)
///     nc for a raw scalar (not wrapped in WithContext)
///     Option for an option (call map(Into::into))
///     [] for a single-itemed collection
///     {} for a map
///
/// * TODO: this doesn't run because it's an un-exported macro...
///
/// ```rust
/// struct Src {
///     a: u32,
///     b: Option<u32>,
///     c: Vec<u32>,
///     d: HashMap<u32, u32>
/// }
///
/// struct Dest {
///     a: u64,
///     b: Option<u64>,
///     c: Vec<u64>,
///     d: HashMap<u64, u64>
/// }
///
/// from_struct!(
///     Src => Dest;
///     a => .,
///     b => Option,
///     c => [],
///     d => {},
/// )
///
/// assert_eq!(
///     Dest::from(
///         Src {
///             a: 10,
///             b: Some(11),
///             c: vec![12, 13],
///             d: { let m = HashMap::new(); m.insert(14, 15); m },
///         }
///     ),
///     Dest {
///         a: 10,
///         b: None,
///         c: vec![12, 13],
///         d: { let m = HashMap::new(); m.insert(14, 15); m },
///     }
/// );
/// ```
macro_rules! from_struct {
    ($src:ident => $dest:ident; $($field:ident => $field_type:tt),+ $(,)?) => {
        impl From<WithContext<$src>> for $dest {
            fn from(src: WithContext<$src>) -> $dest {
                let value = src.value;
                let types_by_ident_by_path = Rc::clone(&src.types_by_ident_by_path);
                $dest {
                    types_by_ident_by_path: Rc::clone(&types_by_ident_by_path),
                    $($field: from_field!(value, types_by_ident_by_path, $field, $field_type)),*
                }
            }
        }
    };
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TargetEnrichedType {
    pub name: TypeIdent,
    pub is_exported: bool,
    pub info: TargetEnrichedTypeInfo,
    pub types_by_ident_by_path: WrappedTypesByIdentByPath,
}

from_struct!(
    FlatType => TargetEnrichedType;
    name => nc,
    is_exported => nc,
    info => .,
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TargetEnrichedTypeInfo {
    Interface(Interface),
    Enum(Enum),
    Alias(Alias),
    Ref(TypeRef),
    Array {
        item_type: Box<TargetEnrichedTypeInfo>,
    },
    Optional {
        item_type: Box<TargetEnrichedTypeInfo>,
    },
    Union(Union),
    Intersection(Intersection),
    Mapped {
        value_type: Box<TargetEnrichedTypeInfo>,
    },
    Func(Func),
    Constructor(Ctor),
    Class(Class),
    Var {
        type_info: Box<TargetEnrichedTypeInfo>,
    },
    NamespaceImport(NamespaceImport),
}

macro_rules! case_conv {
    (match $src:ident :: $variant:ident, $x:ident) => {
        $src::$variant($x)
    };

    ($dest:ident :: $variant:ident, $x:ident, $types_by_ident_by_path:ident) => {
        $dest::$variant(
            WithContext {
                value: $x,
                types_by_ident_by_path: $types_by_ident_by_path,
            }
            .into(),
        )
    };

    (match $field:ident => $src:ident :: $variant:ident, $x:ident) => {
        $src::$variant { $field: $x }
    };

    ($field:ident => $dest:ident :: $variant:ident, $x:ident, $types_by_ident_by_path:ident) => {
        $dest::$variant {
            $field: Box::new(
                WithContext {
                    value: *$x,
                    types_by_ident_by_path: $types_by_ident_by_path,
                }
                .into(),
            ),
        }
    };
}

impl From<WithContext<FlattenedTypeInfo>> for TargetEnrichedTypeInfo {
    fn from(src: WithContext<FlattenedTypeInfo>) -> TargetEnrichedTypeInfo {
        let value = src.value;
        let types_by_ident_by_path = src.types_by_ident_by_path;

        match value {
            case_conv!(match FlattenedTypeInfo::Interface, x) => {
                case_conv!(TargetEnrichedTypeInfo::Interface, x, types_by_ident_by_path)
            }
            case_conv!(match FlattenedTypeInfo::Enum, x) => {
                case_conv!(TargetEnrichedTypeInfo::Enum, x, types_by_ident_by_path)
            }
            case_conv!(match FlattenedTypeInfo::Alias, x) => {
                case_conv!(TargetEnrichedTypeInfo::Alias, x, types_by_ident_by_path)
            }
            case_conv!(match FlattenedTypeInfo::Ref, x) => {
                case_conv!(TargetEnrichedTypeInfo::Ref, x, types_by_ident_by_path)
            }
            case_conv!(match item_type => FlattenedTypeInfo::Array, x) => {
                case_conv!(item_type => TargetEnrichedTypeInfo::Array, x, types_by_ident_by_path)
            }
            case_conv!(match item_type => FlattenedTypeInfo::Optional, x) => {
                case_conv!(item_type => TargetEnrichedTypeInfo::Optional, x, types_by_ident_by_path)
            }
            case_conv!(match FlattenedTypeInfo::Union, x) => {
                case_conv!(TargetEnrichedTypeInfo::Union, x, types_by_ident_by_path)
            }
            case_conv!(match FlattenedTypeInfo::Intersection, x) => case_conv!(
                TargetEnrichedTypeInfo::Intersection,
                x,
                types_by_ident_by_path
            ),
            case_conv!(match value_type => FlattenedTypeInfo::Mapped, x) => {
                case_conv!(value_type => TargetEnrichedTypeInfo::Mapped, x, types_by_ident_by_path)
            }
            case_conv!(match FlattenedTypeInfo::Func, x) => {
                case_conv!(TargetEnrichedTypeInfo::Func, x, types_by_ident_by_path)
            }
            case_conv!(match FlattenedTypeInfo::Constructor, x) => case_conv!(
                TargetEnrichedTypeInfo::Constructor,
                x,
                types_by_ident_by_path
            ),
            case_conv!(match FlattenedTypeInfo::Class, x) => {
                case_conv!(TargetEnrichedTypeInfo::Class, x, types_by_ident_by_path)
            }
            case_conv!(match type_info => FlattenedTypeInfo::Var, x) => {
                case_conv!(type_info => TargetEnrichedTypeInfo::Var, x, types_by_ident_by_path)
            }
            case_conv!(match FlattenedTypeInfo::NamespaceImport, x) => case_conv!(
                TargetEnrichedTypeInfo::NamespaceImport,
                x,
                types_by_ident_by_path
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Member {
    Constructor(Ctor),
    Method(Func),
    Property(TypeRef),
}

impl From<WithContext<FlattenedMember>> for Member {
    fn from(src: WithContext<FlattenedMember>) -> Member {
        let value = src.value;
        let types_by_ident_by_path = src.types_by_ident_by_path;

        match value {
            case_conv!(match FlattenedMember::Constructor, x) => {
                case_conv!(Member::Constructor, x, types_by_ident_by_path)
            }
            case_conv!(match FlattenedMember::Method, x) => {
                case_conv!(Member::Method, x, types_by_ident_by_path)
            }
            case_conv!(match FlattenedMember::Property, x) => {
                case_conv!(Member::Property, x, types_by_ident_by_path)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    pub super_class: Option<TypeRef>,
    pub members: HashMap<String, Member>,
    pub types_by_ident_by_path: WrappedTypesByIdentByPath,
}

from_struct!(
    FlattenedClass => Class;
    super_class => Option,
    members => {},
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ctor {
    pub params: Vec<Param>,
    pub types_by_ident_by_path: WrappedTypesByIdentByPath,
}

from_struct!(
    FlattenedCtor => Ctor;
    params => [],
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: String,
    pub type_info: TypeRef,
    pub is_variadic: bool,
    pub types_by_ident_by_path: WrappedTypesByIdentByPath,
}

from_struct!(
    FlattenedParam => Param;
    name => nc,
    type_info => .,
    is_variadic => nc,
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Func {
    pub type_params: HashMap<String, TypeRef>,
    pub params: Vec<Param>,
    pub return_type: Box<TypeRef>,
    pub types_by_ident_by_path: WrappedTypesByIdentByPath,
}

from_struct!(
    FlattenedFunc => Func;
    type_params => {},
    params => [],
    return_type => box,
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Union {
    pub types: Vec<TargetEnrichedTypeInfo>,
    pub types_by_ident_by_path: WrappedTypesByIdentByPath,
}

from_struct!(
    FlattenedUnion => Union;
    types => [],
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Intersection {
    pub types: Vec<TargetEnrichedTypeInfo>,
    pub types_by_ident_by_path: WrappedTypesByIdentByPath,
}

from_struct!(
    FlattenedIntersection => Intersection;
    types => [],
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Alias {
    pub target: TypeIdent,
    pub types_by_ident_by_path: WrappedTypesByIdentByPath,
}

from_struct!(
    FlattenedAlias => Alias;
    target => nc,
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enum {
    pub members: Vec<EnumMember>,
    pub types_by_ident_by_path: WrappedTypesByIdentByPath,
}

from_struct!(
    FlattenedEnum => Enum;
    members => [],
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumMember {
    pub id: String,
    pub value: Option<String>,
    pub types_by_ident_by_path: WrappedTypesByIdentByPath,
}

from_struct!(
    FlattenedEnumMember => EnumMember;
    id => nc,
    value => nc,
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Indexer {
    pub readonly: bool,
    pub value_type: TypeRef,
    pub types_by_ident_by_path: WrappedTypesByIdentByPath,
}

from_struct!(
    FlattenedIndexer => Indexer;
    readonly => nc,
    value_type => .,
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Interface {
    pub indexer: Option<Indexer>,
    pub extends: Vec<TypeRef>,
    pub fields: HashMap<String, TypeRef>,
    pub types_by_ident_by_path: WrappedTypesByIdentByPath,
}

from_struct!(
    FlattenedInterface => Interface;
    indexer => Option,
    extends => [],
    fields => {},
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeRef {
    pub referent: TypeIdent,
    pub type_params: Vec<TypeRef>,
    pub types_by_ident_by_path: WrappedTypesByIdentByPath,
}

from_struct!(
    FlattenedTypeRef => TypeRef;
    referent => nc,
    type_params => [],
);

#[derive(Debug, Clone, PartialEq)]
struct WithContext<T> {
    value: T,
    types_by_ident_by_path: WrappedTypesByIdentByPath,
}

impl From<WithContext<NamespaceImport>> for NamespaceImport {
    fn from(src: WithContext<NamespaceImport>) -> NamespaceImport {
        src.value
    }
}

// TODO: really don't want to expose the RefCell to the world here but I can't figure out a way to
// hide it. I would love to return an Rc<impl AsRef<TypesByIdentByPath>> but I don't think that's
// doable.
pub fn target_enrich(
    types_by_ident_by_path: SourceTypesByIdentByPath,
) -> WrappedTypesByIdentByPath {
    let enriched = Rc::new(RefCell::new(Default::default()));

    types_by_ident_by_path
        .into_iter()
        .for_each(|(path, types_by_ident)| {
            let types_by_ident = types_by_ident
                .into_iter()
                .map(|(id, typ)| {
                    (
                        id,
                        WithContext {
                            value: typ,
                            types_by_ident_by_path: Rc::clone(&enriched),
                        }
                        .into(),
                    )
                })
                .collect();

            enriched.borrow_mut().insert(path, types_by_ident);
        });

    enriched
}
