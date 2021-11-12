use crate::flattened_ir::{
    Alias as FlattenedAlias, Class as FlattenedClass, Ctor as FlattenedCtor, Enum as FlattenedEnum,
    EnumMember as FlattenedEnumMember, FlatType, FlattenedTypeInfo, Func as FlattenedFunc,
    Indexer as FlattenedIndexer, Interface as FlattenedInterface,
    Intersection as FlattenedIntersection, Member as FlattenedMember,
    NamespaceImport as FlattenedNamespaceImport, Param as FlattenedParam, Tuple as FlattenedTuple,
    TypeParamConfig as FlattenedTypeParamConfig, TypeRef as FlattenedTypeRef,
    Union as FlattenedUnion,
};
pub use crate::flattened_ir::{Builtin, TypeIdent};
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

type SourceTypesByIdentByPath = HashMap<PathBuf, HashMap<TypeIdent, FlatType>>;
pub type TypesByIdentByPath = HashMap<PathBuf, HashMap<TypeIdent, TargetEnrichedType>>;
type WrappedTypesByIdentByPath = Rc<RefCell<TypesByIdentByPath>>;

macro_rules! from_field {
    ($value:ident, $ctx:ident, $field:ident, .) => {
        WithContext {
            value: $value.$field,
            context: $ctx.clone(),
        }
        .into()
    };
    ($value:ident, $ctx:ident, $field:ident, nc) => {
        $value.$field.into()
    };
    ($value:ident, $ctx:ident, $field:ident, box) => {
        Box::new(
            WithContext {
                value: (*$value.$field),
                context: $ctx.clone(),
            }
            .into(),
        )
    };
    ($value:ident, $ctx:ident, $field:ident, []) => {
        $value
            .$field
            .into_iter()
            .map(|value| WithContext {
                value,
                context: $ctx.clone(),
            })
            .map(Into::into)
            .collect()
    };
    ($value:ident, $ctx:ident, $field:ident, Option) => {
        $value
            .$field
            .map(|value| WithContext {
                value,
                context: $ctx.clone(),
            })
            .map(Into::into)
    };
    ($value:ident, $ctx:ident, $field:ident, {}) => {
        $value
            .$field
            .into_iter()
            .map(|(k, v)| {
                (
                    k.into(),
                    WithContext {
                        value: v,
                        context: $ctx.clone(),
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
                let ctx = &src.context;
                $dest {
                    context: ctx.clone(),
                    $($field: from_field!(value, ctx, $field, $field_type)),*
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
    pub context: Context,
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
    Tuple(Tuple),
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

    ($dest:ident :: $variant:ident, $x:ident, $ctx:ident) => {
        $dest::$variant(
            WithContext {
                value: $x,
                context: $ctx,
            }
            .into(),
        )
    };

    (match $field:ident => $src:ident :: $variant:ident, $x:ident) => {
        $src::$variant { $field: $x }
    };

    ($field:ident => $dest:ident :: $variant:ident, $x:ident, $ctx:ident) => {
        $dest::$variant {
            $field: Box::new(
                WithContext {
                    value: *$x,
                    context: $ctx,
                }
                .into(),
            ),
        }
    };
}

impl From<WithContext<FlattenedTypeInfo>> for TargetEnrichedTypeInfo {
    fn from(src: WithContext<FlattenedTypeInfo>) -> TargetEnrichedTypeInfo {
        let value = src.value;
        let ctx = src.context;

        match value {
            case_conv!(match FlattenedTypeInfo::Interface, x) => {
                case_conv!(TargetEnrichedTypeInfo::Interface, x, ctx)
            }
            case_conv!(match FlattenedTypeInfo::Enum, x) => {
                case_conv!(TargetEnrichedTypeInfo::Enum, x, ctx)
            }
            case_conv!(match FlattenedTypeInfo::Alias, x) => {
                case_conv!(TargetEnrichedTypeInfo::Alias, x, ctx)
            }
            case_conv!(match FlattenedTypeInfo::Ref, x) => {
                case_conv!(TargetEnrichedTypeInfo::Ref, x, ctx)
            }
            case_conv!(match item_type => FlattenedTypeInfo::Array, x) => {
                case_conv!(item_type => TargetEnrichedTypeInfo::Array, x, ctx)
            }
            case_conv!(match item_type => FlattenedTypeInfo::Optional, x) => {
                case_conv!(item_type => TargetEnrichedTypeInfo::Optional, x, ctx)
            }
            case_conv!(match FlattenedTypeInfo::Union, x) => {
                case_conv!(TargetEnrichedTypeInfo::Union, x, ctx)
            }
            case_conv!(match FlattenedTypeInfo::Tuple, x) => {
                case_conv!(TargetEnrichedTypeInfo::Tuple, x, ctx)
            }
            case_conv!(match FlattenedTypeInfo::Intersection, x) => {
                case_conv!(TargetEnrichedTypeInfo::Intersection, x, ctx)
            }
            case_conv!(match value_type => FlattenedTypeInfo::Mapped, x) => {
                case_conv!(value_type => TargetEnrichedTypeInfo::Mapped, x, ctx)
            }
            case_conv!(match FlattenedTypeInfo::Func, x) => {
                case_conv!(TargetEnrichedTypeInfo::Func, x, ctx)
            }
            case_conv!(match FlattenedTypeInfo::Constructor, x) => {
                case_conv!(TargetEnrichedTypeInfo::Constructor, x, ctx)
            }
            case_conv!(match FlattenedTypeInfo::Class, x) => {
                case_conv!(TargetEnrichedTypeInfo::Class, x, ctx)
            }
            case_conv!(match type_info => FlattenedTypeInfo::Var, x) => {
                case_conv!(type_info => TargetEnrichedTypeInfo::Var, x, ctx)
            }
            case_conv!(match FlattenedTypeInfo::NamespaceImport, x) => {
                case_conv!(TargetEnrichedTypeInfo::NamespaceImport, x, ctx)
            }
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
        let ctx = src.context;

        match value {
            case_conv!(match FlattenedMember::Constructor, x) => {
                case_conv!(Member::Constructor, x, ctx)
            }
            case_conv!(match FlattenedMember::Method, x) => {
                case_conv!(Member::Method, x, ctx)
            }
            case_conv!(match FlattenedMember::Property, x) => {
                case_conv!(Member::Property, x, ctx)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    pub super_class: Option<TypeRef>,
    pub members: HashMap<String, Member>,
    pub type_params: Vec<(String, TypeParamConfig)>,
    pub implements: Vec<TypeRef>,
    pub context: Context,
}

from_struct!(
    FlattenedClass => Class;
    super_class => Option,
    members => {},
    type_params => {},
    implements => [],
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ctor {
    pub params: Vec<Param>,
    pub context: Context,
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
    pub context: Context,
}

from_struct!(
    FlattenedParam => Param;
    name => nc,
    type_info => .,
    is_variadic => nc,
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeParamConfig {
    pub constraint: Option<TargetEnrichedTypeInfo>,
    pub default_type_arg: Option<TargetEnrichedTypeInfo>,
    pub context: Context,
}

from_struct!(
    FlattenedTypeParamConfig => TypeParamConfig;
    constraint => Option,
    default_type_arg => Option,
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Func {
    pub type_params: Vec<(String, TypeParamConfig)>,
    pub params: Vec<Param>,
    pub return_type: Box<TypeRef>,
    pub class_name: Option<TypeIdent>,
    pub context: Context,
}

impl Func {
    pub fn is_variadic(&self) -> bool {
        self.params.iter().any(|p| p.is_variadic)
    }
}

from_struct!(
    FlattenedFunc => Func;
    type_params => {},
    params => [],
    return_type => box,
    class_name => nc,
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Union {
    pub types: Vec<TargetEnrichedTypeInfo>,
    pub context: Context,
}

from_struct!(
    FlattenedUnion => Union;
    types => [],
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tuple {
    pub types: Vec<TargetEnrichedTypeInfo>,
    pub context: Context,
}

from_struct!(
    FlattenedTuple => Tuple;
    types => [],
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Intersection {
    pub types: Vec<TargetEnrichedTypeInfo>,
    pub context: Context,
}

from_struct!(
    FlattenedIntersection => Intersection;
    types => [],
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Alias {
    pub target: TypeRef,
    pub type_params: Vec<(String, TypeParamConfig)>,
    pub context: Context,
}

from_struct!(
    FlattenedAlias => Alias;
    target => .,
    type_params => {},
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enum {
    pub members: Vec<EnumMember>,
    pub context: Context,
}

from_struct!(
    FlattenedEnum => Enum;
    members => [],
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumMember {
    pub id: String,
    pub value: Option<String>,
    pub context: Context,
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
    pub context: Context,
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
    pub type_params: Vec<(String, TypeParamConfig)>,
    pub context: Context,
}

from_struct!(
    FlattenedInterface => Interface;
    indexer => Option,
    extends => [],
    fields => {},
    type_params => {},
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeRef {
    pub referent: TypeIdent,
    pub type_params: Vec<TypeRef>,
    pub context: Context,
}

from_struct!(
    FlattenedTypeRef => TypeRef;
    referent => nc,
    type_params => [],
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NamespaceImport {
    Default {
        src: PathBuf,
        context: Context,
    },
    All {
        src: PathBuf,
        context: Context,
    },
    Named {
        src: PathBuf,
        name: String,
        context: Context,
    },
}

impl From<WithContext<FlattenedNamespaceImport>> for NamespaceImport {
    fn from(src: WithContext<FlattenedNamespaceImport>) -> NamespaceImport {
        let value = src.value;
        let context = src.context.clone();
        match value {
            FlattenedNamespaceImport::Default { src } => NamespaceImport::Default { src, context },
            FlattenedNamespaceImport::All { src } => NamespaceImport::All { src, context },
            FlattenedNamespaceImport::Named { src, name } => {
                NamespaceImport::Named { src, name, context }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Context {
    pub types_by_ident_by_path: WrappedTypesByIdentByPath,
    pub path: PathBuf,
}

#[derive(Debug, Clone, PartialEq)]
struct WithContext<T> {
    value: T,
    context: Context,
}

// TODO: really don't want to expose the RefCell to the world here but I can't figure out a way to
// hide it. I would love to return an Rc<impl AsRef<TypesByIdentByPath>> but I don't think that's
// doable.
pub fn target_enrich(
    types_by_ident_by_path: SourceTypesByIdentByPath,
) -> WrappedTypesByIdentByPath {
    types_by_ident_by_path.into_iter().fold(
        Rc::new(RefCell::new(Default::default())),
        |enriched, (path, types_by_ident)| {
            let types_by_ident = types_by_ident
                .into_iter()
                .map(|(id, typ)| {
                    (
                        id,
                        WithContext {
                            value: typ,
                            context: Context {
                                types_by_ident_by_path: Rc::clone(&enriched),
                                path: path.clone(),
                            },
                        }
                        .into(),
                    )
                })
                .collect();

            enriched.borrow_mut().insert(path, types_by_ident);
            enriched
        },
    )
}
