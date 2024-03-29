use crate::fs::Fs;
use crate::identifier::Identifier;
use crate::ir::flattened::{
    Alias as FlattenedAlias, Class as FlattenedClass, Ctor as FlattenedCtor,
    CtorGroup as FlattenedCtorGroup, Enum as FlattenedEnum, EnumMember as FlattenedEnumMember,
    FlatType, FlattenedTypeInfo, Func as FlattenedFunc, FuncGroup as FlattenedFuncGroup,
    Indexer as FlattenedIndexer, Interface as FlattenedInterface,
    Intersection as FlattenedIntersection, Member as FlattenedMember,
    NamespaceImport as FlattenedNamespaceImport, Param as FlattenedParam, Tuple as FlattenedTuple,
    TypeParamConfig as FlattenedTypeParamConfig, TypeQuery as FlattenedTypeQuery,
    TypeRef as FlattenedTypeRef, Union as FlattenedUnion,
};
pub use crate::ir::flattened::{Builtin, EnumValue, TypeIdent};
use crate::mod_def::ToModPathIter;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;
use strum_macros::Display as StrumDisplay;

type SourceTypesByIdentByPath = HashMap<PathBuf, HashMap<TypeIdent, FlatType>>;
pub type TypesByIdentByPath = HashMap<PathBuf, HashMap<TypeIdent, TargetEnrichedType>>;
type WrappedTypesByIdentByPath = Rc<RefCell<TypesByIdentByPath>>;

macro_rules! from_field {
    ($value:ident, $ctx:ident, $field:ident, .) => {
        $ctx.wrap($value.$field).into()
    };
    ($value:ident, $ctx:ident, $field:ident, nc) => {
        $value.$field.into()
    };
    ($value:ident, $ctx:ident, $field:ident, box) => {
        Box::new($ctx.wrap(*$value.$field).into())
    };
    ($value:ident, $ctx:ident, $field:ident, []) => {
        $value
            .$field
            .into_iter()
            .map(|value| $ctx.wrap(value))
            .map(Into::into)
            .collect()
    };
    ($value:ident, $ctx:ident, $field:ident, Option) => {
        $value.$field.map(|value| $ctx.wrap(value)).map(Into::into)
    };
    ($value:ident, $ctx:ident, $field:ident, {}) => {
        $value
            .$field
            .into_iter()
            .map(|(k, v)| (k.into(), $ctx.wrap(v).into()))
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
/// ```ignore
/// use std::collections::HashMap;
///
/// struct Src {
///     a: u32,
///     b: Option<u32>,
///     c: Vec<u32>,
///     d: HashMap<u32, u32>
/// }
///
/// #[derive(Debug, PartialEq)]
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
/// );
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

#[derive(Debug, Clone, PartialEq, Eq, StrumDisplay)]
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
    FuncGroup(FuncGroup),
    Constructor(Ctor),
    Class(Class),
    Var {
        type_info: Box<TargetEnrichedTypeInfo>,
    },
    NamespaceImport(NamespaceImport),
    TypeQuery(TypeQuery),
}

macro_rules! case_conv {
    (match $src:ident :: $variant:ident, $x:ident) => {
        $src::$variant($x)
    };

    ($dest:ident :: $variant:ident, $x:ident, $ctx:ident) => {
        $dest::$variant($ctx.wrap($x).into())
    };

    (match $field:ident => $src:ident :: $variant:ident, $x:ident) => {
        $src::$variant { $field: $x }
    };

    ($field:ident => $dest:ident :: $variant:ident, $x:ident, $ctx:ident) => {
        $dest::$variant {
            $field: Box::new($ctx.wrap(*$x).into()),
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
            case_conv!(match FlattenedTypeInfo::FuncGroup, x) => {
                case_conv!(TargetEnrichedTypeInfo::FuncGroup, x, ctx)
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
            case_conv!(match FlattenedTypeInfo::TypeQuery, x) => {
                case_conv!(TargetEnrichedTypeInfo::TypeQuery, x, ctx)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Member {
    Constructor(CtorGroup),
    Method(FuncGroup),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CtorGroup {
    pub overloads: Vec<Ctor>,
    pub context: Context,
}

from_struct!(
    FlattenedCtorGroup => CtorGroup;
    overloads => [],
);

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
    pub constraint: Option<TypeRef>,
    pub default_type_arg: Option<TypeRef>,
    pub context: Context,
}

from_struct!(
    FlattenedTypeParamConfig => TypeParamConfig;
    constraint => Option,
    default_type_arg => Option,
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncGroup {
    pub overloads: Vec<Func>,
    pub widened_fn: Func,
    pub context: Context,
}

from_struct!(
    FlattenedFuncGroup => FuncGroup;
    overloads => [],
    widened_fn => .,
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
    pub types: Vec<TypeRef>,
    pub context: Context,
}

from_struct!(
    FlattenedUnion => Union;
    types => [],
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tuple {
    pub types: Vec<TypeRef>,
    pub context: Context,
}

from_struct!(
    FlattenedTuple => Tuple;
    types => [],
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Intersection {
    pub types: Vec<TypeRef>,
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
    pub value: Option<EnumValue>,
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
    pub constructor: Option<Ctor>,
    pub type_params: Vec<(String, TypeParamConfig)>,
    pub context: Context,
}

from_struct!(
    FlattenedInterface => Interface;
    indexer => Option,
    extends => [],
    fields => {},
    constructor => Option,
    type_params => {},
);

#[derive(Debug, Clone, Eq)]
pub struct TypeRef {
    pub referent: TypeIdent,
    pub type_params: Vec<TypeRef>,
    pub context: Context,
}

impl Hash for TypeRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.referent.hash(state);
        self.type_params.hash(state);
        // intentionally exclude context
    }
}

impl std::cmp::PartialEq for TypeRef {
    fn eq(&self, other: &Self) -> bool {
        // we implement eq ourselves to exclude context

        // destructure to catch structural changes
        let TypeRef {
            referent: other_referent,
            type_params: other_type_params,
            context: _,
        } = other;

        let TypeRef {
            referent,
            type_params,
            context: _,
        } = self;

        referent == other_referent && type_params == other_type_params
    }
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
        let context = src.context;
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
pub enum TypeQuery {
    LookupRef { type_ref: TypeRef, context: Context },
}

impl From<WithContext<FlattenedTypeQuery>> for TypeQuery {
    fn from(src: WithContext<FlattenedTypeQuery>) -> TypeQuery {
        let value = src.value;
        let context = src.context;
        match value {
            FlattenedTypeQuery::LookupRef(type_ref) => TypeQuery::LookupRef {
                type_ref: context.wrap(type_ref).into(),
                context,
            },
        }
    }
}

#[derive(Clone)]
pub struct Context {
    pub types_by_ident_by_path: WrappedTypesByIdentByPath,
    pub path: PathBuf,
    pub base_namespace: Vec<Identifier>,
    // TODO: no reason for this to be an Arc but we always have an Arc when we
    // want to use this and adding a lifetime to Context poisons all target
    // enriched types, which is a huge pain.
    pub fs: Arc<dyn Fs>,
}

impl Context {
    fn wrap<T>(&self, value: T) -> WithContext<T> {
        WithContext {
            value,
            context: self.clone(),
        }
    }

    /// Construct a dummy context.
    /// This is obviously a bad smell. There are some areas in code where
    /// we need to construct some of our TargetEnriched types, all of which
    /// require a Context. Sometimes we don't have a context to chain from.
    pub fn dummy() -> Context {
        Context {
            types_by_ident_by_path: Default::default(),
            path: PathBuf::new(),
            base_namespace: Default::default(),
            fs: Arc::new(crate::fs::MemFs::default()),
        }
    }
}

impl std::cmp::PartialEq for Context {
    fn eq(&self, other: &Self) -> bool {
        // we implement eq ourselves due to the Arc<dyn Fs>

        // destructure to catch structural changes
        let Context {
            types_by_ident_by_path: other_tbibp,
            path: other_path,
            base_namespace: other_bn,
            fs: other_fs,
        } = other;

        let Context {
            types_by_ident_by_path: tbibp,
            path,
            base_namespace: bn,
            fs,
        } = self;

        tbibp == other_tbibp
            && path == other_path
            && bn == other_bn
            && std::ptr::eq(
                Arc::as_ptr(fs) as *const dyn Fs as *const u8,
                Arc::as_ptr(other_fs) as *const dyn Fs as *const u8,
            )
    }
}

impl std::cmp::Eq for Context {}

impl std::fmt::Debug for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.debug_struct("Context").finish()
    }
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
    fs: Arc<dyn Fs>,
) -> WrappedTypesByIdentByPath {
    types_by_ident_by_path.into_iter().fold(
        Rc::new(RefCell::new(Default::default())),
        |enriched, (path, types_by_ident)| {
            let types_by_ident = types_by_ident
                .into_iter()
                .map(|(id, typ)| {
                    let base_namespace = typ.name.to_mod_path_iter(fs.as_ref()).collect();
                    (
                        id,
                        WithContext {
                            value: typ,
                            context: Context {
                                types_by_ident_by_path: Rc::clone(&enriched),
                                path: path.clone(),
                                fs: fs.clone(),
                                base_namespace,
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
