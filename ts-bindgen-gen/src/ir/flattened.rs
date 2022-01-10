use crate::ir::base::{
    Alias as AliasIR, BaseClass as BaseClassIR, BuiltinDate, BuiltinPromise, Class as ClassIR,
    Ctor as CtorIR, Enum as EnumIR, EnumMember as EnumMemberIR, Func as FuncIR,
    Indexer as IndexerIR, Interface as InterfaceIR, Intersection as IntersectionIR, LitBoolean,
    LitNumber, LitString, Member as MemberIR, Param as ParamIR, PrimitiveAny, PrimitiveBigInt,
    PrimitiveBoolean, PrimitiveNull, PrimitiveNumber, PrimitiveObject, PrimitiveString,
    PrimitiveSymbol, PrimitiveUndefined, PrimitiveVoid, Tuple as TupleIR, Type as TypeIR,
    TypeIdent as TypeIdentIR, TypeInfo as TypeInfoIR, TypeName as TypeNameIR,
    TypeParamConfig as TypeParamConfigIR, TypeQuery as TypeQueryIR, TypeRef as TypeRefIR,
    Union as UnionIR,
};
pub use crate::ir::base::{EnumValue, NamespaceImport};
use enum_to_enum::{FromEnum, WithEffects};
use std::collections::HashMap;
use std::iter::{Extend, FromIterator};
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};

static NEXT_ID: AtomicUsize = AtomicUsize::new(1);

trait ApplyNames {
    fn apply_names(self, names_by_id: &HashMap<usize, String>) -> Self;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FlatType {
    pub name: TypeIdent,
    pub is_exported: bool,
    pub info: FlattenedTypeInfo,
}

impl ApplyNames for FlatType {
    fn apply_names(self, names_by_id: &HashMap<usize, String>) -> Self {
        FlatType {
            name: self.name,
            is_exported: self.is_exported,
            info: self.info.apply_names(names_by_id),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Effect {
    CreateType {
        name: String,
        typ: NameableTypeInfo,
        generated_name_id: usize,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectContainer<Value> {
    value: Value,
    effects: Vec<Effect>,
}

type EffectIterator = Box<dyn Iterator<Item = Effect>>;

impl<Value> EffectContainer<Value> {
    fn adapt_effects<T: Fn(Effect) -> Effect>(self, mapper: T) -> EffectContainer<Value> {
        EffectContainer {
            value: self.value,
            effects: self.effects.into_iter().map(mapper).collect(),
        }
    }
}

impl<Value> WithEffects for EffectContainer<Value> {
    type Value = Value;
    type Effect = Effect;

    fn new(value: Self::Value, effects: Vec<Self::Effect>) -> Self {
        Self { value, effects }
    }

    fn into_value_and_effects(self) -> (Self::Value, EffectIterator) {
        (self.value, Box::new(self.effects.into_iter()))
    }
}

impl<Value> From<Option<EffectContainer<Value>>> for EffectContainer<Option<Value>> {
    fn from(src: Option<EffectContainer<Value>>) -> EffectContainer<Option<Value>> {
        src.map(|s| EffectContainer::new(Some(s.value), s.effects))
            .unwrap_or_else(|| EffectContainer::new(None, Default::default()))
    }
}

impl<Value, Coll> FromIterator<EffectContainer<Value>> for EffectContainer<Coll>
where
    Coll: FromIterator<Value> + Default + Extend<Value>,
{
    fn from_iter<I: IntoIterator<Item = EffectContainer<Value>>>(iter: I) -> Self {
        let (vals, effects): (Coll, Vec<EffectIterator>) = iter
            .into_iter()
            .map(|ec| ec.into_value_and_effects())
            .unzip();
        EffectContainer::compose_from(
            vals,
            effects
                .into_iter()
                .flatten()
                .collect::<Vec<Effect>>()
                .into_boxed_slice(),
        )
    }
}

impl<K: std::hash::Hash + Eq, V> FromIterator<(K, EffectContainer<V>)>
    for EffectContainer<HashMap<K, V>>
{
    fn from_iter<I: IntoIterator<Item = (K, EffectContainer<V>)>>(iter: I) -> Self {
        let (vals, effects): (HashMap<K, V>, Vec<EffectIterator>) = iter
            .into_iter()
            .map(|(k, ec)| {
                let (v, es) = ec.into_value_and_effects();
                ((k, v), es)
            })
            .unzip();
        EffectContainer::compose_from(
            vals,
            effects
                .into_iter()
                .flatten()
                .collect::<Vec<Effect>>()
                .into_boxed_slice(),
        )
    }
}

impl<K, V> FromIterator<(K, EffectContainer<V>)> for EffectContainer<Vec<(K, V)>> {
    fn from_iter<I: IntoIterator<Item = (K, EffectContainer<V>)>>(iter: I) -> Self {
        let (vals, effects): (Vec<(K, V)>, Vec<EffectIterator>) = iter
            .into_iter()
            .map(|(k, ec)| {
                let (v, es) = ec.into_value_and_effects();
                ((k, v), es)
            })
            .unzip();
        EffectContainer::compose_from(
            vals,
            effects
                .into_iter()
                .flatten()
                .collect::<Vec<Effect>>()
                .into_boxed_slice(),
        )
    }
}

/// A subset of [`FlattenedTypeInfo`] variants that may need to be lifted and named
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NameableTypeInfo {
    Union(Union),
    Intersection(Intersection),
    Tuple(Tuple),
    Interface(Interface),
}

impl From<NameableTypeInfo> for FlattenedTypeInfo {
    fn from(src: NameableTypeInfo) -> FlattenedTypeInfo {
        match src {
            NameableTypeInfo::Union(u) => FlattenedTypeInfo::Union(u),
            NameableTypeInfo::Intersection(i) => FlattenedTypeInfo::Intersection(i),
            NameableTypeInfo::Tuple(t) => FlattenedTypeInfo::Tuple(t),
            NameableTypeInfo::Interface(i) => FlattenedTypeInfo::Interface(i),
        }
    }
}

/// FlattenedTypeInfo's represent a view of [`crate::ir::TypeInfo`]
/// with all anonymous inner types converted into references to
/// a top-level, named type.
#[derive(Debug, Clone, PartialEq, Eq, FromEnum)]
#[from_enum(TypeInfoIR, effect_container = EffectContainer)]
pub enum FlattenedTypeInfo {
    Interface(Interface),
    Enum(Enum),
    Alias(Alias),
    #[from_case(
        PrimitiveAny,
        PrimitiveNumber,
        PrimitiveObject,
        PrimitiveBoolean,
        PrimitiveBigInt,
        PrimitiveString,
        PrimitiveSymbol,
        PrimitiveVoid,
        PrimitiveUndefined,
        PrimitiveNull,
        BuiltinPromise,
        BuiltinDate,
        LitNumber,
        LitString,
        LitBoolean,
        Ref
    )]
    Ref(TypeRef),
    Array {
        item_type: Box<FlattenedTypeInfo>,
    },
    Tuple(Tuple),
    Optional {
        item_type: Box<FlattenedTypeInfo>,
    },
    Union(Union),
    Intersection(Intersection),
    Mapped {
        value_type: Box<FlattenedTypeInfo>,
    },
    Func(Func),
    Constructor(Ctor),
    Class(Class),
    Var {
        type_info: Box<FlattenedTypeInfo>,
    },
    NamespaceImport(NamespaceImport),
    TypeQuery(TypeQuery),
}

impl ApplyNames for FlattenedTypeInfo {
    fn apply_names(self, names_by_id: &HashMap<usize, String>) -> Self {
        match self {
            FlattenedTypeInfo::Interface(i) => {
                FlattenedTypeInfo::Interface(i.apply_names(names_by_id))
            }
            FlattenedTypeInfo::Enum(e) => FlattenedTypeInfo::Enum(e.apply_names(names_by_id)),
            FlattenedTypeInfo::Alias(a) => FlattenedTypeInfo::Alias(a.apply_names(names_by_id)),
            FlattenedTypeInfo::Ref(r) => FlattenedTypeInfo::Ref(r.apply_names(names_by_id)),
            FlattenedTypeInfo::Array { item_type } => FlattenedTypeInfo::Array {
                item_type: Box::new(item_type.apply_names(names_by_id)),
            },
            FlattenedTypeInfo::Optional { item_type } => FlattenedTypeInfo::Optional {
                item_type: Box::new(item_type.apply_names(names_by_id)),
            },
            FlattenedTypeInfo::Union(u) => FlattenedTypeInfo::Union(u.apply_names(names_by_id)),
            FlattenedTypeInfo::Intersection(i) => {
                FlattenedTypeInfo::Intersection(i.apply_names(names_by_id))
            }
            FlattenedTypeInfo::Tuple(t) => FlattenedTypeInfo::Tuple(t.apply_names(names_by_id)),
            FlattenedTypeInfo::Mapped { value_type } => FlattenedTypeInfo::Mapped {
                value_type: Box::new(value_type.apply_names(names_by_id)),
            },
            FlattenedTypeInfo::Func(f) => FlattenedTypeInfo::Func(f.apply_names(names_by_id)),
            FlattenedTypeInfo::Constructor(c) => {
                FlattenedTypeInfo::Constructor(c.apply_names(names_by_id))
            }
            FlattenedTypeInfo::Class(c) => FlattenedTypeInfo::Class(c.apply_names(names_by_id)),
            FlattenedTypeInfo::Var { type_info } => FlattenedTypeInfo::Var {
                type_info: Box::new(type_info.apply_names(names_by_id)),
            },
            FlattenedTypeInfo::NamespaceImport(n) => FlattenedTypeInfo::NamespaceImport(n),
            FlattenedTypeInfo::TypeQuery(q) => FlattenedTypeInfo::TypeQuery(q.apply_names(names_by_id)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Interface {
    pub indexer: Option<Indexer>,
    pub extends: Vec<TypeRef>,
    pub fields: HashMap<String, TypeRef>,
    pub constructor: Option<Ctor>,
    pub type_params: Vec<(String, TypeParamConfig)>,
}

impl ApplyNames for Interface {
    fn apply_names(self, names_by_id: &HashMap<usize, String>) -> Self {
        Interface {
            indexer: self.indexer.map(|i| i.apply_names(names_by_id)),
            extends: self
                .extends
                .into_iter()
                .map(|e| e.apply_names(names_by_id))
                .collect(),
            fields: self
                .fields
                .into_iter()
                .map(|(k, v)| (k, v.apply_names(names_by_id)))
                .collect(),
            constructor: self.constructor.map(|i| i.apply_names(names_by_id)),
            type_params: self
                .type_params
                .into_iter()
                .map(|(n, t)| (n, t.apply_names(names_by_id)))
                .collect(),
        }
    }
}

macro_rules! combine_effects {
    (@cont $effects:ident ; $item:expr) => {
        EffectContainer::compose_from(
            $item,
            $effects.into_boxed_slice(),
        )
    };
    (@cont $effects:ident $param:ident => $f:tt $(,)? $($params:ident => $fs:tt),* ; $item:expr) => {
        {
            let $param: EffectContainer<_> = $param;
            let ($param, these_effects) = $param.into_value_and_effects();
            $effects.extend(these_effects.map($f));
            combine_effects!(@cont $effects $($params => $fs),* ; $item)
        }
    };
    ($($params:ident => $f:tt),+ ; $item:expr) => {
        {
            let mut effects = Vec::new();
            combine_effects!(@cont effects $($params => $f),+ ; $item)
        }
    };
}

mod effect_mappers {
    use super::Effect;
    use heck::TitleCase;

    pub fn prepend_name<T: AsRef<str>>(prefix: T) -> impl Fn(Effect) -> Effect {
        move |e: Effect| match e {
            Effect::CreateType {
                name,
                typ,
                generated_name_id,
            } => Effect::CreateType {
                name: prefix.as_ref().to_title_case() + &name,
                typ,
                generated_name_id,
            },
        }
    }

    pub fn identity() -> impl Fn(Effect) -> Effect {
        |ec: Effect| ec
    }
}

impl From<InterfaceIR> for EffectContainer<Interface> {
    fn from(src: InterfaceIR) -> EffectContainer<Interface> {
        let indexer = src.indexer.map(EffectContainer::from).into();
        let extends = src.extends.into_iter().map(EffectContainer::from).collect();
        let fields = src
            .fields
            .into_iter()
            .map(|(n, t)| {
                let effects =
                    EffectContainer::from(t).adapt_effects(effect_mappers::prepend_name(&n));
                (n, effects)
            })
            .collect();
        let constructor = src.constructor.map(EffectContainer::from).into();
        let type_params = src
            .type_params
            .into_iter()
            .map(|(n, t)| {
                let effects =
                    EffectContainer::from(t).adapt_effects(effect_mappers::prepend_name(&n));
                (n, effects)
            })
            .collect();

        combine_effects!(
            indexer => (effect_mappers::prepend_name("Indexer")),
            extends => (effect_mappers::identity()),
            fields => (effect_mappers::identity()),
            constructor => (effect_mappers::prepend_name("Ctor")),
            type_params => (effect_mappers::identity());
            Interface {
                indexer,
                extends,
                fields,
                constructor,
                type_params,
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Indexer {
    pub readonly: bool,
    pub value_type: TypeRef,
}

impl ApplyNames for Indexer {
    fn apply_names(self, names_by_id: &HashMap<usize, String>) -> Self {
        Indexer {
            readonly: self.readonly,
            value_type: self.value_type.apply_names(names_by_id),
        }
    }
}

impl From<IndexerIR> for EffectContainer<Indexer> {
    fn from(src: IndexerIR) -> EffectContainer<Indexer> {
        let value_type = (*src.type_info).into();

        combine_effects!(
            value_type => (effect_mappers::identity());
            Indexer {
                readonly: src.readonly,
                value_type
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeIdent {
    Builtin(Builtin),
    GeneratedName {
        id: usize,
    },
    LocalName(String),
    Name {
        file: PathBuf,
        name: String,
    },
    DefaultExport(PathBuf),
    QualifiedName {
        file: PathBuf,
        name_parts: Vec<String>,
    },
    /// ExactNames are passed through directly as identifiers, without mangling
    ExactName(String),
}

impl ApplyNames for TypeIdent {
    fn apply_names(self, names_by_id: &HashMap<usize, String>) -> Self {
        match self {
            TypeIdent::GeneratedName { id } => {
                TypeIdent::LocalName(names_by_id.get(&id).unwrap().clone())
            }
            _ => self,
        }
    }
}

impl From<TypeNameIR> for TypeIdent {
    fn from(src: TypeNameIR) -> TypeIdent {
        let file = src.file;
        match src.name {
            TypeIdentIR::Name(name) => TypeIdent::Name { file, name },
            TypeIdentIR::DefaultExport() => TypeIdent::DefaultExport(file),
            TypeIdentIR::QualifiedName(name_parts) => TypeIdent::QualifiedName { file, name_parts },
        }
    }
}

impl From<TypeNameIR> for EffectContainer<TypeIdent> {
    fn from(src: TypeNameIR) -> EffectContainer<TypeIdent> {
        EffectContainer::new(src.into(), Default::default())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeRef {
    pub referent: TypeIdent,
    pub type_params: Vec<TypeRef>,
}

impl ApplyNames for TypeRef {
    fn apply_names(self, names_by_id: &HashMap<usize, String>) -> Self {
        TypeRef {
            referent: self.referent.apply_names(names_by_id),
            type_params: self
                .type_params
                .into_iter()
                .map(|p| p.apply_names(names_by_id))
                .collect(),
        }
    }
}

/// Convert a [`TypeInfoIR`] to a [`TypeRef`], creating a set of [`Effect`]s, directing the
/// creation of named, top-level types for any anonymous types we encounter.
///
/// This conversion is only valid for a non-top-level [TypeInfoIR] because we assume that
/// anything we find is un-named.
impl From<TypeInfoIR> for EffectContainer<TypeRef> {
    fn from(src: TypeInfoIR) -> EffectContainer<TypeRef> {
        match src {
            TypeInfoIR::Interface(i) => i.into(),
            TypeInfoIR::Enum(_) => panic!("Enum only expected as top-level type"),
            TypeInfoIR::Ref(t) => t.into(),
            TypeInfoIR::Alias(_) => panic!("Alias only expected as top-level type"),
            TypeInfoIR::PrimitiveAny(p) => p.into(),
            TypeInfoIR::PrimitiveNumber(p) => p.into(),
            TypeInfoIR::PrimitiveObject(p) => p.into(),
            TypeInfoIR::PrimitiveBoolean(p) => p.into(),
            TypeInfoIR::PrimitiveBigInt(p) => p.into(),
            TypeInfoIR::PrimitiveString(p) => p.into(),
            TypeInfoIR::PrimitiveSymbol(p) => p.into(),
            TypeInfoIR::PrimitiveVoid(p) => p.into(),
            TypeInfoIR::PrimitiveUndefined(p) => p.into(),
            TypeInfoIR::PrimitiveNull(p) => p.into(),
            TypeInfoIR::BuiltinPromise(b) => b.into(),
            TypeInfoIR::BuiltinDate(b) => b.into(),
            TypeInfoIR::Array { item_type } => {
                let item_type: EffectContainer<TypeRef> = (*item_type).into();
                combine_effects!(
                    item_type => (effect_mappers::identity());
                    TypeRef {
                        referent: TypeIdent::Builtin(Builtin::Array),
                        type_params: vec![item_type],
                    }
                )
            }
            TypeInfoIR::Tuple(t) => t.into(),
            TypeInfoIR::Optional { item_type } => {
                let item_type: EffectContainer<TypeRef> = (*item_type).into();
                combine_effects!(
                    item_type => (effect_mappers::identity());
                    TypeRef {
                        referent: TypeIdent::Builtin(Builtin::Optional),
                        type_params: vec![item_type],
                    }
                )
            }
            TypeInfoIR::Union(u) => u.into(),
            TypeInfoIR::Intersection(i) => i.into(),
            TypeInfoIR::Mapped { value_type } => {
                let value_type: EffectContainer<TypeRef> = (*value_type).into();
                combine_effects!(
                    value_type => (effect_mappers::identity());
                    TypeRef {
                        referent: TypeIdent::Builtin(Builtin::Map),
                        type_params: vec![
                            TypeRef {
                                referent: TypeIdent::Builtin(Builtin::PrimitiveString),
                                type_params: Default::default(),
                            },
                            value_type,
                        ],
                    }
                )
            }
            TypeInfoIR::LitNumber(l) => l.into(),
            TypeInfoIR::LitString(l) => l.into(),
            TypeInfoIR::LitBoolean(l) => l.into(),
            TypeInfoIR::Func(f) => f.into(),
            TypeInfoIR::Constructor(_) => panic!("Constructor only expected as top-level type"),
            TypeInfoIR::Class(_) => panic!("Class only expected as top-level type"),
            TypeInfoIR::Var { type_info: _ } => panic!("Var only expected as a top-level type"),
            TypeInfoIR::NamespaceImport(_) => {
                panic!("Namespace import only expected as a top-level construct")
            }
            TypeInfoIR::TypeQuery(tr) => tr.into(),
        }
    }
}

impl From<TypeRefIR> for EffectContainer<TypeRef> {
    fn from(src: TypeRefIR) -> EffectContainer<TypeRef> {
        let referent = src.referent.into();
        let type_params = src
            .type_params
            .into_iter()
            .map(EffectContainer::from)
            .collect();
        combine_effects!(
            referent => (effect_mappers::identity()),
            type_params => (effect_mappers::identity());
            TypeRef {
                referent,
                type_params,
            }
        )
    }
}

impl From<BaseClassIR> for EffectContainer<TypeRef> {
    fn from(src: BaseClassIR) -> EffectContainer<TypeRef> {
        match src {
            BaseClassIR::Resolved(ti) => ti.into(),
            BaseClassIR::Unresolved(_) => panic!("expected only resolved base classes"),
        }
    }
}

impl From<FuncIR> for EffectContainer<TypeRef> {
    fn from(src: FuncIR) -> EffectContainer<TypeRef> {
        let f: EffectContainer<Func> = src.into();

        combine_effects!(
            f => (effect_mappers::identity());
            TypeRef {
                referent: TypeIdent::Builtin(Builtin::Fn),
                type_params: f.params.into_iter().map(|p| {
                    if p.is_variadic {
                        TypeRef {
                            referent: TypeIdent::Builtin(Builtin::Variadic),
                            type_params: vec![p.type_info],
                        }
                    } else {
                        p.type_info
                    }
                }).chain(
                    std::iter::once(*f.return_type)
                ).collect(),
            }
        )
    }
}

impl From<TypeQueryIR> for EffectContainer<TypeRef> {
    fn from(src: TypeQueryIR) -> EffectContainer<TypeRef> {
        match src {
            TypeQueryIR::LookupRef(tr) => tr.into(),
        }
    }
}

macro_rules! impl_effectful_conversion_from_nameable_type_to_type_ref {
    () => {};
    ($src:path => $nameable:ident) => {
        impl From<$src> for EffectContainer<TypeRef> {
            fn from(src: $src) -> EffectContainer<TypeRef> {
                let id = NEXT_ID.fetch_add(1, Ordering::SeqCst);
                let ec: EffectContainer<_> = src.into();
                let (val, effects) = ec.into_value_and_effects();
                let effect = Effect::CreateType {
                    name: "".to_string(), // our name is filled in as we bubble up
                    typ: NameableTypeInfo::$nameable(val),
                    generated_name_id: id,
                };
                let effects = effects.chain(std::iter::once(effect)).collect();
                let type_ref = TypeRef {
                    referent: TypeIdent::GeneratedName {
                        id,
                    },
                    type_params: Default::default(),
                };

                EffectContainer::new(
                    type_ref,
                    effects,
                )
            }
        }
    };
    ($src:path => $nameable:ident, $($rest_src:path => $rest_nameable:ident),* $(,)?) => {
        impl_effectful_conversion_from_nameable_type_to_type_ref!(
            $src => $nameable
        );
        impl_effectful_conversion_from_nameable_type_to_type_ref!(
            $($rest_src => $rest_nameable),*
        );
    };
}

impl_effectful_conversion_from_nameable_type_to_type_ref!(
    UnionIR => Union,
    IntersectionIR => Intersection,
    TupleIR => Tuple,
    InterfaceIR => Interface,
);

macro_rules! type_ref_from_prims {
    ($(,)*) => {};
    ($prim:ident => $builtin:ident, $($rest:tt)*) => {
        impl From<$prim> for EffectContainer<TypeRef> {
            fn from(_: $prim) -> EffectContainer<TypeRef> {
                EffectContainer::new(
                    TypeRef {
                        referent: TypeIdent::Builtin(Builtin::$builtin),
                        type_params: Default::default(),
                    },
                    Default::default(),
                )
            }
        }

        type_ref_from_prims!($($rest)*);
    };
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Builtin {
    PrimitiveAny,
    PrimitiveNumber,
    PrimitiveObject,
    PrimitiveBoolean,
    PrimitiveBigInt,
    PrimitiveString,
    PrimitiveSymbol,
    PrimitiveVoid,
    PrimitiveUndefined,
    PrimitiveNull,
    Date,
    LitNumber,
    LitBoolean,
    LitString,
    Promise,
    Array,
    Fn,
    Map,
    Optional,
    Variadic,
}

type_ref_from_prims!(
    PrimitiveAny => PrimitiveAny,
    PrimitiveNumber => PrimitiveNumber,
    PrimitiveObject => PrimitiveObject,
    PrimitiveBoolean => PrimitiveBoolean,
    PrimitiveBigInt => PrimitiveBigInt,
    PrimitiveString => PrimitiveString,
    PrimitiveSymbol => PrimitiveSymbol,
    PrimitiveVoid => PrimitiveVoid,
    PrimitiveUndefined => PrimitiveUndefined,
    PrimitiveNull => PrimitiveNull,
    BuiltinDate => Date,
    LitNumber => LitNumber,
    LitBoolean => LitBoolean,
    LitString => LitString,
    BuiltinPromise => Promise,
);

impl From<Box<TypeInfoIR>> for EffectContainer<Box<FlattenedTypeInfo>> {
    fn from(src: Box<TypeInfoIR>) -> EffectContainer<Box<FlattenedTypeInfo>> {
        let ti = EffectContainer::from(*src);
        combine_effects!(
            ti => (effect_mappers::identity());
            Box::new(ti)
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Union {
    pub types: Vec<FlattenedTypeInfo>,
}

impl ApplyNames for Union {
    fn apply_names(self, names_by_id: &HashMap<usize, String>) -> Self {
        Union {
            types: self
                .types
                .into_iter()
                .map(|t| t.apply_names(names_by_id))
                .collect(),
        }
    }
}

impl From<UnionIR> for EffectContainer<Union> {
    fn from(src: UnionIR) -> EffectContainer<Union> {
        let types = src.types.into_iter().map(EffectContainer::from).collect();
        combine_effects!(
            types => (effect_mappers::identity());
            Union {
                types,
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Intersection {
    pub types: Vec<FlattenedTypeInfo>,
}

impl ApplyNames for Intersection {
    fn apply_names(self, names_by_id: &HashMap<usize, String>) -> Self {
        Intersection {
            types: self
                .types
                .into_iter()
                .map(|t| t.apply_names(names_by_id))
                .collect(),
        }
    }
}

impl From<IntersectionIR> for EffectContainer<Intersection> {
    fn from(src: IntersectionIR) -> EffectContainer<Intersection> {
        let types = src.types.into_iter().map(EffectContainer::from).collect();
        combine_effects!(
            types => (effect_mappers::identity());
            Intersection {
                types,
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tuple {
    pub types: Vec<FlattenedTypeInfo>,
}

impl ApplyNames for Tuple {
    fn apply_names(self, names_by_id: &HashMap<usize, String>) -> Self {
        Tuple {
            types: self
                .types
                .into_iter()
                .map(|t| t.apply_names(names_by_id))
                .collect(),
        }
    }
}

impl From<TupleIR> for EffectContainer<Tuple> {
    fn from(src: TupleIR) -> EffectContainer<Tuple> {
        let types = src.types.into_iter().map(EffectContainer::from).collect();
        combine_effects!(
            types => (effect_mappers::identity());
            Tuple {
                types,
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeParamConfig {
    pub constraint: Option<FlattenedTypeInfo>,
    pub default_type_arg: Option<TypeRef>,
}

impl ApplyNames for TypeParamConfig {
    fn apply_names(self, names_by_id: &HashMap<usize, String>) -> Self {
        TypeParamConfig {
            constraint: self.constraint.map(|c| c.apply_names(names_by_id)),
            default_type_arg: self.default_type_arg.map(|d| d.apply_names(names_by_id)),
        }
    }
}

impl From<TypeParamConfigIR> for EffectContainer<TypeParamConfig> {
    fn from(src: TypeParamConfigIR) -> EffectContainer<TypeParamConfig> {
        let constraint: EffectContainer<Option<_>> = src.constraint.map(|c| c.into()).into();
        let default_type_arg: EffectContainer<Option<_>> =
            src.default_type_arg.map(|d| d.into()).into();

        combine_effects!(
            constraint => (effect_mappers::prepend_name("Constraint")),
            default_type_arg => (effect_mappers::prepend_name("Default"));
            TypeParamConfig {
                constraint,
                default_type_arg,
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Func {
    pub type_params: Vec<(String, TypeParamConfig)>,
    pub params: Vec<Param>,
    pub return_type: Box<TypeRef>,
    pub class_name: Option<TypeIdent>,
}

impl ApplyNames for Func {
    fn apply_names(self, names_by_id: &HashMap<usize, String>) -> Self {
        Func {
            type_params: self
                .type_params
                .into_iter()
                .map(|(n, p)| (n, p.apply_names(names_by_id)))
                .collect(),
            params: self
                .params
                .into_iter()
                .map(|p| p.apply_names(names_by_id))
                .collect(),
            return_type: Box::new(self.return_type.apply_names(names_by_id)),
            class_name: self.class_name.map(|n| n.apply_names(names_by_id)),
        }
    }
}

impl From<FuncIR> for EffectContainer<Func> {
    fn from(src: FuncIR) -> EffectContainer<Func> {
        let params = src.params.into_iter().map(EffectContainer::from).collect();
        let return_type = (*src.return_type).into();
        let type_params = src
            .type_params
            .into_iter()
            .map(|(n, p)| (n, p.into()))
            .collect();
        let class_name: EffectContainer<Option<_>> = src.class_name.map(|n| n.into()).into();

        combine_effects!(
            params => (effect_mappers::prepend_name("Params")),
            return_type => (effect_mappers::prepend_name("Return")),
            type_params => (effect_mappers::identity()),
            class_name => (effect_mappers::identity());
            Func {
                type_params,
                params,
                return_type: Box::new(return_type),
                class_name,
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: String,
    pub type_info: TypeRef,
    pub is_variadic: bool,
}

impl ApplyNames for Param {
    fn apply_names(self, names_by_id: &HashMap<usize, String>) -> Self {
        Param {
            name: self.name,
            type_info: self.type_info.apply_names(names_by_id),
            is_variadic: self.is_variadic,
        }
    }
}

impl From<ParamIR> for EffectContainer<Param> {
    fn from(src: ParamIR) -> EffectContainer<Param> {
        let type_info = src.type_info.into();

        combine_effects!(
            type_info => (effect_mappers::prepend_name(src.name.clone() + "Param"));
            Param {
                name: src.name,
                type_info,
                is_variadic: src.is_variadic,
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ctor {
    pub params: Vec<Param>,
}

impl ApplyNames for Ctor {
    fn apply_names(self, names_by_id: &HashMap<usize, String>) -> Self {
        Ctor {
            params: self
                .params
                .into_iter()
                .map(|p| p.apply_names(names_by_id))
                .collect(),
        }
    }
}

impl From<CtorIR> for EffectContainer<Ctor> {
    fn from(src: CtorIR) -> EffectContainer<Ctor> {
        let params = src.params.into_iter().map(EffectContainer::from).collect();
        combine_effects!(
            params => (effect_mappers::identity());
            Ctor {
                params,
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    pub super_class: Option<TypeRef>,
    pub members: HashMap<String, Member>,
    pub type_params: Vec<(String, TypeParamConfig)>,
    pub implements: Vec<TypeRef>,
}

impl ApplyNames for Class {
    fn apply_names(self, names_by_id: &HashMap<usize, String>) -> Self {
        Class {
            super_class: self.super_class.map(|s| s.apply_names(names_by_id)),
            members: self
                .members
                .into_iter()
                .map(|(n, m)| (n, m.apply_names(names_by_id)))
                .collect(),
            type_params: self
                .type_params
                .into_iter()
                .map(|(n, t)| (n, t.apply_names(names_by_id)))
                .collect(),
            implements: self
                .implements
                .into_iter()
                .map(|i| i.apply_names(names_by_id))
                .collect(),
        }
    }
}

impl From<ClassIR> for EffectContainer<Class> {
    fn from(src: ClassIR) -> EffectContainer<Class> {
        let super_class: EffectContainer<Option<_>> = src.super_class.map(|r| (*r).into()).into();
        let type_params = src
            .type_params
            .into_iter()
            .map(|(n, t)| {
                let effects =
                    EffectContainer::from(t).adapt_effects(effect_mappers::prepend_name(&n));
                (n, effects)
            })
            .collect();
        let members = src
            .members
            .into_iter()
            .map(|(n, m)| (n, m.into()))
            .collect();
        let implements = src.implements.into_iter().map(|i| i.into()).collect();
        combine_effects!(
            members => (effect_mappers::identity()),
            super_class => (effect_mappers::identity()),
            type_params => (effect_mappers::identity()),
            implements => (effect_mappers::identity());
            Class {
                members,
                super_class,
                type_params,
                implements,
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, FromEnum)]
#[from_enum(MemberIR, effect_container = EffectContainer)]
pub enum Member {
    Constructor(Ctor),
    Method(Func),
    Property(TypeRef),
}

impl ApplyNames for Member {
    fn apply_names(self, names_by_id: &HashMap<usize, String>) -> Self {
        match self {
            Member::Constructor(c) => Member::Constructor(c.apply_names(names_by_id)),
            Member::Method(c) => Member::Method(c.apply_names(names_by_id)),
            Member::Property(c) => Member::Property(c.apply_names(names_by_id)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumMember {
    pub id: String,
    pub value: Option<EnumValue>,
}

impl From<EnumMemberIR> for EnumMember {
    fn from(src: EnumMemberIR) -> EnumMember {
        EnumMember {
            id: src.id,
            value: src.value,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enum {
    pub members: Vec<EnumMember>,
}

impl ApplyNames for Enum {
    fn apply_names(self, _: &HashMap<usize, String>) -> Self {
        self
    }
}

impl From<EnumIR> for Enum {
    fn from(src: EnumIR) -> Enum {
        Enum {
            members: src.members.into_iter().map(EnumMember::from).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Alias {
    pub target: TypeRef,
    pub type_params: Vec<(String, TypeParamConfig)>,
}

impl From<AliasIR> for EffectContainer<Alias> {
    fn from(src: AliasIR) -> EffectContainer<Alias> {
        let target: EffectContainer<_> = (*src.target).into();
        let type_params = src
            .type_params
            .into_iter()
            .map(|(n, t)| {
                let effects =
                    EffectContainer::from(t).adapt_effects(effect_mappers::prepend_name(&n));
                (n, effects)
            })
            .collect();

        combine_effects!(
            target => (effect_mappers::prepend_name("Aliased")),
            type_params => (effect_mappers::identity());
            Alias { target, type_params }
        )
    }
}

impl ApplyNames for Alias {
    fn apply_names(self, names_by_id: &HashMap<usize, String>) -> Self {
        Alias {
            target: self.target.apply_names(names_by_id),
            type_params: self
                .type_params
                .into_iter()
                .map(|(n, t)| (n, t.apply_names(names_by_id)))
                .collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeQuery {
    LookupRef(TypeRef),
}

impl From<TypeQueryIR> for EffectContainer<TypeQuery> {
    fn from(src: TypeQueryIR) -> EffectContainer<TypeQuery> {
        match src {
            TypeQueryIR::LookupRef(tr) => {
                let our_ref = tr.into();

                combine_effects!(
                    our_ref => (effect_mappers::identity());
                    TypeQuery::LookupRef(our_ref)
                )
            },
        }
    }
}

impl ApplyNames for TypeQuery {
    fn apply_names(self, names_by_id: &HashMap<usize, String>) -> Self {
        match self {
            TypeQuery::LookupRef(tr) => TypeQuery::LookupRef(tr.apply_names(names_by_id)),
        }
    }
}

macro_rules! impl_effectless_conversion {
    () => {};
    ($src:path => $dest:path) => {
        impl From<$src> for EffectContainer<$dest> {
            fn from(src: $src) -> EffectContainer<$dest> {
                EffectContainer::new(src.into(), Default::default())
            }
        }
    };
    ($src:path => $dest:path, $($rest_src:path => $rest_dest:path),* $(,)?) => {
        impl_effectless_conversion!($src => $dest);
        impl_effectless_conversion!($($rest_src => $rest_dest),*);
    };
}

impl_effectless_conversion!(
    NamespaceImport => NamespaceImport,
    EnumIR => Enum,
    EnumMemberIR => EnumMember,
);

pub fn flatten_types<Ts: IntoIterator<Item = TypeIR>>(types: Ts) -> impl Iterator<Item = FlatType> {
    types.into_iter().flat_map(|t| {
        let info = t.info.into();
        let ft = combine_effects!(
            info => (effect_mappers::prepend_name(&t.name.to_name()));
            FlatType {
                name: t.name.into(),
                is_exported: t.is_exported,
                info,
            }
        );
        let (v, effs) = ft.into_value_and_effects();
        let (effs, names_by_id): (Vec<FlatType>, HashMap<usize, String>) = effs
            .map(|eff| match eff {
                Effect::CreateType {
                    name,
                    typ,
                    generated_name_id,
                } => (
                    FlatType {
                        name: TypeIdent::LocalName(name.clone()),
                        is_exported: true,
                        info: typ.into(),
                    },
                    (generated_name_id, name),
                ),
            })
            .unzip();
        effs.into_iter()
            .chain(std::iter::once(v.apply_names(&names_by_id)))
    })
}
