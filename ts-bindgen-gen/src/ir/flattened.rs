use crate::ir::base::{
    Alias as AliasIR, BaseClass as BaseClassIR, BuiltinPromise, Class as ClassIR, Ctor as CtorIR,
    CtorGroup as CtorGroupIR, Enum as EnumIR, EnumMember as EnumMemberIR, Func as FuncIR,
    FuncGroup as FuncGroupIR, Indexer as IndexerIR, Interface as InterfaceIR,
    Intersection as IntersectionIR, JsSysBuiltin as JsSysBuiltinIR, LitBoolean, LitNumber,
    LitString, Member as MemberIR, Param as ParamIR, PrimitiveAny, PrimitiveBigInt,
    PrimitiveBoolean, PrimitiveNull, PrimitiveNumber, PrimitiveObject, PrimitiveString,
    PrimitiveUndefined, PrimitiveVoid, Tuple as TupleIR, Type as TypeIR, TypeIdent as TypeIdentIR,
    TypeInfo as TypeInfoIR, TypeName as TypeNameIR, TypeParamConfig as TypeParamConfigIR,
    TypeQuery as TypeQueryIR, TypeRef as TypeRefIR, Union as UnionIR,
    WebSysBuiltin as WebSysBuiltinIR,
};
pub use crate::ir::base::{EnumValue, NamespaceImport};
use enum_to_enum::WithEffects;
use std::collections::HashMap;
use std::iter;
use std::iter::{Extend, FromIterator};
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};

static NEXT_ID: AtomicUsize = AtomicUsize::new(1);

trait ApplyNames {
    fn apply_names(self, names_by_id: &HashMap<usize, TypeIdent>) -> Self;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FlatType {
    pub name: TypeIdent,
    pub is_exported: bool,
    pub info: FlattenedTypeInfo,
}

impl ApplyNames for FlatType {
    fn apply_names(self, names_by_id: &HashMap<usize, TypeIdent>) -> Self {
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
        file: PathBuf,
        ns: Vec<String>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
struct Namespaced<V> {
    value: V,
    file: PathBuf,
    ns: Vec<String>,
}

impl<V> Namespaced<V> {
    fn new(value: V, name: &TypeNameIR) -> Namespaced<V> {
        let ns = match &name.name {
            TypeIdentIR::Name(_) => Default::default(),
            TypeIdentIR::DefaultExport() => Default::default(),
            TypeIdentIR::QualifiedName(name_parts) => name_parts[0..name_parts.len() - 1].to_vec(),
            TypeIdentIR::TypeEnvironmentParent() => Default::default(),
        };

        Namespaced {
            value,
            ns,
            file: name.file.clone(),
        }
    }

    fn map<MapperRes, Mapper>(self, mapper: Mapper) -> MapperRes
    where
        Mapper: Fn(V, NsMaker) -> MapperRes,
    {
        let ns = NsMaker::new(&self);
        let value = self.value;
        mapper(value, ns)
    }
}

struct NsMaker {
    file: PathBuf,
    ns: Vec<String>,
}

impl NsMaker {
    fn new<V>(nsed: &Namespaced<V>) -> NsMaker {
        NsMaker {
            file: nsed.file.clone(),
            ns: nsed.ns.clone(),
        }
    }

    fn in_ns<V>(&self, value: V) -> Namespaced<V> {
        Namespaced {
            value,
            file: self.file.clone(),
            ns: self.ns.clone(),
        }
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

impl From<Namespaced<NameableTypeInfo>> for FlattenedTypeInfo {
    fn from(src: Namespaced<NameableTypeInfo>) -> FlattenedTypeInfo {
        match src.value {
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FlattenedTypeInfo {
    Interface(Interface),
    Enum(Enum),
    Alias(Alias),
    Ref(TypeRef),
    Array { item_type: Box<FlattenedTypeInfo> },
    Tuple(Tuple),
    Optional { item_type: Box<FlattenedTypeInfo> },
    Union(Union),
    Intersection(Intersection),
    Mapped { value_type: Box<FlattenedTypeInfo> },
    FuncGroup(FuncGroup),
    Constructor(Ctor),
    Class(Class),
    Var { type_info: Box<FlattenedTypeInfo> },
    NamespaceImport(NamespaceImport),
    TypeQuery(TypeQuery),
}

macro_rules! tuple_match_convert {
    ($ns:ident, $to_case:ident ($val:ident)) => {{
        let val_with_effects: EffectContainer<_> = $ns.in_ns($val).into();
        let ($val, effects) = val_with_effects.into_value_and_effects();
        EffectContainer::compose_from($to_case($val), effects.collect())
    }};
}

macro_rules! struct_match_convert {
    ($ns: ident, $to_case:ident { $item: ident }) => {{
        let val_with_effects: EffectContainer<_> = $ns.in_ns($item).into();
        let ($item, effects) = val_with_effects.into_value_and_effects();
        EffectContainer::compose_from($to_case { $item }, effects.collect())
    }};
}

impl From<Namespaced<TypeInfoIR>> for EffectContainer<FlattenedTypeInfo> {
    fn from(src: Namespaced<TypeInfoIR>) -> EffectContainer<FlattenedTypeInfo> {
        src.map(|v, ns| {
            use FlattenedTypeInfo::*; // using paths instead of idents in {tuple,struct}_match_convert is hard...
            match v {
                TypeInfoIR::Interface(v) => tuple_match_convert!(ns, Interface(v)),
                TypeInfoIR::Enum(v) => tuple_match_convert!(ns, Enum(v)),
                TypeInfoIR::Alias(v) => tuple_match_convert!(ns, Alias(v)),
                TypeInfoIR::PrimitiveAny(v) => tuple_match_convert!(ns, Ref(v)),
                TypeInfoIR::PrimitiveNumber(v) => tuple_match_convert!(ns, Ref(v)),
                TypeInfoIR::PrimitiveObject(v) => tuple_match_convert!(ns, Ref(v)),
                TypeInfoIR::PrimitiveBoolean(v) => tuple_match_convert!(ns, Ref(v)),
                TypeInfoIR::PrimitiveBigInt(v) => tuple_match_convert!(ns, Ref(v)),
                TypeInfoIR::PrimitiveString(v) => tuple_match_convert!(ns, Ref(v)),
                TypeInfoIR::PrimitiveVoid(v) => tuple_match_convert!(ns, Ref(v)),
                TypeInfoIR::PrimitiveUndefined(v) => tuple_match_convert!(ns, Ref(v)),
                TypeInfoIR::PrimitiveNull(v) => tuple_match_convert!(ns, Ref(v)),
                TypeInfoIR::BuiltinPromise(v) => tuple_match_convert!(ns, Ref(v)),
                TypeInfoIR::LitNumber(v) => tuple_match_convert!(ns, Ref(v)),
                TypeInfoIR::LitString(v) => tuple_match_convert!(ns, Ref(v)),
                TypeInfoIR::LitBoolean(v) => tuple_match_convert!(ns, Ref(v)),
                TypeInfoIR::WebSysBuiltin(b) => tuple_match_convert!(ns, Ref(b)),
                TypeInfoIR::JsSysBuiltin(b) => tuple_match_convert!(ns, Ref(b)),
                TypeInfoIR::Ref(v) => tuple_match_convert!(ns, Ref(v)),
                TypeInfoIR::Array { item_type } => struct_match_convert!(ns, Array { item_type }),
                TypeInfoIR::Tuple(v) => tuple_match_convert!(ns, Tuple(v)),
                TypeInfoIR::Optional { item_type } => {
                    struct_match_convert!(ns, Optional { item_type })
                }
                TypeInfoIR::Union(v) => tuple_match_convert!(ns, Union(v)),
                TypeInfoIR::Intersection(v) => tuple_match_convert!(ns, Intersection(v)),
                TypeInfoIR::Mapped { value_type } => {
                    struct_match_convert!(ns, Mapped { value_type })
                }
                TypeInfoIR::FuncGroup(v) => tuple_match_convert!(ns, FuncGroup(v)),
                TypeInfoIR::Constructor(v) => tuple_match_convert!(ns, Constructor(v)),
                TypeInfoIR::Class(v) => tuple_match_convert!(ns, Class(v)),
                TypeInfoIR::Var { type_info } => struct_match_convert!(ns, Var { type_info }),
                TypeInfoIR::NamespaceImport(v) => tuple_match_convert!(ns, NamespaceImport(v)),
                TypeInfoIR::TypeQuery(v) => tuple_match_convert!(ns, TypeQuery(v)),
            }
        })
    }
}

impl ApplyNames for FlattenedTypeInfo {
    fn apply_names(self, names_by_id: &HashMap<usize, TypeIdent>) -> Self {
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
            FlattenedTypeInfo::FuncGroup(f) => {
                FlattenedTypeInfo::FuncGroup(f.apply_names(names_by_id))
            }
            FlattenedTypeInfo::Constructor(c) => {
                FlattenedTypeInfo::Constructor(c.apply_names(names_by_id))
            }
            FlattenedTypeInfo::Class(c) => FlattenedTypeInfo::Class(c.apply_names(names_by_id)),
            FlattenedTypeInfo::Var { type_info } => FlattenedTypeInfo::Var {
                type_info: Box::new(type_info.apply_names(names_by_id)),
            },
            FlattenedTypeInfo::NamespaceImport(n) => FlattenedTypeInfo::NamespaceImport(n),
            FlattenedTypeInfo::TypeQuery(q) => {
                FlattenedTypeInfo::TypeQuery(q.apply_names(names_by_id))
            }
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
    fn apply_names(self, names_by_id: &HashMap<usize, TypeIdent>) -> Self {
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
                file,
                ns,
                typ,
                generated_name_id,
            } => Effect::CreateType {
                name: format!("{}_{}", prefix.as_ref().to_title_case(), &name),
                file,
                ns,
                typ,
                generated_name_id,
            },
        }
    }

    pub fn identity() -> impl Fn(Effect) -> Effect {
        |ec: Effect| ec
    }
}

impl From<Namespaced<InterfaceIR>> for EffectContainer<Interface> {
    fn from(src: Namespaced<InterfaceIR>) -> EffectContainer<Interface> {
        src.map(|v, ns| {
            let indexer = v
                .indexer
                .map(|i| ns.in_ns(i))
                .map(EffectContainer::from)
                .into();
            let extends = v
                .extends
                .into_iter()
                .map(|e| ns.in_ns(e))
                .map(EffectContainer::from)
                .collect();
            let fields = v
                .fields
                .into_iter()
                .map(|(n, t)| {
                    let effects = EffectContainer::from(ns.in_ns(t))
                        .adapt_effects(effect_mappers::prepend_name(&n));
                    (n, effects)
                })
                .collect();
            let constructor = v
                .constructor
                .map(|c| ns.in_ns(c))
                .map(EffectContainer::from)
                .into();
            let type_params = v
                .type_params
                .into_iter()
                .map(|(n, t)| {
                    let effects = EffectContainer::from(ns.in_ns(t))
                        .adapt_effects(effect_mappers::prepend_name(&n));
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
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Indexer {
    pub readonly: bool,
    pub value_type: TypeRef,
}

impl ApplyNames for Indexer {
    fn apply_names(self, names_by_id: &HashMap<usize, TypeIdent>) -> Self {
        Indexer {
            readonly: self.readonly,
            value_type: self.value_type.apply_names(names_by_id),
        }
    }
}

impl From<Namespaced<IndexerIR>> for EffectContainer<Indexer> {
    fn from(src: Namespaced<IndexerIR>) -> EffectContainer<Indexer> {
        src.map(|v, ns| {
            let ti = *v.type_info;
            let value_type = ns.in_ns(ti).into();

            combine_effects!(
                value_type => (effect_mappers::identity());
                Indexer {
                    readonly: v.readonly,
                    value_type
                }
            )
        })
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
    fn apply_names(self, names_by_id: &HashMap<usize, TypeIdent>) -> Self {
        match self {
            TypeIdent::GeneratedName { id } => names_by_id.get(&id).unwrap().clone(),
            _ => self,
        }
    }
}

impl From<Namespaced<TypeNameIR>> for TypeIdent {
    fn from(src: Namespaced<TypeNameIR>) -> TypeIdent {
        let src = src.value;
        let file = src.file;
        match src.name {
            TypeIdentIR::Name(name) => TypeIdent::Name { file, name },
            TypeIdentIR::DefaultExport() => TypeIdent::DefaultExport(file),
            TypeIdentIR::QualifiedName(name_parts) => TypeIdent::QualifiedName { file, name_parts },
            TypeIdentIR::TypeEnvironmentParent() => {
                // TODO: should never get to this case
                TypeIdent::Name {
                    file,
                    name: "__parent__".to_string(),
                }
            }
        }
    }
}

impl From<Namespaced<TypeNameIR>> for EffectContainer<TypeIdent> {
    fn from(src: Namespaced<TypeNameIR>) -> EffectContainer<TypeIdent> {
        EffectContainer::new(src.into(), Default::default())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeRef {
    pub referent: TypeIdent,
    pub type_params: Vec<TypeRef>,
}

impl ApplyNames for TypeRef {
    fn apply_names(self, names_by_id: &HashMap<usize, TypeIdent>) -> Self {
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
impl From<Namespaced<TypeInfoIR>> for EffectContainer<TypeRef> {
    fn from(src: Namespaced<TypeInfoIR>) -> EffectContainer<TypeRef> {
        src.map(|v, ns| match v {
            TypeInfoIR::Interface(i) => ns.in_ns(i).into(),
            TypeInfoIR::Enum(_) => panic!("Enum only expected as top-level type"),
            TypeInfoIR::Ref(t) => ns.in_ns(t).into(),
            TypeInfoIR::Alias(_) => panic!("Alias only expected as top-level type"),
            TypeInfoIR::PrimitiveAny(p) => ns.in_ns(p).into(),
            TypeInfoIR::PrimitiveNumber(p) => ns.in_ns(p).into(),
            TypeInfoIR::PrimitiveObject(p) => ns.in_ns(p).into(),
            TypeInfoIR::PrimitiveBoolean(p) => ns.in_ns(p).into(),
            TypeInfoIR::PrimitiveBigInt(p) => ns.in_ns(p).into(),
            TypeInfoIR::PrimitiveString(p) => ns.in_ns(p).into(),
            TypeInfoIR::PrimitiveVoid(p) => ns.in_ns(p).into(),
            TypeInfoIR::PrimitiveUndefined(p) => ns.in_ns(p).into(),
            TypeInfoIR::PrimitiveNull(p) => ns.in_ns(p).into(),
            TypeInfoIR::BuiltinPromise(b) => ns.in_ns(b).into(),
            TypeInfoIR::WebSysBuiltin(b) => ns.in_ns(b).into(),
            TypeInfoIR::JsSysBuiltin(b) => ns.in_ns(b).into(),
            TypeInfoIR::Array { item_type } => {
                let item_type: EffectContainer<TypeRef> = ns.in_ns(*item_type).into();
                combine_effects!(
                    item_type => (effect_mappers::identity());
                    TypeRef {
                        referent: TypeIdent::Builtin(Builtin::Array),
                        type_params: vec![item_type],
                    }
                )
            }
            TypeInfoIR::Tuple(t) => ns.in_ns(t).into(),
            TypeInfoIR::Optional { item_type } => {
                let item_type: EffectContainer<TypeRef> = ns.in_ns(*item_type).into();
                combine_effects!(
                    item_type => (effect_mappers::identity());
                    TypeRef {
                        referent: TypeIdent::Builtin(Builtin::Optional),
                        type_params: vec![item_type],
                    }
                )
            }
            TypeInfoIR::Union(u) => ns.in_ns(u).into(),
            TypeInfoIR::Intersection(i) => ns.in_ns(i).into(),
            TypeInfoIR::Mapped { value_type } => {
                let value_type: EffectContainer<TypeRef> = ns.in_ns(*value_type).into();
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
            TypeInfoIR::LitNumber(l) => ns.in_ns(l).into(),
            TypeInfoIR::LitString(l) => ns.in_ns(l).into(),
            TypeInfoIR::LitBoolean(l) => ns.in_ns(l).into(),
            TypeInfoIR::FuncGroup(f) => ns.in_ns(f).into(),
            TypeInfoIR::Constructor(_) => panic!("Constructor only expected as top-level type"),
            TypeInfoIR::Class(_) => panic!("Class only expected as top-level type"),
            TypeInfoIR::Var { type_info: _ } => panic!("Var only expected as a top-level type"),
            TypeInfoIR::NamespaceImport(_) => {
                panic!("Namespace import only expected as a top-level construct")
            }
            TypeInfoIR::TypeQuery(tr) => ns.in_ns(tr).into(),
        })
    }
}

impl From<Namespaced<TypeRefIR>> for EffectContainer<TypeRef> {
    fn from(src: Namespaced<TypeRefIR>) -> EffectContainer<TypeRef> {
        src.map(|v, ns| {
            let referent = ns.in_ns(v.referent).into();
            let type_params = v
                .type_params
                .into_iter()
                .map(|t| ns.in_ns(t))
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
        })
    }
}

impl From<Namespaced<BaseClassIR>> for EffectContainer<TypeRef> {
    fn from(src: Namespaced<BaseClassIR>) -> EffectContainer<TypeRef> {
        src.map(|v, ns| match v {
            BaseClassIR::Resolved(ti) => ns.in_ns(ti).into(),
            BaseClassIR::Unresolved(_) => panic!("expected only resolved base classes"),
        })
    }
}

impl From<Namespaced<FuncGroupIR>> for EffectContainer<TypeRef> {
    fn from(src: Namespaced<FuncGroupIR>) -> EffectContainer<TypeRef> {
        let f: EffectContainer<FuncGroup> = src.into();

        assert_eq!(
            f.value.overloads.len(),
            1,
            "expected exactly 1 overload for functions in type ref position"
        );

        let f = EffectContainer {
            value: f.value.widened_fn,
            effects: f.effects,
        };

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

impl From<Namespaced<FuncIR>> for EffectContainer<TypeRef> {
    fn from(src: Namespaced<FuncIR>) -> EffectContainer<TypeRef> {
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

impl From<Namespaced<TypeQueryIR>> for EffectContainer<TypeRef> {
    fn from(src: Namespaced<TypeQueryIR>) -> EffectContainer<TypeRef> {
        src.map(|v, ns| match v {
            TypeQueryIR::LookupRef(tr) => ns.in_ns(tr).into(),
        })
    }
}

macro_rules! impl_effectful_conversion_from_nameable_type_to_type_ref {
    () => {};
    ($src:path => $nameable:ident) => {
        impl From<Namespaced<$src>> for EffectContainer<TypeRef> {
            fn from(src: Namespaced<$src>) -> EffectContainer<TypeRef> {
                let id = NEXT_ID.fetch_add(1, Ordering::SeqCst);
                let file = src.file.clone();
                let ns = src.ns.clone();
                let ec: EffectContainer<_> = src.into();
                let (val, effects) = ec.into_value_and_effects();
                let effect = Effect::CreateType {
                    name: "".to_string(), // our name is filled in as we bubble up
                    file,
                    ns,
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
        impl From<Namespaced<$prim>> for EffectContainer<TypeRef> {
            fn from(_: Namespaced<$prim>) -> EffectContainer<TypeRef> {
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
    PrimitiveVoid,
    PrimitiveUndefined,
    PrimitiveNull,
    LitNumber,
    LitBoolean,
    LitString,
    Promise,
    Array,
    Fn,
    Map,
    Optional,
    Variadic,
    Named(String),
}

type_ref_from_prims!(
    PrimitiveAny => PrimitiveAny,
    PrimitiveNumber => PrimitiveNumber,
    PrimitiveObject => PrimitiveObject,
    PrimitiveBoolean => PrimitiveBoolean,
    PrimitiveBigInt => PrimitiveBigInt,
    PrimitiveString => PrimitiveString,
    PrimitiveVoid => PrimitiveVoid,
    PrimitiveUndefined => PrimitiveUndefined,
    PrimitiveNull => PrimitiveNull,
    LitNumber => LitNumber,
    LitBoolean => LitBoolean,
    LitString => LitString,
    BuiltinPromise => Promise,
);

impl From<Namespaced<WebSysBuiltinIR>> for EffectContainer<TypeRef> {
    fn from(src: Namespaced<WebSysBuiltinIR>) -> EffectContainer<TypeRef> {
        EffectContainer::new(
            TypeRef {
                referent: TypeIdent::Builtin(Builtin::Named(format!("web_sys::{}", src.value.0))),
                type_params: Default::default(),
            },
            Default::default(),
        )
    }
}

impl From<Namespaced<JsSysBuiltinIR>> for EffectContainer<TypeRef> {
    fn from(src: Namespaced<JsSysBuiltinIR>) -> EffectContainer<TypeRef> {
        EffectContainer::new(
            TypeRef {
                referent: TypeIdent::Builtin(Builtin::Named(format!("js_sys::{}", src.value.0))),
                type_params: Default::default(),
            },
            Default::default(),
        )
    }
}

impl From<Namespaced<Box<TypeInfoIR>>> for EffectContainer<Box<FlattenedTypeInfo>> {
    fn from(src: Namespaced<Box<TypeInfoIR>>) -> EffectContainer<Box<FlattenedTypeInfo>> {
        src.map(|v, ns| {
            let ti = EffectContainer::from(ns.in_ns(*v));
            combine_effects!(
                ti => (effect_mappers::identity());
                Box::new(ti)
            )
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Union {
    pub types: Vec<FlattenedTypeInfo>,
}

impl ApplyNames for Union {
    fn apply_names(self, names_by_id: &HashMap<usize, TypeIdent>) -> Self {
        Union {
            types: self
                .types
                .into_iter()
                .map(|t| t.apply_names(names_by_id))
                .collect(),
        }
    }
}

impl From<Namespaced<UnionIR>> for EffectContainer<Union> {
    fn from(src: Namespaced<UnionIR>) -> EffectContainer<Union> {
        src.map(|v, ns| {
            let types = v
                .types
                .into_iter()
                .map(|t| ns.in_ns(t))
                .map(EffectContainer::from)
                .collect();
            combine_effects!(
                types => (effect_mappers::identity());
                Union {
                    types,
                }
            )
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Intersection {
    pub types: Vec<FlattenedTypeInfo>,
}

impl ApplyNames for Intersection {
    fn apply_names(self, names_by_id: &HashMap<usize, TypeIdent>) -> Self {
        Intersection {
            types: self
                .types
                .into_iter()
                .map(|t| t.apply_names(names_by_id))
                .collect(),
        }
    }
}

impl From<Namespaced<IntersectionIR>> for EffectContainer<Intersection> {
    fn from(src: Namespaced<IntersectionIR>) -> EffectContainer<Intersection> {
        src.map(|v, ns| {
            let types = v
                .types
                .into_iter()
                .map(|t| ns.in_ns(t))
                .map(EffectContainer::from)
                .collect();
            combine_effects!(
                types => (effect_mappers::identity());
                Intersection {
                    types,
                }
            )
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tuple {
    pub types: Vec<FlattenedTypeInfo>,
}

impl ApplyNames for Tuple {
    fn apply_names(self, names_by_id: &HashMap<usize, TypeIdent>) -> Self {
        Tuple {
            types: self
                .types
                .into_iter()
                .map(|t| t.apply_names(names_by_id))
                .collect(),
        }
    }
}

impl From<Namespaced<TupleIR>> for EffectContainer<Tuple> {
    fn from(src: Namespaced<TupleIR>) -> EffectContainer<Tuple> {
        src.map(|v, ns| {
            let types = v
                .types
                .into_iter()
                .map(|t| ns.in_ns(t))
                .map(EffectContainer::from)
                .collect();
            combine_effects!(
                types => (effect_mappers::identity());
                Tuple {
                    types,
                }
            )
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeParamConfig {
    pub constraint: Option<FlattenedTypeInfo>,
    pub default_type_arg: Option<TypeRef>,
}

impl ApplyNames for TypeParamConfig {
    fn apply_names(self, names_by_id: &HashMap<usize, TypeIdent>) -> Self {
        TypeParamConfig {
            constraint: self.constraint.map(|c| c.apply_names(names_by_id)),
            default_type_arg: self.default_type_arg.map(|d| d.apply_names(names_by_id)),
        }
    }
}

impl From<Namespaced<TypeParamConfigIR>> for EffectContainer<TypeParamConfig> {
    fn from(src: Namespaced<TypeParamConfigIR>) -> EffectContainer<TypeParamConfig> {
        src.map(|v, ns| {
            let constraint: EffectContainer<Option<_>> =
                v.constraint.map(|c| ns.in_ns(c).into()).into();
            let default_type_arg: EffectContainer<Option<_>> =
                v.default_type_arg.map(|d| ns.in_ns(d).into()).into();

            combine_effects!(
                constraint => (effect_mappers::prepend_name("Constraint")),
                default_type_arg => (effect_mappers::prepend_name("Default"));
                TypeParamConfig {
                    constraint,
                    default_type_arg,
                }
            )
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncGroup {
    pub overloads: Vec<Func>,
    pub widened_fn: Func,
}

impl ApplyNames for FuncGroup {
    fn apply_names(self, names_by_id: &HashMap<usize, TypeIdent>) -> Self {
        FuncGroup {
            overloads: self
                .overloads
                .into_iter()
                .map(|o| o.apply_names(names_by_id))
                .collect(),
            widened_fn: self.widened_fn.apply_names(names_by_id),
        }
    }
}

fn combine_to_union(a: &TypeInfoIR, b: &TypeInfoIR) -> TypeInfoIR {
    match (a, b) {
        (TypeInfoIR::Union(u1), TypeInfoIR::Union(u2)) => TypeInfoIR::Union(UnionIR {
            types: u1.types.iter().chain(u2.types.iter()).cloned().collect(),
        }),
        (TypeInfoIR::Union(u1), t2) => TypeInfoIR::Union(UnionIR {
            types: u1.types.iter().chain(iter::once(t2)).cloned().collect(),
        }),
        (t1, TypeInfoIR::Union(u2)) => TypeInfoIR::Union(UnionIR {
            types: u2.types.iter().chain(iter::once(t1)).cloned().collect(),
        }),
        (t1, t2) => TypeInfoIR::Union(UnionIR {
            types: vec![t1.clone(), t2.clone()],
        }),
    }
}

fn to_widened_fn(fns: &[FuncIR]) -> FuncIR {
    FuncIR {
        type_params: fns
            .first()
            // TODO: shouldn't assume all fns have the same type params
            // though typescript doesn't do well with overloaded generic fns
            .map(|f| f.type_params.clone())
            .unwrap_or_default(),
        params: fns
            .iter()
            .fold(vec![], |prev_params, FuncIR { params, .. }| {
                // TODO: need to extend is_variadic params so that variadic
                // params come after all other params and are only unioned
                // with other variadic params
                prev_params
                    .iter()
                    .zip(params.iter())
                    .map(|(prev, param)| ParamIR {
                        name: prev.name.clone(),
                        type_info: combine_to_union(&prev.type_info, &param.type_info),
                        is_variadic: prev.is_variadic && param.is_variadic,
                    })
                    .chain(params.iter().skip(prev_params.len()).cloned())
                    .chain(prev_params.iter().skip(params.len()).cloned())
                    .collect()
            }),
        class_name: fns
            .first()
            .map(|f| f.class_name.clone())
            .unwrap_or_default(),
        return_type: fns
            .iter()
            .skip(1)
            .fold(fns.first().unwrap().return_type.clone(), |t1, t2| {
                Box::new(combine_to_union(t1.as_ref(), t2.return_type.as_ref()))
            }),
    }
}

impl From<Namespaced<FuncGroupIR>> for EffectContainer<FuncGroup> {
    fn from(src: Namespaced<FuncGroupIR>) -> EffectContainer<FuncGroup> {
        src.map(|v, ns| {
            let widened_fn = ns.in_ns(to_widened_fn(&v.overloads)).into();
            let overloads = v
                .overloads
                .into_iter()
                .map(|p| ns.in_ns(p))
                .map(EffectContainer::from)
                .collect();

            combine_effects!(
                overloads => (effect_mappers::identity()),
                widened_fn => (effect_mappers::identity());
                FuncGroup {
                    overloads,
                    widened_fn,
                }
            )
        })
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
    fn apply_names(self, names_by_id: &HashMap<usize, TypeIdent>) -> Self {
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

impl From<Namespaced<FuncIR>> for EffectContainer<Func> {
    fn from(src: Namespaced<FuncIR>) -> EffectContainer<Func> {
        src.map(|v, ns| {
            let params = v
                .params
                .into_iter()
                .map(|p| ns.in_ns(p))
                .map(EffectContainer::from)
                .collect();
            let return_type = ns.in_ns(*v.return_type).into();
            let type_params = v
                .type_params
                .into_iter()
                .map(|(n, p)| (n, ns.in_ns(p).into()))
                .collect();
            let class_name: EffectContainer<Option<_>> =
                v.class_name.map(|n| ns.in_ns(n).into()).into();

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
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: String,
    pub type_info: TypeRef,
    pub is_variadic: bool,
}

impl ApplyNames for Param {
    fn apply_names(self, names_by_id: &HashMap<usize, TypeIdent>) -> Self {
        Param {
            name: self.name,
            type_info: self.type_info.apply_names(names_by_id),
            is_variadic: self.is_variadic,
        }
    }
}

impl From<Namespaced<ParamIR>> for EffectContainer<Param> {
    fn from(src: Namespaced<ParamIR>) -> EffectContainer<Param> {
        src.map(|v, ns| {
            let type_info = ns.in_ns(v.type_info).into();

            combine_effects!(
                type_info => (effect_mappers::prepend_name(v.name.clone() + "Param"));
                Param {
                    name: v.name,
                    type_info,
                    is_variadic: v.is_variadic,
                }
            )
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CtorGroup {
    pub overloads: Vec<Ctor>,
}

impl ApplyNames for CtorGroup {
    fn apply_names(self, names_by_id: &HashMap<usize, TypeIdent>) -> Self {
        CtorGroup {
            overloads: self
                .overloads
                .into_iter()
                .map(|o| o.apply_names(names_by_id))
                .collect(),
        }
    }
}

impl From<Namespaced<CtorGroupIR>> for EffectContainer<CtorGroup> {
    fn from(src: Namespaced<CtorGroupIR>) -> EffectContainer<CtorGroup> {
        src.map(|v, ns| {
            let overloads = v
                .overloads
                .into_iter()
                .map(|o| ns.in_ns(o))
                .map(EffectContainer::from)
                .collect();
            combine_effects!(
                overloads => (effect_mappers::identity());
                CtorGroup {
                    overloads,
                }
            )
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ctor {
    pub params: Vec<Param>,
}

impl ApplyNames for Ctor {
    fn apply_names(self, names_by_id: &HashMap<usize, TypeIdent>) -> Self {
        Ctor {
            params: self
                .params
                .into_iter()
                .map(|p| p.apply_names(names_by_id))
                .collect(),
        }
    }
}

impl From<Namespaced<CtorIR>> for EffectContainer<Ctor> {
    fn from(src: Namespaced<CtorIR>) -> EffectContainer<Ctor> {
        src.map(|v, ns| {
            let params = v
                .params
                .into_iter()
                .map(|p| ns.in_ns(p))
                .map(EffectContainer::from)
                .collect();
            combine_effects!(
                params => (effect_mappers::identity());
                Ctor {
                    params,
                }
            )
        })
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
    fn apply_names(self, names_by_id: &HashMap<usize, TypeIdent>) -> Self {
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

impl From<Namespaced<ClassIR>> for EffectContainer<Class> {
    fn from(src: Namespaced<ClassIR>) -> EffectContainer<Class> {
        src.map(|v, ns| {
            let super_class: EffectContainer<Option<_>> =
                v.super_class.map(|r| ns.in_ns(*r).into()).into();
            let type_params = v
                .type_params
                .into_iter()
                .map(|(n, t)| {
                    let effects = EffectContainer::from(ns.in_ns(t))
                        .adapt_effects(effect_mappers::prepend_name(&n));
                    (n, effects)
                })
                .collect();
            let members = v
                .members
                .into_iter()
                .filter(|(_, m)| {
                    // if we have a javascript type that acts as a namespace, containing
                    // other types, we just ignore those entries.
                    // this is fine IF those classes are exported elsewhere. if they
                    // aren't, we may need to find a way to force them to be exported
                    // (reasonable since they are de facto exported by virtue of being a property on
                    // an exported class)
                    !matches!(m, MemberIR::Property(TypeInfoIR::TypeQuery(_)))
                })
                .map(|(n, m)| {
                    let effects = EffectContainer::from(ns.in_ns(m))
                        .adapt_effects(effect_mappers::prepend_name(&n));
                    (n, effects)
                })
                .collect();
            let implements = v
                .implements
                .into_iter()
                .map(|i| ns.in_ns(i).into())
                .collect();
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
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Member {
    Constructor(CtorGroup),
    Method(FuncGroup),
    Property(TypeRef),
}

impl ApplyNames for Member {
    fn apply_names(self, names_by_id: &HashMap<usize, TypeIdent>) -> Self {
        match self {
            Member::Constructor(c) => Member::Constructor(c.apply_names(names_by_id)),
            Member::Method(c) => Member::Method(c.apply_names(names_by_id)),
            Member::Property(c) => Member::Property(c.apply_names(names_by_id)),
        }
    }
}

impl From<Namespaced<MemberIR>> for EffectContainer<Member> {
    fn from(src: Namespaced<MemberIR>) -> EffectContainer<Member> {
        src.map(|v, ns| match v {
            MemberIR::Constructor(c) => {
                let ctor = ns.in_ns(c).into();
                combine_effects!(
                    ctor => (effect_mappers::identity());
                    Member::Constructor(ctor)
                )
            }
            MemberIR::Method(f) => {
                let f = ns.in_ns(f).into();
                combine_effects!(
                    f => (effect_mappers::identity());
                    Member::Method(f)
                )
            }
            MemberIR::Property(t) => {
                let t = ns.in_ns(t).into();
                combine_effects!(
                    t => (effect_mappers::identity());
                    Member::Property(t)
                )
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumMember {
    pub id: String,
    pub value: Option<EnumValue>,
}

impl From<Namespaced<EnumMemberIR>> for EnumMember {
    fn from(src: Namespaced<EnumMemberIR>) -> EnumMember {
        EnumMember {
            id: src.value.id,
            value: src.value.value,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enum {
    pub members: Vec<EnumMember>,
}

impl ApplyNames for Enum {
    fn apply_names(self, _: &HashMap<usize, TypeIdent>) -> Self {
        self
    }
}

impl From<Namespaced<EnumIR>> for Enum {
    fn from(src: Namespaced<EnumIR>) -> Enum {
        src.map(|v, ns| Enum {
            members: v
                .members
                .into_iter()
                .map(|m| ns.in_ns(m))
                .map(EnumMember::from)
                .collect(),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Alias {
    pub target: TypeRef,
    pub type_params: Vec<(String, TypeParamConfig)>,
}

impl From<Namespaced<AliasIR>> for EffectContainer<Alias> {
    fn from(src: Namespaced<AliasIR>) -> EffectContainer<Alias> {
        src.map(|v, ns| {
            let target: EffectContainer<_> = ns.in_ns(*v.target).into();
            let type_params = v
                .type_params
                .into_iter()
                .map(|(n, t)| {
                    let effects = EffectContainer::from(ns.in_ns(t))
                        .adapt_effects(effect_mappers::prepend_name(&n));
                    (n, effects)
                })
                .collect();

            combine_effects!(
                target => (effect_mappers::prepend_name("Aliased")),
                type_params => (effect_mappers::identity());
                Alias { target, type_params }
            )
        })
    }
}

impl ApplyNames for Alias {
    fn apply_names(self, names_by_id: &HashMap<usize, TypeIdent>) -> Self {
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

impl From<Namespaced<TypeQueryIR>> for EffectContainer<TypeQuery> {
    fn from(src: Namespaced<TypeQueryIR>) -> EffectContainer<TypeQuery> {
        src.map(|v, ns| match v {
            TypeQueryIR::LookupRef(tr) => {
                let our_ref = ns.in_ns(tr).into();

                combine_effects!(
                    our_ref => (effect_mappers::identity());
                    TypeQuery::LookupRef(our_ref)
                )
            }
        })
    }
}

impl ApplyNames for TypeQuery {
    fn apply_names(self, names_by_id: &HashMap<usize, TypeIdent>) -> Self {
        match self {
            TypeQuery::LookupRef(tr) => TypeQuery::LookupRef(tr.apply_names(names_by_id)),
        }
    }
}

impl From<Namespaced<NamespaceImport>> for NamespaceImport {
    fn from(src: Namespaced<NamespaceImport>) -> NamespaceImport {
        src.value
    }
}

macro_rules! impl_effectless_conversion {
    () => {};
    ($src:path => $dest:path) => {
        impl From<Namespaced<$src>> for EffectContainer<$dest> {
            fn from(src: Namespaced<$src>) -> EffectContainer<$dest> {
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
    types
        .into_iter()
        .filter(|t| {
            // skip these marker types
            t.name.name != TypeIdentIR::TypeEnvironmentParent()
        })
        .flat_map(|t| {
            let info = Namespaced::new(t.info, &t.name).into();
            let ft = combine_effects!(
                info => (effect_mappers::prepend_name(&t.name.to_name()));
                FlatType {
                    name: Namespaced::new(t.name.clone(), &t.name).into(),
                    is_exported: t.is_exported,
                    info,
                }
            );
            let (v, effs) = ft.into_value_and_effects();
            let (effs, names_by_id): (Vec<FlatType>, HashMap<usize, TypeIdent>) = effs
                .map(|eff| match eff {
                    Effect::CreateType {
                        name,
                        file,
                        ns,
                        typ,
                        generated_name_id,
                    } => {
                        let ident_ir = if ns.is_empty() {
                            TypeIdentIR::Name(name)
                        } else {
                            let mut name_parts = ns;
                            name_parts.push(name);

                            TypeIdentIR::QualifiedName(name_parts)
                        };
                        let name_ir = TypeNameIR {
                            name: ident_ir,
                            file: file.clone(),
                        };
                        let name: TypeIdent = Namespaced {
                            value: name_ir.clone(),
                            file,
                            ns: Default::default(),
                        }
                        .into();

                        (
                            FlatType {
                                name: name.clone(),
                                is_exported: true,
                                info: Namespaced::new(typ, &name_ir).into(),
                            },
                            (generated_name_id, name),
                        )
                    }
                })
                .unzip();
            effs.into_iter()
                .chain(std::iter::once(v))
                .map(move |t| t.apply_names(&names_by_id))
        })
}

#[cfg(test)]
mod test {
    use super::{flatten_types, FlatType, TypeIdent};
    use crate::fs::MemFs;
    use crate::parse::TsTypes;
    use crate::ArcFs;
    use std::collections::HashMap;
    use std::error::Error;
    use std::path::Path;
    use std::sync::Arc;

    fn get_types_for_code(ts_code: &str) -> Result<HashMap<TypeIdent, FlatType>, Box<dyn Error>> {
        let test_path: &Path = Path::new("/test.d.ts");
        let mut fs: MemFs = Default::default();
        fs.set_cwd(Path::new("/"));
        fs.add_file_at(test_path, ts_code.to_string());

        let types_by_name = TsTypes::parse(Arc::new(fs) as ArcFs, &test_path.to_string_lossy())?
            .iter()
            .find_map(|(path, types_by_name)| {
                if path == test_path {
                    Some(
                        flatten_types(types_by_name.values().cloned())
                            .map(|t| (t.name.clone(), t))
                            .collect::<HashMap<_, _>>(),
                    )
                } else {
                    None
                }
            });

        assert!(types_by_name.is_some());

        Ok(types_by_name.unwrap())
    }

    #[test]
    fn test_nested_generated_names() {
        // a tuple as a param needs to be flattened inside of a union that gets flattened
        let code = r#"
            export type MyType = number | ((a: [string]) => string);
        "#;

        let types = get_types_for_code(code).unwrap();
        let debug = format!("{:?}", types);

        // TODO: this is a wildly hacky way to check this but we want to
        // make sure that nested GeneratedNames are properly transformed
        // into real names. this avoids having to write some generic mapper.
        assert!(!debug.contains("GeneratedName"));
    }
}
