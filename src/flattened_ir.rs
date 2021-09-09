use crate::ir::{
    Alias, BaseClass as BaseClassIR, BuiltinDate, BuiltinPromise, Class as ClassIR, Ctor as CtorIR,
    Enum as EnumIR, EnumMember as EnumMemberIR, Func as FuncIR, Indexer as IndexerIR,
    Interface as InterfaceIR, Intersection as IntersectionIR, LitBoolean, LitNumber, LitString,
    Member as MemberIR, NamespaceImport, Param as ParamIR, PrimitiveAny, PrimitiveBigInt,
    PrimitiveBoolean, PrimitiveNull, PrimitiveNumber, PrimitiveObject, PrimitiveString,
    PrimitiveSymbol, PrimitiveUndefined, PrimitiveVoid, TypeIdent as TypeIdentIR,
    TypeInfo as TypeInfoIR, TypeName as TypeNameIR, TypeRef as TypeRefIR, Union as UnionIR,
};
use enum_to_enum::{FromEnum, WithEffects};
use std::collections::HashMap;
use std::iter::{Extend, FromIterator};
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Effect {
    CreateType { name: String, typ: NameableTypeInfo },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectContainer<Value> {
    value: Value,
    effects: Vec<Effect>,
}

impl<Value> WithEffects for EffectContainer<Value> {
    type Value = Value;
    type Effect = Effect;

    fn new(value: Self::Value, effects: Vec<Self::Effect>) -> Self {
        Self { value, effects }
    }

    fn into_value_and_effects(self) -> (Self::Value, Box<dyn Iterator<Item = Self::Effect>>) {
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
        let (vals, effects): (Coll, Vec<Box<dyn Iterator<Item = Effect>>>) = iter
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
        let (vals, effects): (HashMap<K, V>, Vec<Box<dyn Iterator<Item = Effect>>>) = iter
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Interface {
    pub indexer: Option<Indexer>,
    pub extends: Vec<TypeRef>,
    pub fields: HashMap<String, TypeRef>,
}

macro_rules! combine_effects {
    (@cont $effects:ident ; $item:expr) => {
        EffectContainer::compose_from(
            $item,
            $effects.into_boxed_slice(),
        )
    };
    (@cont $effects:ident $param:ident $(,)? $($params:ident),* ; $item:expr) => {
        {
            let $param: EffectContainer<_> = $param;
            let ($param, these_effects) = $param.into_value_and_effects();
            $effects.extend(these_effects);
            combine_effects!(@cont $effects $($params),* ; $item)
        }
    };
    ($($params:ident),+ ; $item:expr) => {
        {
            let mut effects = Vec::new();
            combine_effects!(@cont effects $($params),+ ; $item)
        }
    };
}

impl From<InterfaceIR> for EffectContainer<Interface> {
    fn from(src: InterfaceIR) -> EffectContainer<Interface> {
        let indexer = src.indexer.map(EffectContainer::from).into();
        let extends = src.extends.into_iter().map(EffectContainer::from).collect();
        let fields = src.fields.into_iter().map(|(n, t)| (n, t.into())).collect();

        combine_effects!(
            indexer,
            extends,
            fields;
            Interface {
                indexer,
                extends,
                fields,
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Indexer {
    pub readonly: bool,
    pub value_type: TypeRef,
}

impl From<IndexerIR> for EffectContainer<Indexer> {
    fn from(src: IndexerIR) -> EffectContainer<Indexer> {
        let value_type = (*src.type_info).into();

        combine_effects!(
            value_type;
            Indexer {
                readonly: src.readonly,
                value_type
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeIdent {
    Builtin {
        rust_type_name: String,
    },
    Name {
        file: PathBuf,
        name: String,
    },
    DefaultExport(PathBuf),
    QualifiedName {
        file: PathBuf,
        name_parts: Vec<String>,
    },
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

/// Convert a [`TypeInfoIR`] to a [`TypeRef`], creating a set of [`Effect`]s, directing the
/// creation of named, top-level types for any anonymous types we encounter.
///
/// This conversion is only really valid for a non-top-level [TypeInfoIR] because we assume that
/// anything we find is un-named.
impl From<TypeInfoIR> for EffectContainer<TypeRef> {
    fn from(src: TypeInfoIR) -> EffectContainer<TypeRef> {
        match src {
            // TODO
            _ => EffectContainer::new(
                TypeRef {
                    referent: TypeIdent::Builtin {
                        rust_type_name: "String".to_string(),
                    },
                    type_params: Default::default(),
                },
                Default::default(),
            ),
            /*Interface(iface),
            Enum {
                members: Vec<EnumMember>,
            },
            Ref(TypeRef),
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
            Union(Union),
            Intersection(Intersection),
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
            Constructor(Ctor),
            Class(Class),
            Var {
                type_info: Box<TypeInfo>,
            },
            NamespaceImport(NamespaceImport),
            */
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
            referent,
            type_params;
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

macro_rules! type_ref_from_prims {
    ($(,)*) => {};
    ($prim:ident => $rust_type_name:ty, $($rest:tt)*) => {
        impl From<$prim> for EffectContainer<TypeRef> {
            fn from(_: $prim) -> EffectContainer<TypeRef> {
                EffectContainer::new(
                    TypeRef {
                        referent: TypeIdent::Builtin {
                            rust_type_name: String::from("$rust_type_name"),
                        },
                        type_params: Default::default(),
                    },
                    Default::default(),
                )
            }
        }
        type_ref_from_prims!($($rest)*);
    };
}

type_ref_from_prims! {
    PrimitiveAny => JsValue,
    PrimitiveNumber => f64,
    PrimitiveObject => std::collections::HashMap<String, JsValue>,
    PrimitiveBoolean => bool,
    PrimitiveBigInt => u64,
    PrimitiveString => String,
    PrimitiveSymbol => (),
    PrimitiveVoid => (),
    PrimitiveUndefined => (),
    PrimitiveNull => (),
    BuiltinDate => js_sys::Date,
    LitNumber => f64,
    LitBoolean => bool,
    LitString => String,
    BuiltinPromise => js_sys::Promise,
}

impl From<Box<TypeInfoIR>> for EffectContainer<Box<FlattenedTypeInfo>> {
    fn from(src: Box<TypeInfoIR>) -> EffectContainer<Box<FlattenedTypeInfo>> {
        let ti = EffectContainer::from(*src);
        combine_effects!(
            ti;
            Box::new(ti)
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Union {
    pub types: Vec<FlattenedTypeInfo>,
}

impl From<UnionIR> for EffectContainer<Union> {
    fn from(src: UnionIR) -> EffectContainer<Union> {
        let types = src.types.into_iter().map(EffectContainer::from).collect();
        combine_effects!(
            types;
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

impl From<IntersectionIR> for EffectContainer<Intersection> {
    fn from(src: IntersectionIR) -> EffectContainer<Intersection> {
        let types = src.types.into_iter().map(EffectContainer::from).collect();
        combine_effects!(
            types;
            Intersection {
                types,
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Func {
    pub type_params: HashMap<String, TypeRef>,
    pub params: Vec<Param>,
    pub return_type: Box<TypeRef>,
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

        combine_effects!(
            params,
            return_type,
            type_params;
            Func {
                type_params,
                params,
                return_type: Box::new(return_type),
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

impl From<ParamIR> for EffectContainer<Param> {
    fn from(src: ParamIR) -> EffectContainer<Param> {
        let type_info = src.type_info.into();

        combine_effects!(
            type_info;
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

impl From<CtorIR> for EffectContainer<Ctor> {
    fn from(src: CtorIR) -> EffectContainer<Ctor> {
        let params = src.params.into_iter().map(EffectContainer::from).collect();
        combine_effects!(
            params;
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
}

impl From<ClassIR> for EffectContainer<Class> {
    fn from(src: ClassIR) -> EffectContainer<Class> {
        let super_class: EffectContainer<Option<_>> = src.super_class.map(|r| (*r).into()).into();
        let members = src
            .members
            .into_iter()
            .map(|(n, m)| (n, m.into()))
            .collect();
        combine_effects!(
            members,
            super_class;
            Class {
                members,
                super_class,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumMember {
    pub id: String,
    pub value: Option<String>,
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

impl From<EnumIR> for Enum {
    fn from(src: EnumIR) -> Enum {
        Enum {
            members: src.members.into_iter().map(EnumMember::from).collect(),
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
    Alias => Alias,
    EnumIR => Enum,
    EnumMemberIR => EnumMember,
);
