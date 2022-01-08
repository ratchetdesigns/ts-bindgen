use crate::codegen::resolve_target_type::ResolveTargetType;
use crate::identifier::{
    make_identifier, to_camel_case_ident, to_ident, to_snake_case_ident, Identifier,
};
use crate::ir::{Builtin, TargetEnrichedTypeInfo, TypeIdent, TypeRef};

pub trait Named {
    fn to_name(&self) -> (&str, Identifier);
}

impl Named for Builtin {
    fn to_name(&self) -> (&str, Identifier) {
        match self {
            Builtin::PrimitiveAny => ("JsValue", to_ident("JsValue")),
            Builtin::PrimitiveNumber => ("f64", to_ident("f64")),
            // TODO: make a wrapper in rt to allow objects to be null or undefined
            Builtin::PrimitiveObject => (
                "std::collections::HashMap<String, JsValue>",
                make_identifier!(std::collections::HashMap<String, JsValue>),
            ),
            Builtin::PrimitiveBoolean => ("bool", to_ident("bool")),
            Builtin::PrimitiveBigInt => ("u64", to_ident("u64")),
            Builtin::PrimitiveString => ("String", to_ident("String")),
            Builtin::PrimitiveSymbol => ("js_sys::Symbol", make_identifier!(js_sys::Symbol)),
            // TODO: is this correct?
            Builtin::PrimitiveVoid => ("()", to_ident("()")),
            Builtin::PrimitiveUndefined => (
                "ts_bindgen_rt::Undefined",
                make_identifier!(ts_bindgen_rt::Undefined),
            ),
            Builtin::PrimitiveNull => {
                ("ts_bindgen_rt::Null", make_identifier!(ts_bindgen_rt::Null))
            }
            Builtin::Date => ("js_sys::Date", make_identifier!(js_sys::Date)),
            Builtin::LitNumber => Builtin::PrimitiveNumber.to_name(),
            Builtin::LitBoolean => Builtin::PrimitiveBoolean.to_name(),
            Builtin::LitString => Builtin::PrimitiveString.to_name(),
            Builtin::Promise => ("js_sys::Promise", make_identifier!(js_sys::Promise)),
            Builtin::Array => ("Vec", to_ident("Vec")),
            Builtin::Fn => ("Fn", to_ident("Fn")),
            Builtin::Map => (
                "std::collections::HashMap",
                make_identifier!(std::collections::HashMap),
            ),
            Builtin::Optional => ("Option", to_ident("Option")),
            Builtin::Variadic => ("", to_ident("")),
        }
    }
}

pub struct CasedTypeIdent<'a> {
    pub type_ident: &'a TypeIdent,
    pub type_info: &'a TargetEnrichedTypeInfo,
}

fn name_for_type_ident_and_info<'a>(
    type_ident: &'a TypeIdent,
    type_info: &TargetEnrichedTypeInfo,
) -> (&'a str, Identifier) {
    let target_type = type_info.resolve_target_type();
    let caser = target_type
        .as_ref()
        .map(casing_for_type)
        .unwrap_or(&to_camel_case_ident);
    match type_ident {
        TypeIdent::Builtin(builtin) => builtin.to_name(),
        TypeIdent::GeneratedName { .. } => {
            panic!("expected all generated names to be resolved")
        }
        TypeIdent::LocalName(n) => (n, caser(n)),
        TypeIdent::Name { file: _, name } => (name, caser(name)),
        TypeIdent::DefaultExport(_) => ("default", make_identifier!(default)),
        TypeIdent::QualifiedName { name_parts, .. } => {
            let n = name_parts.last().expect("bad qualified name");
            (n, caser(n))
        }
        TypeIdent::ExactName(n) => (n, to_ident(n)),
    }
}

impl<'a> Named for CasedTypeIdent<'a> {
    fn to_name(&self) -> (&str, Identifier) {
        name_for_type_ident_and_info(self.type_ident, self.type_info)
    }
}

impl Named for TypeRef {
    fn to_name(&self) -> (&str, Identifier) {
        let target_type = self.resolve_target_type();
        let (n, mut id) = target_type
            .map(|t| name_for_type_ident_and_info(&self.referent, &t))
            .unwrap_or_else(|| {
                let js_name = self.referent.js_name();
                (js_name, to_camel_case_ident(js_name))
            });
        id.type_params = self.type_params.iter().map(|t| t.to_name().1).collect();
        (n, id)
    }
}

trait JsName {
    fn js_name(&self) -> &str;
}

impl JsName for Builtin {
    fn js_name(&self) -> &str {
        self.to_name().0
    }
}

impl JsName for TypeIdent {
    fn js_name(&self) -> &str {
        match self {
            TypeIdent::Builtin(builtin) => builtin.js_name(),
            TypeIdent::GeneratedName { .. } => {
                panic!("expected all generated names to be resolved")
            }
            TypeIdent::LocalName(n) => n,
            TypeIdent::Name { file: _, name } => name,
            TypeIdent::DefaultExport(_) => "default",
            TypeIdent::QualifiedName { name_parts, .. } => {
                let n = name_parts.last().expect("bad qualified name");
                n
            }
            TypeIdent::ExactName(n) => n,
        }
    }
}

pub trait SimpleNamed {
    /// Converts self to a simple name, without any type parameters
    fn to_simple_name(&self) -> Identifier;
}

impl SimpleNamed for TypeRef {
    fn to_simple_name(&self) -> Identifier {
        let (_, mut id) = self.to_name();
        id.type_params.clear();
        id
    }
}

fn casing_for_type<T: AsRef<str>>(typ: &TargetEnrichedTypeInfo) -> &dyn Fn(T) -> Identifier {
    match typ {
        TargetEnrichedTypeInfo::Func(_) => &to_snake_case_ident as &dyn Fn(T) -> Identifier,
        TargetEnrichedTypeInfo::Ref(tr)
            if matches!(&tr.referent, TypeIdent::Builtin(Builtin::Fn)) =>
        {
            &to_snake_case_ident as &dyn Fn(T) -> Identifier
        }
        _ => &to_camel_case_ident as &dyn Fn(T) -> Identifier,
    }
}
