use crate::codegen::ns_path::ToNsPath;
use crate::codegen::resolve_target_type::ResolveTargetType;
use crate::fs::Fs;
use crate::identifier::{
    make_identifier, to_camel_case_ident, to_ident, to_snake_case_ident, Identifier,
};
use crate::ir::{Builtin, TargetEnrichedTypeInfo, TypeIdent, TypeRef};
use crate::mod_def::ToModPathIter;

pub trait Named {
    /// Returns a tuple of the the js name as a string and the corresponding rust identifier
    fn to_name(&self) -> (&str, Identifier);

    /// Returns a tuple of the js name as a string and the corresponding rust identifier.
    /// The identifier is qualified relative to the provided ns_base.
    ///
    /// For example, if the provided base is in namespace A::B::C
    /// and self is in namespace A::B::D, the returned relative qualified
    /// name would be super::D::<name>.
    fn to_rel_qualified_name<FS: Fs, T: ToModPathIter>(
        &self,
        fs: &FS,
        ns_base: &T,
    ) -> (&str, Identifier);
}

impl Named for Builtin {
    fn to_name(&self) -> (&str, Identifier) {
        match self {
            Builtin::PrimitiveAny => ("JsValue", to_ident("JsValue")),
            Builtin::PrimitiveNumber => ("f64", to_ident("f64")),
            // TODO: make a wrapper in rt to allow objects to be null or undefined
            Builtin::PrimitiveObject => ("JsValue", to_ident("JsValue")),
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
                "std::collections::HashMap<String, JsValue>",
                make_identifier!(std::collections::HashMap<String, JsValue>),
            ),
            Builtin::Optional => ("Option", to_ident("Option")),
            Builtin::Variadic => ("", to_ident("")),
        }
    }

    fn to_rel_qualified_name<FS: Fs, T: ToModPathIter>(
        &self,
        _fs: &FS,
        _ns_base: &T,
    ) -> (&str, Identifier) {
        // builtins are global - no namespacing required
        self.to_name()
    }
}

/// A `CasedTypeIdent` holds a `TypeIdent` along with a `TargetEnrichedTypeInfo` where the
/// `TargetEnrichedTypeInfo` determines the casing that will be applied to the `TypeIdent`.
///
/// For example, functions are snake_case, enums are CamelCase, and classes are CamelCase.
pub struct CasedTypeIdent<'a> {
    pub type_ident: &'a TypeIdent,
    pub type_info: &'a TargetEnrichedTypeInfo,
}

fn name_for_type_ident_and_info<'a>(
    type_ident: &'a TypeIdent,
    type_info: &TargetEnrichedTypeInfo,
    ns: &[Identifier],
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
        TypeIdent::Name { file: _, name } => (name, caser(name).in_namespace_parts(ns)),
        TypeIdent::DefaultExport(_) => {
            ("default", make_identifier!(default).in_namespace_parts(ns))
        }
        TypeIdent::QualifiedName { name_parts, .. } => {
            let n = name_parts.last().expect("bad qualified name");
            (n, caser(n).in_namespace_parts(ns))
        }
        TypeIdent::ExactName(n) => (n, to_ident(n)),
    }
}

impl<'a> Named for CasedTypeIdent<'a> {
    fn to_name(&self) -> (&str, Identifier) {
        name_for_type_ident_and_info(self.type_ident, self.type_info, &[])
    }

    fn to_rel_qualified_name<FS: Fs, T: ToModPathIter>(
        &self,
        fs: &FS,
        ns_base: &T,
    ) -> (&str, Identifier) {
        let ns = self.type_ident.to_ns_path(fs, ns_base);
        name_for_type_ident_and_info(self.type_ident, self.type_info, &ns)
    }
}

/// Given a TypeRef and an identifier for it, determine whether to retain
/// the type_params on the identifier or not
fn retain_target_type_params(type_ref: &TypeRef, id: &Identifier) -> bool {
    // TODO: this is ugly...
    // we keep HashMap<String, JsValue> but maybe should keep other type params?
    !id.type_params.is_empty()
        && matches!(&type_ref.referent, TypeIdent::Builtin(_))
        && !matches!(
            &type_ref.referent,
            TypeIdent::Builtin(Builtin::Fn | Builtin::Map)
        )
}

fn name_for_type_ref<'a>(tr: &'a TypeRef, ns: &[Identifier]) -> (&'a str, Identifier) {
    let target_type = tr.resolve_target_type();
    let (n, mut id) = target_type
        .map(|t| name_for_type_ident_and_info(&tr.referent, &t, &ns))
        .unwrap_or_else(|| {
            let js_name = tr.referent.js_name();
            (js_name, to_camel_case_ident(js_name))
        });
    if !retain_target_type_params(tr, &id) {
        id.type_params = tr.type_params.iter().map(|t| t.to_name().1).collect();
    }
    (n, id.in_namespace_parts(ns))
}

impl Named for TypeRef {
    fn to_name(&self) -> (&str, Identifier) {
        name_for_type_ref(self, &[])
    }

    fn to_rel_qualified_name<FS: Fs, T: ToModPathIter>(
        &self,
        fs: &FS,
        ns_base: &T,
    ) -> (&str, Identifier) {
        let ns = self.to_ns_path(fs, ns_base);
        name_for_type_ref(self, &ns)
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
        if !retain_target_type_params(self, &id) {
            id.type_params.clear();
        }
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
