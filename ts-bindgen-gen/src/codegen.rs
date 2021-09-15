use crate::flattened_ir::{
    flatten_types, Alias, Ctor, Enum, EnumMember, FlatType, FlattenedTypeInfo, Func, Indexer,
    Interface, Intersection, NamespaceImport, Param, TypeIdent, TypeRef, Union,
};
use crate::ir::{
    PrimitiveAny as PrimitiveAnyIR, Type as TypeIR, TypeIdent as TypeIdentIR,
    TypeInfo as TypeInfoIR, TypeName as TypeNameIR,
};
use heck::{CamelCase, SnakeCase};
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::{identity, From};
use std::fmt::{Display, Formatter};
use std::iter;
use std::path::{Component, Path, PathBuf};
use std::rc::Rc;
use syn::{parse_str as parse_syn_str, Token};
use unicode_xid::UnicodeXID;

#[derive(Debug, Clone, PartialEq)]
struct Identifier(Vec<Ident>);

impl ToTokens for Identifier {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        toks.append_separated(self.0.iter(), <Token![::]>::default());
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let mut idents = self.0.iter();
        if let Some(id) = idents.next() {
            write!(f, "{}", id)?;
        }
        for i in idents {
            write!(f, "::{}", i)?;
        }

        Ok(())
    }
}

impl From<Ident> for Identifier {
    fn from(src: Ident) -> Identifier {
        Identifier(vec![src])
    }
}

#[derive(Debug, Clone)]
struct MutModDef {
    name: Identifier,
    types: Vec<TypeIR>,
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
        types: Vec<TypeIR>,
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
    name: Identifier,
    types: Vec<TypeIR>,
    children: Vec<ModDef>,
}

trait ToModPathIter {
    fn to_mod_path_iter(&self) -> Box<dyn Iterator<Item = Identifier>>;
}

impl ToModPathIter for Path {
    fn to_mod_path_iter(&self) -> Box<dyn Iterator<Item = Identifier>> {
        Box::new(
            self.canonicalize()
                .expect("canonicalize failed")
                .components()
                .filter_map(|c| match c {
                    Component::Normal(s) => Some(s.to_string_lossy()),
                    _ => None,
                })
                .rev()
                .take_while(|p| p != "node_modules")
                .map(|p| p.as_ref().to_string())
                .collect::<Vec<String>>()
                .into_iter()
                .rev()
                .map(|n| to_ns_name(&n)),
        )
    }
}

impl ToModPathIter for TypeIdentIR {
    fn to_mod_path_iter(&self) -> Box<dyn Iterator<Item = Identifier>> {
        if let TypeIdentIR::QualifiedName(names) = &self {
            Box::new(
                (&names[..names.len() - 1])
                    .to_vec()
                    .into_iter()
                    .map(|n| to_snake_case_ident(&n)),
            )
        } else {
            Box::new(vec![].into_iter())
        }
    }
}

impl ToModPathIter for TypeIdent {
    fn to_mod_path_iter(&self) -> Box<dyn Iterator<Item = Identifier>> {
        match self {
            TypeIdent::QualifiedName { file, name_parts } => Box::new(
                file.to_mod_path_iter().chain(
                    (&name_parts[..name_parts.len() - 1])
                        .to_vec()
                        .into_iter()
                        .map(|n| to_snake_case_ident(&n)),
                ),
            ),
            TypeIdent::Name { file, .. } => file.to_mod_path_iter(),
            _ => Box::new((vec![]).into_iter()),
        }
    }
}

impl ToModPathIter for TypeRef {
    fn to_mod_path_iter(&self) -> Box<dyn Iterator<Item = Identifier>> {
        self.referent.to_mod_path_iter()
    }
}

// TODO: maybe don't make "index" namespaces and put their types in the parent
impl From<&HashMap<PathBuf, HashMap<TypeIdentIR, TypeIR>>> for ModDef {
    fn from(types_by_name_by_file: &HashMap<PathBuf, HashMap<TypeIdentIR, TypeIR>>) -> Self {
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
                let mod_path = path.to_mod_path_iter().collect::<Vec<Identifier>>();
                let last_idx = mod_path.len() - 1;

                mod_path
                    .iter()
                    .enumerate()
                    .fold(root.clone(), move |parent, (i, mod_name)| {
                        let mut parent = parent.borrow_mut();
                        let types = if i == last_idx {
                            types_by_name.values().cloned().collect::<Vec<TypeIR>>()
                        } else {
                            Default::default()
                        };
                        parent.add_child_mod(mod_name.clone(), types)
                    });

                types_by_name
                    .iter()
                    .filter_map(|(name, typ)| {
                        if let TypeIdentIR::QualifiedName { .. } = name {
                            Some((name.to_mod_path_iter().collect::<Vec<Identifier>>(), typ))
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

    #[test]
    fn mod_def_from_types_by_name_by_file() -> std::io::Result<()> {
        let mut tbnbf: HashMap<PathBuf, HashMap<TypeIdentIR, TypeIR>> = HashMap::new();
        let b_c = PathBuf::from("/tmp/a/node_modules/b/c");
        std::fs::DirBuilder::new()
            .recursive(true)
            .create(b_c.parent().unwrap())?;
        std::fs::File::create(&b_c)?;

        tbnbf.insert(b_c.clone(), {
            let mut tbn = HashMap::new();
            tbn.insert(
                TypeIdentIR::Name("my_mod".to_string()),
                TypeIR {
                    name: TypeNameIR {
                        file: b_c.clone(),
                        name: TypeIdentIR::Name("my_mod".to_string()),
                    },
                    is_exported: true,
                    info: TypeInfoIR::PrimitiveAny(PrimitiveAnyIR {}),
                },
            );
            tbn
        });

        let mods: ModDef = (&tbnbf).into();
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
                        types: vec![TypeIR {
                            name: TypeNameIR {
                                file: b_c,
                                name: TypeIdentIR::Name("my_mod".to_string()),
                            },
                            is_exported: true,
                            info: TypeInfoIR::PrimitiveAny(PrimitiveAnyIR {})
                        }],
                        children: Default::default(),
                    }]
                }]
            }
        );

        Ok(())
    }
}

fn map_to_ident<T: AsRef<str>, F: Fn(&str) -> String>(s: T, map: F) -> Identifier {
    // TODO: make these paths, not idents

    // make sure we have valid characters
    let mut chars = s.as_ref().chars();
    let first: String = chars
        .by_ref()
        .take(1)
        .map(|first| {
            if UnicodeXID::is_xid_start(first) && first != '_' {
                first.to_string()
            } else {
                "".to_string()
            }
        })
        .collect();

    let rest: String = chars
        .map(|c| {
            if UnicodeXID::is_xid_continue(c) {
                c
            } else {
                '_'
            }
        })
        .collect();

    // now, make sure we have a valid rust identifier (no keyword collissions)
    let reconstructed = first + &rest;
    let mut full_ident = map(&reconstructed);
    while parse_syn_str::<syn::Ident>(&full_ident).is_err() {
        full_ident += "_";
    }

    format_ident!("{}", &full_ident).into()
}

fn to_ident<T: AsRef<str>>(s: T) -> Identifier {
    map_to_ident(s, ToString::to_string)
}

fn to_camel_case_ident<T: AsRef<str>>(s: T) -> Identifier {
    map_to_ident(s, |s| s.to_camel_case())
}

fn to_ns_name<T: AsRef<str>>(ns: T) -> Identifier {
    map_to_ident(
        ns.as_ref()
            .trim_end_matches(".d.ts")
            .trim_end_matches(".ts"),
        |s| s.to_snake_case(),
    )
}

fn to_snake_case_ident<T: AsRef<str>>(s: T) -> Identifier {
    map_to_ident(s, |s| s.to_snake_case())
}

#[cfg(test)]
mod ident_tests {
    use super::*;

    #[test]
    fn snake_case_ident_test() {
        assert_eq!(
            to_snake_case_ident("IsThisSnake_Case").to_string(),
            "is_this_snake_case"
        );

        assert_eq!(
            to_snake_case_ident("2IsThisSnake_Case").to_string(),
            "is_this_snake_case"
        );

        assert_eq!(to_snake_case_ident("fn").to_string(), "fn_");
    }

    #[test]
    fn ns_name_test() {
        assert_eq!(
            to_ns_name("IsThisSnake_Case").to_string(),
            "is_this_snake_case"
        );

        assert_eq!(
            to_ns_name("2IsThisSnake_Case").to_string(),
            "is_this_snake_case"
        );

        assert_eq!(to_ns_name("mod").to_string(), "mod_");
    }

    #[test]
    fn camel_case_ident_test() {
        assert_eq!(
            to_camel_case_ident("thisIsMixedCase").to_string(),
            "ThisIsMixedCase"
        );

        assert_eq!(
            to_camel_case_ident("2is_this_snake_case").to_string(),
            "IsThisSnakeCase"
        );

        assert_eq!(to_camel_case_ident("super").to_string(), "Super");

        assert_eq!(to_camel_case_ident("1super").to_string(), "Super");

        assert_eq!(to_camel_case_ident("a_b_c").to_string(), "ABC");

        assert_eq!(to_camel_case_ident("ab_bc_cd").to_string(), "AbBcCd");
    }
}

impl ToTokens for ModDef {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let mod_name = &self.name;
        let types = &self.types;
        let flat_types: Vec<_> = flatten_types(types.clone()).collect();
        println!("HERE {:?}", &flat_types);
        let children = &self.children;

        // TODO: would be nice to do something like use super::super::... as ts_bindgen_root and be
        // able to refer to it in future use clauses. just need to get the nesting level here
        let our_toks = quote! {
            #[cfg(target_arch = "wasm32")]
            pub mod #mod_name {
                use wasm_bindgen::prelude::*;

                #(#flat_types)*

                #(#children)*
            }
        };

        toks.append_all(our_toks);
    }
}

impl ToTokens for EnumMember {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let id = to_camel_case_ident(&self.id);
        let our_toks = {
            if let Some(value) = &self.value {
                quote! {
                    #id = #value
                }
            } else {
                quote! {
                    #id
                }
            }
        };
        toks.append_all(our_toks);
    }
}

trait ToNsPath<T: ?Sized> {
    // TODO: would love to return a generic ToTokens...
    fn to_ns_path(&self, current_mod: &T) -> TokenStream2;
}

impl<T, U> ToNsPath<T> for U
where
    T: ToModPathIter,
    U: ToModPathIter + ?Sized,
{
    fn to_ns_path(&self, current_mod: &T) -> TokenStream2 {
        let ns_len = current_mod.to_mod_path_iter().count();
        let mut use_path = vec![format_ident!("super").into(); ns_len];
        use_path.extend(self.to_mod_path_iter());
        quote! {
            #(#use_path)::*
        }
    }
}

fn to_unique_ident<T: Fn(&str) -> bool>(mut desired: String, taken: &T) -> Identifier {
    while taken(&desired) {
        desired += "_";
    }

    to_ident(&desired)
}

fn get_recursive_fields(
    Interface {
        extends, fields, ..
    }: &Interface,
) -> HashMap<String, TypeRef> {
    fields
        .iter()
        .map(|(n, t)| (n.clone(), t.clone()))
        // TODO: need to recursively expand base class fields
        /*.chain(
            extends
                .iter()
                .flat_map(|base| get_recursive_fields(base).into_iter()),
        )*/
        .collect()
}

impl ToTokens for Param {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let param_name = to_snake_case_ident(&self.name);
        let typ = &self.type_info;
        let full_type = if self.is_variadic {
            quote! {
                &[#typ]
            }
        } else {
            quote! {
                #typ
            }
        };

        let our_toks = quote! {
            #param_name: #full_type
        };
        toks.extend(our_toks);
    }
}

struct NamedFunc<'a> {
    func: &'a Func,
    js_name: &'a str,
}

impl<'a> ToTokens for NamedFunc<'a> {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let fn_name = to_snake_case_ident(self.js_name);

        let param_toks: Vec<TokenStream2> =
            self.func.params.iter().map(|p| quote! { #p }).collect();

        let return_type = &self.func.return_type;

        let our_toks = quote! {
            fn #fn_name(#(#param_toks),*) -> #return_type;
        };

        toks.extend(our_toks);
    }
}

trait Named {
    fn to_name(&self) -> (&str, Identifier);
}

impl Named for TypeIdent {
    fn to_name(&self) -> (&str, Identifier) {
        match self {
            TypeIdent::Builtin { rust_type_name } => (rust_type_name, to_ident(rust_type_name)),
            TypeIdent::GeneratedName { .. } => {
                panic!("expected all generated names to be resolved")
            }
            TypeIdent::LocalName(n) => (n, to_camel_case_ident(n)),
            TypeIdent::Name { file, name } => (name, to_camel_case_ident(name)),
            TypeIdent::DefaultExport(_) => panic!("didn't expect default exports"),
            TypeIdent::QualifiedName { name_parts, .. } => {
                let n = name_parts.last().expect("bad qualified name");
                (n, to_camel_case_ident(n))
            }
        }
    }
}

impl ToTokens for FlatType {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let (js_name, name) = self.name.to_name();

        let our_toks = match &self.info {
            FlattenedTypeInfo::Interface(iface) => {
                let Interface { indexer, .. } = iface;
                let extended_fields = &iface.fields; //TODO: get_recursive_fields(iface);

                let mut field_toks = extended_fields
                    .iter()
                    .map(|(js_field_name, typ)| {
                        let field_name = to_snake_case_ident(js_field_name);
                        quote! {
                            #[serde(rename = #js_field_name)]
                            pub #field_name: #typ
                        }
                    })
                    .collect::<Vec<TokenStream2>>();

                if let Some(Indexer {
                    readonly: _,
                    value_type,
                }) = &indexer
                {
                    let extra_fields_name = to_unique_ident("extra_fields".to_string(), &|x| {
                        extended_fields.contains_key(x)
                    });

                    field_toks.push(quote! {
                        #[serde(flatten)]
                        pub #extra_fields_name: std::collections::HashMap<String, #value_type>
                    });
                }

                quote! {
                    #[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
                    pub struct #name {
                        #(#field_toks),*
                    }
                }
            }
            FlattenedTypeInfo::Enum(Enum { members }) => {
                quote! {
                    #[wasm_bindgen]
                    #[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
                    pub enum #name {
                        #(#members),*
                    }
                }
            }
            FlattenedTypeInfo::Alias(Alias { target }) => {
                let use_path = target.to_ns_path(&self.name);

                quote! {
                    use #use_path as #name;
                }
            }
            //FlattenedTypeInfo::Ref(_) => panic!("ref isn't a top-level type"),
            //FlattenedTypeInfo::Array { .. } => panic!("Array isn't a top-level type"),
            //FlattenedTypeInfo::Optional { .. } => panic!("Optional isn't a top-level type"),
            FlattenedTypeInfo::Union(Union { types }) => {
                let members = types.iter().map(|t| {
                    let t_str = quote! { #t }
                        .to_string()
                        .replace("<", "Of")
                        .replace(">", "")
                        .replace("&", "")
                        .replace("[", "")
                        .replace("]", "");
                    let case = to_camel_case_ident(format!("{}Case", t_str));

                    quote! {
                        #case(#t)
                    }
                });

                quote! {
                    #[wasm_bindgen]
                    #[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
                    pub enum #name {
                        #(#members),*
                    }
                }
            }
            /*TypeInfo::Intersection {
                types: Vec<TypeInfo>,
            },
            TypeInfo::Mapped {
                value_type: Box<TypeInfo>,
            },
            TypeInfo::LitNumber {
                n: f64,
            },
            TypeInfo::LitString {
                s: String,
            },
            TypeInfo::LitBoolean {
                b: bool,
            },
            TypeInfo::Func(func) => {
                let js_name = self.name.to_name();
                let attrs = {
                    let mut attrs = vec![quote! { js_name = #js_name }];
                    if func.is_variadic() {
                        attrs.push(quote! { variadic });
                    }
                    attrs
                };
                let func = NamedFunc { js_name, func };

                quote! {
                    #[wasm_bindgen]
                    extern "C" {
                        #[wasm_bindgen(#(#attrs),*)]
                        #func
                    }
                }
            }
            TypeInfo::Constructor {
                params: Vec<Param>,
                return_type: Box<TypeInfo>,
            },
            TypeInfo::Class(Class {
                super_class,
                members,
            }) => {
                let mut attrs = vec![quote! { js_name = #js_name }];
                if let Some(TypeRef {
                    referent,
                    type_params,
                }) = super_class.as_ref().map(|sc| &**sc)
                {
                    let super_name = to_camel_case_ident(referent.to_name());

                    attrs.push(quote! {
                        extends = #super_name
                    });
                }

                let members: Vec<TokenStream2> = members
                    .iter()
                    .map(|(member_js_name, member)| match member {
                        Member::Constructor(ctor) => {
                            let param_toks: Vec<TokenStream2> =
                                ctor.params.iter().map(|p| quote! { #p }).collect();

                            quote! {
                                #[wasm_bindgen(constructor)]
                                fn new(#(#param_toks),*) -> #name;
                            }
                        }
                        Member::Method(func) => {
                            let f = NamedFunc {
                                js_name: member_js_name,
                                func,
                            };
                            let mut attrs = vec![
                                quote! {js_name = #member_js_name},
                                quote! {method},
                                quote! {js_class = #js_name},
                            ];
                            if func.is_variadic() {
                                attrs.push(quote! { variadic });
                            }

                            quote! {
                                #[wasm_bindgen(#(#attrs),*)]
                                #f
                            }
                        }
                        Member::Property(typ) => {
                            let member_name = to_snake_case_ident(member_js_name);
                            let setter_name = format_ident!("set_{}", member_name);
                            quote! {
                                #[wasm_bindgen(method, structural, getter = #member_js_name)]
                                fn #member_name(this: &#name) -> #typ;

                                #[wasm_bindgen(method, structural, setter = #member_js_name)]
                                fn #setter_name(this: &#name, value: #typ);
                            }
                        }
                    })
                    .collect();

                quote! {
                    #[wasm_bindgen]
                    extern "C" {
                        #[wasm_bindgen(#(#attrs),*)]
                        type #name;

                        #(#members)*
                    }
                }
            }
            TypeInfo::Var {
                type_info: Box<TypeInfo>,
            },*/
            FlattenedTypeInfo::NamespaceImport(NamespaceImport::All { src }) => {
                let ns = src.as_path().to_ns_path(&self.name);
                let vis = if self.is_exported {
                    let vis = format_ident!("pub");
                    quote! { #vis }
                } else {
                    quote! {}
                };
                let name = to_snake_case_ident(js_name);

                quote! {
                    #vis use #ns as #name;
                }
            }
            FlattenedTypeInfo::NamespaceImport(NamespaceImport::Default { src }) => {
                let ns = src.as_path().to_ns_path(&self.name);
                let vis = if self.is_exported {
                    let vis = format_ident!("pub");
                    quote! { #vis }
                } else {
                    quote! {}
                };
                let default_export = to_ident("default");

                quote! {
                    #vis use #ns::#default_export as #name;
                }
            }
            FlattenedTypeInfo::NamespaceImport(NamespaceImport::Named {
                src,
                name: item_name,
            }) => {
                let ns = src.as_path().to_ns_path(&self.name);
                let vis = if self.is_exported {
                    let vis = format_ident!("pub");
                    quote! { #vis }
                } else {
                    quote! {}
                };
                let item_name = to_camel_case_ident(item_name);

                quote! {
                    #vis use #ns::#item_name as #name;
                }
            }
            _ => {
                quote! {}
            }
        };

        toks.append_all(our_toks);
    }
}

impl ToTokens for FlattenedTypeInfo {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let our_toks = match &self {
            FlattenedTypeInfo::Interface(_) => {
                panic!("interface in type info");
            }
            FlattenedTypeInfo::Enum(_) => {
                panic!("enum in type info");
            }
            FlattenedTypeInfo::Ref(TypeRef {
                referent,
                type_params: _,
            }) => {
                let (_, local_name) = referent.to_name();

                quote! {
                    #local_name
                }
            }
            FlattenedTypeInfo::Alias(Alias { target }) => {
                // TODO: need to get the local name for the alias (stored on the Type right now)
                let (_, local_name) = target.to_name();

                quote! {
                    #local_name
                }
            }
            FlattenedTypeInfo::Array { item_type } => {
                quote! {
                    Vec<#item_type>
                }
            }
            FlattenedTypeInfo::Optional { item_type } => {
                quote! {
                    Option<#item_type>
                }
            }
            FlattenedTypeInfo::Union(Union { types: _ }) => {
                // TODO
                quote! {}
            }
            FlattenedTypeInfo::Intersection(Intersection { types: _ }) => {
                // TODO
                quote! {}
            }
            FlattenedTypeInfo::Mapped { value_type } => {
                quote! {
                    std::collections::HashMap<String, #value_type>
                }
            }
            FlattenedTypeInfo::Func(Func {
                params,
                type_params: _,
                return_type,
            }) => {
                let param_toks: Vec<TokenStream2> = params
                    .iter()
                    .map(|p| {
                        let typ = &p.type_info;

                        if p.is_variadic {
                            quote! {
                                &[#typ]
                            }
                        } else {
                            quote! {
                                #typ
                            }
                        }
                    })
                    .collect();

                quote! {
                    &Closure<dyn Fn(#(#param_toks),*) -> #return_type>
                }
            }
            /*
            FlattenedTypeInfo::Constructor {
                params: Vec<Param>,
                return_type: Box<TypeInfo>,
            },
            FlattenedTypeInfo::Class(Class {
                members: HashMap<String, Member>,
            }),
            FlattenedTypeInfo::Var {
                type_info: Box<TypeInfo>,
            },
            FlattenedTypeInfo::GenericType {
                name: String,
                constraint: Box<TypeInfo>,
            },*/
            FlattenedTypeInfo::NamespaceImport(_) => panic!("namespace import in type info"),
            _ => {
                quote! {}
            }
        };

        toks.append_all(our_toks);
    }
}

impl ToTokens for TypeRef {
    fn to_tokens(&self, toks: &mut TokenStream2) {
        let our_toks = {
            let (_, name) = self.referent.to_name();
            if self.type_params.is_empty() {
                quote! { #name }
            } else {
                let type_params = self.type_params.iter().map(|p| quote! { #p });
                quote! { #name<#(#type_params),*> }
            }
        };

        toks.append_all(our_toks);
    }
}
