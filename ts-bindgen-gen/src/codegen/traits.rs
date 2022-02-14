use crate::codegen::funcs::{AccessType, FnPrototypeExt, PropertyAccessor};
use crate::codegen::generics::{render_type_params, render_type_params_with_constraints};
use crate::codegen::generics::{ResolveGeneric, TypeEnvImplying};
use crate::codegen::named::Named;
use crate::codegen::resolve_target_type::ResolveTargetType;
use crate::identifier::{to_snake_case_ident, Identifier};
use crate::ir::{
    Class, Context, Func, Interface, Intersection, Member, TargetEnrichedTypeInfo, TypeIdent,
    TypeParamConfig, TypeRef,
};
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use std::collections::{HashMap, HashSet};
use std::iter;

#[derive(Debug, Clone)]
pub enum TraitMember {
    Method {
        name: Identifier,
        method: Func,
    },
    Getter {
        name: Identifier,
        prop: PropertyAccessor,
    },
    Setter {
        name: Identifier,
        prop: PropertyAccessor,
    },
}

impl TraitMember {
    fn name(&self) -> &Identifier {
        match self {
            TraitMember::Method { name, .. } => name,
            TraitMember::Getter { name, .. } => name,
            TraitMember::Setter { name, .. } => name,
        }
    }

    fn is_fallible(&self) -> bool {
        match self {
            TraitMember::Method { .. } => true,
            TraitMember::Getter { .. } => true,
            TraitMember::Setter { .. } => true,
        }
    }
}

pub fn to_type_ref(
    referent: &TypeIdent,
    type_params: &[(String, TypeParamConfig)],
    ctx: &Context,
) -> TypeRef {
    TypeRef {
        referent: referent.clone(),
        type_params: type_params
            .iter()
            .map(|(n, _)| TypeRef {
                referent: TypeIdent::LocalName(n.clone()),
                type_params: Default::default(),
                context: ctx.clone(),
            })
            .collect(),
        context: ctx.clone(),
    }
}

pub fn render_trait_defn<T: std::fmt::Debug>(
    name: &Identifier,
    item_name: &TypeIdent,
    type_params: &[(String, TypeParamConfig)],
    is_public: bool,
    item: &T,
    ctx: &Context,
) -> TokenStream2
where
    T: Traitable,
{
    let tps = render_type_params(type_params);
    let full_name = quote! {
        #name #tps
    };
    let tps_with_constraints = render_type_params_with_constraints(
        type_params,
        &[
            quote! { std::clone::Clone },
            quote! { serde::ser::Serialize },
            quote! { serde::de::DeserializeOwned },
        ],
    );
    let class_name = || item_name.clone();
    let item_ref = to_type_ref(item_name, type_params, ctx);
    let member_to_trait_member = |type_env: &HashMap<String, TypeRef>, (n, m): (String, Member)| {
        let name = to_snake_case_ident(&n);
        match m {
            Member::Constructor(_) => Default::default(),
            Member::Method(f) => f
                .overloads
                .iter()
                // TODO: name needs to be modified based on method
                .map(|o| TraitMember::Method {
                    name: name.clone(),
                    method: o.clone(),
                })
                .collect(),
            Member::Property(t) => {
                let getter = PropertyAccessor {
                    property_name: name.clone(),
                    typ: t.resolve_generic_in_env(type_env).into_owned(),
                    class_name: class_name(),
                    access_type: AccessType::Getter,
                };
                let setter = PropertyAccessor {
                    property_name: name.clone(),
                    typ: t.resolve_generic_in_env(type_env).into_owned(),
                    class_name: class_name(),
                    access_type: AccessType::Setter,
                };
                vec![
                    TraitMember::Setter {
                        name: name.prefix_name("set_"),
                        prop: setter,
                    },
                    TraitMember::Getter { name, prop: getter },
                ]
            }
        }
    };
    let super_decl = if item.has_super_traits() {
        let supers = item.super_traits().map(|s| s.trait_name());
        quote! {
            : #(#supers)+*
        }
    } else {
        Default::default()
    };
    let super_impls = {
        // TODO: would be nice to mark Identifiers with the type of identifier they are
        // e.g. mark anything coming out of a TraitName::trait_name as a trait
        // identifier
        let make_impl = |i: Super| {
            let tr = &i.item;
            let trait_name = tr.trait_name();
            let supers: Vec<_> = tr
                .super_traits()
                .map(|s| s.resolve_generic_in_env(&tr.type_env()).trait_name())
                .collect();
            let preds = if supers.is_empty() {
                quote! {}
            } else {
                quote! {
                    where #full_name: #(#supers)+*
                }
            };
            let method_impls = tr
                .methods()
                .flat_map(|(n, m)| member_to_trait_member(&tr.type_env(), (n, m)).into_iter())
                .map(|trait_member| {
                    let proto = trait_member.exposed_to_rust_fn_decl(
                        trait_member.name(),
                        trait_member.is_fallible(),
                        Some(ctx),
                    );
                    let imp = item.wrap_invocation(&i.implementor, &trait_member, ctx);
                    quote! {
                        #proto {
                            #imp
                        }
                    }
                });
            quote! {
                impl #tps_with_constraints #trait_name for #full_name #preds {
                    #(#method_impls)*
                }
            }
        };
        let super_impls = item
            .recursive_super_traits(item_ref.clone(), &item_ref.type_env())
            .chain(Box::new(iter::once(Super {
                item: item_ref.clone(),
                implementor: item_ref,
            })))
            .map(&make_impl);
        quote! {
            #(#super_impls)*
        }
    };

    let method_decls = item
        .methods()
        .flat_map(|nm| member_to_trait_member(&Default::default(), nm))
        .map(|f| f.exposed_to_rust_fn_decl(f.name(), f.is_fallible(), Some(ctx)))
        .map(|t| {
            quote! {
                #t;
            }
        });

    let trait_name = name.trait_name();
    let vis = if is_public {
        let vis = format_ident!("pub");
        quote! { #vis }
    } else {
        quote! {}
    };

    quote! {
        #[allow(non_camel_case_types)]
        #vis trait #trait_name #tps #super_decl {
            #(#method_decls)*
        }

        #super_impls
    }
}

/// Represents a superclass or implemented interface.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Super {
    /// `item` is the superclass or implemented interface.
    pub item: TypeRef,
    /// `implementor` is set to the type that contains the actual implementation.
    /// For example, a typescript interface, `I`, embeds all members from extended
    /// interfaces directly within it so implementor will always refer to
    /// `I`.
    /// However, a typescript class, `C`, which extends a class `Base`, will not
    /// contain `Base`'s members so when `Super::item` is set to `Base` or an
    /// interface implemented by `Base`, `Super::implementor` will be set
    /// to `Base`.
    pub implementor: TypeRef,
}

type BoxedSuperIter<'a> = Box<dyn Iterator<Item = Super> + 'a>;
type BoxedTypeRefIter<'a> = Box<dyn Iterator<Item = TypeRef> + 'a>;
type BoxedMemberIter<'a> = Box<dyn Iterator<Item = (String, Member)> + 'a>;

pub trait IsTraitable {
    fn is_traitable(&self) -> bool;
}

impl IsTraitable for TargetEnrichedTypeInfo {
    fn is_traitable(&self) -> bool {
        matches!(
            self,
            TargetEnrichedTypeInfo::Interface(_) | TargetEnrichedTypeInfo::Class(_)
        )
    }
}

pub trait Traitable {
    fn has_super_traits(&self) -> bool;

    fn super_traits(&self) -> BoxedTypeRefIter<'_>;

    fn methods(&self) -> BoxedMemberIter<'_>;

    fn contains_implementation(&self) -> bool;

    fn wrap_invocation(
        &self,
        member_defn_source: &TypeRef,
        trait_member: &TraitMember,
        in_context: &Context,
    ) -> TokenStream2;

    fn recursive_super_traits(
        &self,
        implementor: TypeRef,
        type_env: &HashMap<String, TypeRef>,
    ) -> BoxedSuperIter<'_> {
        Box::new(self.super_traits().fold(
            Box::new(iter::empty()) as BoxedSuperIter<'_>,
            |cur, s| {
                let s = s.resolve_generic_in_env(type_env).into_owned();
                let implementor = if s.contains_implementation() {
                    s.clone()
                } else {
                    implementor.clone()
                };
                let supers = s
                    .resolve_target_type()
                    .map(|t| {
                        Box::new(
                            t.recursive_super_traits(implementor.clone(), &s.type_env())
                                .collect::<Vec<_>>()
                                .into_iter(),
                        ) as BoxedSuperIter<'_>
                    })
                    .unwrap_or_else(|| Box::new(iter::empty()) as BoxedSuperIter<'_>);
                let s_iter = Box::new(iter::once(Super {
                    item: s.clone(),
                    implementor,
                })) as BoxedSuperIter<'_>;

                Box::new(cur.chain(s_iter).chain(supers)) as BoxedSuperIter<'_>
            },
        )) as BoxedSuperIter<'_>
    }
}

impl Traitable for Interface {
    fn has_super_traits(&self) -> bool {
        !self.extends.is_empty()
    }

    fn super_traits(&self) -> BoxedTypeRefIter<'_> {
        Box::new(self.extends.iter().cloned()) as BoxedTypeRefIter<'_>
    }

    fn methods(&self) -> BoxedMemberIter<'_> {
        Box::new(
            self.fields
                .iter()
                .map(|(n, t)| (n.clone(), Member::Property(t.clone()))),
        )
    }

    fn contains_implementation(&self) -> bool {
        // interfaces in an inheritance tree do not contain implementation,
        // their implementation is denormalized onto the root item
        false
    }

    fn wrap_invocation(
        &self,
        member_defn_source: &TypeRef,
        trait_member: &TraitMember,
        in_context: &Context,
    ) -> TokenStream2 {
        let name = trait_member.name();
        let cn = member_defn_source
            .to_rel_qualified_name(in_context.fs.as_ref(), &in_context.base_namespace)
            .1;
        let fq_name = &name.in_namespace(&cn);
        let slf = quote! { self };
        match trait_member {
            TraitMember::Method { method, .. } => {
                method.fully_qualified_invoke_with_name(fq_name, Some(slf))
            }
            TraitMember::Getter { prop, .. } => {
                let property_name = &prop.property_name;
                quote! {
                    Ok(self.#property_name.clone())
                }
            }
            TraitMember::Setter { prop, .. } => {
                let property_name = &prop.property_name;
                quote! {
                    self.#property_name = value;
                    Ok(())
                }
            }
        }
    }
}

impl Traitable for Class {
    fn has_super_traits(&self) -> bool {
        self.super_class.is_some() || !self.implements.is_empty()
    }

    fn super_traits(&self) -> BoxedTypeRefIter<'_> {
        let super_class = self
            .super_class
            .as_ref()
            .map(|s| Box::new(iter::once(s).cloned()) as BoxedTypeRefIter<'_>)
            .unwrap_or_else(|| Box::new(iter::empty()) as BoxedTypeRefIter<'_>);
        let implements = self.implements.iter().cloned();

        Box::new(super_class.chain(implements))
    }

    fn methods(&self) -> BoxedMemberIter<'_> {
        Box::new(self.members.iter().map(|(n, m)| (n.clone(), m.clone())))
    }

    fn contains_implementation(&self) -> bool {
        true
    }

    fn wrap_invocation(
        &self,
        member_defn_source: &TypeRef,
        trait_member: &TraitMember,
        in_context: &Context,
    ) -> TokenStream2 {
        let cn = member_defn_source
            .to_rel_qualified_name(in_context.fs.as_ref(), &in_context.base_namespace)
            .1;
        let name = trait_member.name();
        let name = &name.in_namespace(&cn);
        let slf = quote! { self };
        let target = quote! { &target };
        let mut_target = quote! { &mut target };
        let is_direct_member = member_defn_source
            .resolve_target_type()
            .map(|t| match t {
                TargetEnrichedTypeInfo::Class(c) => c == *self,
                _ => false,
            })
            .unwrap_or(false);
        let tgt_mut = if matches!(trait_member, TraitMember::Setter { .. }) {
            quote! { mut }
        } else {
            quote! {}
        };
        let conv = if is_direct_member {
            quote! {
                let #tgt_mut target = #slf;
            }
        } else {
            quote! {
                let #tgt_mut target: #member_defn_source = #slf.into();
            }
        };
        match trait_member {
            TraitMember::Method { method, .. } => {
                let inv = method.fully_qualified_invoke_with_name(name, Some(&target));
                quote! {
                    #conv
                    #inv
                }
            }
            TraitMember::Getter { prop, .. } => {
                let f = prop.getter_fn();

                let inv = f.fully_qualified_invoke_with_name(name, Some(&target));
                quote! {
                    #conv
                    #inv
                }
            }
            TraitMember::Setter { prop, .. } => {
                let f = prop.setter_fn();
                let inv = f.fully_qualified_invoke_with_name(name, Some(&mut_target));
                quote! {
                    #conv
                    #inv
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::fs::MemFs;
    use crate::ir::to_final_ir;
    use crate::parse::{ArcFs, TsTypes};
    use std::path::Path;
    use std::sync::Arc;

    #[test]
    fn test_recursive_traits() {
        let ts_code = r#"
            class First {}
            class Second extends First {}
            class Third extends Second {}
        "#;
        let test_path: &Path = Path::new("/test.d.ts");
        let mut fs: MemFs = Default::default();
        fs.set_cwd(Path::new("/"));
        fs.add_file_at(test_path, ts_code.to_string());

        let arc_fs = Arc::new(fs) as ArcFs;

        let tbnbf = TsTypes::parse(arc_fs.clone(), &test_path.to_string_lossy()).unwrap();

        let tbnbf = to_final_ir(tbnbf, arc_fs);
        let tbnbf = tbnbf.borrow();
        let types = tbnbf.get(test_path).unwrap();

        let name = TypeIdent::Name {
            name: "Third".to_string(),
            file: test_path.to_path_buf(),
        };
        let third = types.get(&name).unwrap();

        if let TargetEnrichedTypeInfo::Class(c) = &third.info {
            let implementor = TypeRef {
                referent: name,
                type_params: Default::default(),
                context: c.context.clone(),
            };
            let supers: Vec<_> = c
                .recursive_super_traits(implementor, &Default::default())
                .collect();
            assert_eq!(supers.len(), 2);
            assert!(supers.iter().any(|s| {
                if let TypeIdent::Name { file: _, name } = &s.item.referent {
                    name == "First"
                } else {
                    false
                }
            }));
            assert!(supers.iter().any(|s| {
                if let TypeIdent::Name { file: _, name } = &s.item.referent {
                    name == "Second"
                } else {
                    false
                }
            }));
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_recursive_traits_with_namespaces() {
        let ts_code = r#"
            declare namespace n {
                class First {}
                namespace First {
                    class Second extends First {}
                    class Third extends Second {}
                }
            }
        "#;
        let test_path: &Path = Path::new("/test.d.ts");
        let mut fs: MemFs = Default::default();
        fs.set_cwd(Path::new("/"));
        fs.add_file_at(test_path, ts_code.to_string());

        let arc_fs = Arc::new(fs) as ArcFs;

        let tbnbf = TsTypes::parse(arc_fs.clone(), &test_path.to_string_lossy()).unwrap();

        let tbnbf = to_final_ir(tbnbf, arc_fs);
        let tbnbf = tbnbf.borrow();
        let types = tbnbf.get(test_path).unwrap();

        let name = TypeIdent::QualifiedName {
            name_parts: vec!["n".to_string(), "First".to_string(), "Third".to_string()],
            file: test_path.to_path_buf(),
        };
        let third = types.get(&name).unwrap();

        if let TargetEnrichedTypeInfo::Class(c) = &third.info {
            let implementor = TypeRef {
                referent: name,
                type_params: Default::default(),
                context: c.context.clone(),
            };
            let supers: Vec<_> = c
                .recursive_super_traits(implementor, &Default::default())
                .collect();
            assert_eq!(supers.len(), 2);
            assert!(supers.iter().any(|s| {
                if let TypeIdent::QualifiedName {
                    file: _,
                    name_parts,
                } = &s.item.referent
                {
                    name_parts.last() == Some(&"First".to_string())
                } else {
                    false
                }
            }));
            assert!(supers.iter().any(|s| {
                if let TypeIdent::QualifiedName {
                    file: _,
                    name_parts,
                } = &s.item.referent
                {
                    name_parts.last() == Some(&"Second".to_string())
                } else {
                    false
                }
            }));
        } else {
            assert!(false);
        }
    }
}

macro_rules! delegate_traitable_for_type_info {
    ($self:ident, $x:ident, $invocation:expr, $default:expr $(,)?) => {
        match $self {
            TargetEnrichedTypeInfo::Class($x) => $invocation,
            TargetEnrichedTypeInfo::Interface($x) => $invocation,
            TargetEnrichedTypeInfo::Intersection($x) => $invocation,
            _ => $default,
        }
    };
}

impl Traitable for TargetEnrichedTypeInfo {
    fn has_super_traits(&self) -> bool {
        delegate_traitable_for_type_info!(self, x, x.has_super_traits(), false)
    }

    fn super_traits(&self) -> BoxedTypeRefIter<'_> {
        delegate_traitable_for_type_info!(
            self,
            x,
            x.super_traits(),
            Box::new(iter::empty()) as BoxedTypeRefIter<'_>,
        )
    }

    fn methods(&self) -> BoxedMemberIter<'_> {
        delegate_traitable_for_type_info!(
            self,
            x,
            x.methods(),
            Box::new(iter::empty()) as BoxedMemberIter<'_>,
        )
    }

    fn contains_implementation(&self) -> bool {
        delegate_traitable_for_type_info!(self, x, x.contains_implementation(), false,)
    }

    fn wrap_invocation(
        &self,
        member_defn_source: &TypeRef,
        trait_member: &TraitMember,
        in_context: &Context,
    ) -> TokenStream2 {
        let empty = quote! {};
        delegate_traitable_for_type_info!(
            self,
            x,
            x.wrap_invocation(member_defn_source, trait_member, in_context),
            empty,
        )
    }
}

impl Traitable for TypeRef {
    fn has_super_traits(&self) -> bool {
        self.resolve_target_type()
            .map(|t| t.has_super_traits())
            .unwrap_or(false)
    }

    fn super_traits(&self) -> BoxedTypeRefIter<'_> {
        self.resolve_target_type()
            .map(|t| {
                Box::new(t.super_traits().collect::<Vec<_>>().into_iter()) as BoxedTypeRefIter<'_>
            })
            .unwrap_or_else(|| Box::new(iter::empty()) as BoxedTypeRefIter<'_>)
    }

    fn methods(&self) -> BoxedMemberIter<'_> {
        self.resolve_target_type()
            .map(|t| Box::new(t.methods().collect::<Vec<_>>().into_iter()) as BoxedMemberIter<'_>)
            .unwrap_or_else(|| Box::new(iter::empty()) as BoxedMemberIter<'_>)
    }

    fn contains_implementation(&self) -> bool {
        self.resolve_target_type()
            .map(|t| t.contains_implementation())
            .unwrap_or(false)
    }

    fn wrap_invocation(
        &self,
        member_defn_source: &TypeRef,
        trait_member: &TraitMember,
        in_context: &Context,
    ) -> TokenStream2 {
        self.resolve_target_type()
            .map(|t| t.wrap_invocation(member_defn_source, trait_member, in_context))
            .unwrap_or_else(|| quote! {})
    }
}

impl Traitable for Intersection {
    // TODO: we should support classes in intersections too

    fn has_super_traits(&self) -> bool {
        self.types
            .iter()
            .filter_map(|t| t.resolve_target_type())
            .any(|t| t.has_super_traits())
    }

    fn super_traits(&self) -> BoxedTypeRefIter<'_> {
        Box::new(
            self.types
                .iter()
                .filter_map(|t| t.resolve_target_type())
                .flat_map(|t| t.super_traits().collect::<Vec<_>>().into_iter())
                .collect::<HashSet<_>>()
                .into_iter(),
        ) as BoxedTypeRefIter<'_>
    }

    fn methods(&self) -> BoxedMemberIter<'_> {
        Box::new(
            self.types
                .iter()
                .filter_map(|t| t.resolve_target_type())
                .flat_map(|t| t.methods().collect::<Vec<_>>().into_iter()),
        ) as BoxedMemberIter<'_>
    }

    fn contains_implementation(&self) -> bool {
        false
    }

    fn wrap_invocation(
        &self,
        member_defn_source: &TypeRef,
        trait_member: &TraitMember,
        in_context: &Context,
    ) -> TokenStream2 {
        let desired_name = trait_member.name();
        let is_desired_name = |n: &str| {
            &to_snake_case_ident(n) == desired_name
                || &to_snake_case_ident(format!("set_{}", n)) == desired_name
        };

        self.types
            .iter()
            .filter_map(|t| t.resolve_target_type())
            .find_map(|t| {
                t.methods().find_map(|(name, _)| {
                    if is_desired_name(&name) {
                        Some(t.wrap_invocation(member_defn_source, trait_member, in_context))
                    } else {
                        None
                    }
                })
            })
            .unwrap_or_default()
    }
}

pub trait TraitName {
    fn trait_name(&self) -> Identifier;
}

impl TraitName for Identifier {
    fn trait_name(&self) -> Identifier {
        self.suffix_name("_Trait")
    }
}

impl TraitName for TypeRef {
    fn trait_name(&self) -> Identifier {
        self.to_name().1.trait_name()
    }
}
