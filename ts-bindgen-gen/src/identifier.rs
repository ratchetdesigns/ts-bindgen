use heck::{CamelCase, SnakeCase};
use proc_macro2::Ident;
use quote::format_ident;
use std::fmt::{Display, Formatter};
use std::iter;
use syn::parse_str as parse_syn_str;
use unicode_xid::UnicodeXID;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Identifier {
    pub type_parts: Vec<Ident>,
    pub type_params: Vec<Identifier>,
}

macro_rules! make_identifier {
    ($type_part:ident) => {
        crate::identifier::Identifier::new_ident(quote::format_ident!(stringify!($type_part)))
    };
    ($($type_parts:ident)::*) => {
        crate::identifier::Identifier {
            type_parts: vec![$(quote::format_ident!(stringify!($type_parts))),*],
            type_params: Default::default(),
        }
    };
    ($($type_parts:ident)::*<$($type_params:ident),+>) => {
        crate::identifier::Identifier {
            type_parts: vec![$(quote::format_ident!(stringify!($type_parts))),*],
            type_params: vec![$(make_identifier!($type_params)),*],
        }
    };
}

pub(crate) use make_identifier;

impl Identifier {
    pub fn new_ident(id: Ident) -> Identifier {
        Identifier {
            type_parts: vec![id],
            type_params: Default::default(),
        }
    }

    pub fn in_namespace(&self, ns: &Identifier) -> Identifier {
        Identifier {
            type_parts: ns
                .type_parts
                .iter()
                .chain(self.type_parts.iter())
                .cloned()
                .collect(),
            type_params: self.type_params.clone(),
        }
    }

    pub fn in_namespace_parts(&self, ns: &[Identifier]) -> Identifier {
        Identifier {
            type_parts: ns
                .iter()
                .fold(
                    Box::new(iter::empty()) as Box<dyn Iterator<Item = &Ident>>,
                    |i, n| {
                        Box::new(i.chain(n.type_parts.iter())) as Box<dyn Iterator<Item = &Ident>>
                    },
                )
                .chain(self.type_parts.iter())
                .cloned()
                .collect(),
            type_params: self.type_params.clone(),
        }
    }

    pub fn prefix_name(&self, prefix: &str) -> Identifier {
        let mut type_parts = self.type_parts.clone();
        if let Some(last_part) = type_parts.pop() {
            let last_part = format_ident!("{}{}", prefix, last_part);
            type_parts.push(last_part);
        }

        Identifier {
            type_parts,
            type_params: self.type_params.clone(),
        }
    }

    pub fn suffix_name(&self, suffix: &str) -> Identifier {
        let mut type_parts = self.type_parts.clone();
        if let Some(last_part) = type_parts.pop() {
            let last_part = format_ident!("{}{}", last_part, suffix);
            type_parts.push(last_part);
        }

        Identifier {
            type_parts,
            type_params: self.type_params.clone(),
        }
    }

    pub fn without_type_params(&self) -> Identifier {
        Identifier {
            type_parts: self.type_parts.clone(),
            type_params: Default::default(),
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let mut idents = self.type_parts.iter();
        if let Some(id) = idents.next() {
            write!(f, "{}", id)?;
        }
        for i in idents {
            write!(f, "::{}", i)?;
        }

        if !self.type_params.is_empty() {
            write!(f, "<")?;
            let mut type_params = self.type_params.iter();
            if let Some(tp) = type_params.next() {
                write!(f, "{}", tp)?;
            }
            for tp in type_params {
                write!(f, ", {}", tp)?;
            }
            write!(f, ">")?;
        }

        Ok(())
    }
}

impl From<Ident> for Identifier {
    fn from(src: Ident) -> Identifier {
        Identifier::new_ident(src)
    }
}

pub fn map_to_ident<T: AsRef<str>, F: Fn(&str) -> String>(s: T, map: F) -> Identifier {
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
    if parse_syn_str::<syn::Ident>(&full_ident).is_err() {
        // first append _s to try to make it an ident
        full_ident += "_";
    }
    if parse_syn_str::<syn::Ident>(&full_ident).is_err() {
        // now, prepend underscores to try to make it an ident
        full_ident = format!("_{}", full_ident);
    }

    format_ident!("{}", &full_ident).into()
}

pub fn to_ident<T: AsRef<str>>(s: T) -> Identifier {
    map_to_ident(s, ToString::to_string)
}

pub fn to_camel_case_ident<T: AsRef<str>>(s: T) -> Identifier {
    map_to_ident(s, |s| s.to_camel_case())
}

pub fn to_ns_name<T: AsRef<str>>(ns: T) -> Identifier {
    map_to_ident(
        ns.as_ref()
            .trim_end_matches(".d.ts")
            .trim_end_matches(".ts"),
        |s| s.to_snake_case(),
    )
}

pub fn to_snake_case_ident<T: AsRef<str>>(s: T) -> Identifier {
    map_to_ident(s, |s| s.to_snake_case())
}

pub fn to_unique_ident<T: Fn(&str) -> bool>(mut desired: String, taken: &T) -> Identifier {
    while taken(&desired) {
        desired += "_";
    }

    to_ident(&desired)
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
