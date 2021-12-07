use proptest::bool::ANY;
use proptest::collection::vec;
use proptest::prelude::*;
use proptest::string::string_regex;

pub fn arb_path_part() -> impl Strategy<Value = String> {
    string_regex("[a-zA-Z0-9_-]+").unwrap()
}

pub fn arb_path_ext() -> impl Strategy<Value = String> {
    string_regex("[a-zA-Z0-9_.-]+").unwrap()
}

pub fn arb_path() -> impl Strategy<Value = String> {
    (ANY, vec(arb_path_part(), 10)).prop_map(|(is_abs, parts)| make_path(is_abs, &parts))
}

pub fn arb_abs_path() -> impl Strategy<Value = String> {
    vec(arb_path_part(), 10).prop_map(|v| make_path(true, &v))
}

pub fn arb_rel_path() -> impl Strategy<Value = String> {
    vec(arb_path_part(), 10).prop_map(|v| make_path(false, &v))
}

pub fn arb_ts_ext<'a>() -> impl Strategy<Value = &'a str> {
    prop_oneof![
        Just("d.ts"),
        Just("ts"),
        Just("tsx"),
        Just("js"),
        Just("jsx"),
        Just("json"),
    ]
}

pub fn arb_ext() -> impl Strategy<Value = String> {
    string_regex("[\\w].*").unwrap()
}

fn make_path(make_abs: bool, parts: &[String]) -> String {
    let abs_part = if make_abs { "/" } else { "" };
    format!("{}{}", abs_part, parts.join("/"))
}
