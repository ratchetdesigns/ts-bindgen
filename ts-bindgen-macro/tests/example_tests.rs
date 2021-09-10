#[test]
fn example_passing_tests() {
    let t = trybuild::TestCases::new();
    t.pass("tests/examples/*_pass.rs");
}

fn main() {}
