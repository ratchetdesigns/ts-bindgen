# ts-bindgen-gen &emsp; [![CI](https://github.com/ratchetdesigns/ts-bindgen/actions/workflows/ci.yml/badge.svg)](https://github.com/ratchetdesigns/ts-bindgen/actions/workflows/ci.yml) [![Latest Version](https://img.shields.io/crates/v/ts-bindgen-gen.svg)](https://crates.io/crates/ts-bindgen-gen) [![Rust Documentation](https://docs.rs/ts-bindgen-gen/badge.svg)](https://docs.rs/ts-bindgen-gen) ![Crates.io](https://img.shields.io/crates/l/ts-bindgen-gen)

ts-bindgen-gen contains the typescript parsing and rust [wasm-bindgen](https://rustwasm.github.io/docs/wasm-bindgen/) binding generation logic for [ts-bindgen](https://github.com/ratchetdesigns/ts-bindgen).

For usage information about ts-bindgen, check out the [ts-bindgen docs](https://docs.rs/ts-bindgen), [online playground](https://ts-bindgen.ratchetdesigns.com), or [repo](https://github.com/ratchetdesigns/ts-bindgen).

# Status

ts-bindgen is currently alpha software and it should be expected that any or all of the following might change significantly from version to version:
1. Generated bindings
2. Exposed interfaces
3. Internal implementation

There are currently known issues that will prevent generation of reasonable bindings for some typescript idioms.

We welcome contributions and issues!

# Overview

The overall flow is:
1. [Parse](src/parse.rs) typescript from [swc_ecma_ast](https://docs.rs/swc_ecma_ast) into our [base ir](src/ir/base.rs)
2. [Transform](src/ir/transform) our [base ir](src/ir/base.rs) into a [flattened ir](src/ir/flattened.rs) and then into a [target-enriched ir](src/ir/target_enriched.rs). The flattened ir takes types that are inlined and unnamed in typescript (e.g. the implied `number | string` enum in `interface { a: number | string }`) and "flattens" them to named, top-level types while fixing up references. The target-enriched ir propagates file and namespace context down through the AST.
3. Construct a [Module Definition](src/mod_def.rs) hierarchy representing the rust modules and types in each module.
4. [Generate](src/codegen/mod.rs) our rust bindings by walking the ModDef hierarchy and generating a [proc_macro2::TokenStream](https://docs.rs/proc-macro2).

# Todo

 - Lots of [todos](https://github.com/ratchetdesigns/ts-bindgen/search?q=todo) in the code to address.
 - Function overloads are not yet handled.
 - Many builtins are codegen'ed to JsValue instead of their proper js-sys or web-sys types.
 - We should generate better [async bindings](https://rustwasm.github.io/docs/wasm-bindgen/reference/js-promises-and-rust-futures.html).
 - We don't yet handle tsconfig [typeRoots](https://www.typescriptlang.org/tsconfig#typeRoots).
 - The implicit assumption that every x.d.ts has a corresponding x.js is incorrect - we need to separately resolve .js and .d.ts files and preserve the .js path in generated bindings.
 - Issues with typescript modules and name resolution (e.g. referencing A.B in a typescript module should look for A.B in all ancestors but, as implemented, looks for B in all ancestors).
 - Rarely used typescript namespaces are not properly handled yet (they should result in js_namespace attrs in wasm-bindgen) and require special handling in bundling to work (see the [paperjs example](../ts-bindgen/examples/paperjs).

# Needed refactorings

 - Codegen is too complex and would be *much* better served by another ir transformation pass to convert to a data representation of the rust code we want to generate and **then** generating a TokenStream more straightforwardly from that representation. This would enable, for example, references to a single generated name instead of hoping to re-generate the same name in multiple places.
 - Builtins should not be named but should be more of a config.
 - The ir transformation pipeline could be better served by something like [frunk::Generic](https://docs.rs/frunk/latest/frunk/generic/index.html).

# License

Copyright 2022 Adam Berger, Ratchet Designs.

ts-bindgen is licensed under either of the [MIT](https://github.com/ratchetdesigns/ts-bindgen/blob/master/LICENSE-MIT) or [Apache](https://github.com/ratchetdesigns/ts-bindgen/blob/master/LICENSE-APACHE) licenses, at your option.

# Credit

ts-bindgen is crafted thoughtfully by [Ratchet Designs](https://ratchetdesigns.com)
