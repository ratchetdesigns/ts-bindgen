# ts-bindgen &emsp; [![CI](https://github.com/ratchetdesigns/ts-bindgen/actions/workflows/ci.yml/badge.svg)](https://github.com/ratchetdesigns/ts-bindgen/actions/workflows/ci.yml) [![Latest Version](https://img.shields.io/crates/v/ts-bindgen.svg)](https://crates.io/crates/ts-bindgen) [![Rust Documentation](https://docs.rs/ts-bindgen/badge.svg)](https://docs.rs/ts-bindgen) ![Crates.io](https://img.shields.io/crates/l/ts-bindgen)

ts-bindgen: automatically generate rust [wasm-bindgen](https://rustwasm.github.io/docs/wasm-bindgen/) bindings for typescript definitions to easily interact with javascript libraries from rust compiled to wasm.

# Try it out in your browser

Head to [ts-bindgen.ratchetdesigns.com](https://ts-bindgen.ratchetdesigns.com) to try ts-bindgen in your browser now by copy/pasting typescript definitions to see the rust bindings it generates.

# Status

ts-bindgen is currently alpha software and it should be expected that any or all of the following might change significantly from version to version:
1. Generated bindings
2. Exposed interfaces
3. Internal implementation

There are currently known issues that will prevent generation of reasonable bindings for some typescript idioms.

We welcome contributions and issues!

# Quickstart

Head over to [ts-bindgen.ratchetdesigns.com](https://ts-bindgen.ratchetdesigns.com) to generate rust bindings by copy/pasting typescript definitions in your browser.

To start generating bindings in your rust projects, you have a few options:

1. Run `cargo run ts-bindgen -- --output src/bindings.rs your-ts-module` to generate `src/bindings.rs` from `your-ts-module` where `your-ts-module` is a `./path/to/defs.d.ts`, `/path/to/defs.d.ts`, or `module-in-node_modules`.
2. Invoke `cargo run ts-bindgen -- --output src/bindings.rs --rerun-if-changed your-ts-module` in your `build.rs`.
3. Add `ts-bindgen = { version = "0.3.0", default-features = false }` to your `cargo.toml` dependencies and invoke `ts_bindgen::generate_rust_string_for_typescript(ts_bindgen::StdFs, "your-module")` in your `build.rs`. Note: while the ts-bindgen executable will rustfmt the generated bindings, `generate_rust_string_for_typescript` does not.

The generated bindings depend on the ts-bindgen runtime: ts-bindgen-rt, wasm-bindgen, serde, and (optionally) js-sys and web-sys so add the following to your `Cargo.toml`:

```toml
[dependencies]
ts-bindgen-rt = "0.3.0"
wasm-bindgen = "0.2.63"
serde = { version = "1.0", features = ["derive"] }
js-sys = "0.3.53" # optional, depending on your input typescript
web-sys = { version = "0.3.53", features = ["Window"] } # optional, update with features for any types your bindings use
```

# Generated bindings

ts-bindgen seeks reasonable rust ergonomics and frequently wraps wasm-bindgen bindings to present a (hopefully) more "rust-y" interface.
For example, functions and methods are wrapped to convert their arguments and return values to/from javascript/rust representations, javascript classes are wrapped in newtype structs, and traits are generated for javscript class inheritance hierarcies.
To more faithfully preserve javascript semantics of null/undefined, functions, etc., we implement a custom (de)serializer similar to [serde-wasm-bindgen](https://github.com/cloudflare/serde-wasm-bindgen) to marshall between JsValues and generated bindings.
We expect that there are many opportunities for performance improvement that have not yet been explored.

# Structure

ts-bindgen consists of a few crates:
 - ts-bindgen - the public interface, consisting of the ts-bindgen executable and library
 - [ts-bindgen-gen](https://github.com/ratchetdesigns/ts-bindgen/tree/master/ts-bindgen-gen/README.md) - the meat of parsing ts and generating rust bindings
 - [ts-bindgen-rt](https://github.com/ratchetdesigns/ts-bindgen/tree/master/ts-bindgen-rt/README.md) - the runtime that generated bindings depend on
 - ts-bindgen-web (unpublished) - the code for [ts-bindgen.ratchetdesigns.com](https://ts-bindgen.ratchetdesigns.com)
 - ts-bindgen-macro (unpublished) - likely a bad idea for a macro to generate bindings. Bindings have become too complex not to be inspected while coding against them and ts-bindgen-gen has not attempted to preserve macro hygeine.

# License

Copyright 2022 Adam Berger, Ratchet Designs.

ts-bindgen is licensed under either of the [MIT](https://github.com/ratchetdesigns/ts-bindgen/blob/master/LICENSE-MIT) or [Apache](https://github.com/ratchetdesigns/ts-bindgen/blob/master/LICENSE-APACHE) licenses, at your option.

# Credit

ts-bindgen is crafted thoughtfully by [Ratchet Designs](https://ratchetdesigns.com)
