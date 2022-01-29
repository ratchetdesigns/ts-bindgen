# ts-bindgen-rt &emsp; [![CI](https://github.com/ratchetdesigns/ts-bindgen/actions/workflows/ci.yml/badge.svg)](https://github.com/ratchetdesigns/ts-bindgen/actions/workflows/ci.yml) [![Latest Version](https://img.shields.io/crates/v/ts-bindgen-rt.svg)](https://crates.io/crates/ts-bindgen-rt) [![Rust Documentation](https://docs.rs/ts-bindgen-rt/badge.svg)](https://docs.rs/ts-bindgen-rt) ![Crates.io](https://img.shields.io/crates/l/ts-bindgen-rt)

ts-bindgen-rt contains the runtime that generated bindings depend on for [ts-bindgen](https://github.com/ratchetdesigns/ts-bindgen).

For usage information about ts-bindgen, check out the [ts-bindgen docs](https://docs.rs/ts-bindgen), [online playground](https://ts-bindgen.ratchetdesigns.com), or [repo](https://github.com/ratchetdesigns/ts-bindgen).

# Status

ts-bindgen is currently alpha software and it should be expected that any or all of the following might change significantly from version to version:
1. Generated bindings
2. Exposed interfaces
3. Internal implementation

There are currently known issues that will prevent generation of reasonable bindings for some typescript idioms.

We welcome contributions and issues!

# Overview

ts-bindgen-rt provides the runtime library that our generated bindings depend on, primarily for serialization and deserialization between javascript (JsValues and rust).

# License

ts-bindgen is dual-licensed under the [MIT](https://github.com/ratchetdesigns/ts-bindgen/blob/master/LICENSE-MIT) and [Apache](https://github.com/ratchetdesigns/ts-bindgen/blob/master/LICENSE-APACHE) licenses.

# Credit

ts-bindgen is crafted thoughtfully by [Ratchet Designs](https://ratchetdesigns.com)
