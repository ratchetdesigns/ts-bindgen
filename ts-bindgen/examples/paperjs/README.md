# ts-bindgen paper.js example
[Paper.js](https://paperjs.org) is a vector graphics library with typescript types. This example generates rust [wasm-bindgen](https://rustwasm.github.io/docs/wasm-bindgen/) bindings for paper.js and runs our rust version of the [chain](http://paperjs.org/examples/chain/) example against those bindings from wasm.

# Developing
1. Run `cargo check` to execute `npm ci` and `cargo run --manifest-path ../../Cargo.toml -- --output src/paper.rs paperjs`, which regenerates rust bindings in `./src/paper.rs`.
2. Update the rust code in `./src/lib.rs`.
3. Build the wasm bundle: `wasm-pack build --target web --dev`
4. Start a web server to serve the paperjs directory and load `index.html`.
