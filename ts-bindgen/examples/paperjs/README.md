# ts-bindgen paper.js example
[Paper.js](https://paperjs.org) is a vector graphics library with typescript types. This example generates rust [wasm-bindgen](https://rustwasm.github.io/docs/wasm-bindgen/) bindings for paper.js and runs our rust version of the [chain](http://paperjs.org/examples/chain/) example against those bindings from wasm.

# Developing

Note: the maintainers develop in a [container](https://hub.docker.com/r/ratchetdesigns/wasm-pack) but you do you.

1. Run `cargo check --target wasm32-unknown-unknown` to execute `npm ci` and `cargo run --manifest-path ../../Cargo.toml -- --output src/paper.rs paperjs`, which regenerates rust bindings in `./src/paper.rs`.
2. Update the rust code in `./src/lib.rs`.
3. Build the wasm bundle: `wasm-pack build --target web --dev` (this will also re-generate the bindings)
4. Build the js bundle: `npx --yes --package=rollup -- rollup index.js --file pkg/bundle.js --format iife -g node_modules/paper/dist/paper.js:paper`
5. Start a web server to serve the paperjs directory and load `index.html`.
