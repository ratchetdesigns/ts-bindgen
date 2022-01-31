set -e

wasm-pack build --release --target web
npx --yes --package=rollup -- rollup index.js --file public/bundle.js --format iife
cp pkg/ts_bindgen_web_bg.wasm public/
