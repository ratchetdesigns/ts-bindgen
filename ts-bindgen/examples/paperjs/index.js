import 'paper';
import init, { chain_example } from './pkg/ts_bindgen_example_paperjs.js';

window.onload = function() {
  init('./pkg/ts_bindgen_example_paperjs_bg.wasm').then(() => chain_example(document.getElementById('canvas')));
};
