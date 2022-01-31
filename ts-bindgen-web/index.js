import start_app from "./pkg/ts_bindgen_web";

window.MonacoEnvironment = {
  getWorkerUrl: function(workerId, label) {
    return `data:text/javascript;charset=utf-8,${encodeURIComponent(`
      self.MonacoEnvironment = {
        baseUrl: 'https://unpkg.com/monaco-editor@%5E0.20.0/min/'
      };
      importScripts('https://unpkg.com/monaco-editor@%5E0.20.0/min/vs/base/worker/workerMain.js');
    `)}`;
  }
};

window.onload = function() {
  start_app();
};
