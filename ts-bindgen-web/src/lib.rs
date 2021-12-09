use monaco::{api::CodeEditorOptions, sys::editor::BuiltinTheme, yew::CodeEditor};
use std::rc::Rc;
use wasm_bindgen::prelude::*;
use yew::{html, Component, Context, Html};

const CONTENT: &str = include_str!("lib.rs");

fn get_options(lang: &str) -> CodeEditorOptions {
    CodeEditorOptions::default()
        .with_language(lang.to_owned())
        .with_value(CONTENT.to_owned())
        .with_builtin_theme(BuiltinTheme::VsDark)
}

struct App {
    ts_options: Rc<CodeEditorOptions>,
    rust_options: Rc<CodeEditorOptions>,
}

impl Component for App {
    type Message = ();
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        Self {
            ts_options: Rc::new(get_options("typescript")),
            rust_options: Rc::new(get_options("rust")),
        }
    }

    fn update(&mut self, _ctx: &Context<Self>, _msg: Self::Message) -> bool {
        false
    }

    fn view(&self, _ctx: &Context<Self>) -> Html {
        html! {
            <>
                <div class="top-bar">
                    <a class="top-bar-logo" href="https://ratchetdesigns.com">
                        <img src="logo.png" />
                    </a>
                </div>
                <div class="main-area">
                    <div class="pane">
                        <div class="file-header">
                            <div class="title">{"Input typescript definitions (.d.ts)"}</div>
                            <button class="top-bar-btn">{"Generate"}</button>
                        </div>
                        <CodeEditor options={Rc::clone(&self.ts_options)} />
                    </div>
                    <div class="separator" />
                    <div class="pane">
                        <div class="file-header">
                            <div class="title">{"Rust wasm-bindgen bindings"}</div>
                        </div>
                        <CodeEditor options={Rc::clone(&self.rust_options)} />
                    </div>
                </div>
            </>
        }
    }
}

#[wasm_bindgen(start)]
pub fn start_app() {
    yew::start_app::<App>();
}
