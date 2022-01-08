mod fmt;

use fmt::rust_fmt;
use monaco::{
    api::CodeEditorOptions,
    sys::editor::{BuiltinTheme, IStandaloneCodeEditor},
    yew::{CodeEditor, CodeEditorLink},
};
use std::rc::Rc;
use ts_bindgen::generate_rust_string_for_typescript_string;
use wasm_bindgen::prelude::*;
use yew::{html, Component, Context, Html};

fn get_options(lang: &str) -> CodeEditorOptions {
    let opts = CodeEditorOptions::default()
        .with_language(lang.to_owned())
        .with_builtin_theme(BuiltinTheme::VsDark);
    if lang == "typescript" {
        opts.with_value(
            r#"
        type MyType = number | string | null;
        "#
            .to_owned(),
        )
    } else {
        opts
    }
}

struct App {
    ts_options: Rc<CodeEditorOptions>,
    ts_link: CodeEditorLink,
    rust_options: Rc<CodeEditorOptions>,
    rust_link: CodeEditorLink,
}

enum Msg {
    Generate,
}

fn set_auto_layout(editor: &CodeEditorLink) {
    editor.with_editor(|model| {
        let editor: &IStandaloneCodeEditor = model.as_ref();
        let opts = editor.get_raw_options();
        opts.set_automatic_layout(Some(true));
        editor.update_options_editor(&opts);
    });
}

impl Component for App {
    type Message = Msg;
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        Self {
            ts_options: Rc::new(get_options("typescript")),
            ts_link: Default::default(),
            rust_options: Rc::new(get_options("rust")),
            rust_link: Default::default(),
        }
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::Generate => {
                self.ts_link.with_editor(|model| {
                    let ts = model
                        .get_model()
                        .map(|m| m.get_value())
                        .unwrap_or_else(|| String::from(""));
                    let rust = generate_rust_string_for_typescript_string("example", ts)
                        .map_err(|err| Box::new(err) as Box<dyn std::error::Error + 'static>)
                        .and_then(rust_fmt)
                        .unwrap_or_else(|err| format!("// Error generating typescript rust bindings.\n{}", err));
                    self.rust_link.with_editor(|rs_model| {
                        rs_model.get_model().map(|m| m.set_value(&rust));
                    });
                });
            }
        }
        false
    }

    fn rendered(&mut self, _ctx: &Context<Self>, first_render: bool) {
        if first_render {
            set_auto_layout(&self.ts_link);
            set_auto_layout(&self.rust_link);
        }
    }


    fn view(&self, ctx: &Context<Self>) -> Html {
        let on_generate = ctx.link().callback(|_| Msg::Generate);

        html! {
            <>
                <div class="top-bar">
                    <a class="top-bar-logo" href="https://ratchetdesigns.com">
                        <img alt="Ratchet Designs" src="logo.png" />
                    </a>
                </div>
                <div class="main-area">
                    <div class="pane">
                        <div class="file-header">
                            <div class="title">{"Input typescript definitions (.d.ts)"}</div>
                            <button onclick={on_generate} class="top-bar-btn">{"Generate"}</button>
                        </div>
                        <CodeEditor options={Rc::clone(&self.ts_options)} link={self.ts_link.clone()}/>
                    </div>
                    <div class="separator" />
                    <div class="pane">
                        <div class="file-header">
                            <div class="title">{"Rust wasm-bindgen bindings"}</div>
                        </div>
                        <CodeEditor options={Rc::clone(&self.rust_options)} link={self.rust_link.clone()} />
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
