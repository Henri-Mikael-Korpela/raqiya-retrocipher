use frontend_macros::html;
use wasm_bindgen::prelude::*;
use web_sys::{Element, HtmlDivElement};

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = raq, js_name = createHTMLElement)]
    fn create_html_element(name: &str, style: &str, template: &str) -> Element;
}

/// TODO: Replace with Bloom compiler logic
#[wasm_bindgen]
pub fn get_root() -> Element {
    set_panic_hook();

    let window = web_sys::window().unwrap();
    let document = window.document().unwrap();

    let root_elem = document
        .create_element("div")
        .unwrap()
        .dyn_into::<HtmlDivElement>()
        .unwrap();
    root_elem.style().set_property("display", "grid").unwrap();
    root_elem
        .style()
        .set_property("grid-template-columns", "1fr 1fr 1fr")
        .unwrap();

    let code_editor_elem = create_html_element(
        "bloom-code-editor",
        &format!(
            "
        .container {{
            code {{
                background-color: #282c34;
                border: 1px solid #282c34;
                border-radius: 5px;
                color: white;
                display:block;
                font-family: monospace;
                font-size: 20px;
                padding: 10px;
            }}
        }}"
        ),
        html! {
            <div class="container">
                <code contenteditable="true">fn main</code>
            </div>
        },
    );
    root_elem.append_child(&code_editor_elem).unwrap();

    let assembly_output_elem = create_html_element(
        "bloom-assembly-output",
        &format!(
            "
        .container {{
            code {{
                background-color: #282c34;
                border: 1px solid #282c34;
                border-radius: 5px;
                color: white;
                display:block;
                font-family: monospace;
                font-size: 20px;
                padding: 10px;
            }}
        }}"
        ),
        html! {
            <div class="container">
                <code>assembly output</code>
            </div>
        },
    );
    root_elem.append_child(&assembly_output_elem).unwrap();

    root_elem.into()
}
pub fn set_panic_hook() {
    // When the `console_error_panic_hook` feature is enabled, we can call the
    // `set_panic_hook` function at least once during initialization, and then
    // we will get better error messages if our code ever panics.
    //
    // For more details see
    // https://github.com/rustwasm/console_error_panic_hook#readme
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}
