mod paper;

use js_sys::{Object, Reflect};
use paper::paper::dist::paper::paper::{
    MouseEvent, PaperScope, PaperScopeSetupParamsElementParam, Path, PathAddParamsSegmentParam,
    PathItem_Trait, Point, ViewOnMouseMove,
};
use wasm_bindgen::prelude::*;
use web_sys::{window, HtmlCanvasElement};
use std::rc::Rc;

/// Translated from http://paperjs.org/examples/chain/
#[wasm_bindgen]
pub fn chain_example(view: &HtmlCanvasElement) -> Result<(), JsValue> {
    let point_count = 25;
    let point_distance = 35.0;

    let scope = PaperScope::new();
    scope.setup(PaperScopeSetupParamsElementParam::WebSysHtmlCanvasElement(
        view.clone(),
    ))?;
    scope.install(window().unwrap().into())?;

    let path = Path::new_FnJsValueToPath(PathDescription::new("#E4141B", 20, "round").into());

    let start = scope
        .view()?
        .center()?
        .divide_FnPointToPoint(Point::new_FnF64AndF64ToPoint(10.0, 1.0))?;

    let points: Vec<_> = (0..point_count)
        .into_iter()
        .map(|i| {
            start.add_FnPointToPoint(Point::new_FnF64AndF64ToPoint(
                (i as f64) * point_distance,
                0.0,
            ))
        })
        .map(|p| p.map(|p| PathAddParamsSegmentParam::Point(p)))
        .collect::<Result<Vec<_>, _>>()?;
    path.add(points.into_boxed_slice())?;

    let mouse_handler = Rc::new(move |args: Box<[JsValue]>| -> Result<JsValue, JsValue> {
        let event = args.into_iter().next().ok_or::<JsValue>("no event passed".into())?;
        let event = MouseEvent(event.clone().into());
        path.first_segment()?.set_point(event.point()?)?;

        let segments = path.segments()?;
        for segment in segments.into_iter().take(point_count - 1) {
            let next_segment = segment.next()?;
            let cur_point = segment.point()?;
            let next_point = next_segment.point()?;
            let vector = cur_point.subtract_FnPointToPoint(next_point)?;
            vector.set_length(point_distance)?;
            next_segment.set_point(cur_point.subtract_FnPointToPoint(vector)?)?;
        }

        path.smooth(SmoothOptions::new("continuous").into())?;

        Ok(JsValue::null())
    }) as Rc<dyn Fn(Box<[JsValue]>) -> Result<JsValue, JsValue>>;
    scope
        .view()?
        .set_on_mouse_move(ViewOnMouseMove::FnJsValueToJsValue(
            mouse_handler
        ))?;

    Ok(())
}

struct SmoothOptions {
    smoothing_type: String,
}

impl SmoothOptions {
    fn new(smoothing_type: &str) -> SmoothOptions {
        SmoothOptions {
            smoothing_type: smoothing_type.to_string(),
        }
    }
}

impl From<SmoothOptions> for JsValue {
    fn from(src: SmoothOptions) -> JsValue {
        let o = Object::new();
        Reflect::set(&o, &"type".into(), &src.smoothing_type.clone().into()).unwrap();
        o.into()
    }
}

struct PathDescription {
    stroke_color: String,
    stroke_width: u32,
    stroke_cap: String,
}

impl PathDescription {
    fn new(stroke_color: &str, stroke_width: u32, stroke_cap: &str) -> PathDescription {
        PathDescription {
            stroke_color: stroke_color.to_string(),
            stroke_width,
            stroke_cap: stroke_cap.to_string(),
        }
    }
}

impl From<PathDescription> for JsValue {
    fn from(src: PathDescription) -> JsValue {
        let o = Object::new();
        Reflect::set(&o, &"strokeColor".into(), &src.stroke_color.clone().into()).unwrap();
        Reflect::set(&o, &"strokeWidth".into(), &src.stroke_width.clone().into()).unwrap();
        Reflect::set(&o, &"strokeCap".into(), &src.stroke_cap.clone().into()).unwrap();
        o.into()
    }
}

#[cfg(test)]
mod test {
    extern crate wasm_bindgen_test;
    use wasm_bindgen_test::*;

    #[wasm_bindgen_test]
    fn basic_test() {}
}
