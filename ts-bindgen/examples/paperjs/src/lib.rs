mod paper;

use js_sys::{Object, Reflect};
use paper::paper::dist::paper::paper::{
    MouseEvent, PaperScope, PaperScopeSetupParamsElementParam, Path, PathAddParamsSegmentParam,
    PathItem_Trait, Point, ViewOnMouseMove,
};
use wasm_bindgen::prelude::*;
use web_sys::{window, HtmlCanvasElement};

/// Translated from http://paperjs.org/examples/chain/
#[wasm_bindgen]
pub fn chain_example(view: &HtmlCanvasElement) -> Result<(), JsValue> {
    let point_count = 25;
    let point_distance = 35.0;

    let scope = PaperScope::new();
    let view_js: &JsValue = view.as_ref();
    scope.setup(PaperScopeSetupParamsElementParam::JsValueCase(
        view_js.clone(),
    ))?;
    scope.install(window().unwrap().into())?;

    let path = Path::new(PathDescription::new("#E4141B", 20, "round").into());

    let start = scope
        .view()?
        .center()?
        .divide(Point::new(PointParam::new(10.0, 1.0).into()))?;

    let points: Vec<_> = (0..point_count)
        .into_iter()
        .map(|i| {
            start.add(Point::new(
                PointParam::new((i as f64) * point_distance, 0.0).into(),
            ))
        })
        .map(|p| p.map(|p| PathAddParamsSegmentParam::PointCase(p)))
        .collect::<Result<Vec<_>, _>>()?;
    path.add(points.into_boxed_slice())?;

    let mouse_handler = Closure::wrap(Box::new(move |event: JsValue| -> Result<(), JsValue> {
        // TODO: nicer JsValue -> class conversion
        let event = MouseEvent(event.into());
        path.first_segment()?.set_point(event.point()?)?;

        let segments = path.segments()?;
        for segment in segments.into_iter().take(point_count - 1) {
            let next_segment = segment.next()?;
            let cur_point = segment.point()?;
            let next_point = next_segment.point()?;
            let vector = cur_point.subtract(next_point)?;
            vector.set_length(point_distance)?;
            next_segment.set_point(cur_point.subtract(vector)?)?;
        }

        path.smooth(SmoothOptions::new("continuous").into())?;

        Ok(())
    }) as Box<dyn Fn(JsValue) -> Result<(), JsValue>>);
    scope
        .view()?
        .set_on_mouse_move(ViewOnMouseMove::JsValueCase(
            mouse_handler.into_js_value().into(),
        ))?;

    Ok(())
}

struct PointParam {
    pub x: f64,
    pub y: f64,
}

impl PointParam {
    fn new(x: f64, y: f64) -> PointParam {
        PointParam { x, y }
    }
}

impl From<PointParam> for JsValue {
    fn from(src: PointParam) -> JsValue {
        let o = Object::new();
        Reflect::set(&o, &"x".into(), &src.x.into()).unwrap();
        Reflect::set(&o, &"y".into(), &src.y.into()).unwrap();
        o.into()
    }
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
