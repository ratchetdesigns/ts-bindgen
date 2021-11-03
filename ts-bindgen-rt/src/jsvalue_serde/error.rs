use serde::{de, ser};
use std::error;
use std::fmt::{Display, Formatter};
use wasm_bindgen::JsValue;

pub type Result<R> = std::result::Result<R, Error>;

#[derive(Debug, Clone)]
pub struct Error {}

#[wasm_bindgen::prelude::wasm_bindgen]
extern "C" {
    #[wasm_bindgen::prelude::wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
    #[wasm_bindgen::prelude::wasm_bindgen(js_namespace = console, js_name = log)]
    fn log_data(s: &str, jsv: &JsValue);
}

impl From<JsValue> for Error {
    fn from(_src: JsValue) -> Self {
        Error {}
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "Error")
    }
}

impl de::StdError for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl ser::Error for Error {
    fn custom<T: Display>(_msg: T) -> Error {
        Error {}
    }
}

impl de::Error for Error {
    fn custom<T: Display>(msg: T) -> Error {
        Error {}
    }

    fn invalid_type(unexp: de::Unexpected, exp: &dyn de::Expected) -> Self {
        log(&format!("invalid_type {:?}; expected {}", unexp, exp));
        Error {}
    }
}

pub trait ExpectValue<T> {
    fn expect_value(self, msg: &str) -> Result<T>;
}

impl<T> ExpectValue<T> for Option<T> {
    fn expect_value(self, msg: &str) -> Result<T> {
        if let Some(val) = self {
            Ok(val)
        } else {
            Err(Error {})
        }
    }
}
