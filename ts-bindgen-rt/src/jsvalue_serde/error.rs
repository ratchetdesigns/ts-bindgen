use js_sys::{Error as JsError, Reflect};
use serde::{de, ser};
use std::error;
use std::fmt::{Display, Formatter};
use wasm_bindgen::JsValue;

pub type Result<R> = std::result::Result<R, Error>;

/// Error type for serde errors
#[derive(Debug, Clone)]
pub enum Error {
    /// A custom error with a message
    Custom(String),
    /// An error indicating that an invalid type was found
    InvalidType(String),
    /// A javascript error occured
    JsError {
        /// The message from the javascript error
        msg: Option<String>,
        /// The stack from the javascript error
        stack: Option<String>,
    },
}

impl From<JsValue> for Error {
    fn from(src: JsValue) -> Self {
        let msg_key: JsValue = "message".into();
        let msg = if Reflect::has(&src, &msg_key).unwrap_or(false) {
            Reflect::get(&src, &msg_key)
                .map(|v| v.as_string())
                .unwrap_or(None)
        } else {
            None
        };

        let stack_key: JsValue = "stack".into();
        let stack = if Reflect::has(&src, &stack_key).unwrap_or(false) {
            Reflect::get(&src, &stack_key)
                .map(|v| v.as_string())
                .unwrap_or(None)
        } else {
            None
        };

        Error::JsError { msg, stack }
    }
}

impl From<Error> for JsValue {
    fn from(src: Error) -> Self {
        JsError::new(&src.to_string()).into()
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "jsvalue_serde error: {:?}", self)
    }
}

impl de::StdError for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl ser::Error for Error {
    fn custom<T: Display>(msg: T) -> Error {
        Error::Custom(format!("{}", msg))
    }
}

impl de::Error for Error {
    fn custom<T: Display>(msg: T) -> Error {
        Error::Custom(format!("{}", msg))
    }

    fn invalid_type(unexp: de::Unexpected, exp: &dyn de::Expected) -> Self {
        Error::InvalidType(format!("received {}, expected {}", unexp, exp))
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
            Err(Error::InvalidType(format!("expected: {}", msg)))
        }
    }
}
