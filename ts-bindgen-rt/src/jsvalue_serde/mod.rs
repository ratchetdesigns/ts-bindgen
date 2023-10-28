//! serde implementation for JsValues

mod de;
mod error;
mod ser;

pub use self::de::from_jsvalue;
pub use self::error::Error;
pub use self::ser::{to_jsvalue, JSVALUE_NEWTYPE_STRUCT, UNDEFINED_UNIT_STRUCT};
