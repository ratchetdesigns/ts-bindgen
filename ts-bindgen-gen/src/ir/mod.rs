pub mod base;
mod flattened;
mod target_enriched;
mod transform;

pub use target_enriched::*;
pub use transform::to_final_ir;
