use crate::ir::base::{Type as TypeIR, TypeIdent as TypeIdentIR};
use crate::ir::flattened::{flatten_types, FlatType, TypeIdent as FlatTypeIdent};
use crate::ir::target_enriched::{
    target_enrich, TargetEnrichedType, TypeIdent as TargetEnrichedTypeIdent,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

type Init = HashMap<PathBuf, HashMap<TypeIdentIR, TypeIR>>;
type Flat = HashMap<PathBuf, HashMap<FlatTypeIdent, FlatType>>;
type Final = HashMap<PathBuf, HashMap<TargetEnrichedTypeIdent, TargetEnrichedType>>;

fn init_to_flat(src: Init) -> Flat {
    src.iter()
        .map(|(path, types_by_name)| {
            let flat_types = flatten_types(types_by_name.values().cloned());
            (
                path.clone(),
                flat_types.fold(
                    HashMap::new() as HashMap<FlatTypeIdent, FlatType>,
                    |mut m, t| {
                        m.insert(t.name.clone(), t);
                        m
                    },
                ),
            )
        })
        .collect()
}

pub fn to_final_ir(src: Init) -> Rc<RefCell<Final>> {
    target_enrich(init_to_flat(src))
}
