mod aggregate;
mod basic;
mod dims;
mod interface;
mod typedef;

use lyra_semantic::record::{Packing, RecordKind};
use lyra_semantic::symbols::GlobalSymbolId;
use lyra_semantic::types::{
    AssocIndex, ConstEvalError, ConstInt, IntegralKw, NetKind, SymbolType, SymbolTypeError, Ty,
    UnpackedDim,
};

use super::*;

pub(super) fn find_symbol(
    db: &dyn salsa::Database,
    file: SourceFile,
    name: &str,
) -> GlobalSymbolId {
    let def = def_index_file(db, file);
    let (local_id, _sym) = def
        .symbols
        .iter()
        .find(|(_, s)| s.name == name)
        .unwrap_or_else(|| panic!("symbol '{name}' not found"));
    GlobalSymbolId {
        file: file.file_id(db),
        local: local_id,
    }
}

pub(super) fn get_type(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
    name: &str,
) -> SymbolType {
    let gsym = find_symbol(db, file, name);
    let sym_ref = SymbolRef::new(db, unit, gsym);
    type_of_symbol(db, sym_ref)
}

pub(super) fn get_type_raw(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
    name: &str,
) -> SymbolType {
    let gsym = find_symbol(db, file, name);
    let sym_ref = SymbolRef::new(db, unit, gsym);
    type_of_symbol_raw(db, sym_ref)
}
