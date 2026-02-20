mod calls;
mod indexing;
mod literals;
mod members;
mod operators;

use lyra_semantic::type_infer::{
    BitVecType, BitWidth, ExprType, ExprTypeErrorKind, ExprView, Signedness,
};
use lyra_semantic::types::{ConstEvalError, RealKw, Ty};

use super::*;

/// Find the first parameter's init expression `AstId` and return its `ExprType`.
pub(super) fn expr_type_of_first_param(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> ExprType {
    let def = def_index_file(db, file);
    let (sym_id, _) = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == lyra_semantic::symbols::SymbolKind::Parameter)
        .expect("should have a parameter");
    let decl_ast_id = def.symbol_to_decl[sym_id.index()].expect("param should have decl");
    let init_ast_id = def
        .decl_to_init_expr
        .get(&decl_ast_id)
        .expect("param should be tracked")
        .expect("param should have init");
    let expr_ref = ExprRef::new(db, unit, init_ast_id);
    type_of_expr(db, expr_ref)
}

/// Find a named parameter's init expression and return its `ExprType`.
pub(super) fn expr_type_of_named_param(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
    name: &str,
) -> ExprType {
    let def = def_index_file(db, file);
    let (sym_id, _) = def
        .symbols
        .iter()
        .find(|(_, s)| {
            s.kind == lyra_semantic::symbols::SymbolKind::Parameter && s.name.as_str() == name
        })
        .unwrap_or_else(|| panic!("parameter '{name}' not found"));
    let decl_ast_id = def.symbol_to_decl[sym_id.index()].expect("param should have decl");
    let init_ast_id = def
        .decl_to_init_expr
        .get(&decl_ast_id)
        .expect("param should be tracked")
        .expect("param should have init");
    let expr_ref = ExprRef::new(db, unit, init_ast_id);
    type_of_expr(db, expr_ref)
}

pub(super) fn bv(width: u32, signed: Signedness, four_state: bool) -> ExprType {
    ExprType::bitvec(BitVecType {
        width: BitWidth::Known(width),
        signed,
        four_state,
    })
}

pub(super) fn bv_u(width: u32) -> ExprType {
    bv(width, Signedness::Unsigned, false)
}

pub(super) fn bv_u4(width: u32) -> ExprType {
    bv(width, Signedness::Unsigned, true)
}

pub(super) fn bv_context() -> ExprType {
    ExprType::bitvec(BitVecType {
        width: BitWidth::ContextDependent,
        signed: Signedness::Unsigned,
        four_state: false,
    })
}

pub(super) fn bv_s(width: u32) -> ExprType {
    bv(width, Signedness::Signed, false)
}

pub(super) fn one_bit() -> ExprType {
    bv_u(1)
}

pub(super) fn one_bit_4() -> ExprType {
    bv_u4(1)
}
