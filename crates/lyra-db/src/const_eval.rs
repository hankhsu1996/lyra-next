use lyra_semantic::types::{ConstEvalError, ConstInt};

use crate::pipeline::{ast_id_map, parse_file};
use crate::semantic::{def_index_file, resolve_index_file};
use crate::{CompilationUnit, source_file_by_id};

/// Identifies a constant expression for evaluation.
///
/// Const-eval depends on the compilation unit because name resolution
/// (imports, global definitions, package visibility) is unit-scoped.
#[salsa::interned]
pub struct ConstExprRef<'db> {
    pub unit: CompilationUnit,
    pub expr_ast_id: lyra_ast::ErasedAstId,
}

/// Evaluate a constant integer expression (Salsa-tracked with cycle recovery).
///
/// Returns `ConstInt::Known(v)` on success, `ConstInt::Error(e)` on failure.
/// Cycles (e.g. `parameter A = B; parameter B = A;`) are detected by Salsa
/// and recovered via `const_eval_recover`.
#[salsa::tracked(recovery_fn = const_eval_recover)]
pub fn eval_const_int<'db>(db: &'db dyn salsa::Database, expr_ref: ConstExprRef<'db>) -> ConstInt {
    let unit = expr_ref.unit(db);
    let expr_ast_id = expr_ref.expr_ast_id(db);
    let file_id = expr_ast_id.file();

    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return ConstInt::Error(ConstEvalError::Unresolved);
    };

    let parse = parse_file(db, source_file);
    let map = ast_id_map(db, source_file);

    let Some(node) = map.get_node(&parse.syntax(), expr_ast_id) else {
        return ConstInt::Error(ConstEvalError::Unresolved);
    };

    let resolve_name = |name_node: &lyra_parser::SyntaxNode| -> Result<i64, ConstEvalError> {
        // Look up the NameRef/QualifiedName's AstId
        let name_ast_id = map
            .erased_ast_id(name_node)
            .ok_or(ConstEvalError::Unresolved)?;

        // Resolve the name to a GlobalSymbolId
        let resolve = resolve_index_file(db, source_file, unit);
        let resolution = resolve
            .resolutions
            .get(&name_ast_id)
            .ok_or(ConstEvalError::Unresolved)?;

        // Look up target symbol -- only parameters are allowed
        let target_file_id = resolution.symbol.file;
        let target_local = resolution.symbol.local;
        let target_file =
            source_file_by_id(db, unit, target_file_id).ok_or(ConstEvalError::Unresolved)?;
        let target_def = def_index_file(db, target_file);
        let target_sym = target_def.symbols.get(target_local);

        if target_sym.kind != lyra_semantic::symbols::SymbolKind::Parameter {
            return Err(ConstEvalError::NonConstant);
        }

        // Look up declarator AstId via symbol_to_decl
        let decl_ast_id = target_def
            .symbol_to_decl
            .get(target_local.index())
            .and_then(|opt| *opt)
            .ok_or(ConstEvalError::Unresolved)?;

        // Look up init expression AstId
        let init_ast_id = target_def
            .decl_to_init_expr
            .get(&decl_ast_id)
            .ok_or(ConstEvalError::Unresolved)?
            .ok_or(ConstEvalError::Unresolved)?;

        // Recursively evaluate via Salsa (cycle detection here)
        let init_ref = ConstExprRef::new(db, unit, init_ast_id);
        match eval_const_int(db, init_ref) {
            ConstInt::Known(v) => Ok(v),
            ConstInt::Error(e) => Err(e),
            ConstInt::Unevaluated(_) => Err(ConstEvalError::Unsupported),
        }
    };

    match lyra_semantic::const_eval::eval_const_expr(&node, &resolve_name) {
        Ok(v) => ConstInt::Known(v),
        Err(e) => ConstInt::Error(e),
    }
}

fn const_eval_recover<'db>(
    _db: &'db dyn salsa::Database,
    _cycle: &salsa::Cycle,
    _expr_ref: ConstExprRef<'db>,
) -> ConstInt {
    ConstInt::Error(ConstEvalError::Cycle)
}
