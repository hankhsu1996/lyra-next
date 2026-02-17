use lyra_semantic::type_infer::{ExprType, ExprTypeErrorKind, InferCtx};
use lyra_semantic::types::ConstInt;

use crate::const_eval::{ConstExprRef, eval_const_int};
use crate::pipeline::{ast_id_map, parse_file};
use crate::semantic::resolve_index_file;
use crate::type_queries::{SymbolRef, type_of_symbol};
use crate::{CompilationUnit, source_file_by_id};

/// Identifies an expression for type inference.
///
/// Includes the compilation unit because name resolution is unit-scoped.
#[salsa::interned]
pub struct ExprRef<'db> {
    pub unit: CompilationUnit,
    pub expr_ast_id: lyra_ast::ErasedAstId,
}

/// Infer the type of an expression (Salsa-tracked).
#[salsa::tracked]
pub fn type_of_expr<'db>(db: &'db dyn salsa::Database, expr_ref: ExprRef<'db>) -> ExprType {
    let unit = expr_ref.unit(db);
    let expr_ast_id = expr_ref.expr_ast_id(db);
    let file_id = expr_ast_id.file();

    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return ExprType::Error(ExprTypeErrorKind::Unresolved);
    };

    let parse = parse_file(db, source_file);
    let map = ast_id_map(db, source_file);

    let Some(node) = map.get_node(&parse.syntax(), expr_ast_id) else {
        return ExprType::Error(ExprTypeErrorKind::Unresolved);
    };

    let ctx = DbInferCtx {
        db,
        unit,
        source_file,
        ast_id_map: map,
    };

    lyra_semantic::type_infer::infer_expr_type(&node, &ctx, None)
}

/// Database-backed implementation of `InferCtx`.
struct DbInferCtx<'a> {
    db: &'a dyn salsa::Database,
    unit: CompilationUnit,
    source_file: crate::SourceFile,
    ast_id_map: &'a lyra_ast::AstIdMap,
}

impl InferCtx for DbInferCtx<'_> {
    fn type_of_name(&self, name_node: &lyra_parser::SyntaxNode) -> ExprType {
        let Some(name_ast_id) = self.ast_id_map.erased_ast_id(name_node) else {
            return ExprType::Error(ExprTypeErrorKind::Unresolved);
        };

        let resolve = resolve_index_file(self.db, self.source_file, self.unit);
        let Some(resolution) = resolve.resolutions.get(&name_ast_id) else {
            return ExprType::Error(ExprTypeErrorKind::Unresolved);
        };

        let sym_ref = SymbolRef::new(self.db, self.unit, resolution.symbol);
        let sym_type = type_of_symbol(self.db, sym_ref);
        ExprType::from_symbol_type(&sym_type)
    }

    fn symbol_type_of_name(
        &self,
        name_node: &lyra_parser::SyntaxNode,
    ) -> Option<lyra_semantic::types::SymbolType> {
        let name_ast_id = self.ast_id_map.erased_ast_id(name_node)?;
        let resolve = resolve_index_file(self.db, self.source_file, self.unit);
        let resolution = resolve.resolutions.get(&name_ast_id)?;
        let sym_ref = SymbolRef::new(self.db, self.unit, resolution.symbol);
        Some(type_of_symbol(self.db, sym_ref))
    }

    fn const_eval(&self, expr_node: &lyra_parser::SyntaxNode) -> ConstInt {
        let Some(ast_id) = self.ast_id_map.erased_ast_id(expr_node) else {
            return ConstInt::Error(lyra_semantic::types::ConstEvalError::Unsupported);
        };
        let expr_ref = ConstExprRef::new(self.db, self.unit, ast_id);
        eval_const_int(self.db, expr_ref)
    }
}
