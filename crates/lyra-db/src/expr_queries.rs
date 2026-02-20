use lyra_semantic::coerce::IntegralCtx;
use lyra_semantic::symbols::{GlobalSymbolId, SymbolKind};
use lyra_semantic::type_infer::{
    CallableKind, CallablePort, CallableSigRef, ExprType, ExprTypeErrorKind, InferCtx,
    ResolveCallableError, Signedness,
};
use lyra_semantic::types::ConstInt;

use crate::callable_queries::{CallableRef, callable_signature};
use crate::const_eval::{ConstExprRef, eval_const_int};
use crate::module_sig::CallableKind as DbCallableKind;
use crate::pipeline::{ast_id_map, parse_file};
use crate::semantic::{def_index_file, resolve_index_file};
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

/// Salsa-interned key for an integral context (width, signedness, four-state).
#[salsa::interned]
pub struct IntegralCtxKey<'db> {
    pub width: Option<u32>,
    pub signed: Signedness,
    pub four_state: bool,
}

impl IntegralCtxKey<'_> {
    pub fn to_ctx(&self, db: &dyn salsa::Database) -> IntegralCtx {
        IntegralCtx {
            width: self.width(db),
            signed: self.signed(db),
            four_state: self.four_state(db),
        }
    }
}

/// Infer the type of an expression under a given integral context (Salsa-tracked).
#[salsa::tracked]
pub fn type_of_expr_in_ctx<'db>(
    db: &'db dyn salsa::Database,
    expr_ref: ExprRef<'db>,
    ctx_key: IntegralCtxKey<'db>,
) -> ExprType {
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

    let integral_ctx = ctx_key.to_ctx(db);
    lyra_semantic::type_infer::infer_expr_type(&node, &ctx, Some(&integral_ctx))
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

    fn resolve_callable(
        &self,
        callee_node: &lyra_parser::SyntaxNode,
    ) -> Result<GlobalSymbolId, ResolveCallableError> {
        let Some(ast_id) = self.ast_id_map.erased_ast_id(callee_node) else {
            return Err(ResolveCallableError::NotFound);
        };

        let resolve = resolve_index_file(self.db, self.source_file, self.unit);
        let Some(res) = resolve.resolutions.get(&ast_id) else {
            return Err(ResolveCallableError::NotFound);
        };

        let target_id = res.symbol;

        // Check that the resolved symbol is actually a function or task
        let Some(target_file) = source_file_by_id(self.db, self.unit, target_id.file) else {
            return Err(ResolveCallableError::NotFound);
        };
        let target_def = def_index_file(self.db, target_file);
        let target_info = target_def.symbols.get(target_id.local);

        match target_info.kind {
            SymbolKind::Function | SymbolKind::Task => Ok(target_id),
            other => Err(ResolveCallableError::NotACallable(other)),
        }
    }

    fn callable_sig(&self, sym: GlobalSymbolId) -> Option<CallableSigRef> {
        let callable_ref = CallableRef::new(self.db, self.unit, sym);
        let sig = callable_signature(self.db, callable_ref);
        let kind = match sig.kind {
            DbCallableKind::Function => CallableKind::Function,
            DbCallableKind::Task => CallableKind::Task,
        };
        let eval = |expr_ast_id: lyra_ast::ErasedAstId| -> ConstInt {
            let expr_ref = ConstExprRef::new(self.db, self.unit, expr_ast_id);
            eval_const_int(self.db, expr_ref)
        };
        let normalize = |ty: &lyra_semantic::types::Ty| -> lyra_semantic::types::Ty {
            lyra_semantic::normalize_ty(ty, &eval)
        };
        let ports: Vec<CallablePort> = sig
            .ports
            .iter()
            .map(|p| CallablePort {
                name: p.name.clone(),
                ty: normalize(&p.ty),
                has_default: p.has_default,
            })
            .collect();
        Some(CallableSigRef {
            kind,
            return_ty: normalize(&sig.return_ty),
            ports: ports.into(),
        })
    }
}
