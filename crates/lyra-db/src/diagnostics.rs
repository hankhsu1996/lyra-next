use lyra_ast::{AstIdMap, AstNode, Expr, HasSyntax};
use lyra_semantic::coerce::IntegralCtx;
use lyra_semantic::type_check::TypeCheckCtx;
use lyra_semantic::type_infer::ExprType;
use lyra_semantic::types::SymbolType;

use crate::checks_index::{CheckKind, checks_index};
use crate::enum_queries::{EnumRef, enum_sem, enum_variant_index};
use crate::expr_queries::{ExprRef, IntegralCtxKey};
use crate::facts::modport::field_access_facts;
use crate::pipeline::ast_id_map as query_ast_id_map;
use crate::pipeline::{ast_id_map, parse_file, preprocess_file};
use crate::record_queries::{RecordRef, record_diagnostics};
use crate::semantic::{
    def_index_file, global_def_index, import_conflicts_file, resolve_core_file, resolve_index_file,
};
use crate::type_queries::{SymbolRef, type_of_symbol};
use crate::{CompilationUnit, SourceFile, source_file_by_id};

/// Convert parse, preprocess, and semantic errors into structured diagnostics (Salsa-cached).
#[salsa::tracked(return_ref)]
pub fn file_diagnostics(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> Vec<lyra_diag::Diagnostic> {
    let pp = preprocess_file(db, file);
    let parse = parse_file(db, file);
    let def = def_index_file(db, file);
    let resolve = resolve_index_file(db, file, unit);
    let conflicts = import_conflicts_file(db, file, unit);
    let core = resolve_core_file(db, file, unit);
    let mut diags =
        crate::lower_diag::lower_file_diagnostics(file.file_id(db), pp, parse, def, resolve);
    diags.extend(crate::lower_diag::lower_import_conflicts(
        file.file_id(db),
        pp,
        def,
        conflicts,
    ));
    diags.extend(crate::lower_diag::lower_wildcard_local_conflicts(
        file.file_id(db),
        pp,
        def,
        core,
    ));
    diags.extend(type_diagnostics(db, file, unit).iter().cloned());

    // Enum base type diagnostics
    let file_id = file.file_id(db);
    for enum_def in &*def.enum_defs {
        let enum_id = lyra_semantic::enum_def::EnumId::new(enum_def.enum_type_site);
        let eref = EnumRef::new(db, unit, enum_id);
        let sem = enum_sem(db, eref);
        for diag in &*sem.diags {
            let chosen = crate::lower_diag::choose_best_diag_span(diag.primary, diag.label);
            let (primary_span, _) =
                crate::lower_diag::map_span_or_fallback(file_id, &pp.source_map, chosen);
            diags.push(crate::lower_diag::lower_semantic_diag(
                diag,
                primary_span,
                &pp.source_map,
            ));
        }
        if !sem.value_diags.is_empty() {
            let ast_map = query_ast_id_map(db, file);
            for vd in &*sem.value_diags {
                if let Some(d) = crate::lower_type_check::lower_enum_value_diag(
                    vd,
                    file_id,
                    ast_map,
                    &pp.source_map,
                ) {
                    diags.push(d);
                }
            }
        }
    }

    // Enum variant expansion diagnostics (range bounds, collisions)
    let ev_idx = enum_variant_index(db, file, unit);
    for diag in &*ev_idx.diagnostics {
        let chosen = crate::lower_diag::choose_best_diag_span(diag.primary, diag.label);
        let (primary_span, _) =
            crate::lower_diag::map_span_or_fallback(file_id, &pp.source_map, chosen);
        diags.push(crate::lower_diag::lower_semantic_diag(
            diag,
            primary_span,
            &pp.source_map,
        ));
    }

    // Record diagnostics (type resolution errors + packed union width)
    for record_def in &*def.record_defs {
        let record_id = lyra_semantic::record::RecordId::new(record_def.record_type_site);
        let rref = RecordRef::new(db, unit, record_id);
        diags.extend(record_diagnostics(db, rref).iter().cloned());
    }

    // Type-extraction internal errors (MissingSite in normalized types)
    for (sym_id, _sym) in def.symbols.iter() {
        let gsym = lyra_semantic::symbols::GlobalSymbolId {
            file: file_id,
            local: sym_id,
        };
        let sym_ref = SymbolRef::new(db, unit, gsym);
        let errors = crate::type_queries::type_of_symbol_internal_errors(db, sym_ref);
        for fact in errors {
            if let Some(span) = pp.source_map.map_span(fact.site.text_range()) {
                diags.push(crate::lower_diag::internal_error_diag(&fact.detail, span));
            }
        }
    }

    diags
}

/// Per-file type-check diagnostics (Salsa-cached).
///
/// Iterates the `ChecksIndex` for this file and dispatches each entry
/// to the appropriate pure `check_*` function in `lyra-semantic`.
#[salsa::tracked(return_ref)]
pub fn type_diagnostics(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> Vec<lyra_diag::Diagnostic> {
    let parse = parse_file(db, file);
    let map = ast_id_map(db, file);
    let pp = preprocess_file(db, file);
    let index = checks_index(db, file);
    let facts = field_access_facts(db, file, unit);

    let ctx = DbTypeCheckCtx {
        db,
        unit,
        source_file: file,
        ast_id_map: map,
    };

    let root = parse.syntax();
    let mut items = Vec::new();
    for entry in &*index.entries {
        let Some(node) = map.get_node(&root, entry.site) else {
            continue;
        };
        let fallback = entry.site;
        match entry.kind {
            CheckKind::ContinuousAssign => {
                if let Some(ca) = lyra_ast::ContinuousAssign::cast(node) {
                    lyra_semantic::type_check::check_continuous_assign(
                        &ca, &ctx, fallback, &mut items,
                    );
                }
            }
            CheckKind::AssignStmt => {
                if let Some(assign) = lyra_ast::AssignStmt::cast(node) {
                    lyra_semantic::type_check::check_assign_stmt(
                        &assign, &ctx, fallback, &mut items,
                    );
                }
            }
            CheckKind::VarDecl => {
                if let Some(vd) = lyra_ast::VarDecl::cast(node) {
                    lyra_semantic::type_check::check_var_decl(&vd, &ctx, fallback, &mut items);
                }
            }
            CheckKind::SystemTfCall => {
                if let Some(stf) = lyra_ast::SystemTfCall::cast(node) {
                    lyra_semantic::type_check::check_system_call(&stf, &ctx, fallback, &mut items);
                }
            }
            CheckKind::FieldExpr => {
                if let Some(field) = lyra_ast::FieldExpr::cast(node) {
                    lyra_semantic::type_check::check_field_direction(
                        &field,
                        &ctx,
                        facts,
                        entry.access,
                        &mut items,
                    );
                }
            }
            CheckKind::CastExpr => {
                if let Some(cast) = lyra_ast::CastExpr::cast(node) {
                    lyra_semantic::type_check::check_cast_expr(&cast, &ctx, fallback, &mut items);
                }
            }
            CheckKind::StreamOperandItem => {
                if let Some(soi) = lyra_ast::StreamOperandItem::cast(node) {
                    lyra_semantic::type_check::check_stream_operand(
                        &soi, &ctx, fallback, &mut items,
                    );
                }
            }
            CheckKind::CallExpr => {
                if let Some(call) = lyra_ast::CallExpr::cast(node) {
                    lyra_semantic::type_check::check_method_call(&call, &ctx, &mut items);
                }
            }
        }
    }

    let mut seen = std::collections::HashSet::new();
    let mut diags = Vec::new();
    for item in &items {
        crate::lower_type_check::lower_type_check_item(
            db,
            unit,
            item,
            &pp.source_map,
            &mut seen,
            &mut diags,
        );
    }
    diags
}

struct DbTypeCheckCtx<'a> {
    db: &'a dyn salsa::Database,
    unit: CompilationUnit,
    source_file: SourceFile,
    ast_id_map: &'a lyra_ast::AstIdMap,
}

impl TypeCheckCtx for DbTypeCheckCtx<'_> {
    fn file_id(&self) -> lyra_source::FileId {
        self.source_file.file_id(self.db)
    }

    fn ast_id_map(&self) -> &AstIdMap {
        self.ast_id_map
    }

    fn expr_type(&self, expr: &Expr) -> ExprType {
        let Some(ast_id) = self.ast_id_map.erased_ast_id(expr.syntax()) else {
            return ExprType::error(lyra_semantic::type_infer::ExprTypeErrorKind::Unresolved);
        };
        let expr_ref = ExprRef::new(self.db, self.unit, ast_id);
        crate::expr_queries::type_of_expr(self.db, expr_ref)
    }

    fn expr_type_in_ctx(&self, expr: &Expr, ctx: &IntegralCtx) -> ExprType {
        let Some(ast_id) = self.ast_id_map.erased_ast_id(expr.syntax()) else {
            return ExprType::error(lyra_semantic::type_infer::ExprTypeErrorKind::Unresolved);
        };
        let expr_ref = ExprRef::new(self.db, self.unit, ast_id);
        let ctx_key = IntegralCtxKey::new(self.db, ctx.width, ctx.signed, ctx.four_state);
        crate::expr_queries::type_of_expr_in_ctx(self.db, expr_ref, ctx_key)
    }

    fn expr_type_stmt(&self, expr: &Expr) -> ExprType {
        let Some(ast_id) = self.ast_id_map.erased_ast_id(expr.syntax()) else {
            return ExprType::error(lyra_semantic::type_infer::ExprTypeErrorKind::Unresolved);
        };
        let expr_ref = ExprRef::new(self.db, self.unit, ast_id);
        crate::expr_queries::type_of_expr_stmt(self.db, expr_ref)
    }

    fn expr_type_with_expected(
        &self,
        expr: &Expr,
        expected: &lyra_semantic::types::Ty,
    ) -> ExprType {
        crate::expr_queries::infer_expr_with_expected_inline(
            self.db,
            self.unit,
            self.source_file,
            self.ast_id_map,
            expr,
            expected,
        )
    }

    fn symbol_type_of_declarator(&self, declarator: &lyra_ast::Declarator) -> Option<SymbolType> {
        let ast_id = self.ast_id_map.id_of(declarator)?;
        let def = def_index_file(self.db, self.source_file);
        let sym_id = def.name_site_to_symbol.get(&ast_id).copied()?;
        let gsym = lyra_semantic::symbols::GlobalSymbolId {
            file: self.source_file.file_id(self.db),
            local: sym_id,
        };
        let sym_ref = SymbolRef::new(self.db, self.unit, gsym);
        Some(type_of_symbol(self.db, sym_ref))
    }

    fn resolve_type_arg(
        &self,
        utr: &lyra_semantic::UserTypeRef,
    ) -> Option<lyra_semantic::types::Ty> {
        crate::resolve_helpers::resolve_type_arg_impl(
            self.db,
            self.unit,
            self.source_file,
            self.ast_id_map,
            utr,
        )
    }

    fn const_eval_int(&self, expr: &Expr) -> Option<i64> {
        let ast_id = self.ast_id_map.erased_ast_id(expr.syntax())?;
        let expr_ref = crate::const_eval::ConstExprRef::new(self.db, self.unit, ast_id);
        match crate::const_eval::eval_const_int(self.db, expr_ref) {
            lyra_semantic::types::ConstInt::Known(v) => Some(v),
            _ => None,
        }
    }

    fn enum_known_value_set(
        &self,
        id: &lyra_semantic::enum_def::EnumId,
    ) -> Option<std::sync::Arc<[i64]>> {
        let eref = EnumRef::new(self.db, self.unit, *id);
        crate::enum_queries::enum_known_value_set(self.db, eref)
    }

    fn is_modport_target_lvalue(&self, expr_id: lyra_ast::ErasedAstId) -> bool {
        let file_id = expr_id.file();
        let Some(src) = source_file_by_id(self.db, self.unit, file_id) else {
            return false;
        };
        expr_is_assignable_ref(self.db, src, expr_id)
    }
}

/// Check whether a modport expression target is an assignable reference.
/// Modport ports require a concrete variable, not a concat or stream.
fn expr_is_assignable_ref(
    db: &dyn salsa::Database,
    file: SourceFile,
    expr_id: lyra_ast::ErasedAstId,
) -> bool {
    let parse = parse_file(db, file);
    let map = ast_id_map(db, file);
    let Some(node) = map.get_node(&parse.syntax(), expr_id) else {
        return false;
    };
    let Some(expr) = Expr::cast(node) else {
        return false;
    };
    lyra_semantic::lhs::is_assignable_ref(&expr)
}

/// Unit-level diagnostics: duplicate definitions in the definitions namespace.
///
/// Walks `GlobalDefIndex.definitions()`, finds adjacent entries with the
/// same name, and emits one diagnostic per duplicate. Catches module/module,
/// package/package, and module/package name collisions.
#[salsa::tracked(return_ref)]
pub fn unit_diagnostics(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
) -> Box<[lyra_diag::Diagnostic]> {
    let global = global_def_index(db, unit);
    let defs = global.definitions();
    let mut diags = Vec::new();

    let mut i = 0;
    while i < defs.len() {
        let (name, _, _) = &defs[i];
        let mut j = i + 1;
        while j < defs.len() && defs[j].0 == *name {
            j += 1;
        }
        if j - i > 1 {
            // Duplicate group: emit diagnostics for entries [i+1..j]
            for (_, dup_def_id, _) in &defs[(i + 1)..j] {
                let dup_file_id = dup_def_id.file();
                if let Some(dup_file) = source_file_by_id(db, unit, dup_file_id) {
                    let dup_def = def_index_file(db, dup_file);
                    if let Some(entry) = dup_def.def_entry(*dup_def_id) {
                        let pp = preprocess_file(db, dup_file);
                        let label_range = entry.name_span.text_range();
                        if let Some(span) = pp.source_map.map_span(label_range) {
                            diags.push(
                                lyra_diag::Diagnostic::new(
                                    lyra_diag::Severity::Error,
                                    lyra_diag::DiagnosticCode::DUPLICATE_DEFINITION,
                                    lyra_diag::Message::new(
                                        lyra_diag::MessageId::DuplicateDefinitionInUnit,
                                        vec![lyra_diag::Arg::Name(name.clone())],
                                    ),
                                )
                                .with_label(lyra_diag::Label {
                                    kind: lyra_diag::LabelKind::Primary,
                                    span,
                                    message: lyra_diag::Message::simple(
                                        lyra_diag::MessageId::RedefinedHere,
                                    ),
                                }),
                            );
                        }
                    }
                }
            }
        }
        i = j;
    }

    diags.into_boxed_slice()
}
