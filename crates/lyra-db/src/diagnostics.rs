use lyra_semantic::coerce::IntegralCtx;
use lyra_semantic::type_check::{TypeCheckCtx, TypeCheckKind};
use lyra_semantic::type_infer::ExprType;
use lyra_semantic::types::SymbolType;

use crate::expr_queries::{ExprRef, IntegralCtxKey};
use crate::pipeline::{ast_id_map, parse_file, preprocess_file};
use crate::semantic::{
    def_index_file, global_def_index, import_conflicts_file, resolve_index_file,
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
    let mut diags =
        crate::lower_diag::lower_file_diagnostics(file.file_id(db), pp, parse, def, resolve);
    diags.extend(crate::lower_diag::lower_import_conflicts(
        file.file_id(db),
        pp,
        def,
        conflicts,
    ));
    diags.extend(type_diagnostics(db, file, unit).iter().cloned());
    diags
}

/// Per-file type-check diagnostics (Salsa-cached).
#[salsa::tracked(return_ref)]
pub fn type_diagnostics(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> Vec<lyra_diag::Diagnostic> {
    let parse = parse_file(db, file);
    let map = ast_id_map(db, file);
    let pp = preprocess_file(db, file);

    let ctx = DbTypeCheckCtx {
        db,
        unit,
        source_file: file,
        ast_id_map: map,
    };

    let items = lyra_semantic::type_check::check_types(&parse.syntax(), &ctx);

    let mut seen = std::collections::HashSet::new();
    let mut diags = Vec::new();
    for item in &items {
        match &item.kind {
            TypeCheckKind::Truncation {
                lhs_width,
                rhs_width,
            } => {
                let Some(assign_span) = pp.source_map.map_span(item.assign_range) else {
                    continue;
                };
                // Dedup by (assign start, lhs range, rhs range, code) to prevent
                // duplicate diagnostics if future walker paths overlap.
                let key = (assign_span.range.start(), item.lhs_range, item.rhs_range);
                if !seen.insert(key) {
                    continue;
                }
                let lhs_span = pp
                    .source_map
                    .map_span(item.lhs_range)
                    .unwrap_or(assign_span);
                let rhs_span = pp
                    .source_map
                    .map_span(item.rhs_range)
                    .unwrap_or(assign_span);

                diags.push(
                    lyra_diag::Diagnostic::new(
                        lyra_diag::Severity::Warning,
                        lyra_diag::DiagnosticCode::WIDTH_MISMATCH,
                        lyra_diag::Message::new(
                            lyra_diag::MessageId::WidthMismatch,
                            vec![
                                lyra_diag::Arg::Width(*lhs_width),
                                lyra_diag::Arg::Width(*rhs_width),
                            ],
                        ),
                    )
                    .with_label(lyra_diag::Label {
                        kind: lyra_diag::LabelKind::Primary,
                        span: assign_span,
                        message: lyra_diag::Message::new(
                            lyra_diag::MessageId::WidthMismatch,
                            vec![
                                lyra_diag::Arg::Width(*lhs_width),
                                lyra_diag::Arg::Width(*rhs_width),
                            ],
                        ),
                    })
                    .with_label(lyra_diag::Label {
                        kind: lyra_diag::LabelKind::Secondary,
                        span: lhs_span,
                        message: lyra_diag::Message::new(
                            lyra_diag::MessageId::BitsWide,
                            vec![lyra_diag::Arg::Width(*lhs_width)],
                        ),
                    })
                    .with_label(lyra_diag::Label {
                        kind: lyra_diag::LabelKind::Secondary,
                        span: rhs_span,
                        message: lyra_diag::Message::new(
                            lyra_diag::MessageId::BitsWide,
                            vec![lyra_diag::Arg::Width(*rhs_width)],
                        ),
                    }),
                );
            }
        }
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
    fn expr_type(&self, node: &lyra_parser::SyntaxNode) -> ExprType {
        let Some(ast_id) = self.ast_id_map.erased_ast_id(node) else {
            return ExprType::Error(lyra_semantic::type_infer::ExprTypeErrorKind::Unresolved);
        };
        let expr_ref = ExprRef::new(self.db, self.unit, ast_id);
        crate::expr_queries::type_of_expr(self.db, expr_ref)
    }

    fn expr_type_in_ctx(&self, node: &lyra_parser::SyntaxNode, ctx: &IntegralCtx) -> ExprType {
        let Some(ast_id) = self.ast_id_map.erased_ast_id(node) else {
            return ExprType::Error(lyra_semantic::type_infer::ExprTypeErrorKind::Unresolved);
        };
        let expr_ref = ExprRef::new(self.db, self.unit, ast_id);
        let ctx_key = IntegralCtxKey::new(self.db, ctx.width, ctx.signed, ctx.four_state);
        crate::expr_queries::type_of_expr_in_ctx(self.db, expr_ref, ctx_key)
    }

    fn symbol_type_of_declarator(
        &self,
        declarator: &lyra_parser::SyntaxNode,
    ) -> Option<SymbolType> {
        let ast_id = self.ast_id_map.erased_ast_id(declarator)?;
        let def = def_index_file(self.db, self.source_file);
        let sym_id = def.decl_to_symbol.get(&ast_id).copied()?;
        let gsym = lyra_semantic::symbols::GlobalSymbolId {
            file: self.source_file.file_id(self.db),
            local: sym_id,
        };
        let sym_ref = SymbolRef::new(self.db, self.unit, gsym);
        Some(type_of_symbol(self.db, sym_ref))
    }
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
                    if let Some(&sym_id) = dup_def.decl_to_symbol.get(&dup_def_id.ast_id()) {
                        let sym = dup_def.symbols.get(sym_id);
                        let pp = preprocess_file(db, dup_file);
                        if let Some(span) = pp.source_map.map_span(sym.def_range) {
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
