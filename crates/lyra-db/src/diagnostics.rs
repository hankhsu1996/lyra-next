use lyra_semantic::coerce::IntegralCtx;
use lyra_semantic::type_check::{TypeCheckCtx, TypeCheckItem};
use lyra_semantic::type_infer::ExprType;
use lyra_semantic::types::SymbolType;

use crate::enum_queries::{EnumRef, enum_sem, enum_variant_index};
use crate::expr_queries::{ExprRef, IntegralCtxKey};
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
        let enum_id = lyra_semantic::enum_def::EnumId {
            file: file_id,
            owner: enum_def.owner.clone(),
            ordinal: enum_def.ordinal,
        };
        let eref = EnumRef::new(db, unit, enum_id);
        let sem = enum_sem(db, eref);
        for diag in &*sem.diags {
            let (primary_span, _) =
                crate::lower_diag::map_span_or_fallback(file_id, &pp.source_map, diag.range);
            diags.push(crate::lower_diag::lower_semantic_diag(
                diag,
                primary_span,
                &pp.source_map,
            ));
        }
    }

    // Enum variant expansion diagnostics (range bounds, collisions)
    let ev_idx = enum_variant_index(db, file, unit);
    for diag in &*ev_idx.diagnostics {
        let (primary_span, _) =
            crate::lower_diag::map_span_or_fallback(file_id, &pp.source_map, diag.range);
        diags.push(crate::lower_diag::lower_semantic_diag(
            diag,
            primary_span,
            &pp.source_map,
        ));
    }

    // Record diagnostics (type resolution errors + packed union width)
    for record_def in &*def.record_defs {
        let record_id = lyra_semantic::record::RecordId {
            file: file_id,
            owner: record_def.owner.clone(),
            ordinal: record_def.ordinal,
        };
        let rref = RecordRef::new(db, unit, record_id);
        diags.extend(record_diagnostics(db, rref).iter().cloned());
    }

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
        lower_type_check_item(db, unit, item, &pp.source_map, &mut seen, &mut diags);
    }
    diags
}

fn lower_type_check_item(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    seen: &mut std::collections::HashSet<(
        lyra_source::TextSize,
        lyra_source::TextRange,
        lyra_source::TextRange,
    )>,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    match item {
        TypeCheckItem::AssignTruncation {
            assign_range,
            lhs_range,
            rhs_range,
            lhs_width,
            rhs_width,
        } => {
            let Some(assign_span) = source_map.map_span(*assign_range) else {
                return;
            };
            let key = (assign_span.range.start(), *lhs_range, *rhs_range);
            if !seen.insert(key) {
                return;
            }
            let lhs_span = source_map.map_span(*lhs_range).unwrap_or(assign_span);
            let rhs_span = source_map.map_span(*rhs_range).unwrap_or(assign_span);
            diags.push(truncation_diag(
                *lhs_width,
                *rhs_width,
                assign_span,
                lhs_span,
                rhs_span,
            ));
        }
        TypeCheckItem::BitsNonDataType {
            call_range,
            arg_range,
        } => {
            let Some(call_span) = source_map.map_span(*call_range) else {
                return;
            };
            let arg_span = source_map.map_span(*arg_range).unwrap_or(call_span);
            diags.push(
                lyra_diag::Diagnostic::new(
                    lyra_diag::Severity::Error,
                    lyra_diag::DiagnosticCode::BITS_NON_DATA_TYPE,
                    lyra_diag::Message::simple(lyra_diag::MessageId::BitsNonDataType),
                )
                .with_label(lyra_diag::Label {
                    kind: lyra_diag::LabelKind::Primary,
                    span: arg_span,
                    message: lyra_diag::Message::simple(lyra_diag::MessageId::NotADataType),
                }),
            );
        }
        TypeCheckItem::EnumAssignFromNonEnum { .. } | TypeCheckItem::EnumAssignWrongEnum { .. } => {
            lower_enum_assign_item(db, unit, item, source_map, diags);
        }
        TypeCheckItem::ConversionArgCategory {
            call_range,
            arg_range,
            fn_name,
            expected,
        } => {
            let Some(call_span) = source_map.map_span(*call_range) else {
                return;
            };
            let arg_span = source_map.map_span(*arg_range).unwrap_or(call_span);
            let msg_args = vec![
                lyra_diag::Arg::Name(fn_name.clone()),
                lyra_diag::Arg::Name(smol_str::SmolStr::new(expected)),
            ];
            diags.push(conversion_diag(
                lyra_diag::MessageId::ConversionArgCategory,
                msg_args,
                arg_span,
            ));
        }
        TypeCheckItem::ConversionWidthMismatch {
            call_range,
            arg_range,
            fn_name,
            expected_width,
            actual_width,
        } => {
            let Some(call_span) = source_map.map_span(*call_range) else {
                return;
            };
            let arg_span = source_map.map_span(*arg_range).unwrap_or(call_span);
            let msg_args = vec![
                lyra_diag::Arg::Name(fn_name.clone()),
                lyra_diag::Arg::Width(*expected_width),
                lyra_diag::Arg::Width(*actual_width),
            ];
            diags.push(conversion_diag(
                lyra_diag::MessageId::ConversionWidthMismatch,
                msg_args,
                arg_span,
            ));
        }
    }
}

fn lower_enum_assign_item(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let (assign_range, lhs_range, rhs_range, msg_id, rhs_label, lhs_enum) = match item {
        TypeCheckItem::EnumAssignFromNonEnum {
            assign_range,
            lhs_range,
            rhs_range,
            lhs_enum,
            rhs_ty,
        } => (
            *assign_range,
            *lhs_range,
            *rhs_range,
            lyra_diag::MessageId::EnumAssignFromNonEnum,
            rhs_ty.pretty(),
            lhs_enum,
        ),
        TypeCheckItem::EnumAssignWrongEnum {
            assign_range,
            lhs_range,
            rhs_range,
            lhs_enum,
            rhs_enum,
        } => (
            *assign_range,
            *lhs_range,
            *rhs_range,
            lyra_diag::MessageId::EnumAssignWrongEnum,
            enum_name(db, unit, rhs_enum),
            lhs_enum,
        ),
        _ => return,
    };
    let Some(assign_span) = source_map.map_span(assign_range) else {
        return;
    };
    let rhs_span = source_map.map_span(rhs_range).unwrap_or(assign_span);
    let lhs_span = source_map.map_span(lhs_range).unwrap_or(assign_span);
    let lhs_name = enum_name(db, unit, lhs_enum);
    diags.push(enum_assign_diag(
        msg_id, rhs_label, lhs_name, rhs_span, lhs_span,
    ));
}

fn enum_name(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    id: &lyra_semantic::enum_def::EnumId,
) -> smol_str::SmolStr {
    let Some(sf) = source_file_by_id(db, unit, id.file) else {
        return smol_str::SmolStr::new_static("<anonymous enum>");
    };
    let def = def_index_file(db, sf);
    for enum_def in &*def.enum_defs {
        if enum_def.owner == id.owner && enum_def.ordinal == id.ordinal {
            return enum_def
                .name
                .clone()
                .unwrap_or_else(|| smol_str::SmolStr::new_static("<anonymous enum>"));
        }
    }
    smol_str::SmolStr::new_static("<anonymous enum>")
}

fn truncation_diag(
    lhs_width: u32,
    rhs_width: u32,
    assign_span: lyra_source::Span,
    lhs_span: lyra_source::Span,
    rhs_span: lyra_source::Span,
) -> lyra_diag::Diagnostic {
    let width_args = || {
        vec![
            lyra_diag::Arg::Width(lhs_width),
            lyra_diag::Arg::Width(rhs_width),
        ]
    };
    lyra_diag::Diagnostic::new(
        lyra_diag::Severity::Warning,
        lyra_diag::DiagnosticCode::WIDTH_MISMATCH,
        lyra_diag::Message::new(lyra_diag::MessageId::WidthMismatch, width_args()),
    )
    .with_label(lyra_diag::Label {
        kind: lyra_diag::LabelKind::Primary,
        span: assign_span,
        message: lyra_diag::Message::new(lyra_diag::MessageId::WidthMismatch, width_args()),
    })
    .with_label(lyra_diag::Label {
        kind: lyra_diag::LabelKind::Secondary,
        span: lhs_span,
        message: lyra_diag::Message::new(
            lyra_diag::MessageId::BitsWide,
            vec![lyra_diag::Arg::Width(lhs_width)],
        ),
    })
    .with_label(lyra_diag::Label {
        kind: lyra_diag::LabelKind::Secondary,
        span: rhs_span,
        message: lyra_diag::Message::new(
            lyra_diag::MessageId::BitsWide,
            vec![lyra_diag::Arg::Width(rhs_width)],
        ),
    })
}

fn enum_assign_diag(
    msg_id: lyra_diag::MessageId,
    rhs_label: smol_str::SmolStr,
    lhs_name: smol_str::SmolStr,
    rhs_span: lyra_source::Span,
    lhs_span: lyra_source::Span,
) -> lyra_diag::Diagnostic {
    lyra_diag::Diagnostic::new(
        lyra_diag::Severity::Error,
        lyra_diag::DiagnosticCode::ENUM_ASSIGN_INCOMPAT,
        lyra_diag::Message::new(
            msg_id,
            vec![
                lyra_diag::Arg::Name(rhs_label.clone()),
                lyra_diag::Arg::Name(lhs_name.clone()),
            ],
        ),
    )
    .with_label(lyra_diag::Label {
        kind: lyra_diag::LabelKind::Primary,
        span: rhs_span,
        message: lyra_diag::Message::new(
            msg_id,
            vec![
                lyra_diag::Arg::Name(rhs_label),
                lyra_diag::Arg::Name(lhs_name.clone()),
            ],
        ),
    })
    .with_label(lyra_diag::Label {
        kind: lyra_diag::LabelKind::Secondary,
        span: lhs_span,
        message: lyra_diag::Message::new(
            lyra_diag::MessageId::EnumTypeHere,
            vec![lyra_diag::Arg::Name(lhs_name)],
        ),
    })
}

fn conversion_diag(
    msg_id: lyra_diag::MessageId,
    msg_args: Vec<lyra_diag::Arg>,
    arg_span: lyra_source::Span,
) -> lyra_diag::Diagnostic {
    lyra_diag::Diagnostic::new(
        lyra_diag::Severity::Error,
        lyra_diag::DiagnosticCode::CONVERSION_ARG_TYPE,
        lyra_diag::Message::new(msg_id, msg_args.clone()),
    )
    .with_label(lyra_diag::Label {
        kind: lyra_diag::LabelKind::Primary,
        span: arg_span,
        message: lyra_diag::Message::new(msg_id, msg_args),
    })
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
            return ExprType::error(lyra_semantic::type_infer::ExprTypeErrorKind::Unresolved);
        };
        let expr_ref = ExprRef::new(self.db, self.unit, ast_id);
        crate::expr_queries::type_of_expr(self.db, expr_ref)
    }

    fn expr_type_in_ctx(&self, node: &lyra_parser::SyntaxNode, ctx: &IntegralCtx) -> ExprType {
        let Some(ast_id) = self.ast_id_map.erased_ast_id(node) else {
            return ExprType::error(lyra_semantic::type_infer::ExprTypeErrorKind::Unresolved);
        };
        let expr_ref = ExprRef::new(self.db, self.unit, ast_id);
        let ctx_key = IntegralCtxKey::new(self.db, ctx.width, ctx.signed, ctx.four_state);
        crate::expr_queries::type_of_expr_in_ctx(self.db, expr_ref, ctx_key)
    }

    fn expr_type_stmt(&self, node: &lyra_parser::SyntaxNode) -> ExprType {
        let Some(ast_id) = self.ast_id_map.erased_ast_id(node) else {
            return ExprType::error(lyra_semantic::type_infer::ExprTypeErrorKind::Unresolved);
        };
        let expr_ref = ExprRef::new(self.db, self.unit, ast_id);
        crate::expr_queries::type_of_expr_stmt(self.db, expr_ref)
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

    fn resolve_type_arg(
        &self,
        name_node: &lyra_parser::SyntaxNode,
    ) -> Option<lyra_semantic::types::Ty> {
        crate::resolve_helpers::resolve_type_arg_impl(
            self.db,
            self.unit,
            self.source_file,
            self.ast_id_map,
            name_node,
        )
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
