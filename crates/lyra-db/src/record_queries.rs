use lyra_semantic::def_index::ExpectedNs;
use lyra_semantic::diagnostic::{SemanticDiag, SemanticDiagKind};
use lyra_semantic::modport_def::ModportDefId;
use lyra_semantic::modport_def::ModportTarget;
use lyra_semantic::record::{FieldSem, Packing, RecordId, RecordKind, RecordSem, TypeRef};
use lyra_semantic::symbols::Namespace;
use lyra_semantic::types::{ConstInt, ModportView, ModportViewEntry, ModportViewTarget, Ty};
use smol_str::SmolStr;

use crate::const_eval::{ConstExprRef, eval_const_int};
use crate::pipeline::preprocess_file;
use crate::semantic::{
    compilation_unit_env, def_index_file, global_def_index, name_graph_file, package_scope_index,
    symbol_at_name_ast,
};
use crate::ty_resolve::{FieldTyError, FieldTyErrorKind, classify_for_record_field};
use crate::type_queries::{TyRef, bit_width_total};
use crate::{CompilationUnit, source_file_by_id};

/// Identifies a record (struct/union) for semantic resolution.
#[salsa::interned]
pub struct RecordRef<'db> {
    pub unit: CompilationUnit,
    pub record_id: RecordId,
}

/// A lowered field type: raw Ty (may contain Unevaluated dims) plus optional error.
///
/// Diagnostic-metadata-free: no ranges, no formatted strings. Salsa backdates
/// when only spans change in the source, keeping `bit_width_total` stable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoweredFieldTy {
    pub ty: Ty,
    pub(crate) err: Option<FieldTyError>,
}

/// A raw field: name + unevaluated type (no const-eval normalization).
///
/// Used by `type_of_expr_raw` for const-eval-safe record member lookup
/// without depending on `record_sem` (which runs const-eval for dim normalization).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RawField {
    pub name: SmolStr,
    pub ty: Ty,
}

/// Raw record fields: names zipped with lowered types (Salsa-tracked).
///
/// Reads `record_field_tys` for raw lowered types and pairs them with
/// field names from `RecordDef`. No const-eval normalization.
#[salsa::tracked]
pub fn record_fields_raw<'db>(
    db: &'db dyn salsa::Database,
    rref: RecordRef<'db>,
) -> Box<[RawField]> {
    let unit = rref.unit(db);
    let record_id = rref.record_id(db);
    let file_id = record_id.file();

    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return Box::new([]);
    };

    let lowered = record_field_tys(db, rref);
    let def = def_index_file(db, source_file);

    let Some(record_def) = def.record_def_by_id(record_id) else {
        return Box::new([]);
    };

    record_def
        .fields
        .iter()
        .zip(lowered.iter())
        .map(|(f, lt)| RawField {
            name: f.name.clone(),
            ty: lt.ty.clone(),
        })
        .collect()
}

/// Pure type lowering for record fields (Salsa-tracked).
///
/// Resolves each field's `TypeRef` to a raw `Ty` without const-eval or
/// normalization. Returns compact, diagnostic-metadata-free output suitable
/// for backdating.
#[salsa::tracked]
pub fn record_field_tys<'db>(
    db: &'db dyn salsa::Database,
    rref: RecordRef<'db>,
) -> Box<[LoweredFieldTy]> {
    let unit = rref.unit(db);
    let record_id = rref.record_id(db);
    let file_id = record_id.file();

    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return Box::new([]);
    };

    let def = def_index_file(db, source_file);

    let Some(record_def) = def.record_def_by_id(record_id) else {
        return Box::new([]);
    };

    let graph = name_graph_file(db, source_file);
    let global = global_def_index(db, unit);
    let pkg_scope = package_scope_index(db, unit);
    let cu_env = compilation_unit_env(db, unit);

    let mut result = Vec::with_capacity(record_def.fields.len());

    for field in &*record_def.fields {
        let lowered = match &field.ty {
            TypeRef::Resolved(ty) => LoweredFieldTy {
                ty: ty.clone(),
                err: None,
            },
            TypeRef::Named { name, span } => {
                let resolve_result = lyra_semantic::resolve_name_in_scope(
                    graph,
                    global,
                    pkg_scope,
                    cu_env,
                    record_def.scope,
                    name,
                    ExpectedNs::TypeThenValue,
                );
                let path = Box::new([name.clone()]) as Box<[SmolStr]>;
                let (ty, err) = classify_for_record_field(db, unit, file_id, &resolve_result, path);
                // Preserve span file for FieldSem construction downstream
                let _ = span;
                LoweredFieldTy { ty, err }
            }
            TypeRef::Qualified { segments, span } => {
                let resolve_result = lyra_semantic::resolve_qualified_name(
                    segments,
                    global,
                    pkg_scope,
                    ExpectedNs::TypeThenValue,
                );
                let path = segments.clone();
                let (ty, err) = classify_for_record_field(db, unit, file_id, &resolve_result, path);
                let _ = span;
                LoweredFieldTy { ty, err }
            }
        };
        result.push(lowered);
    }

    result.into_boxed_slice()
}

/// Resolve a record's field types (Salsa-tracked).
///
/// Reads `record_field_tys` for raw lowered types, normalizes dims via
/// const-eval, and builds the sorted field lookup table. Type resolution
/// errors are converted to `SemanticDiag` using spans from `RecordDef`.
#[salsa::tracked]
pub fn record_sem<'db>(db: &'db dyn salsa::Database, rref: RecordRef<'db>) -> RecordSem {
    let unit = rref.unit(db);
    let record_id = rref.record_id(db);
    let file_id = record_id.file();

    let lowered_tys = record_field_tys(db, rref);

    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return empty_record_sem();
    };

    let def = def_index_file(db, source_file);

    let Some(record_def) = def.record_def_by_id(record_id) else {
        return empty_record_sem();
    };

    let eval = |expr_ast_id: lyra_ast::ErasedAstId| -> ConstInt {
        let expr_ref = ConstExprRef::new(db, unit, expr_ast_id);
        eval_const_int(db, expr_ref)
    };

    let mut fields = Vec::new();
    let mut diags = Vec::new();

    for (i, field) in record_def.fields.iter().enumerate() {
        let lowered = &lowered_tys[i];

        // Normalize unevaluated dims
        let ty = lyra_semantic::normalize_ty(&lowered.ty, &eval);

        // Convert field type errors to SemanticDiag using spans from RecordDef
        if let Some(err) = &lowered.err {
            let range = match &field.ty {
                TypeRef::Named { span, .. } | TypeRef::Qualified { span, .. } => span.range,
                TypeRef::Resolved(_) => lyra_source::TextRange::default(),
            };
            let display_name = err.path.join("::");
            match err.kind {
                FieldTyErrorKind::UndeclaredType => {
                    diags.push(SemanticDiag {
                        kind: SemanticDiagKind::UndeclaredType {
                            name: SmolStr::new(&display_name),
                        },
                        range,
                    });
                }
                FieldTyErrorKind::NotAType => {
                    diags.push(SemanticDiag {
                        kind: SemanticDiagKind::NotAType {
                            name: SmolStr::new(&display_name),
                        },
                        range,
                    });
                }
            }
        }

        let span = match &field.ty {
            TypeRef::Named { span, .. } | TypeRef::Qualified { span, .. } => *span,
            TypeRef::Resolved(_) => lyra_source::Span {
                file: file_id,
                range: lyra_source::TextRange::default(),
            },
        };

        fields.push(FieldSem {
            name: field.name.clone(),
            ty,
            span,
        });
    }

    // Build sorted lookup table
    let mut lookup: Vec<(SmolStr, u32)> = fields
        .iter()
        .enumerate()
        .map(|(i, f)| (f.name.clone(), i as u32))
        .collect();
    lookup.sort_by(|(a, _), (b, _)| a.cmp(b));

    RecordSem {
        fields: fields.into_boxed_slice(),
        field_lookup: lookup.into_boxed_slice(),
        diags: diags.into_boxed_slice(),
    }
}

fn empty_record_sem() -> RecordSem {
    RecordSem {
        fields: Box::new([]),
        field_lookup: Box::new([]),
        diags: Box::new([]),
    }
}

/// Per-record diagnostics: type resolution errors + packed union width validation.
#[salsa::tracked(return_ref)]
pub fn record_diagnostics<'db>(
    db: &'db dyn salsa::Database,
    rref: RecordRef<'db>,
) -> Box<[lyra_diag::Diagnostic]> {
    let unit = rref.unit(db);
    let record_id = rref.record_id(db);
    let file_id = record_id.file();

    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return Box::new([]);
    };

    let pp = preprocess_file(db, source_file);
    let sem = record_sem(db, rref);

    // Lower record_sem.diags
    let mut diags = Vec::new();
    for diag in &*sem.diags {
        let (primary_span, _) =
            crate::lower_diag::map_span_or_fallback(file_id, &pp.source_map, diag.range);
        diags.push(crate::lower_diag::lower_semantic_diag(
            diag,
            primary_span,
            &pp.source_map,
        ));
    }

    // Packed union width validation
    let def = def_index_file(db, source_file);
    let record_def = def.record_def_by_id(record_id);
    if let Some(record_def) = record_def
        && record_def.kind == RecordKind::Union
        && record_def.packing == Packing::Packed
    {
        check_packed_union_widths(db, unit, source_file, &sem, record_def, pp, &mut diags);
    }

    diags.into_boxed_slice()
}

fn check_packed_union_widths(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    source_file: crate::SourceFile,
    sem: &RecordSem,
    record_def: &lyra_semantic::record::RecordDef,
    pp: &lyra_preprocess::PreprocOutput,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let file_id = source_file.file_id(db);
    let widths: Vec<Option<u32>> = sem
        .fields
        .iter()
        .map(|f| {
            let ty_ref = TyRef::new(db, f.ty.clone());
            bit_width_total(db, unit, ty_ref)
        })
        .collect();

    // Find first field with known width as reference
    let mut ref_width: Option<u32> = None;
    let mut ref_idx: usize = 0;
    for (i, w) in widths.iter().enumerate() {
        if let Some(w) = w {
            ref_width = Some(*w);
            ref_idx = i;
            break;
        }
    }

    let Some(expected_w) = ref_width else {
        return;
    };

    for (i, w) in widths.iter().enumerate() {
        if i == ref_idx {
            continue;
        }
        let Some(actual_w) = w else {
            continue;
        };
        if *actual_w == expected_w {
            continue;
        }

        let field_name = &sem.fields[i].name;
        let mismatch_range = record_def.fields[i]
            .name_span
            .map_or(lyra_source::TextRange::default(), |ns| ns.text_range());
        let ref_range = record_def.fields[ref_idx]
            .name_span
            .map_or(lyra_source::TextRange::default(), |ns| ns.text_range());

        let mismatch_span = pp
            .source_map
            .map_span(mismatch_range)
            .unwrap_or(lyra_source::Span {
                file: file_id,
                range: lyra_source::TextRange::default(),
            });
        let ref_span = pp
            .source_map
            .map_span(ref_range)
            .unwrap_or(lyra_source::Span {
                file: file_id,
                range: lyra_source::TextRange::default(),
            });

        diags.push(
            lyra_diag::Diagnostic::new(
                lyra_diag::Severity::Error,
                lyra_diag::DiagnosticCode::PACKED_UNION_WIDTH,
                lyra_diag::Message::new(
                    lyra_diag::MessageId::PackedUnionWidthMismatch,
                    vec![
                        lyra_diag::Arg::Name(field_name.clone()),
                        lyra_diag::Arg::Width(*actual_w),
                        lyra_diag::Arg::Width(expected_w),
                    ],
                ),
            )
            .with_label(lyra_diag::Label {
                kind: lyra_diag::LabelKind::Primary,
                span: mismatch_span,
                message: lyra_diag::Message::new(
                    lyra_diag::MessageId::PackedUnionWidthMismatch,
                    vec![
                        lyra_diag::Arg::Name(field_name.clone()),
                        lyra_diag::Arg::Width(*actual_w),
                        lyra_diag::Arg::Width(expected_w),
                    ],
                ),
            })
            .with_label(lyra_diag::Label {
                kind: lyra_diag::LabelKind::Secondary,
                span: ref_span,
                message: lyra_diag::Message::new(
                    lyra_diag::MessageId::ExpectedMemberWidth,
                    vec![lyra_diag::Arg::Width(expected_w)],
                ),
            }),
        );
    }
}

/// Resolved modport: the member view plus diagnostics.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModportSem {
    pub view: ModportView,
    pub diags: Box<[SemanticDiag]>,
}

/// Identifies a modport for semantic resolution.
#[salsa::interned]
pub struct ModportRef<'db> {
    pub unit: CompilationUnit,
    pub modport_id: ModportDefId,
}

/// Resolve a modport's member entries against the interface scope (Salsa-tracked).
#[salsa::tracked]
pub fn modport_sem<'db>(db: &'db dyn salsa::Database, mref: ModportRef<'db>) -> ModportSem {
    let unit = mref.unit(db);
    let modport_id = mref.modport_id(db);

    let Some(gsym) = symbol_at_name_ast(db, unit, modport_id.owner.global_def().ast_id()) else {
        return empty_modport_sem();
    };

    let Some(src) = source_file_by_id(db, unit, gsym.file) else {
        return empty_modport_sem();
    };
    let def = def_index_file(db, src);

    let Some(modport_def) = def.modport_defs.get(&modport_id) else {
        return empty_modport_sem();
    };

    let iface_scope = def.symbols.get(gsym.local).scope;

    let mut entries = Vec::new();
    let mut diags = Vec::new();
    let mut seen_ports: std::collections::HashMap<SmolStr, lyra_source::TextRange> =
        std::collections::HashMap::new();

    for entry in &*modport_def.entries {
        // Duplicate port name detection (source-order, before sorting)
        if let Some(&first_range) = seen_ports.get(&entry.port_name) {
            diags.push(SemanticDiag {
                kind: SemanticDiagKind::DuplicateDefinition {
                    name: entry.port_name.clone(),
                    original: first_range,
                },
                range: entry.span.range,
            });
            continue;
        }
        seen_ports.insert(entry.port_name.clone(), entry.span.range);

        let target = match &entry.target {
            ModportTarget::ImplicitMember { member_name } => {
                let resolved =
                    def.scopes
                        .resolve(&def.symbols, iface_scope, Namespace::Value, member_name);
                let Some(member_sym) = resolved else {
                    diags.push(SemanticDiag {
                        kind: SemanticDiagKind::UnresolvedName {
                            name: member_name.clone(),
                        },
                        range: entry.span.range,
                    });
                    continue;
                };
                ModportViewTarget::Member(member_sym)
            }
            ModportTarget::Expr(expr_id) => ModportViewTarget::Expr(*expr_id),
            ModportTarget::Empty => ModportViewTarget::Empty,
        };

        entries.push(ModportViewEntry {
            port_name: entry.port_name.clone(),
            direction: entry.direction,
            port_id: entry.port_id,
            target,
        });
    }

    ModportSem {
        view: ModportView::new(entries),
        diags: diags.into_boxed_slice(),
    }
}

fn empty_modport_sem() -> ModportSem {
    ModportSem {
        view: ModportView::new(Vec::new()),
        diags: Box::new([]),
    }
}
