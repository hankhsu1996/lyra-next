use lyra_ast::UnpackedDimSource;
use lyra_semantic::def_index::ExpectedNs;
use lyra_semantic::diagnostic::{DiagSpan, SemanticDiag, SemanticDiagKind};
use lyra_semantic::record::{FieldSem, Packing, RecordId, RecordKind, RecordSem, TypeRef};
use lyra_semantic::types::{ConstInt, Ty};
use smol_str::SmolStr;

use crate::const_eval::{ConstExprRef, eval_const_int};
use crate::pipeline::{ast_id_map, parse_file, preprocess_file};
use crate::semantic::def_index_file;
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

    let parse = parse_file(db, source_file);
    let map = ast_id_map(db, source_file);
    let resolve_env = crate::semantic::resolve_env(db, source_file, unit);

    let mut result = Vec::with_capacity(record_def.fields.len());

    for field in &*record_def.fields {
        let lowered = match &field.ty {
            TypeRef::Resolved(ty) => LoweredFieldTy {
                ty: ty.clone(),
                err: None,
            },
            TypeRef::Named { name, .. } => {
                let resolve_result = lyra_semantic::resolve_name_in_scope(
                    &resolve_env,
                    record_def.scope,
                    name,
                    ExpectedNs::TypeThenValue,
                );
                let (ty, err) =
                    classify_for_record_field(db, unit, file_id, &resolve_result, name.clone());
                LoweredFieldTy { ty, err }
            }
            TypeRef::Qualified { path, .. } => {
                let resolve_result = lyra_semantic::resolve_qualified_path(
                    path,
                    resolve_env.global,
                    resolve_env.pkg_scope,
                    resolve_env.cu_scope,
                    ExpectedNs::TypeThenValue,
                );
                let display = SmolStr::new(path.display_name());
                let (ty, err) =
                    classify_for_record_field(db, unit, file_id, &resolve_result, display);
                LoweredFieldTy { ty, err }
            }
        };

        // Apply field declarator unpacked dims
        let dim_src = UnpackedDimSource::from_name_site(&parse.syntax(), map, field.name_site);
        let ty = wrap_field_unpacked_dims(db, unit, source_file, lowered.ty, dim_src.as_ref(), map);
        result.push(LoweredFieldTy {
            ty,
            err: lowered.err,
        });
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

        // Convert field type errors to SemanticDiag using the def-anchored type site
        if let Some(err) = &lowered.err {
            let primary = DiagSpan::Site(field.best_type_site());
            match err.kind {
                FieldTyErrorKind::UndeclaredType => {
                    diags.push(SemanticDiag {
                        kind: SemanticDiagKind::UndeclaredType {
                            name: err.path.clone(),
                        },
                        primary,
                        label: None,
                    });
                }
                FieldTyErrorKind::NotAType => {
                    diags.push(SemanticDiag {
                        kind: SemanticDiagKind::NotAType {
                            name: err.path.clone(),
                        },
                        primary,
                        label: None,
                    });
                }
            }
        }

        fields.push(FieldSem {
            name: field.name.clone(),
            ty,
            type_site: field.best_type_site(),
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

fn wrap_field_unpacked_dims(
    db: &dyn salsa::Database,
    unit: crate::CompilationUnit,
    source_file: crate::SourceFile,
    ty: Ty,
    owner: Option<&UnpackedDimSource>,
    ast_id_map: &lyra_ast::AstIdMap,
) -> Ty {
    let Some(owner) = owner else { return ty };
    let dims: Vec<lyra_semantic::types::UnpackedDim> = owner
        .unpacked_dimensions()
        .map(|dim| {
            crate::type_queries::resolve_unpacked_dim(db, unit, source_file, &dim, ast_id_map)
        })
        .collect();
    if dims.is_empty() {
        return ty;
    }
    lyra_semantic::wrap_unpacked(ty, &dims)
}

/// Returns the forbidden type category if this type cannot appear in an untagged union.
///
/// LRM 7.3: untagged unions shall not contain dynamic arrays, associative
/// arrays, queues, chandle, or event as data members.
///
/// Checks the direct (outermost) type only. Does not chase through
/// typedefs or nested array element types -- typedef resolution has
/// already been applied by `record_sem` before this point.
fn forbidden_union_member_category(ty: &Ty) -> Option<&'static str> {
    match ty {
        Ty::Chandle => Some("chandle"),
        Ty::Event => Some("event"),
        Ty::Array { dim, .. } => match dim {
            lyra_semantic::types::UnpackedDim::Unsized => Some("dynamic array"),
            lyra_semantic::types::UnpackedDim::Queue { .. } => Some("queue"),
            lyra_semantic::types::UnpackedDim::Assoc(_) => Some("associative array"),
            _ => None,
        },
        _ => None,
    }
}

/// Returns the non-integral category label if this type cannot appear in a packed record.
///
/// LRM 7.2.1 / 7.3.1: packed structs and packed unions shall contain only
/// integral data types. Enums, integral types, and other packed records are
/// integral. Error and void are skipped (handled separately).
fn non_integral_packed_member_category(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    ty: &Ty,
) -> Option<&'static str> {
    match ty {
        Ty::Record(id) => {
            let file_id = id.file();
            let source_file = source_file_by_id(db, unit, file_id)?;
            let def = def_index_file(db, source_file);
            let record_def = def.record_def_by_id(*id)?;
            match record_def.packing {
                Packing::Packed | Packing::SoftPacked => None,
                Packing::Unpacked => match record_def.kind {
                    RecordKind::Struct => Some("unpacked struct"),
                    RecordKind::Union | RecordKind::TaggedUnion => Some("unpacked union"),
                },
            }
        }
        Ty::Real(_) => Some("real"),
        Ty::String => Some("string"),
        Ty::Chandle => Some("chandle"),
        Ty::Event => Some("event"),
        Ty::Interface(_) => Some("interface"),
        Ty::Array { .. } => Some("unpacked array"),
        Ty::Integral(_) | Ty::Enum(_) | Ty::Error | Ty::Void => None,
    }
}

fn record_kind_str(kind: RecordKind) -> &'static str {
    match kind {
        RecordKind::Struct => "struct",
        RecordKind::Union | RecordKind::TaggedUnion => "union",
    }
}

fn packing_str(packing: Packing) -> &'static str {
    match packing {
        Packing::Packed => "packed",
        Packing::SoftPacked => "soft packed",
        Packing::Unpacked => "unpacked",
    }
}

fn empty_record_sem() -> RecordSem {
    RecordSem {
        fields: Box::new([]),
        field_lookup: Box::new([]),
        diags: Box::new([]),
    }
}

/// Per-record diagnostics: type resolution errors + member validation.
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
    let def = def_index_file(db, source_file);
    let record_def = def.record_def_by_id(record_id);

    // Collect SemanticDiags: record_sem type-resolution errors + validation checks
    let mut sem_diags: Vec<SemanticDiag> = sem.diags.to_vec();

    if let Some(record_def) = record_def {
        // Void member restriction (LRM 7.3.2): void only in tagged unions
        if record_def.kind != RecordKind::TaggedUnion {
            for (def_field, sem_field) in record_def.fields.iter().zip(sem.fields.iter()) {
                if sem_field.ty == Ty::Void {
                    sem_diags.push(SemanticDiag {
                        kind: SemanticDiagKind::VoidMemberNonTagged {
                            name: sem_field.name.clone(),
                        },
                        primary: DiagSpan::Site(def_field.best_type_site()),
                        label: None,
                    });
                }
            }
        }

        // Untagged union member type restriction (LRM 7.3)
        if record_def.kind == RecordKind::Union {
            for (def_field, sem_field) in record_def.fields.iter().zip(sem.fields.iter()) {
                if let Some(category) = forbidden_union_member_category(&sem_field.ty) {
                    sem_diags.push(SemanticDiag {
                        kind: SemanticDiagKind::IllegalUnionMemberType {
                            name: sem_field.name.clone(),
                            category: SmolStr::new(category),
                        },
                        primary: DiagSpan::Site(def_field.best_type_site()),
                        label: None,
                    });
                }
            }
        }

        // Non-integral member in packed record (LRM 7.2.1 / 7.3.1)
        if matches!(record_def.packing, Packing::Packed | Packing::SoftPacked) {
            let rk = record_kind_str(record_def.kind);
            let pk = packing_str(record_def.packing);
            for (i, def_field) in record_def.fields.iter().enumerate() {
                let Some(sem_field) = sem.fields.get(i) else {
                    continue;
                };
                if sem_field.ty == Ty::Void {
                    continue;
                }
                if record_def.kind == RecordKind::Union
                    && forbidden_union_member_category(&sem_field.ty).is_some()
                {
                    continue;
                }
                if let Some(category) = non_integral_packed_member_category(db, unit, &sem_field.ty)
                {
                    sem_diags.push(SemanticDiag {
                        kind: SemanticDiagKind::NonIntegralPackedMember {
                            name: sem_field.name.clone(),
                            record_kind: SmolStr::new(rk),
                            packing: SmolStr::new(pk),
                            category: SmolStr::new(category),
                        },
                        primary: DiagSpan::Name(def_field.name_span),
                        label: None,
                    });
                }
            }
        }
    }

    // Lower all SemanticDiags through the shared pipeline
    let mut diags = Vec::new();
    for diag in &sem_diags {
        let chosen = crate::lower_diag::choose_best_diag_span(diag.primary, diag.label);
        let (primary_span, _) =
            crate::lower_diag::map_span_or_fallback(file_id, &pp.source_map, chosen);
        diags.push(crate::lower_diag::lower_semantic_diag(
            diag,
            primary_span,
            &pp.source_map,
        ));
    }

    // Packed union width validation (structural, not SemanticDiag-based)
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
            .text_range_or(record_def.fields[i].name_site.text_range());
        let ref_range = record_def.fields[ref_idx]
            .name_span
            .text_range_or(record_def.fields[ref_idx].name_site.text_range());

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
                lyra_diag::code::PACKED_UNION_WIDTH,
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
