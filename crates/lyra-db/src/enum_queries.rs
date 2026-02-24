use std::collections::{HashMap, HashSet};

use lyra_ast::ErasedAstId;
use lyra_semantic::def_index::ExpectedNs;
use lyra_semantic::diagnostic::{SemanticDiag, SemanticDiagKind};
use lyra_semantic::enum_def::{
    EnumId, EnumMemberRangeKind, EnumSem, EnumValueDiag, EnumVariantIndex, EnumVariantTarget,
};
use lyra_semantic::record::TypeRef;
use lyra_semantic::type_infer::{BitVecType, BitWidth, Signedness};
use lyra_semantic::types::{ConstInt, Ty};
use smol_str::SmolStr;

use crate::const_eval::{ConstExprRef, eval_const_int};
use crate::semantic::{
    compilation_unit_env, def_index_file, global_def_index, name_graph_file, package_scope_index,
};
use crate::ty_resolve::resolve_result_to_ty;
use crate::{CompilationUnit, SourceFile, source_file_by_id};

/// Identifies an enum for semantic resolution.
#[salsa::interned]
pub struct EnumRef<'db> {
    pub unit: CompilationUnit,
    pub enum_id: EnumId,
}

/// Internal error classification for enum base resolution.
enum EnumBaseError {
    NonIntegral,
    DimsNotConstant,
    TyError,
}

/// A single expanded variant from an enum range member.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpandedVariant {
    pub name: SmolStr,
    pub variant_ordinal: u32,
    pub def_range: lyra_source::TextRange,
    pub source_member: u32,
}

/// Result of expanding an enum's range members.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpandedEnum {
    pub variants: Box<[ExpandedVariant]>,
    pub diagnostics: Box<[SemanticDiag]>,
}

const MAX_ENUM_RANGE_COUNT: u64 = 65536;

/// Expand an enum's range members into flat variant lists (Salsa-tracked).
///
/// Plain members (no range) are skipped -- they are real symbols in scope.
/// Only range members (`name[N]`, `name[N:M]`) produce expanded variants.
#[salsa::tracked]
pub fn enum_variants<'db>(db: &'db dyn salsa::Database, eref: EnumRef<'db>) -> ExpandedEnum {
    let unit = eref.unit(db);
    let enum_id = eref.enum_id(db);
    let file_id = enum_id.file();

    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return ExpandedEnum {
            variants: Box::new([]),
            diagnostics: Box::new([]),
        };
    };

    let def = def_index_file(db, source_file);
    let Some(enum_def) = def.enum_def_by_id(enum_id) else {
        return ExpandedEnum {
            variants: Box::new([]),
            diagnostics: Box::new([]),
        };
    };

    let mut out = ExpandOutput::default();

    for (member_idx, member) in enum_def.members.iter().enumerate() {
        let Some(ref range_kind) = member.range else {
            out.ordinal += 1;
            continue;
        };
        let name_ident_range = member
            .name_span
            .text_range_or(member.name_site.text_range());
        let diag_range = member.range_text_range.unwrap_or(name_ident_range);
        match range_kind {
            EnumMemberRangeKind::Count(expr_id) => {
                let result = eval_const_int(db, ConstExprRef::new(db, unit, *expr_id));
                expand_count(
                    &result,
                    member,
                    member_idx as u32,
                    diag_range,
                    name_ident_range,
                    &mut out,
                );
            }
            EnumMemberRangeKind::FromTo(from_id, to_id) => {
                let from_val = eval_const_int(db, ConstExprRef::new(db, unit, *from_id));
                let to_val = eval_const_int(db, ConstExprRef::new(db, unit, *to_id));
                expand_from_to(
                    &from_val,
                    &to_val,
                    member,
                    member_idx as u32,
                    diag_range,
                    name_ident_range,
                    &mut out,
                );
            }
        }
    }

    ExpandedEnum {
        variants: out.variants.into_boxed_slice(),
        diagnostics: out.diagnostics.into_boxed_slice(),
    }
}

#[derive(Default)]
struct ExpandOutput {
    variants: Vec<ExpandedVariant>,
    diagnostics: Vec<SemanticDiag>,
    ordinal: u32,
}

fn expand_count(
    result: &ConstInt,
    member: &lyra_semantic::enum_def::EnumMemberDef,
    member_idx: u32,
    diag_range: lyra_source::TextRange,
    name_ident_range: lyra_source::TextRange,
    out: &mut ExpandOutput,
) {
    let ConstInt::Known(n) = *result else {
        out.diagnostics.push(SemanticDiag {
            kind: SemanticDiagKind::EnumRangeBoundNotEvaluable,
            range: diag_range,
        });
        return;
    };
    if n < 0 {
        out.diagnostics.push(SemanticDiag {
            kind: SemanticDiagKind::EnumRangeCountNegative { count: n },
            range: diag_range,
        });
    } else if n.cast_unsigned() > MAX_ENUM_RANGE_COUNT {
        out.diagnostics.push(SemanticDiag {
            kind: SemanticDiagKind::EnumRangeTooLarge {
                count: n.cast_unsigned(),
            },
            range: diag_range,
        });
    } else {
        for i in 0..n {
            out.variants.push(ExpandedVariant {
                name: SmolStr::new(format!("{}{i}", member.name)),
                variant_ordinal: out.ordinal,
                def_range: name_ident_range,
                source_member: member_idx,
            });
            out.ordinal += 1;
        }
    }
}

fn expand_from_to(
    from_val: &ConstInt,
    to_val: &ConstInt,
    member: &lyra_semantic::enum_def::EnumMemberDef,
    member_idx: u32,
    diag_range: lyra_source::TextRange,
    name_ident_range: lyra_source::TextRange,
    out: &mut ExpandOutput,
) {
    let (&ConstInt::Known(from), &ConstInt::Known(to)) = (from_val, to_val) else {
        out.diagnostics.push(SemanticDiag {
            kind: SemanticDiagKind::EnumRangeBoundNotEvaluable,
            range: diag_range,
        });
        return;
    };
    let count = (from - to).unsigned_abs() + 1;
    if count > MAX_ENUM_RANGE_COUNT {
        out.diagnostics.push(SemanticDiag {
            kind: SemanticDiagKind::EnumRangeTooLarge { count },
            range: diag_range,
        });
        return;
    }
    let step: i64 = if from <= to { 1 } else { -1 };
    let mut val = from;
    for _ in 0..count {
        out.variants.push(ExpandedVariant {
            name: SmolStr::new(format!("{}{val}", member.name)),
            variant_ordinal: out.ordinal,
            def_range: name_ident_range,
            source_member: member_idx,
        });
        out.ordinal += 1;
        val += step;
    }
}

/// Build the per-file enum variant index for range-generated names (Salsa-tracked).
///
/// Iterates all enum defs in a file, expands their range members via
/// `enum_variants`, and builds a `(ScopeId, name) -> EnumVariantTarget`
/// index. Collision checking is done here.
#[salsa::tracked(return_ref)]
pub fn enum_variant_index(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> EnumVariantIndex {
    let def = def_index_file(db, file);

    let mut by_scope: HashMap<lyra_semantic::scopes::ScopeId, HashMap<SmolStr, EnumVariantTarget>> =
        HashMap::new();
    let mut diagnostics = Vec::new();

    for (def_idx, enum_def) in def.enum_defs.iter().enumerate() {
        let enum_id = def.enum_id(lyra_semantic::enum_def::EnumDefIdx(def_idx as u32));
        let eref = EnumRef::new(db, unit, enum_id);
        let expanded = enum_variants(db, eref);

        // Collect diagnostics from expansion
        diagnostics.extend(expanded.diagnostics.iter().cloned());

        // Build existing-names set for collision detection
        let scope_data = def.scopes.get(enum_def.scope);
        let existing_names: HashSet<&str> = scope_data
            .value_ns
            .iter()
            .map(|sym_id| def.symbols.get(*sym_id).name.as_str())
            .collect();

        let scope_map = by_scope.entry(enum_def.scope).or_default();

        for variant in &*expanded.variants {
            // Check collision with other generated names in this scope
            if scope_map.contains_key(&variant.name) {
                let diag_range = variant.def_range;
                diagnostics.push(SemanticDiag {
                    kind: SemanticDiagKind::DuplicateDefinition {
                        name: variant.name.clone(),
                        original: diag_range,
                    },
                    range: diag_range,
                });
                continue;
            }
            // Check collision with real symbols in scope
            if existing_names.contains(variant.name.as_str()) {
                let diag_range = variant.def_range;
                diagnostics.push(SemanticDiag {
                    kind: SemanticDiagKind::DuplicateDefinition {
                        name: variant.name.clone(),
                        original: diag_range,
                    },
                    range: diag_range,
                });
                continue;
            }
            scope_map.insert(
                variant.name.clone(),
                EnumVariantTarget {
                    enum_id,
                    variant_ordinal: variant.variant_ordinal,
                    def_range: variant.def_range,
                },
            );
        }
    }

    EnumVariantIndex {
        by_scope,
        diagnostics: diagnostics.into_boxed_slice(),
    }
}

/// Resolve an enum's base type (Salsa-tracked).
///
/// Looks up the `EnumDef` by owner + ordinal from the definition index,
/// resolves the base `TypeRef`, validates it is integral, and computes
/// the canonical `BitVecType`.
#[salsa::tracked]
pub fn enum_sem<'db>(db: &'db dyn salsa::Database, eref: EnumRef<'db>) -> EnumSem {
    let unit = eref.unit(db);
    let enum_id = eref.enum_id(db);
    let file_id = enum_id.file();

    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return empty_enum_sem();
    };

    let def = def_index_file(db, source_file);

    let Some(enum_def) = def.enum_def_by_id(enum_id) else {
        return empty_enum_sem();
    };

    let graph = name_graph_file(db, source_file);
    let global = global_def_index(db, unit);
    let pkg_scope = package_scope_index(db, unit);
    let cu_env = compilation_unit_env(db, unit);

    let eval = |expr_ast_id: lyra_ast::ErasedAstId| -> ConstInt {
        let expr_ref = ConstExprRef::new(db, unit, expr_ast_id);
        eval_const_int(db, expr_ref)
    };

    let base_range = enum_def.base.range;
    let mut diags = Vec::new();

    // Resolve the base TypeRef to a Ty
    let base_ty = match &enum_def.base.tref {
        TypeRef::Resolved(ty) => lyra_semantic::normalize_ty(ty, &eval),
        TypeRef::Named { name, span } => {
            let result = lyra_semantic::resolve_name_in_scope(
                graph,
                global,
                pkg_scope,
                cu_env,
                enum_def.scope,
                name,
                ExpectedNs::TypeThenValue,
            );
            resolve_result_to_ty(db, unit, file_id, &result, name, span.range, &mut diags)
        }
        TypeRef::Qualified { segments, span } => {
            let result = lyra_semantic::resolve_qualified_name(
                segments,
                global,
                pkg_scope,
                ExpectedNs::TypeThenValue,
            );
            let display_name = segments.join("::");
            resolve_result_to_ty(
                db,
                unit,
                file_id,
                &result,
                &display_name,
                span.range,
                &mut diags,
            )
        }
    };

    // Validate the resolved Ty and compute BitVecType
    let base_int = match classify_enum_base(&base_ty) {
        Ok(bv) => Some(bv),
        Err(EnumBaseError::TyError) => None,
        Err(EnumBaseError::NonIntegral) => {
            diags.push(SemanticDiag {
                kind: SemanticDiagKind::IllegalEnumBaseType {
                    name: base_ty.pretty(),
                },
                range: base_range,
            });
            None
        }
        Err(EnumBaseError::DimsNotConstant) => {
            diags.push(SemanticDiag {
                kind: SemanticDiagKind::EnumBaseDimsNotConstant,
                range: base_range,
            });
            None
        }
    };

    // Compute per-member values with validation
    let mv_output = compute_member_values(db, unit, enum_def, base_int.as_ref());

    EnumSem {
        base_ty,
        base_int,
        member_values: mv_output.values,
        diags: diags.into_boxed_slice(),
        value_diags: mv_output.diags.into_boxed_slice(),
    }
}

/// Classify a resolved enum base type into a `BitVecType` or an error.
fn classify_enum_base(ty: &Ty) -> Result<BitVecType, EnumBaseError> {
    match ty {
        Ty::Error => Err(EnumBaseError::TyError),
        Ty::Integral(i) => {
            let signed = if i.signed {
                Signedness::Signed
            } else {
                Signedness::Unsigned
            };
            let four_state = i.keyword.four_state();
            let width = if i.packed.is_empty() {
                BitWidth::Known(i.keyword.base_width())
            } else {
                match i.try_packed_width() {
                    Some(w) => BitWidth::Known(w),
                    None => return Err(EnumBaseError::DimsNotConstant),
                }
            };
            Ok(BitVecType {
                width,
                signed,
                four_state,
            })
        }
        _ => Err(EnumBaseError::NonIntegral),
    }
}

struct MemberValueOutput {
    values: std::sync::Arc<[ConstInt]>,
    diags: Vec<EnumValueDiag>,
}

/// Pure: compute variant count from evaluated bounds.
fn range_count_from_bounds(from: i64, to: i64) -> u64 {
    (i128::from(from) - i128::from(to)).unsigned_abs() as u64 + 1
}

/// DB: evaluate range bounds via const-eval, return (from, to) or None.
fn range_bounds_const(
    member: &lyra_semantic::enum_def::EnumMemberDef,
    db: &dyn salsa::Database,
    unit: CompilationUnit,
) -> Option<(i64, i64)> {
    match member.range.as_ref()? {
        EnumMemberRangeKind::Count(expr_id) => {
            let result = eval_const_int(db, ConstExprRef::new(db, unit, *expr_id));
            match result {
                ConstInt::Known(n) if n >= 0 => Some((0, n - 1)),
                _ => None,
            }
        }
        EnumMemberRangeKind::FromTo(from_id, to_id) => {
            let from_val = eval_const_int(db, ConstExprRef::new(db, unit, *from_id));
            let to_val = eval_const_int(db, ConstExprRef::new(db, unit, *to_id));
            match (from_val, to_val) {
                (ConstInt::Known(f), ConstInt::Known(t)) => Some((f, t)),
                _ => None,
            }
        }
    }
}

/// Check if a value fits in the given base type's representable range.
fn fits_in_base(value: i128, width: u32, signed: bool) -> bool {
    if width == 0 {
        return false;
    }
    if width >= 128 {
        return true;
    }
    if signed {
        let lo = -(1i128 << (width - 1));
        let hi = (1i128 << (width - 1)) - 1;
        value >= lo && value <= hi
    } else {
        let hi = (1i128 << width) - 1;
        value >= 0 && value <= hi
    }
}

/// Downcast an i128 value to `ConstInt`, recording Overflow if it doesn't fit in i64.
fn store_member_value(values: &mut Vec<ConstInt>, raw: i128) {
    use lyra_semantic::types::ConstEvalError;
    if let Ok(v) = i64::try_from(raw) {
        values.push(ConstInt::Known(v));
    } else {
        values.push(ConstInt::Error(ConstEvalError::Overflow));
    }
}

/// Compute member values with full validation: overflow, duplicate, sized literal width.
fn compute_member_values(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    enum_def: &lyra_semantic::enum_def::EnumDef,
    base_int: Option<&BitVecType>,
) -> MemberValueOutput {
    use lyra_semantic::types::ConstEvalError;

    let mut values = Vec::with_capacity(enum_def.members.len());
    let mut diags = Vec::new();

    // Pre-pass: sized literal width check
    if let Some(bv) = base_int
        && let BitWidth::Known(base_w) = bv.width
    {
        for member in &*enum_def.members {
            if let Some(lit_w) = member.init_literal_width
                && lit_w != base_w
            {
                diags.push(EnumValueDiag::SizedLiteralWidth {
                    anchor: member.name_site,
                    literal_width: lit_w,
                    base_width: base_w,
                });
            }
        }
    }

    // Track all produced variants for overflow + duplicate detection.
    // (anchor, value, member_name) -- one per produced variant.
    let mut produced: Vec<(ErasedAstId, i128, SmolStr)> = Vec::new();

    let mut next_raw: i128 = 0;
    let mut next_raw_valid = true;

    for member in &*enum_def.members {
        let raw: i128;
        let raw_valid: bool;

        if let Some(expr_id) = member.init {
            let result = eval_const_int(db, ConstExprRef::new(db, unit, expr_id));
            if let ConstInt::Known(v) = result {
                raw = i128::from(v);
                raw_valid = true;
            } else {
                values.push(result);
                continue;
            }
        } else if next_raw_valid {
            raw = next_raw;
            raw_valid = true;
        } else {
            values.push(ConstInt::Error(ConstEvalError::AutoIncrementAfterUnknown));
            continue;
        }

        if member.range.is_some() {
            // Range member: compute variant count and produce N variants
            if let Some((from, to)) = range_bounds_const(member, db, unit) {
                let count = range_count_from_bounds(from, to);
                store_member_value(&mut values, raw);
                let step: i64 = if from <= to { 1 } else { -1 };
                let mut label_val = from;
                for i in 0..count {
                    let variant_value = raw + i128::from(i);
                    let variant_name = SmolStr::new(format!("{}{label_val}", member.name));
                    produced.push((member.name_site, variant_value, variant_name));
                    label_val += step;
                }
                next_raw = raw + i128::from(count);
                next_raw_valid = true;
            } else {
                values.push(ConstInt::Error(ConstEvalError::NonConstant));
                next_raw_valid = false;
            }
        } else {
            // Plain member: produce 1 variant
            store_member_value(&mut values, raw);
            produced.push((member.name_site, raw, member.name.clone()));
            next_raw = raw + 1;
            next_raw_valid = raw_valid;
        }
    }

    check_overflow_and_duplicates(&produced, base_int, &mut diags);

    MemberValueOutput {
        values: std::sync::Arc::from(values),
        diags,
    }
}

/// Post-pass: check overflow against base type and detect duplicate values.
fn check_overflow_and_duplicates(
    produced: &[(ErasedAstId, i128, SmolStr)],
    base_int: Option<&BitVecType>,
    diags: &mut Vec<EnumValueDiag>,
) {
    let mut overflow_set: HashSet<usize> = HashSet::new();
    if let Some(bv) = base_int
        && let BitWidth::Known(w) = bv.width
    {
        let signed = bv.signed == Signedness::Signed;
        for (i, (anchor, value, name)) in produced.iter().enumerate() {
            if !fits_in_base(*value, w, signed) {
                diags.push(EnumValueDiag::Overflow {
                    anchor: *anchor,
                    value: *value,
                    width: w,
                    signed,
                    member_name: name.clone(),
                });
                overflow_set.insert(i);
            }
        }
    }

    let mut seen: HashMap<i128, (ErasedAstId, SmolStr)> = HashMap::new();
    for (i, (anchor, value, name)) in produced.iter().enumerate() {
        if overflow_set.contains(&i) {
            continue;
        }
        if let Some((original, _orig_name)) = seen.get(value) {
            diags.push(EnumValueDiag::DuplicateValue {
                anchor: *anchor,
                original: *original,
                value: *value,
                member_name: name.clone(),
            });
        } else {
            seen.insert(*value, (*anchor, name.clone()));
        }
    }
}

/// Sorted set of known enum member values. Returns None if any member value is not Known.
#[salsa::tracked]
pub fn enum_known_value_set<'db>(
    db: &'db dyn salsa::Database,
    eref: EnumRef<'db>,
) -> Option<std::sync::Arc<[i64]>> {
    let sem = enum_sem(db, eref);
    let mut vals: Vec<i64> = Vec::with_capacity(sem.member_values.len());
    for v in &*sem.member_values {
        match v {
            ConstInt::Known(n) => vals.push(*n),
            _ => return None,
        }
    }
    vals.sort_unstable();
    vals.dedup();
    Some(std::sync::Arc::from(vals))
}

fn empty_enum_sem() -> EnumSem {
    EnumSem {
        base_ty: Ty::Error,
        base_int: None,
        member_values: std::sync::Arc::from(Vec::<ConstInt>::new()),
        diags: Box::new([]),
        value_diags: Box::new([]),
    }
}
