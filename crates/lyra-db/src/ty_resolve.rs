use lyra_semantic::diagnostic::{SemanticDiag, SemanticDiagKind};
use lyra_semantic::resolve_index::CoreResolveResult;
use lyra_semantic::symbols::GlobalSymbolId;
use lyra_semantic::types::Ty;
use lyra_source::FileId;
use smol_str::SmolStr;

use crate::semantic::def_index_file;
use crate::type_queries::{SymbolRef, type_of_symbol};
use crate::{CompilationUnit, source_file_by_id};

/// Outcome of classifying a resolved type reference.
enum TypeResolveOutcome {
    Ok(Ty),
    UndeclaredType,
    NotAType,
}

/// Classify a `CoreResolveResult` into a type or an error category.
///
/// Pure classification with no diagnostic emission. Both `resolve_result_to_ty`
/// and `classify_for_record_field` delegate to this.
fn classify_resolve_result(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    defining_file: FileId,
    result: &CoreResolveResult,
) -> TypeResolveOutcome {
    use lyra_semantic::resolve_index::CoreResolution;

    match result {
        CoreResolveResult::Resolved(resolution) => {
            let gsym = match resolution {
                CoreResolution::Local { symbol, .. } => GlobalSymbolId {
                    file: defining_file,
                    local: *symbol,
                },
                CoreResolution::Global { decl, .. } => {
                    let target_file_id = decl.file();
                    let Some(target_file) = source_file_by_id(db, unit, target_file_id) else {
                        return TypeResolveOutcome::Ok(Ty::Error);
                    };
                    let target_def = def_index_file(db, target_file);
                    let Some(&sym_id) = target_def.decl_to_symbol.get(&decl.ast_id()) else {
                        return TypeResolveOutcome::Ok(Ty::Error);
                    };
                    GlobalSymbolId {
                        file: target_file_id,
                        local: sym_id,
                    }
                }
            };
            let sym_ref = SymbolRef::new(db, unit, gsym);
            let sym_type = type_of_symbol(db, sym_ref);
            match &sym_type {
                lyra_semantic::types::SymbolType::TypeAlias(ty) => {
                    TypeResolveOutcome::Ok(ty.clone())
                }
                lyra_semantic::types::SymbolType::Value(_) => TypeResolveOutcome::NotAType,
                _ => TypeResolveOutcome::Ok(Ty::Error),
            }
        }
        CoreResolveResult::Unresolved(_) => TypeResolveOutcome::UndeclaredType,
    }
}

/// Resolve a `CoreResolveResult` to a `Ty`, emitting diagnostics for failures.
///
/// Shared by record and enum queries that need to resolve named type references.
pub(crate) fn resolve_result_to_ty(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    defining_file: FileId,
    result: &CoreResolveResult,
    name: &str,
    range: lyra_source::TextRange,
    diags: &mut Vec<SemanticDiag>,
) -> Ty {
    match classify_resolve_result(db, unit, defining_file, result) {
        TypeResolveOutcome::Ok(ty) => ty,
        TypeResolveOutcome::NotAType => {
            diags.push(SemanticDiag {
                kind: SemanticDiagKind::NotAType {
                    name: SmolStr::new(name),
                },
                range,
            });
            Ty::Error
        }
        TypeResolveOutcome::UndeclaredType => {
            diags.push(SemanticDiag {
                kind: SemanticDiagKind::UndeclaredType {
                    name: SmolStr::new(name),
                },
                range,
            });
            Ty::Error
        }
    }
}

/// Error classification for record field type resolution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FieldTyError {
    pub kind: FieldTyErrorKind,
    pub path: Box<[SmolStr]>,
}

/// Why a record field's type reference could not be resolved.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum FieldTyErrorKind {
    UndeclaredType,
    NotAType,
}

/// Classify a `CoreResolveResult` for a record field, returning a `Ty` and
/// optional `FieldTyError` with structured path segments.
pub(crate) fn classify_for_record_field(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    defining_file: FileId,
    result: &CoreResolveResult,
    path: Box<[SmolStr]>,
) -> (Ty, Option<FieldTyError>) {
    match classify_resolve_result(db, unit, defining_file, result) {
        TypeResolveOutcome::Ok(ty) => (ty, None),
        TypeResolveOutcome::NotAType => (
            Ty::Error,
            Some(FieldTyError {
                kind: FieldTyErrorKind::NotAType,
                path,
            }),
        ),
        TypeResolveOutcome::UndeclaredType => (
            Ty::Error,
            Some(FieldTyError {
                kind: FieldTyErrorKind::UndeclaredType,
                path,
            }),
        ),
    }
}
