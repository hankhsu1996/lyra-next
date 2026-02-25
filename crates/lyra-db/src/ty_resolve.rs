use lyra_semantic::diagnostic::{DiagSpan, SemanticDiag, SemanticDiagKind};
use lyra_semantic::global_index::DefinitionKind;
use lyra_semantic::interface_id::InterfaceDefId;
use lyra_semantic::resolve_index::CoreResolveResult;
use lyra_semantic::symbols::{GlobalDefId, GlobalSymbolId};
use lyra_semantic::types::{InterfaceType, Ty};
use lyra_source::FileId;
use smol_str::SmolStr;

use crate::semantic::{def_index_file, global_def_index};
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
                CoreResolution::Def { def } => {
                    return TypeResolveOutcome::Ok(def_target_ty(db, unit, *def));
                }
                CoreResolution::Pkg { name_site, .. } => {
                    let target_file_id = name_site.file();
                    let Some(target_file) = source_file_by_id(db, unit, target_file_id) else {
                        return TypeResolveOutcome::Ok(Ty::Error);
                    };
                    let target_def = def_index_file(db, target_file);
                    let Some(&sym_id) = target_def.name_site_to_symbol.get(name_site) else {
                        return TypeResolveOutcome::Ok(Ty::Error);
                    };
                    GlobalSymbolId {
                        file: target_file_id,
                        local: sym_id,
                    }
                }
                CoreResolution::EnumVariant(target) => {
                    return TypeResolveOutcome::Ok(Ty::Enum(target.enum_id));
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

/// Anchor for type resolution diagnostics.
#[derive(Clone, Copy)]
pub(crate) struct TypeResolveDiagAnchor {
    pub primary: DiagSpan,
    pub label: Option<DiagSpan>,
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
    anchor: TypeResolveDiagAnchor,
    diags: &mut Vec<SemanticDiag>,
) -> Ty {
    match classify_resolve_result(db, unit, defining_file, result) {
        TypeResolveOutcome::Ok(ty) => ty,
        TypeResolveOutcome::NotAType => {
            diags.push(SemanticDiag {
                kind: SemanticDiagKind::NotAType {
                    name: SmolStr::new(name),
                },
                primary: anchor.primary,
                label: anchor.label,
            });
            Ty::Error
        }
        TypeResolveOutcome::UndeclaredType => {
            diags.push(SemanticDiag {
                kind: SemanticDiagKind::UndeclaredType {
                    name: SmolStr::new(name),
                },
                primary: anchor.primary,
                label: anchor.label,
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

/// Semantic classification of a definition-namespace target.
pub(crate) enum DefTargetSem {
    Interface(InterfaceDefId),
    Other,
}

/// Classify a `GlobalDefId` into its semantic role.
///
/// All consumers that need to branch on definition kind should call this
/// single canonical function rather than re-checking `entry.kind` themselves.
pub(crate) fn def_target_sem(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    def_id: GlobalDefId,
) -> Option<DefTargetSem> {
    let target_file_id = def_id.ast_id().file();
    let target_file = source_file_by_id(db, unit, target_file_id)?;
    let target_def = def_index_file(db, target_file);
    let entry = target_def.def_entry(def_id)?;
    match entry.kind {
        DefinitionKind::Interface => {
            let global = global_def_index(db, unit);
            InterfaceDefId::try_from_global_index(global, def_id)
                .map(DefTargetSem::Interface)
                .or(Some(DefTargetSem::Other))
        }
        _ => Some(DefTargetSem::Other),
    }
}

/// Classify a definition-namespace target into a `Ty`.
///
/// Thin wrapper over `def_target_sem`. Only interfaces produce a meaningful
/// type (`Ty::Interface`); all other definition kinds produce `Ty::Error`.
pub(crate) fn def_target_ty(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    def_id: GlobalDefId,
) -> Ty {
    match def_target_sem(db, unit, def_id) {
        Some(DefTargetSem::Interface(iface)) => Ty::Interface(InterfaceType {
            iface,
            modport: None,
        }),
        _ => Ty::Error,
    }
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
