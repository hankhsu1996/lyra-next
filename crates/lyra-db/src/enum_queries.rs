use lyra_semantic::def_index::ExpectedNs;
use lyra_semantic::diagnostic::{SemanticDiag, SemanticDiagKind};
use lyra_semantic::enum_def::{EnumId, EnumSem};
use lyra_semantic::record::TypeRef;
use lyra_semantic::type_infer::{BitVecType, BitWidth, Signedness};
use lyra_semantic::types::{ConstInt, Ty};

use crate::const_eval::{ConstExprRef, eval_const_int};
use crate::semantic::{
    compilation_unit_env, def_index_file, global_def_index, name_graph_file, package_scope_index,
};
use crate::ty_resolve::resolve_result_to_ty;
use crate::{CompilationUnit, source_file_by_id};

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

/// Resolve an enum's base type (Salsa-tracked).
///
/// Looks up the `EnumDef` by owner + ordinal from the definition index,
/// resolves the base `TypeRef`, validates it is integral, and computes
/// the canonical `BitVecType`.
#[salsa::tracked]
pub fn enum_sem<'db>(db: &'db dyn salsa::Database, eref: EnumRef<'db>) -> EnumSem {
    let unit = eref.unit(db);
    let enum_id = eref.enum_id(db);
    let file_id = enum_id.file;

    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return empty_enum_sem();
    };

    let def = def_index_file(db, source_file);

    let enum_def = def
        .enum_defs
        .iter()
        .find(|ed| ed.owner == enum_id.owner && ed.ordinal == enum_id.ordinal);
    let Some(enum_def) = enum_def else {
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

    EnumSem {
        base_ty,
        base_int,
        diags: diags.into_boxed_slice(),
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

fn empty_enum_sem() -> EnumSem {
    EnumSem {
        base_ty: Ty::Error,
        base_int: None,
        diags: Box::new([]),
    }
}
