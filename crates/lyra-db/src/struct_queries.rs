use lyra_semantic::aggregate::{FieldSem, StructId, StructSem, TypeRef};
use lyra_semantic::def_index::ExpectedNs;
use lyra_semantic::diagnostic::{SemanticDiag, SemanticDiagKind};
use lyra_semantic::resolve_index::{CoreResolution, CoreResolveResult};
use lyra_semantic::symbols::GlobalSymbolId;
use lyra_semantic::types::{ConstInt, Ty};
use lyra_source::FileId;
use smol_str::SmolStr;

use crate::const_eval::{ConstExprRef, eval_const_int};
use crate::semantic::{
    compilation_unit_env, def_index_file, global_def_index, name_graph_file, package_scope_index,
};
use crate::type_queries::{SymbolRef, type_of_symbol};
use crate::{CompilationUnit, source_file_by_id};

/// Identifies a struct for semantic resolution.
#[salsa::interned]
pub struct StructRef<'db> {
    pub unit: CompilationUnit,
    pub struct_id: StructId,
}

/// Resolve a struct's field types (Salsa-tracked).
///
/// Looks up the `StructDef` from the definition index, resolves each
/// field's `TypeRef` in the struct's defining scope, and builds the
/// sorted field lookup table.
#[salsa::tracked]
pub fn struct_sem<'db>(db: &'db dyn salsa::Database, sref: StructRef<'db>) -> StructSem {
    let unit = sref.unit(db);
    let struct_id = sref.struct_id(db);
    let file_id = struct_id.file;

    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return empty_struct_sem();
    };

    let def = def_index_file(db, source_file);

    // Find the StructDef by owner + ordinal
    let struct_def = def
        .struct_defs
        .iter()
        .find(|sd| sd.owner == struct_id.owner && sd.ordinal == struct_id.ordinal);
    let Some(struct_def) = struct_def else {
        return empty_struct_sem();
    };

    // Get resolution context for the struct's defining file
    let graph = name_graph_file(db, source_file);
    let global = global_def_index(db, unit);
    let pkg_scope = package_scope_index(db, unit);
    let cu_env = compilation_unit_env(db, unit);

    let eval = |expr_ast_id: lyra_ast::ErasedAstId| -> ConstInt {
        let expr_ref = ConstExprRef::new(db, unit, expr_ast_id);
        eval_const_int(db, expr_ref)
    };

    let mut fields = Vec::new();
    let mut diags = Vec::new();

    for field in &*struct_def.fields {
        let (ty, span) = match &field.ty {
            TypeRef::Resolved(ty) => {
                let span = lyra_source::Span {
                    file: file_id,
                    range: lyra_source::TextRange::default(),
                };
                (lyra_semantic::normalize_ty(ty, &eval), span)
            }
            TypeRef::Named { name, span } => {
                let result = lyra_semantic::resolve_name_in_scope(
                    graph,
                    global,
                    pkg_scope,
                    cu_env,
                    struct_def.scope,
                    name,
                    ExpectedNs::TypeThenValue,
                );
                let ty =
                    resolve_result_to_ty(db, unit, file_id, &result, name, span.range, &mut diags);
                (ty, *span)
            }
            TypeRef::Qualified { segments, span } => {
                let result = lyra_semantic::resolve_qualified_name(
                    segments,
                    global,
                    pkg_scope,
                    ExpectedNs::TypeThenValue,
                );
                let display_name = segments.join("::");
                let ty = resolve_result_to_ty(
                    db,
                    unit,
                    file_id,
                    &result,
                    &display_name,
                    span.range,
                    &mut diags,
                );
                (ty, *span)
            }
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

    StructSem {
        fields: fields.into_boxed_slice(),
        field_lookup: lookup.into_boxed_slice(),
        diags: diags.into_boxed_slice(),
    }
}

fn resolve_result_to_ty(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    defining_file: FileId,
    result: &CoreResolveResult,
    name: &str,
    range: lyra_source::TextRange,
    diags: &mut Vec<SemanticDiag>,
) -> Ty {
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
                        return Ty::Error;
                    };
                    let target_def = def_index_file(db, target_file);
                    let Some(&sym_id) = target_def.decl_to_symbol.get(&decl.ast_id()) else {
                        return Ty::Error;
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
                lyra_semantic::types::SymbolType::Value(ty)
                | lyra_semantic::types::SymbolType::TypeAlias(ty) => ty.clone(),
                _ => Ty::Error,
            }
        }
        CoreResolveResult::Unresolved(_) => {
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

fn empty_struct_sem() -> StructSem {
    StructSem {
        fields: Box::new([]),
        field_lookup: Box::new([]),
        diags: Box::new([]),
    }
}
