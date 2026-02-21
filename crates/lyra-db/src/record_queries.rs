use lyra_semantic::def_index::ExpectedNs;
use lyra_semantic::diagnostic::{SemanticDiag, SemanticDiagKind};
use lyra_semantic::modport_def::ModportDefId;
use lyra_semantic::record::{FieldSem, RecordId, RecordSem, TypeRef};
use lyra_semantic::symbols::Namespace;
use lyra_semantic::types::{ConstInt, ModportView};
use smol_str::SmolStr;

use crate::const_eval::{ConstExprRef, eval_const_int};
use crate::semantic::{
    compilation_unit_env, def_index_file, def_symbol, global_def_index, name_graph_file,
    package_scope_index,
};
use crate::ty_resolve::resolve_result_to_ty;
use crate::{CompilationUnit, source_file_by_id};

/// Identifies a record (struct/union) for semantic resolution.
#[salsa::interned]
pub struct RecordRef<'db> {
    pub unit: CompilationUnit,
    pub record_id: RecordId,
}

/// Resolve a record's field types (Salsa-tracked).
///
/// Looks up the `RecordDef` from the definition index, resolves each
/// field's `TypeRef` in the record's defining scope, and builds the
/// sorted field lookup table.
#[salsa::tracked]
pub fn record_sem<'db>(db: &'db dyn salsa::Database, rref: RecordRef<'db>) -> RecordSem {
    let unit = rref.unit(db);
    let record_id = rref.record_id(db);
    let file_id = record_id.file;

    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return empty_record_sem();
    };

    let def = def_index_file(db, source_file);

    // Find the RecordDef by owner + ordinal
    let record_def = def
        .record_defs
        .iter()
        .find(|rd| rd.owner == record_id.owner && rd.ordinal == record_id.ordinal);
    let Some(record_def) = record_def else {
        return empty_record_sem();
    };

    // Get resolution context for the record's defining file
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

    for field in &*record_def.fields {
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
                    record_def.scope,
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

    let Some(gsym) = def_symbol(db, unit, modport_id.owner.global_def()) else {
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
    let mut seen = std::collections::HashMap::new();

    for entry in &*modport_def.entries {
        let resolved = def.scopes.resolve(
            &def.symbols,
            iface_scope,
            Namespace::Value,
            &entry.member_name,
        );
        let Some(member_sym) = resolved else {
            diags.push(SemanticDiag {
                kind: SemanticDiagKind::UnresolvedName {
                    name: entry.member_name.clone(),
                },
                range: entry.span.range,
            });
            continue;
        };
        if let Some(&first_range) = seen.get(&member_sym) {
            diags.push(SemanticDiag {
                kind: SemanticDiagKind::DuplicateDefinition {
                    name: entry.member_name.clone(),
                    original: first_range,
                },
                range: entry.span.range,
            });
            continue;
        }
        seen.insert(member_sym, entry.span.range);
        entries.push((member_sym, entry.direction));
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
