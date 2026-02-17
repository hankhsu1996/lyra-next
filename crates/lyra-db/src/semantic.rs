use lyra_semantic::def_index::DefIndex;
use lyra_semantic::global_index::{
    DefinitionKind, GlobalDefIndex, PackageScope, PackageScopeIndex,
};
use lyra_semantic::name_graph::NameGraph;
use lyra_semantic::resolve_index::{CoreResolveOutput, ResolveIndex};
use lyra_semantic::scopes::ScopeKind;
use lyra_semantic::symbols::GlobalDefId;
use smol_str::SmolStr;

use crate::pipeline::{ast_id_map, parse_file};
use crate::{CompilationUnit, SourceFile, source_file_by_id};

/// Build per-file definition index (Salsa-cached).
///
/// Collects declarations, scopes, exports, and use-sites from the parse tree.
/// Does NOT resolve name uses -- see `resolve_index_file`.
#[salsa::tracked(return_ref)]
pub fn def_index_file(db: &dyn salsa::Database, file: SourceFile) -> DefIndex {
    let parse = parse_file(db, file);
    let map = ast_id_map(db, file);
    lyra_semantic::build_def_index(file.file_id(db), parse, map)
}

/// Extract offset-independent name graph from the definition index (Salsa-cached).
///
/// On whitespace-only edits, this query re-executes but produces an equal
/// result (no ranges), so Salsa backdates it and skips re-running the
/// expensive `resolve_core_file` downstream.
#[salsa::tracked(return_ref)]
pub fn name_graph_file(db: &dyn salsa::Database, file: SourceFile) -> NameGraph {
    NameGraph::from_def_index(def_index_file(db, file))
}

/// Build the global definition index for a compilation unit (Salsa-cached).
///
/// Aggregates module and package names with `GlobalDefId`s from all files'
/// `DefIndex` exports. On whitespace-only edits, `ErasedAstId` values are
/// topology-stable, so the result backdates correctly.
#[salsa::tracked(return_ref)]
pub fn global_def_index(db: &dyn salsa::Database, unit: CompilationUnit) -> GlobalDefIndex {
    let mut entries: Vec<(SmolStr, GlobalDefId, DefinitionKind)> = Vec::new();
    for file in unit.files(db) {
        let def = def_index_file(db, *file);
        // Collect all definition-namespace constructs (module, interface, program, primitive, config)
        for &sym_id in &*def.exports.definitions {
            let sym = def.symbols.get(sym_id);
            if let Some(def_kind) = DefinitionKind::from_symbol_kind(sym.kind)
                && let Some(&Some(ast_id)) = def.symbol_to_decl.get(sym_id.index())
            {
                entries.push((sym.name.clone(), GlobalDefId::new(ast_id), def_kind));
            }
        }
        // Collect packages (separate namespace per LRM 3.13(b))
        for &sym_id in &*def.exports.packages {
            let sym = def.symbols.get(sym_id);
            if let Some(&Some(ast_id)) = def.symbol_to_decl.get(sym_id.index()) {
                entries.push((
                    sym.name.clone(),
                    GlobalDefId::new(ast_id),
                    DefinitionKind::Package,
                ));
            }
        }
    }
    lyra_semantic::global_index::build_global_def_index(&entries)
}

/// Build the package scope index for a compilation unit (Salsa-cached).
///
/// Extracts symbols from package scopes in all files, split by namespace.
#[salsa::tracked(return_ref)]
pub fn package_scope_index(db: &dyn salsa::Database, unit: CompilationUnit) -> PackageScopeIndex {
    let mut packages = Vec::new();
    for file in unit.files(db) {
        let def = def_index_file(db, *file);
        // Find package symbols and their scopes
        for &sym_id in &*def.exports.packages {
            let pkg_sym = def.symbols.get(sym_id);
            let pkg_scope = pkg_sym.scope;
            let scope_data = def.scopes.get(pkg_scope);
            if scope_data.kind != ScopeKind::Package {
                continue;
            }

            let mut value_ns: Vec<(SmolStr, GlobalDefId)> = Vec::new();
            let mut type_ns: Vec<(SmolStr, GlobalDefId)> = Vec::new();

            // Collect Value-namespace symbols from this package scope
            for &child_sym_id in &*scope_data.value_ns {
                let child_sym = def.symbols.get(child_sym_id);
                if let Some(&Some(ast_id)) = def.symbol_to_decl.get(child_sym_id.index()) {
                    value_ns.push((child_sym.name.clone(), GlobalDefId::new(ast_id)));
                }
            }

            // Collect Type-namespace symbols
            for &child_sym_id in &*scope_data.type_ns {
                let child_sym = def.symbols.get(child_sym_id);
                if let Some(&Some(ast_id)) = def.symbol_to_decl.get(child_sym_id.index()) {
                    type_ns.push((child_sym.name.clone(), GlobalDefId::new(ast_id)));
                }
            }

            value_ns.sort_by(|(a, _), (b, _)| a.cmp(b));
            type_ns.sort_by(|(a, _), (b, _)| a.cmp(b));

            packages.push(PackageScope {
                name: pkg_sym.name.clone(),
                value_ns: value_ns.into_boxed_slice(),
                type_ns: type_ns.into_boxed_slice(),
            });
        }
    }
    lyra_semantic::global_index::build_package_scope_index(packages)
}

/// Resolve all use-sites using only offset-independent data (Salsa-cached).
///
/// Depends on `name_graph_file`, `global_def_index`, and
/// `package_scope_index`. When all are backdated (e.g. whitespace edit),
/// this query is NOT re-executed.
#[salsa::tracked(return_ref)]
pub fn resolve_core_file(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> CoreResolveOutput {
    let graph = name_graph_file(db, file);
    let global = global_def_index(db, unit);
    let pkg_scope = package_scope_index(db, unit);
    lyra_semantic::build_resolve_core(graph, global, pkg_scope)
}

/// Build per-file resolution index (Salsa-cached).
///
/// Combines offset-independent resolve results from `resolve_core_file`
/// with offset-dependent data from `def_index_file` (`ast_ids`, ranges)
/// to produce the final `HashMap` and diagnostics. Trivially cheap.
#[salsa::tracked(return_ref)]
pub fn resolve_index_file(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> ResolveIndex {
    let def = def_index_file(db, file);
    let core = resolve_core_file(db, file, unit);
    let lookup_decl = |def_id: GlobalDefId| -> Option<lyra_semantic::symbols::SymbolId> {
        let target_file_id = def_id.file();
        let target_file = source_file_by_id(db, unit, target_file_id)?;
        let target_def = def_index_file(db, target_file);
        target_def.decl_to_symbol.get(&def_id.ast_id()).copied()
    };
    lyra_semantic::build_resolve_index(def, core, &lookup_decl)
}
