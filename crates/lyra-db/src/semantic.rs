use lyra_semantic::def_index::{CompilationUnitEnv, DefIndex, ImplicitImport, ImportName};
use lyra_semantic::global_index::{
    DefinitionKind, GlobalDefIndex, PackageLocalFacts, PackageScope, PackageScopeIndex,
};
use lyra_semantic::instance_decl::InstanceDeclIdx;
use lyra_semantic::name_graph::NameGraph;
use lyra_semantic::resolve_index::{
    CoreResolveOutput, CoreResolveResult, ImportConflict, ResolveIndex,
};
use lyra_semantic::scopes::ScopeKind;
use lyra_semantic::symbols::{GlobalDefId, GlobalSymbolId};
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
/// Resolves export declarations to include re-exported symbols.
#[salsa::tracked(return_ref)]
pub fn package_scope_index(db: &dyn salsa::Database, unit: CompilationUnit) -> PackageScopeIndex {
    // Phase 1: Collect local facts for each package
    let mut all_facts: Vec<PackageLocalFacts> = Vec::new();
    for file in unit.files(db) {
        let def = def_index_file(db, *file);
        let graph = name_graph_file(db, *file);
        for &sym_id in &*def.exports.packages {
            let pkg_sym = def.symbols.get(sym_id);
            let pkg_scope = pkg_sym.scope;
            let scope_data = def.scopes.get(pkg_scope);
            if scope_data.kind != ScopeKind::Package {
                continue;
            }

            let mut value_ns: Vec<(SmolStr, GlobalDefId)> = Vec::new();
            let mut type_ns: Vec<(SmolStr, GlobalDefId)> = Vec::new();

            for &child_sym_id in &*scope_data.value_ns {
                let child_sym = def.symbols.get(child_sym_id);
                if let Some(&Some(ast_id)) = def.symbol_to_decl.get(child_sym_id.index()) {
                    value_ns.push((child_sym.name.clone(), GlobalDefId::new(ast_id)));
                }
            }

            for &child_sym_id in &*scope_data.type_ns {
                let child_sym = def.symbols.get(child_sym_id);
                if let Some(&Some(ast_id)) = def.symbol_to_decl.get(child_sym_id.index()) {
                    type_ns.push((child_sym.name.clone(), GlobalDefId::new(ast_id)));
                }
            }

            value_ns.sort_by(|(a, _), (b, _)| a.cmp(b));
            type_ns.sort_by(|(a, _), (b, _)| a.cmp(b));

            // Collect import package names for *::* export resolution
            let import_packages: Vec<SmolStr> = graph
                .imports()
                .iter()
                .filter(|imp| imp.scope == pkg_scope)
                .map(|imp| imp.package.clone())
                .collect();
            let import_names = graph
                .imports()
                .iter()
                .filter(|imp| imp.scope == pkg_scope)
                .map(|imp| imp.name.clone())
                .collect();

            all_facts.push(PackageLocalFacts {
                name: pkg_sym.name.clone(),
                value_ns,
                type_ns,
                imports: import_names,
                import_packages,
                export_decls: graph.export_decls().iter().map(|e| e.key.clone()).collect(),
            });
        }
    }

    // Phase 2: Build initial package scopes (local defs only, no export resolution)
    let mut base_scopes: Vec<PackageScope> = all_facts
        .iter()
        .map(|f| {
            PackageScope::new(
                f.name.clone(),
                f.value_ns.clone().into_boxed_slice(),
                f.type_ns.clone().into_boxed_slice(),
            )
        })
        .collect();
    base_scopes.sort_by(|a, b| a.pkg_name().cmp(b.pkg_name()));

    // Phase 3: Resolve exports for packages that have export declarations
    let has_exports = all_facts.iter().any(|f| !f.export_decls.is_empty());
    let mut packages = if has_exports {
        all_facts
            .iter()
            .map(|facts| {
                if facts.export_decls.is_empty() {
                    PackageScope::new(
                        facts.name.clone(),
                        facts.value_ns.clone().into_boxed_slice(),
                        facts.type_ns.clone().into_boxed_slice(),
                    )
                } else {
                    lyra_semantic::global_index::compute_public_surface(facts, &|dep_name| {
                        base_scopes
                            .binary_search_by(|p| p.pkg_name().as_str().cmp(dep_name))
                            .ok()
                            .map(|idx| base_scopes[idx].clone())
                    })
                }
            })
            .collect()
    } else {
        base_scopes
    };

    // Phase 4: Add builtin std package if no user-defined std exists
    let has_std = packages.iter().any(|p| p.pkg_name() == "std");
    if !has_std {
        packages.push(PackageScope::new(
            SmolStr::new_static("std"),
            Box::new([]),
            Box::new([]),
        ));
    }

    lyra_semantic::global_index::build_package_scope_index(packages)
}

/// Build the compilation-unit environment (Salsa-cached).
///
/// Contains implicit imports (e.g., `import std::*`) that are visible
/// in all files within the compilation unit. User-defined `package std`
/// takes precedence over the builtin empty surface.
#[salsa::tracked(return_ref)]
pub fn compilation_unit_env(
    _db: &dyn salsa::Database,
    _unit: CompilationUnit,
) -> CompilationUnitEnv {
    CompilationUnitEnv {
        implicit_imports: vec![ImplicitImport {
            package: SmolStr::new_static("std"),
            name: ImportName::Wildcard,
        }]
        .into(),
    }
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
    let cu_env = compilation_unit_env(db, unit);
    lyra_semantic::build_resolve_core(graph, global, pkg_scope, cu_env)
}

/// Detect import conflicts per LRM 26.5 (Salsa-cached).
///
/// Depends on `name_graph_file` (per-file, backdates on whitespace edits)
/// and `package_scope_index` (per-unit).
#[salsa::tracked(return_ref)]
pub fn import_conflicts_file(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> Box<[ImportConflict]> {
    let graph = name_graph_file(db, file);
    let pkg_scope = package_scope_index(db, unit);
    lyra_semantic::detect_import_conflicts(graph, pkg_scope)
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
    let global = global_def_index(db, unit);
    let lookup_decl = |def_id: GlobalDefId| -> Option<lyra_semantic::symbols::SymbolId> {
        let target_file_id = def_id.file();
        let target_file = source_file_by_id(db, unit, target_file_id)?;
        let target_def = def_index_file(db, target_file);
        target_def.decl_to_symbol.get(&def_id.ast_id()).copied()
    };
    let instance_filter =
        |idx: InstanceDeclIdx| -> bool { instance_decl_is_interface(core, def, global, idx) };
    lyra_semantic::build_resolve_index(def, core, &lookup_decl, &instance_filter)
}

/// Check whether an instance declaration refers to an interface type.
///
/// Uses the already-computed `resolve_core_file` result to look up the
/// instance's type-name use-site resolution, then checks the global
/// definition index for `DefinitionKind::Interface`.
fn instance_decl_is_interface(
    core: &CoreResolveOutput,
    def: &DefIndex,
    global: &GlobalDefIndex,
    idx: InstanceDeclIdx,
) -> bool {
    let decl = def.instance_decl(idx);
    let Some(result) = core.resolutions.get(decl.type_use_site_idx as usize) else {
        return false;
    };
    if let CoreResolveResult::Resolved(lyra_semantic::resolve_index::CoreResolution::Global {
        decl: def_id,
        ..
    }) = result
    {
        matches!(global.def_kind(*def_id), Some(DefinitionKind::Interface))
    } else {
        false
    }
}

/// Canonical `GlobalDefId` -> `GlobalSymbolId` resolution (Salsa-tracked).
///
/// Single path from a cross-file definition identity to the symbol that
/// declares it. Both `modport_sem` and `member_lookup` use this.
#[salsa::tracked]
pub fn def_symbol(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    def_id: GlobalDefId,
) -> Option<GlobalSymbolId> {
    let file_id = def_id.file();
    let source_file = source_file_by_id(db, unit, file_id)?;
    let def = def_index_file(db, source_file);
    let local = def.decl_to_symbol.get(&def_id.ast_id()).copied()?;
    Some(GlobalSymbolId {
        file: file_id,
        local,
    })
}
