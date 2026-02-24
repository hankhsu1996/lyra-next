use std::collections::HashMap;

use lyra_ast::ErasedAstId;
use lyra_semantic::def_index::{CompilationUnitEnv, DefIndex, ImplicitImport, ImportName};
use lyra_semantic::enum_def::PkgEnumVariantIndex;
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

use crate::enum_queries::enum_variant_index;
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
            if let Some(def_kind) = DefinitionKind::from_symbol_kind(sym.kind) {
                entries.push((sym.name.clone(), GlobalDefId::new(sym.decl_site), def_kind));
            }
        }
        // Collect packages (separate namespace per LRM 3.13(b))
        for &sym_id in &*def.exports.packages {
            let sym = def.symbols.get(sym_id);
            entries.push((
                sym.name.clone(),
                GlobalDefId::new(sym.decl_site),
                DefinitionKind::Package,
            ));
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

            let mut value_ns: Vec<(SmolStr, ErasedAstId)> = Vec::new();
            let mut type_ns: Vec<(SmolStr, ErasedAstId)> = Vec::new();

            for &child_sym_id in &*scope_data.value_ns {
                let child_sym = def.symbols.get(child_sym_id);
                value_ns.push((child_sym.name.clone(), child_sym.name_site));
            }

            for &child_sym_id in &*scope_data.type_ns {
                let child_sym = def.symbols.get(child_sym_id);
                type_ns.push((child_sym.name.clone(), child_sym.name_site));
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
/// Base version: no enum variant data. Used by `base_resolve_index`
/// (the const-eval path) to avoid cycles with `enum_variant_index`.
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
    lyra_semantic::build_resolve_core(graph, global, pkg_scope, cu_env, None, None)
}

/// Enriched core resolution with enum variant data (Salsa-cached).
///
/// Includes range-generated enum variant names (LRM 6.19.3) from both
/// local enums and wildcard-imported packages.
#[salsa::tracked(return_ref)]
fn enriched_resolve_core(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> CoreResolveOutput {
    let graph = name_graph_file(db, file);
    let global = global_def_index(db, unit);
    let pkg_scope = package_scope_index(db, unit);
    let cu_env = compilation_unit_env(db, unit);
    let local_ev = enum_variant_index(db, file, unit);
    let pkg_ev = build_pkg_enum_variant_index(db, unit, graph, global);
    lyra_semantic::build_resolve_core(
        graph,
        global,
        pkg_scope,
        cu_env,
        Some(local_ev),
        if pkg_ev.is_empty() {
            None
        } else {
            Some(&pkg_ev)
        },
    )
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

/// Base per-file resolution index without enum enrichment (Salsa-cached).
///
/// Only for `eval_const_int` to avoid cycles with `enum_variant_index`.
#[salsa::tracked(return_ref)]
pub fn base_resolve_index(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> ResolveIndex {
    let def = def_index_file(db, file);
    let core = resolve_core_file(db, file, unit);
    let global = global_def_index(db, unit);
    let lookup_decl = |ast_id: ErasedAstId| -> Option<lyra_semantic::symbols::SymbolId> {
        let target_file_id = ast_id.file();
        let target_file = source_file_by_id(db, unit, target_file_id)?;
        let target_def = def_index_file(db, target_file);
        target_def.name_site_to_symbol.get(&ast_id).copied()
    };
    let instance_filter =
        |idx: InstanceDeclIdx| -> bool { instance_decl_is_interface(core, def, global, idx) };
    lyra_semantic::build_resolve_index(def, core, &lookup_decl, &instance_filter)
}

/// Build per-file resolution index (Salsa-cached).
///
/// Enriched version with enum variant data. All consumers use this
/// except `eval_const_int` which uses `base_resolve_index`.
#[salsa::tracked(return_ref)]
pub fn resolve_index_file(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> ResolveIndex {
    let def = def_index_file(db, file);
    let core = enriched_resolve_core(db, file, unit);
    let global = global_def_index(db, unit);
    let lookup_decl = |ast_id: ErasedAstId| -> Option<lyra_semantic::symbols::SymbolId> {
        let target_file_id = ast_id.file();
        let target_file = source_file_by_id(db, unit, target_file_id)?;
        let target_def = def_index_file(db, target_file);
        target_def.name_site_to_symbol.get(&ast_id).copied()
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
    if let CoreResolveResult::Resolved(lyra_semantic::resolve_index::CoreResolution::Def {
        def: def_id,
    }) = result
    {
        matches!(global.def_kind(*def_id), Some(DefinitionKind::Interface))
    } else {
        false
    }
}

/// Canonical cross-file `name_site` anchor -> `GlobalSymbolId` resolution
/// (Salsa-tracked).
///
/// Resolves a cross-file `name_site` anchor to the symbol declared at
/// that site. Used by `modport_sem`, `member_lookup`, and other consumers.
#[salsa::tracked]
pub fn symbol_at_name_site(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    name_site: ErasedAstId,
) -> Option<GlobalSymbolId> {
    let file_id = name_site.file();
    let source_file = source_file_by_id(db, unit, file_id)?;
    let def = def_index_file(db, source_file);
    let local = def.name_site_to_symbol.get(&name_site).copied()?;
    Some(GlobalSymbolId {
        file: file_id,
        local,
    })
}

/// Resolve a `ModportDefId` to its declared name (Salsa-tracked).
#[salsa::tracked]
pub fn modport_name(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    mp_id: lyra_semantic::modport_def::ModportDefId,
) -> SmolStr {
    let file_id = mp_id.owner.global_def().file();
    let Some(src) = source_file_by_id(db, unit, file_id) else {
        return SmolStr::default();
    };
    let def = def_index_file(db, src);
    def.modport_defs
        .get(&mp_id)
        .map(|d| d.name.clone())
        .unwrap_or_default()
}

/// Build per-package enum variant index for wildcard imports.
///
/// Iterates wildcard imports in the file's name graph, resolves each
/// package, and extracts range-generated enum variant names from the
/// package's scope in the target file's `enum_variant_index`.
fn build_pkg_enum_variant_index(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    graph: &NameGraph,
    global: &GlobalDefIndex,
) -> PkgEnumVariantIndex {
    let mut result: PkgEnumVariantIndex = HashMap::new();
    let mut seen_packages: std::collections::HashSet<SmolStr> = std::collections::HashSet::new();

    for imp in graph.imports() {
        if imp.name != ImportName::Wildcard {
            continue;
        }
        if !seen_packages.insert(imp.package.clone()) {
            continue;
        }
        let Some(def_id) = global.resolve_package(&imp.package) else {
            continue;
        };
        let pkg_file_id = def_id.file();
        let Some(pkg_source_file) = source_file_by_id(db, unit, pkg_file_id) else {
            continue;
        };
        let pkg_def = def_index_file(db, pkg_source_file);
        let Some(pkg_scope_id) = pkg_def.package_scope(&imp.package) else {
            continue;
        };
        let ev_idx = enum_variant_index(db, pkg_source_file, unit);
        if let Some(scope_map) = ev_idx.by_scope.get(&pkg_scope_id) {
            result.insert(imp.package.clone(), scope_map.clone());
        }
    }
    result
}
