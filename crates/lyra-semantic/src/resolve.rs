use std::collections::HashMap;

use lyra_source::TextRange;
use smol_str::SmolStr;

use crate::def_index::{DefIndex, ExpectedNs, ImportName, NamePath};
use crate::diagnostic::{SemanticDiag, SemanticDiagKind};
use crate::global_index::{GlobalDefIndex, PackageScopeIndex};
use crate::name_graph::NameGraph;
use crate::resolve_index::{
    CoreResolution, CoreResolveOutput, CoreResolveResult, ImportError, Resolution, ResolveIndex,
    UnresolvedReason,
};
use crate::scopes::ScopeId;
use crate::symbols::{GlobalDefId, GlobalSymbolId, Namespace, SymbolId};

/// Resolve all use-sites using only offset-independent data.
///
/// Returns `CoreResolveOutput` with reason codes for unresolved names.
///
/// Resolution precedence for `Value`/`Type` namespace (LRM 26.3):
/// 1. Lexical scope (local declarations)
/// 2. Explicit imports in scope chain
/// 3. Wildcard imports in scope chain
///
/// `Definition` namespace: `GlobalDefIndex::resolve_definition`.
/// `Qualified` paths: resolved via `GlobalDefIndex` + `PackageScopeIndex`.
pub fn build_resolve_core(
    graph: &NameGraph,
    global: &GlobalDefIndex,
    pkg_scope: &PackageScopeIndex,
) -> CoreResolveOutput {
    // Validate imports first
    let mut import_errors = Vec::new();
    for (idx, imp) in graph.imports.iter().enumerate() {
        if !pkg_scope.has_package(&imp.package) {
            import_errors.push(ImportError {
                import_idx: idx as u32,
                reason: UnresolvedReason::PackageNotFound {
                    package: imp.package.clone(),
                },
            });
            continue;
        }
        if let ImportName::Explicit(ref member) = imp.name
            && pkg_scope.resolve_any_ns(&imp.package, member).is_none()
        {
            import_errors.push(ImportError {
                import_idx: idx as u32,
                reason: UnresolvedReason::MemberNotFound {
                    package: imp.package.clone(),
                    member: member.clone(),
                },
            });
        }
    }

    let resolutions: Box<[CoreResolveResult]> = graph
        .use_entries
        .iter()
        .map(|entry| match &entry.path {
            NamePath::Simple(name) => resolve_simple(
                graph,
                global,
                pkg_scope,
                entry.scope,
                name,
                entry.expected_ns,
            ),
            NamePath::Qualified { segments } => {
                resolve_qualified(segments, global, pkg_scope, entry.expected_ns)
            }
        })
        .collect();

    CoreResolveOutput {
        resolutions,
        import_errors: import_errors.into_boxed_slice(),
    }
}

fn ns_list(expected: ExpectedNs) -> ([Namespace; 2], usize) {
    match expected {
        ExpectedNs::Exact(ns) => ([ns, ns], 1),
        ExpectedNs::TypeThenValue => ([Namespace::Type, Namespace::Value], 2),
    }
}

fn resolve_simple(
    graph: &NameGraph,
    global: &GlobalDefIndex,
    pkg_scope: &PackageScopeIndex,
    scope: ScopeId,
    name: &str,
    expected: ExpectedNs,
) -> CoreResolveResult {
    if let ExpectedNs::Exact(Namespace::Definition) = expected {
        return if let Some((def_id, _)) = global.resolve_definition(name) {
            CoreResolveResult::Resolved(CoreResolution::Global {
                decl: def_id,
                namespace: Namespace::Definition,
            })
        } else {
            CoreResolveResult::Unresolved(UnresolvedReason::NotFound)
        };
    }

    let (nss, len) = ns_list(expected);
    for &ns in &nss[..len] {
        // 1. Lexical scope
        if let Some(sym_id) = graph.scopes.resolve(graph, scope, ns, name) {
            return CoreResolveResult::Resolved(CoreResolution::Local {
                symbol: sym_id,
                namespace: graph.symbol_kinds[sym_id.0 as usize].namespace(),
            });
        }
        // 2. Explicit imports in scope chain
        if let Some(resolution) = resolve_via_explicit_import(graph, pkg_scope, scope, name, ns) {
            return CoreResolveResult::Resolved(resolution);
        }
        // 3. Wildcard imports in scope chain
        match resolve_via_wildcard_import(graph, pkg_scope, scope, name, ns) {
            WildcardResult::Found(resolution) => {
                return CoreResolveResult::Resolved(resolution);
            }
            WildcardResult::Ambiguous(candidates) => {
                return CoreResolveResult::Unresolved(UnresolvedReason::AmbiguousWildcardImport {
                    candidates,
                });
            }
            WildcardResult::NotFound => {}
        }
    }
    CoreResolveResult::Unresolved(UnresolvedReason::NotFound)
}

fn resolve_qualified(
    segments: &[SmolStr],
    global: &GlobalDefIndex,
    pkg_scope: &PackageScopeIndex,
    expected: ExpectedNs,
) -> CoreResolveResult {
    if segments.len() != 2 {
        return CoreResolveResult::Unresolved(UnresolvedReason::UnsupportedQualifiedPath {
            len: segments.len(),
        });
    }
    let pkg_name = &segments[0];
    let member_name = &segments[1];

    if global.resolve_package(pkg_name).is_none() {
        return CoreResolveResult::Unresolved(UnresolvedReason::PackageNotFound {
            package: pkg_name.clone(),
        });
    }

    let (nss, len) = ns_list(match expected {
        ExpectedNs::Exact(Namespace::Definition) => ExpectedNs::Exact(Namespace::Value),
        other => other,
    });
    for &ns in &nss[..len] {
        if let Some(def_id) = pkg_scope.resolve(pkg_name, member_name, ns) {
            return CoreResolveResult::Resolved(CoreResolution::Global {
                decl: def_id,
                namespace: ns,
            });
        }
    }
    CoreResolveResult::Unresolved(UnresolvedReason::MemberNotFound {
        package: pkg_name.clone(),
        member: member_name.clone(),
    })
}

fn resolve_via_explicit_import(
    graph: &NameGraph,
    pkg_scope: &PackageScopeIndex,
    scope: ScopeId,
    name: &str,
    ns: Namespace,
) -> Option<CoreResolution> {
    // Walk scope chain looking for explicit imports
    let mut current = Some(scope);
    while let Some(sid) = current {
        for imp in &*graph.imports {
            if imp.scope == sid
                && let ImportName::Explicit(ref member) = imp.name
                && member.as_str() == name
                && let Some(def_id) = pkg_scope.resolve(&imp.package, name, ns)
            {
                return Some(CoreResolution::Global {
                    decl: def_id,
                    namespace: ns,
                });
            }
        }
        current = graph.scopes.get(sid).parent;
    }
    None
}

enum WildcardResult {
    Found(CoreResolution),
    Ambiguous(Box<[SmolStr]>),
    NotFound,
}

fn resolve_via_wildcard_import(
    graph: &NameGraph,
    pkg_scope: &PackageScopeIndex,
    scope: ScopeId,
    name: &str,
    ns: Namespace,
) -> WildcardResult {
    let mut current = Some(scope);
    while let Some(sid) = current {
        let mut found: Option<(GlobalDefId, SmolStr)> = None;
        let mut ambiguous_pkgs: Vec<SmolStr> = Vec::new();

        for imp in &*graph.imports {
            if imp.scope == sid
                && imp.name == ImportName::Wildcard
                && let Some(def_id) = pkg_scope.resolve(&imp.package, name, ns)
            {
                if let Some((existing_id, _)) = &found {
                    if *existing_id != def_id {
                        if ambiguous_pkgs.is_empty() {
                            ambiguous_pkgs
                                .push(found.as_ref().map(|(_, p)| p.clone()).unwrap_or_default());
                        }
                        ambiguous_pkgs.push(imp.package.clone());
                    }
                } else {
                    found = Some((def_id, imp.package.clone()));
                }
            }
        }

        if !ambiguous_pkgs.is_empty() {
            return WildcardResult::Ambiguous(ambiguous_pkgs.into_boxed_slice());
        }
        if let Some((def_id, _)) = found {
            return WildcardResult::Found(CoreResolution::Global {
                decl: def_id,
                namespace: ns,
            });
        }
        current = graph.scopes.get(sid).parent;
    }
    WildcardResult::NotFound
}

/// Build the per-file resolution index from pre-computed core results.
///
/// Zips `def.use_sites` with `core.resolutions`, builds the `HashMap`
/// and diagnostics. Import errors are also mapped to diagnostics.
///
/// `lookup_decl` maps a `GlobalDefId` (from `CoreResolution::Global`)
/// to a `SymbolId` in the target file.
pub fn build_resolve_index(
    def: &DefIndex,
    core: &CoreResolveOutput,
    lookup_decl: &dyn Fn(GlobalDefId) -> Option<SymbolId>,
) -> ResolveIndex {
    let mut resolutions = HashMap::new();
    let mut diagnostics = Vec::new();

    for (use_site, result) in def.use_sites.iter().zip(core.resolutions.iter()) {
        match result {
            CoreResolveResult::Resolved(CoreResolution::Local { symbol, namespace }) => {
                resolutions.insert(
                    use_site.ast_id,
                    Resolution {
                        symbol: GlobalSymbolId {
                            file: def.file,
                            local: *symbol,
                        },
                        namespace: *namespace,
                    },
                );
            }
            CoreResolveResult::Resolved(CoreResolution::Global { decl, namespace }) => {
                if let Some(local) = lookup_decl(*decl) {
                    resolutions.insert(
                        use_site.ast_id,
                        Resolution {
                            symbol: GlobalSymbolId {
                                file: decl.file(),
                                local,
                            },
                            namespace: *namespace,
                        },
                    );
                }
            }
            CoreResolveResult::Unresolved(reason) => {
                let diag = reason_to_diagnostic(reason, &use_site.path, use_site.range);
                diagnostics.push(diag);
            }
        }
    }

    // Map import errors to diagnostics
    for err in &core.import_errors {
        let imp = &def.imports[err.import_idx as usize];
        let path = match &imp.name {
            ImportName::Explicit(member) => NamePath::Qualified {
                segments: Box::new([imp.package.clone(), member.clone()]),
            },
            ImportName::Wildcard => NamePath::Simple(SmolStr::new(format!("{}::*", imp.package))),
        };
        let diag = reason_to_diagnostic(&err.reason, &path, imp.range);
        diagnostics.push(diag);
    }

    ResolveIndex {
        file: def.file,
        resolutions,
        diagnostics: diagnostics.into_boxed_slice(),
    }
}

fn reason_to_diagnostic(
    reason: &UnresolvedReason,
    path: &NamePath,
    range: TextRange,
) -> SemanticDiag {
    match reason {
        UnresolvedReason::NotFound => SemanticDiag {
            kind: SemanticDiagKind::UnresolvedName {
                name: SmolStr::new(path.display_name()),
            },
            range,
        },
        UnresolvedReason::PackageNotFound { package } => SemanticDiag {
            kind: SemanticDiagKind::PackageNotFound {
                package: package.clone(),
            },
            range,
        },
        UnresolvedReason::MemberNotFound { package, member } => SemanticDiag {
            kind: SemanticDiagKind::MemberNotFound {
                package: package.clone(),
                member: member.clone(),
            },
            range,
        },
        UnresolvedReason::AmbiguousWildcardImport { candidates } => SemanticDiag {
            kind: SemanticDiagKind::AmbiguousWildcardImport {
                name: SmolStr::new(path.display_name()),
                candidates: candidates.clone(),
            },
            range,
        },
        UnresolvedReason::UnsupportedQualifiedPath { .. } => SemanticDiag {
            kind: SemanticDiagKind::UnsupportedQualifiedPath {
                path: SmolStr::new(path.display_name()),
            },
            range,
        },
    }
}
