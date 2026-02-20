use std::collections::HashMap;

use lyra_source::TextRange;
use smallvec::{SmallVec, smallvec};
use smol_str::SmolStr;

use crate::def_index::{
    CompilationUnitEnv, DefIndex, ExpectedNs, ExportDeclId, ExportKey, ImportName, NamePath,
};
use crate::diagnostic::{SemanticDiag, SemanticDiagKind};
use crate::global_index::{GlobalDefIndex, PackageScopeIndex};
use crate::name_graph::NameGraph;
use crate::resolve_index::{
    CoreResolution, CoreResolveOutput, CoreResolveResult, ImportConflict, ImportConflictKind,
    ImportError, Resolution, ResolveIndex, UnresolvedReason,
};
use crate::scopes::{ScopeId, SymbolNameLookup};
use crate::symbols::{GlobalDefId, GlobalSymbolId, Namespace, NsMask, SymbolId};

/// Shared resolution context for `resolve_simple` and related functions.
struct ResolveCtx<'a> {
    graph: &'a NameGraph,
    global: &'a GlobalDefIndex,
    pkg_scope: &'a PackageScopeIndex,
    cu_env: &'a CompilationUnitEnv,
}

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
    cu_env: &CompilationUnitEnv,
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

    let ctx = ResolveCtx {
        graph,
        global,
        pkg_scope,
        cu_env,
    };
    let resolutions: Box<[CoreResolveResult]> = graph
        .use_entries
        .iter()
        .map(|entry| match &entry.path {
            NamePath::Simple(name) => {
                resolve_simple(&ctx, entry.scope, name, entry.expected_ns, entry.order_key)
            }
            // Qualified names (P::x) bypass positional import filtering
            // because visibility is determined solely by the package
            // qualification, not by import declaration position.
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

/// A name imported into a scope -- by explicit import or export-triggered
/// wildcard promotion.
pub(crate) struct RealizedBinding {
    pub name: SmolStr,
    pub target: GlobalDefId,
    pub ns: Namespace,
    pub origins: SmallVec<[ImportOrigin; 1]>,
}

pub(crate) enum ImportOrigin {
    /// Direct `import P::N` statement.
    Explicit { package: SmolStr },
    /// `export P::N` triggered promotion of a wildcard candidate.
    ExportTriggered { id: ExportDeclId, package: SmolStr },
}

impl ImportOrigin {
    pub fn package(&self) -> &SmolStr {
        match self {
            Self::Explicit { package } | Self::ExportTriggered { package, .. } => package,
        }
    }
}

/// Intermediate record for accumulating wildcard conflict info across namespaces.
struct WcConflictEntry {
    name: SmolStr,
    explicit_package: SmolStr,
    wildcard_package: SmolStr,
    ns: NsMask,
    export_sources: SmallVec<[ExportDeclId; 1]>,
}

/// Canonical sort key for deterministic origin ordering:
/// `Explicit` (0) before `ExportTriggered` (1), then package name, then ordinal.
fn origin_sort_key(o: &ImportOrigin) -> (u8, &str, u32) {
    match o {
        ImportOrigin::Explicit { package } => (0, package.as_str(), 0),
        ImportOrigin::ExportTriggered { id, package } => (1, package.as_str(), id.ordinal),
    }
}

/// Build the effective imported bindings for a single scope.
///
/// Single source of truth for a scope's realized imports:
/// 1. Explicit imports
/// 2. Wildcard candidate map
/// 3. Export-triggered promotion
/// 4. Dedup by (name, ns, target)
pub(crate) fn build_effective_imports(
    graph: &NameGraph,
    pkg_scope: &PackageScopeIndex,
    scope_id: ScopeId,
) -> Vec<RealizedBinding> {
    let mut realized = Vec::new();

    // Step 1: explicit imports -> realized
    for imp in graph.imports_for_scope(scope_id) {
        if let ImportName::Explicit(ref member) = imp.name {
            for &ns in &[Namespace::Value, Namespace::Type] {
                if let Some(target) = pkg_scope.resolve(&imp.package, member, ns) {
                    realized.push(RealizedBinding {
                        name: member.clone(),
                        target,
                        ns,
                        origins: smallvec![ImportOrigin::Explicit {
                            package: imp.package.clone(),
                        }],
                    });
                }
            }
        }
    }

    // Step 2: build wildcard candidate maps (split by namespace)
    let mut value_candidates: HashMap<SmolStr, Vec<GlobalDefId>> = HashMap::new();
    let mut type_candidates: HashMap<SmolStr, Vec<GlobalDefId>> = HashMap::new();
    for imp in graph.imports_for_scope(scope_id) {
        if imp.name == ImportName::Wildcard
            && let Some(surface) = pkg_scope.public_surface(&imp.package)
        {
            for (name, id) in surface.value_iter() {
                value_candidates.entry(name.clone()).or_default().push(*id);
            }
            for (name, id) in surface.type_iter() {
                type_candidates.entry(name.clone()).or_default().push(*id);
            }
        }
    }
    // Canonicalize candidate sets for binary_search in Step 3
    for ids in value_candidates.values_mut() {
        ids.sort();
        ids.dedup();
    }
    for ids in type_candidates.values_mut() {
        ids.sort();
        ids.dedup();
    }

    // Step 3: export-triggered promotion (explicit named exports only)
    for export in graph.exports_for_scope(scope_id) {
        if let ExportKey::Explicit {
            ref package,
            ref name,
        } = export.key
        {
            let cands_by_ns = [
                (Namespace::Value, &value_candidates),
                (Namespace::Type, &type_candidates),
            ];
            for (ns, cands) in &cands_by_ns {
                if let Some(target) = pkg_scope.resolve(package, name, *ns) {
                    let in_candidates = cands
                        .get(name)
                        .is_some_and(|ids| ids.binary_search(&target).is_ok());
                    if in_candidates {
                        realized.push(RealizedBinding {
                            name: name.clone(),
                            target,
                            ns: *ns,
                            origins: smallvec![ImportOrigin::ExportTriggered {
                                id: export.id,
                                package: package.clone(),
                            }],
                        });
                    }
                }
            }
        }
    }

    // Step 4: dedup by (name, ns, target), merging origins on collision
    realized.sort_by(|a, b| (&a.name, &a.ns, &a.target).cmp(&(&b.name, &b.ns, &b.target)));
    realized.dedup_by(|b, a| {
        a.name == b.name && a.ns == b.ns && a.target == b.target && {
            a.origins.extend(b.origins.drain(..));
            true
        }
    });
    // Canonicalize origin order
    for binding in &mut realized {
        binding
            .origins
            .sort_by(|a, b| origin_sort_key(a).cmp(&origin_sort_key(b)));
        binding
            .origins
            .dedup_by(|a, b| origin_sort_key(a) == origin_sort_key(b));
    }

    realized
}

/// Detect import conflicts per LRM 26.5.
///
/// Check 1 (26.5a): realized binding vs local declaration
/// (covers both explicit imports and export-triggered wildcard promotion).
/// Check 2 (26.5b): explicit import conflicts with wildcard-provided name
/// from a different package (different `GlobalDefId`).
pub fn detect_import_conflicts(
    graph: &NameGraph,
    pkg_scope: &PackageScopeIndex,
) -> Box<[ImportConflict]> {
    let mut conflicts = Vec::new();

    for scope_idx in 0..graph.scopes.len() {
        let scope_id = ScopeId(scope_idx as u32);
        let scope = graph.scopes.get(scope_id);

        // Shared binding table
        let effective = build_effective_imports(graph, pkg_scope, scope_id);

        // Check 1 (26.5a): realized binding vs local
        for binding in &effective {
            let has_local = match binding.ns {
                Namespace::Value => scope
                    .value_ns
                    .binary_search_by(|id| graph.name(*id).cmp(binding.name.as_str()))
                    .is_ok(),
                Namespace::Type => scope
                    .type_ns
                    .binary_search_by(|id| graph.name(*id).cmp(binding.name.as_str()))
                    .is_ok(),
                Namespace::Definition => false,
            };
            if has_local {
                let package = binding.origins[0].package().clone();
                conflicts.push(ImportConflict {
                    scope: scope_id,
                    name: binding.name.clone(),
                    kind: ImportConflictKind::ExplicitVsLocal {
                        package,
                        ns: NsMask::from_namespace(binding.ns),
                    },
                    export_sources: export_sources_from(binding),
                });
            }
        }

        // Check 2 (26.5b): realized binding vs wildcard from different package
        detect_wildcard_conflicts(graph, pkg_scope, scope_id, &effective, &mut conflicts);
    }

    conflicts.into_boxed_slice()
}

fn export_sources_from(binding: &RealizedBinding) -> SmallVec<[ExportDeclId; 1]> {
    binding
        .origins
        .iter()
        .filter_map(|o| match o {
            ImportOrigin::ExportTriggered { id, .. } => Some(*id),
            ImportOrigin::Explicit { .. } => None,
        })
        .collect()
}

/// Check 2 (26.5b): realized binding vs wildcard from a different package.
/// Covers both explicit imports and export-triggered promotions.
fn detect_wildcard_conflicts(
    graph: &NameGraph,
    pkg_scope: &PackageScopeIndex,
    scope_id: ScopeId,
    effective: &[RealizedBinding],
    conflicts: &mut Vec<ImportConflict>,
) {
    let wildcard_packages: SmallVec<[&SmolStr; 4]> = graph
        .imports_for_scope(scope_id)
        .iter()
        .filter(|i| i.name == ImportName::Wildcard)
        .map(|i| &i.package)
        .collect();

    // Accumulate NsMask per (name, explicit_pkg, wildcard_pkg) to avoid
    // duplicate diagnostics when both namespaces conflict.
    let mut entries: Vec<WcConflictEntry> = Vec::new();

    for binding in effective {
        let explicit_pkg = binding.origins[0].package();
        for &wc_pkg in &wildcard_packages {
            if wc_pkg == explicit_pkg {
                continue;
            }
            if let Some(wid) = pkg_scope.resolve(wc_pkg, &binding.name, binding.ns)
                && wid != binding.target
            {
                entries.push(WcConflictEntry {
                    name: binding.name.clone(),
                    explicit_package: explicit_pkg.clone(),
                    wildcard_package: wc_pkg.clone(),
                    ns: NsMask::from_namespace(binding.ns),
                    export_sources: export_sources_from(binding),
                });
            }
        }
    }

    // Sort by (name, explicit_pkg, wildcard_pkg), then merge adjacent entries
    entries.sort_by(|a, b| {
        (&a.name, &a.explicit_package, &a.wildcard_package).cmp(&(
            &b.name,
            &b.explicit_package,
            &b.wildcard_package,
        ))
    });
    entries.dedup_by(|b, a| {
        a.name == b.name
            && a.explicit_package == b.explicit_package
            && a.wildcard_package == b.wildcard_package
            && {
                a.ns = a.ns.union(b.ns);
                for id in b.export_sources.drain(..) {
                    if !a.export_sources.contains(&id) {
                        a.export_sources.push(id);
                    }
                }
                true
            }
    });

    for entry in entries {
        conflicts.push(ImportConflict {
            scope: scope_id,
            name: entry.name,
            kind: ImportConflictKind::ExplicitVsWildcard {
                explicit_package: entry.explicit_package,
                wildcard_package: entry.wildcard_package,
                ns: entry.ns,
            },
            export_sources: entry.export_sources,
        });
    }
}

fn ns_list(expected: ExpectedNs) -> ([Namespace; 2], usize) {
    match expected {
        ExpectedNs::Exact(ns) => ([ns, ns], 1),
        ExpectedNs::TypeThenValue => ([Namespace::Type, Namespace::Value], 2),
    }
}

/// Non-positioned name resolution: all imports visible regardless of
/// source position. Bypasses LRM 26.3 positional import filtering.
///
/// Public API for ad-hoc resolution (struct field type resolution, etc.)
/// without going through the use-site / resolve-index pipeline.
/// Use-site resolution goes through `build_resolve_core` which supplies
/// each use-site's actual `order_key`.
pub fn resolve_name_in_scope(
    graph: &NameGraph,
    global: &GlobalDefIndex,
    pkg_scope: &PackageScopeIndex,
    cu_env: &CompilationUnitEnv,
    scope: ScopeId,
    name: &str,
    expected: ExpectedNs,
) -> CoreResolveResult {
    let ctx = ResolveCtx {
        graph,
        global,
        pkg_scope,
        cu_env,
    };
    resolve_simple(&ctx, scope, name, expected, u32::MAX)
}

/// Resolve a qualified name (e.g. `Pkg::member`).
///
/// Public API for ad-hoc resolution without going through the
/// use-site / resolve-index pipeline.
pub fn resolve_qualified_name(
    segments: &[SmolStr],
    global: &GlobalDefIndex,
    pkg_scope: &PackageScopeIndex,
    expected: ExpectedNs,
) -> CoreResolveResult {
    resolve_qualified(segments, global, pkg_scope, expected)
}

fn resolve_simple(
    ctx: &ResolveCtx<'_>,
    scope: ScopeId,
    name: &str,
    expected: ExpectedNs,
    use_order_key: u32,
) -> CoreResolveResult {
    if let ExpectedNs::Exact(Namespace::Definition) = expected {
        return if let Some((def_id, _)) = ctx.global.resolve_definition(name) {
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
        if let Some(sym_id) = ctx.graph.scopes.resolve(ctx.graph, scope, ns, name) {
            return CoreResolveResult::Resolved(CoreResolution::Local {
                symbol: sym_id,
                namespace: ctx.graph.symbol_kinds[sym_id.0 as usize].namespace(),
            });
        }
        // 2. Explicit imports in scope chain (positional filter)
        if let Some(resolution) =
            resolve_via_explicit_import(ctx.graph, ctx.pkg_scope, scope, name, ns, use_order_key)
        {
            return CoreResolveResult::Resolved(resolution);
        }
        // 3. Wildcard imports in scope chain (positional filter)
        match resolve_via_wildcard_import(ctx.graph, ctx.pkg_scope, scope, name, ns, use_order_key)
        {
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
        // 4. CU-level implicit imports (e.g., std)
        match resolve_via_implicit_import(ctx.pkg_scope, ctx.cu_env, name, ns) {
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

    // For type-position lookups, fall back to the definition namespace.
    // Interface names used as types (e.g. `my_bus b;`) live in the
    // definition namespace per IEEE 1800 section 3.13(a).
    if matches!(expected, ExpectedNs::TypeThenValue)
        && let Some((def_id, _)) = ctx.global.resolve_definition(name)
    {
        return CoreResolveResult::Resolved(CoreResolution::Global {
            decl: def_id,
            namespace: Namespace::Definition,
        });
    }

    CoreResolveResult::Unresolved(UnresolvedReason::NotFound)
}

/// Qualified names (`Pkg::member`) bypass positional import filtering
/// because their visibility is determined solely by the package
/// qualification, not by import declaration position.
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
    use_order_key: u32,
) -> Option<CoreResolution> {
    // Walk scope chain looking for explicit imports
    let mut current = Some(scope);
    while let Some(sid) = current {
        for imp in graph.imports_for_scope(sid) {
            if let ImportName::Explicit(ref member) = imp.name
                && member.as_str() == name
                && imp.order_key < use_order_key
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
    use_order_key: u32,
) -> WildcardResult {
    let mut current = Some(scope);
    while let Some(sid) = current {
        let mut found: Option<(GlobalDefId, SmolStr)> = None;
        let mut ambiguous_pkgs: Vec<SmolStr> = Vec::new();

        for imp in graph.imports_for_scope(sid) {
            if imp.name == ImportName::Wildcard
                && imp.order_key < use_order_key
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

fn resolve_via_implicit_import(
    pkg_scope: &PackageScopeIndex,
    cu_env: &CompilationUnitEnv,
    name: &str,
    ns: Namespace,
) -> WildcardResult {
    let mut found: Option<(GlobalDefId, SmolStr)> = None;
    let mut ambiguous_pkgs: Vec<SmolStr> = Vec::new();

    for imp in &*cu_env.implicit_imports {
        if imp.name == ImportName::Wildcard {
            if let Some(def_id) = pkg_scope.resolve(&imp.package, name, ns) {
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
        } else if let ImportName::Explicit(ref member) = imp.name
            && member.as_str() == name
            && let Some(def_id) = pkg_scope.resolve(&imp.package, name, ns)
        {
            return WildcardResult::Found(CoreResolution::Global {
                decl: def_id,
                namespace: ns,
            });
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
                if let Some(diag) = check_type_mismatch(
                    use_site.expected_ns,
                    *namespace,
                    &use_site.path,
                    use_site.range,
                ) {
                    diagnostics.push(diag);
                }
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
                    if let Some(diag) = check_type_mismatch(
                        use_site.expected_ns,
                        *namespace,
                        &use_site.path,
                        use_site.range,
                    ) {
                        diagnostics.push(diag);
                    }
                }
            }
            CoreResolveResult::Unresolved(reason) => {
                let diag = reason_to_diagnostic(
                    reason,
                    use_site.expected_ns,
                    &use_site.path,
                    use_site.range,
                );
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
        let diag = reason_to_diagnostic(
            &err.reason,
            ExpectedNs::Exact(Namespace::Value),
            &path,
            imp.range,
        );
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
    expected_ns: ExpectedNs,
    path: &NamePath,
    range: TextRange,
) -> SemanticDiag {
    match reason {
        UnresolvedReason::NotFound => {
            let name = SmolStr::new(path.display_name());
            let kind = if matches!(
                expected_ns,
                ExpectedNs::TypeThenValue | ExpectedNs::Exact(Namespace::Type)
            ) {
                SemanticDiagKind::UndeclaredType { name }
            } else {
                SemanticDiagKind::UnresolvedName { name }
            };
            SemanticDiag { kind, range }
        }
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

fn check_type_mismatch(
    expected_ns: ExpectedNs,
    actual_ns: Namespace,
    path: &NamePath,
    range: TextRange,
) -> Option<SemanticDiag> {
    if matches!(expected_ns, ExpectedNs::TypeThenValue) && actual_ns == Namespace::Value {
        Some(SemanticDiag {
            kind: SemanticDiagKind::NotAType {
                name: SmolStr::new(path.display_name()),
            },
            range,
        })
    } else {
        None
    }
}
