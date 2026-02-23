use std::collections::HashMap;

use lyra_source::TextRange;
use smallvec::{SmallVec, smallvec};
use smol_str::SmolStr;

use crate::def_index::{
    CompilationUnitEnv, DefIndex, ExpectedNs, ExportDeclId, ExportKey, ImportName, LocalDeclId,
    NamePath,
};
use crate::diagnostic::{SemanticDiag, SemanticDiagKind};
use crate::enum_def::{EnumVariantIndex, PkgEnumVariantIndex};
use crate::global_index::{GlobalDefIndex, PackageScopeIndex};
use crate::name_graph::NameGraph;
use crate::resolve_index::{
    CoreResolution, CoreResolveOutput, CoreResolveResult, ImportConflict, ImportConflictKind,
    ImportError, Resolution, ResolveIndex, ResolvedTarget, UnresolvedReason, WildcardLocalConflict,
};
use crate::scopes::{ScopeId, SymbolNameLookup};
use crate::symbols::{GlobalSymbolId, Namespace, NsMask, SymbolId};

/// Shared resolution context for `resolve_simple` and related functions.
struct ResolveCtx<'a> {
    graph: &'a NameGraph,
    global: &'a GlobalDefIndex,
    pkg_scope: &'a PackageScopeIndex,
    cu_env: &'a CompilationUnitEnv,
    local_enum_variants: Option<&'a EnumVariantIndex>,
    pkg_enum_variants: Option<&'a PkgEnumVariantIndex>,
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
    local_enum_variants: Option<&EnumVariantIndex>,
    pkg_enum_variants: Option<&PkgEnumVariantIndex>,
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
        local_enum_variants,
        pkg_enum_variants,
    };
    let resolutions: Box<[CoreResolveResult]> = graph
        .use_entries
        .iter()
        .map(|entry| match &entry.path {
            NamePath::Simple(name) => {
                resolve_simple(&ctx, entry.scope, name, entry.expected_ns, entry.order_key)
            }
            NamePath::Qualified { segments } => {
                resolve_qualified(segments, global, pkg_scope, entry.expected_ns)
            }
        })
        .collect();

    let wildcard_local_conflicts = detect_wildcard_local_conflicts(&ctx, &graph.use_entries);

    CoreResolveOutput {
        resolutions,
        import_errors: import_errors.into_boxed_slice(),
        wildcard_local_conflicts: wildcard_local_conflicts.into_boxed_slice(),
    }
}

/// A name imported into a scope -- by explicit import or export-triggered
/// wildcard promotion.
pub(crate) struct RealizedBinding {
    pub name: SmolStr,
    pub target: lyra_ast::ErasedAstId,
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
    let mut value_candidates: HashMap<SmolStr, Vec<lyra_ast::ErasedAstId>> = HashMap::new();
    let mut type_candidates: HashMap<SmolStr, Vec<lyra_ast::ErasedAstId>> = HashMap::new();
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
        local_enum_variants: None,
        pkg_enum_variants: None,
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
            CoreResolveResult::Resolved(CoreResolution::Def { def: def_id })
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
        // 1b. Local enum variant names (range-generated, LRM 6.19.3)
        if ns == Namespace::Value
            && let Some(ev_idx) = ctx.local_enum_variants
        {
            let mut s = scope;
            loop {
                if let Some(inner) = ev_idx.by_scope.get(&s)
                    && let Some(target) = inner.get(name)
                {
                    return CoreResolveResult::Resolved(CoreResolution::EnumVariant(
                        target.clone(),
                    ));
                }
                match ctx.graph.scopes.get(s).parent {
                    Some(p) => s = p,
                    None => break,
                }
            }
        }
        // 2. Explicit imports in scope chain (positional filter)
        if let Some(resolution) =
            resolve_via_explicit_import(ctx.graph, ctx.pkg_scope, scope, name, ns, use_order_key)
        {
            return CoreResolveResult::Resolved(resolution);
        }
        // 3. Wildcard imports in scope chain (positional filter)
        match resolve_via_wildcard_import(ctx, scope, name, ns, use_order_key) {
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
    if matches!(expected, ExpectedNs::TypeThenValue)
        && let Some((def_id, _)) = ctx.global.resolve_definition(name)
    {
        return CoreResolveResult::Resolved(CoreResolution::Def { def: def_id });
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
        if let Some(ast_id) = pkg_scope.resolve(pkg_name, member_name, ns) {
            return CoreResolveResult::Resolved(CoreResolution::pkg(ast_id, ns));
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
                && let Some(ast_id) = pkg_scope.resolve(&imp.package, name, ns)
            {
                return Some(CoreResolution::pkg(ast_id, ns));
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

/// Result from wildcard import resolution: symbol, enum variant, or not found.
enum WildcardFound {
    Symbol(lyra_ast::ErasedAstId, SmolStr),
    EnumVariant(crate::enum_def::EnumVariantTarget, SmolStr),
}

fn resolve_via_wildcard_import(
    ctx: &ResolveCtx<'_>,
    scope: ScopeId,
    name: &str,
    ns: Namespace,
    use_order_key: u32,
) -> WildcardResult {
    let mut current = Some(scope);
    while let Some(sid) = current {
        let mut found: Option<WildcardFound> = None;
        let mut ambiguous_pkgs: Vec<SmolStr> = Vec::new();

        for imp in ctx.graph.imports_for_scope(sid) {
            if imp.name != ImportName::Wildcard || imp.order_key >= use_order_key {
                continue;
            }
            // Try regular package scope resolution first
            if let Some(def_id) = ctx.pkg_scope.resolve(&imp.package, name, ns) {
                match &found {
                    Some(WildcardFound::Symbol(existing_id, _)) if *existing_id != def_id => {
                        push_first_pkg(found.as_ref(), &mut ambiguous_pkgs);
                        ambiguous_pkgs.push(imp.package.clone());
                    }
                    Some(WildcardFound::EnumVariant(..)) => {
                        push_first_pkg(found.as_ref(), &mut ambiguous_pkgs);
                        ambiguous_pkgs.push(imp.package.clone());
                    }
                    None => {
                        found = Some(WildcardFound::Symbol(def_id, imp.package.clone()));
                    }
                    _ => {} // Same def_id, no conflict
                }
                continue;
            }
            // Try package-level enum variants (range-generated names)
            if ns == Namespace::Value
                && let Some(pkg_ev) = ctx.pkg_enum_variants
                && let Some(pkg_map) = pkg_ev.get(imp.package.as_str())
                && let Some(target) = pkg_map.get(name)
            {
                match &found {
                    Some(WildcardFound::EnumVariant(existing, _)) if *existing != *target => {
                        push_first_pkg(found.as_ref(), &mut ambiguous_pkgs);
                        ambiguous_pkgs.push(imp.package.clone());
                    }
                    Some(WildcardFound::Symbol(..)) => {
                        push_first_pkg(found.as_ref(), &mut ambiguous_pkgs);
                        ambiguous_pkgs.push(imp.package.clone());
                    }
                    None => {
                        found = Some(WildcardFound::EnumVariant(
                            target.clone(),
                            imp.package.clone(),
                        ));
                    }
                    _ => {} // Same target, no conflict
                }
            }
        }

        if !ambiguous_pkgs.is_empty() {
            return WildcardResult::Ambiguous(ambiguous_pkgs.into_boxed_slice());
        }
        match found {
            Some(WildcardFound::Symbol(ast_id, _)) => {
                return WildcardResult::Found(CoreResolution::pkg(ast_id, ns));
            }
            Some(WildcardFound::EnumVariant(target, _)) => {
                return WildcardResult::Found(CoreResolution::EnumVariant(target));
            }
            None => {}
        }
        current = ctx.graph.scopes.get(sid).parent;
    }
    WildcardResult::NotFound
}

/// Push the package name from the first wildcard match into the ambiguity list
/// (only on the first ambiguity detection).
fn push_first_pkg(found: Option<&WildcardFound>, ambiguous: &mut Vec<SmolStr>) {
    if ambiguous.is_empty()
        && let Some(WildcardFound::Symbol(_, p) | WildcardFound::EnumVariant(_, p)) = found
    {
        ambiguous.push(p.clone());
    }
}

fn resolve_via_implicit_import(
    pkg_scope: &PackageScopeIndex,
    cu_env: &CompilationUnitEnv,
    name: &str,
    ns: Namespace,
) -> WildcardResult {
    let mut found: Option<(lyra_ast::ErasedAstId, SmolStr)> = None;
    let mut ambiguous_pkgs: Vec<SmolStr> = Vec::new();

    for imp in &*cu_env.implicit_imports {
        if imp.name == ImportName::Wildcard {
            if let Some(ast_id) = pkg_scope.resolve(&imp.package, name, ns) {
                if let Some((existing_id, _)) = &found {
                    if *existing_id != ast_id {
                        if ambiguous_pkgs.is_empty() {
                            ambiguous_pkgs
                                .push(found.as_ref().map(|(_, p)| p.clone()).unwrap_or_default());
                        }
                        ambiguous_pkgs.push(imp.package.clone());
                    }
                } else {
                    found = Some((ast_id, imp.package.clone()));
                }
            }
        } else if let ImportName::Explicit(ref member) = imp.name
            && member.as_str() == name
            && let Some(ast_id) = pkg_scope.resolve(&imp.package, name, ns)
        {
            return WildcardResult::Found(CoreResolution::pkg(ast_id, ns));
        }
    }

    if !ambiguous_pkgs.is_empty() {
        return WildcardResult::Ambiguous(ambiguous_pkgs.into_boxed_slice());
    }
    if let Some((ast_id, _)) = found {
        return WildcardResult::Found(CoreResolution::pkg(ast_id, ns));
    }
    WildcardResult::NotFound
}

/// Detect LRM 26.3 wildcard-local declaration conflicts.
///
/// For each scope with wildcard imports, check if any local declaration
/// conflicts with a name provided by a wildcard import where a use-site
/// "realizes" the wildcard name before the local declaration.
///
/// A wildcard name is "realized" when a use-site references the name
/// after the wildcard import but before the local declaration. The
/// use-site need not resolve via the wildcard -- the realization occurs
/// simply by the name being used in a context where the wildcard
/// provides it (LRM 26.3).
fn detect_wildcard_local_conflicts(
    ctx: &ResolveCtx<'_>,
    use_entries: &[crate::name_graph::UseEntry],
) -> Vec<WildcardLocalConflict> {
    use std::collections::HashMap as StdHashMap;
    let graph = ctx.graph;

    let mut by_local: StdHashMap<LocalDeclId, WildcardLocalConflict> = StdHashMap::new();

    for scope_idx in 0..graph.scopes.len() {
        let scope_id = ScopeId(scope_idx as u32);

        // Find wildcard imports in this scope that provide names
        let wildcard_imports: Vec<_> = graph
            .imports_for_scope(scope_id)
            .iter()
            .filter(|imp| imp.name == ImportName::Wildcard)
            .collect();
        if wildcard_imports.is_empty() {
            continue;
        }

        let local_decls = graph.local_decls_for_scope(scope_id);
        if local_decls.is_empty() {
            continue;
        }

        // For each local_decl, check if a wildcard import provides the same
        // name and there's a use-site between import and local_decl that
        // "realizes" the name.
        for local in local_decls {
            for wc_imp in &wildcard_imports {
                // The wildcard import must be before the local declaration
                if wc_imp.order_key >= local.order_key {
                    continue;
                }

                // Check if the wildcard provides this name in the local's namespace
                if ctx
                    .pkg_scope
                    .resolve(&wc_imp.package, &local.name, local.namespace)
                    .is_none()
                {
                    continue;
                }

                // Find a use-site that realizes the name: any use-site referencing
                // this name where order_key is between import and local_decl.
                // The use-site can be in this scope or any descendant scope.
                let realizing_use = find_realizing_use_site(
                    ctx,
                    use_entries,
                    scope_id,
                    &local.name,
                    local.namespace,
                    wc_imp.order_key,
                    local.order_key,
                );

                if let Some(use_idx) = realizing_use {
                    let existing = by_local.entry(local.id).or_insert(WildcardLocalConflict {
                        local_decl_id: local.id,
                        namespace: local.namespace,
                        import_id: wc_imp.id,
                        use_site_idx: use_idx as u32,
                    });
                    // Keep the earliest use-site
                    if use_entries[use_idx].order_key
                        < use_entries[existing.use_site_idx as usize].order_key
                    {
                        existing.use_site_idx = use_idx as u32;
                        existing.import_id = wc_imp.id;
                    }
                    break; // One wildcard is enough to trigger the conflict
                }
            }
        }
    }

    let mut conflicts: Vec<WildcardLocalConflict> = by_local.into_values().collect();
    conflicts.sort_by_key(|c| (c.local_decl_id.scope, c.local_decl_id.ordinal));
    conflicts
}

/// Find a use-site that "realizes" a wildcard-imported name.
///
/// Returns the index of the earliest use-site in `scope` or any descendant
/// that references `name` with a compatible namespace and has an `order_key`
/// between `after_key` (exclusive) and `before_key` (exclusive).
fn find_realizing_use_site(
    ctx: &ResolveCtx<'_>,
    use_entries: &[crate::name_graph::UseEntry],
    scope: ScopeId,
    name: &str,
    ns: Namespace,
    after_key: u32,
    before_key: u32,
) -> Option<usize> {
    let mut best: Option<(usize, u32)> = None;

    for (idx, entry) in use_entries.iter().enumerate() {
        // Must be a simple name
        let Some(use_name) = entry.path.as_simple() else {
            continue;
        };
        if use_name != name {
            continue;
        }
        // Must be in the right order_key window
        if entry.order_key <= after_key || entry.order_key >= before_key {
            continue;
        }
        // Must match namespace
        let ns_match = match entry.expected_ns {
            ExpectedNs::Exact(expected_ns) => expected_ns == ns,
            ExpectedNs::TypeThenValue => ns == Namespace::Type || ns == Namespace::Value,
        };
        if !ns_match {
            continue;
        }
        // Must be in scope or a descendant of scope
        if !is_scope_or_descendant(ctx.graph, entry.scope, scope) {
            continue;
        }
        // Check that no local declaration or explicit import in the
        // use-site's lexical chain (before reaching the wildcard scope)
        // would take precedence over the wildcard import.
        if has_higher_precedence_binding(
            ctx.graph,
            ctx.pkg_scope,
            entry.scope,
            scope,
            name,
            ns,
            entry.order_key,
        ) {
            continue;
        }

        if best.is_none() || entry.order_key < best.as_ref().map_or(u32::MAX, |(_, k)| *k) {
            best = Some((idx, entry.order_key));
        }
    }

    best.map(|(idx, _)| idx)
}

/// Check if `child` is the same as `ancestor` or a descendant of it.
fn is_scope_or_descendant(graph: &NameGraph, child: ScopeId, ancestor: ScopeId) -> bool {
    let mut current = Some(child);
    while let Some(s) = current {
        if s == ancestor {
            return true;
        }
        current = graph.scopes.get(s).parent;
    }
    false
}

/// Check if any scope between `start` (inclusive) and `stop` (exclusive)
/// in the parent chain has a local declaration or an explicit import
/// that would shadow the wildcard import for the given name.
///
/// An explicit import `import pkg::name;` in a descendant scope has
/// higher precedence than a wildcard import in an ancestor scope
/// (LRM 26.3 resolution order), so a use-site resolved via such an
/// explicit import does not count as a wildcard realization.
fn has_higher_precedence_binding(
    graph: &NameGraph,
    pkg_scope: &PackageScopeIndex,
    start: ScopeId,
    stop: ScopeId,
    name: &str,
    ns: Namespace,
    use_order_key: u32,
) -> bool {
    let mut current = Some(start);
    while let Some(s) = current {
        if s == stop {
            return false;
        }
        // Check local declarations (scope-wide visibility)
        let scope_data = graph.scopes.get(s);
        let bindings = match ns {
            Namespace::Value => &scope_data.value_ns,
            Namespace::Type => &scope_data.type_ns,
            Namespace::Definition => return false,
        };
        if bindings
            .binary_search_by(|id| graph.name(*id).cmp(name))
            .is_ok()
        {
            return true;
        }
        // Check explicit imports (positional: must precede the use-site)
        for imp in graph.imports_for_scope(s) {
            if let ImportName::Explicit(ref member) = imp.name
                && member.as_str() == name
                && imp.order_key < use_order_key
                && pkg_scope.resolve(&imp.package, name, ns).is_some()
            {
                return true;
            }
        }
        current = scope_data.parent;
    }
    false
}

/// Build the per-file resolution index from pre-computed core results.
///
/// Zips `def.use_sites` with `core.resolutions`, builds the `HashMap`
/// and diagnostics. Import errors are also mapped to diagnostics.
///
/// `lookup_decl` maps an `ErasedAstId` (`name_ast` anchor from
/// `CoreResolution::Def` or `CoreResolution::Pkg`) to a `SymbolId`
/// in the target file.
///
/// `instance_filter` is called when core resolution resolves a use-site
/// to an `Instance` symbol. Returns `true` if the instance should be
/// kept as a valid resolution (e.g., interface instances). Returns
/// `false` to reject the resolution and emit an unresolved diagnostic
/// (e.g., module instances are not valid in value expressions).
pub fn build_resolve_index(
    def: &DefIndex,
    core: &CoreResolveOutput,
    lookup_decl: &dyn Fn(lyra_ast::ErasedAstId) -> Option<SymbolId>,
    instance_filter: &dyn Fn(crate::instance_decl::InstanceDeclIdx) -> bool,
) -> ResolveIndex {
    let mut resolutions = HashMap::new();
    let mut diagnostics = Vec::new();

    for (use_site, result) in def.use_sites.iter().zip(core.resolutions.iter()) {
        match result {
            CoreResolveResult::Resolved(CoreResolution::Local { symbol, namespace }) => {
                // Filter instance symbols: only interface instances are
                // valid in value expressions. Module instances are rejected.
                if let crate::record::SymbolOrigin::Instance(idx) = def.symbols.get(*symbol).origin
                    && !instance_filter(idx)
                {
                    let diag = reason_to_diagnostic(
                        &UnresolvedReason::NotFound,
                        use_site.expected_ns,
                        &use_site.path,
                        use_site.range,
                    );
                    diagnostics.push(diag);
                    continue;
                }
                resolutions.insert(
                    use_site.ast_id,
                    Resolution {
                        target: ResolvedTarget::Symbol(GlobalSymbolId {
                            file: def.file,
                            local: *symbol,
                        }),
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
            CoreResolveResult::Resolved(CoreResolution::Def { def }) => {
                resolve_cross_file(
                    def.ast_id(),
                    Namespace::Definition,
                    use_site,
                    lookup_decl,
                    &mut resolutions,
                    &mut diagnostics,
                );
            }
            CoreResolveResult::Resolved(CoreResolution::Pkg { ast, namespace }) => {
                if *namespace == Namespace::Definition {
                    diagnostics.push(SemanticDiag {
                        kind: SemanticDiagKind::InternalError {
                            detail: SmolStr::new("Pkg resolution carries Definition namespace"),
                        },
                        range: use_site.range,
                    });
                } else {
                    resolve_cross_file(
                        *ast,
                        *namespace,
                        use_site,
                        lookup_decl,
                        &mut resolutions,
                        &mut diagnostics,
                    );
                }
            }
            CoreResolveResult::Resolved(CoreResolution::EnumVariant(target)) => {
                resolutions.insert(
                    use_site.ast_id,
                    Resolution {
                        target: ResolvedTarget::EnumVariant(target.clone()),
                        namespace: Namespace::Value,
                    },
                );
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

    map_import_errors(def, core, &mut diagnostics);

    ResolveIndex {
        file: def.file,
        resolutions,
        diagnostics: diagnostics.into_boxed_slice(),
    }
}

fn map_import_errors(
    def: &DefIndex,
    core: &CoreResolveOutput,
    diagnostics: &mut Vec<SemanticDiag>,
) {
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
}

fn resolve_cross_file(
    anchor: lyra_ast::ErasedAstId,
    namespace: Namespace,
    use_site: &crate::def_index::UseSite,
    lookup_decl: &dyn Fn(lyra_ast::ErasedAstId) -> Option<SymbolId>,
    resolutions: &mut HashMap<lyra_ast::ErasedAstId, Resolution>,
    diagnostics: &mut Vec<SemanticDiag>,
) {
    if let Some(local) = lookup_decl(anchor) {
        resolutions.insert(
            use_site.ast_id,
            Resolution {
                target: ResolvedTarget::Symbol(GlobalSymbolId {
                    file: anchor.file(),
                    local,
                }),
                namespace,
            },
        );
        if let Some(diag) = check_type_mismatch(
            use_site.expected_ns,
            namespace,
            &use_site.path,
            use_site.range,
        ) {
            diagnostics.push(diag);
        }
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
