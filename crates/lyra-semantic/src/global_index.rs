use crate::Site;
use crate::def_index::DefIndex;
use smol_str::SmolStr;

use crate::symbols::{GlobalDefId, Namespace};

/// Whether a global definition is a module or package.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefinitionKind {
    Module,
    Package,
    Interface,
    Program,
    Primitive,
    Config,
}

impl DefinitionKind {
    pub fn is_instantiable(self) -> bool {
        matches!(self, Self::Module | Self::Interface)
    }
}

/// Global definition index for the definitions name space
/// (IEEE 1800-2023 section 3.13(a)), scoped to one compilation unit.
///
/// Modules and packages share the same namespace per IEEE 3.13(a).
/// Stores only stable, offset-independent data: names and
/// `GlobalDefId`s. `GlobalDefId` is a semantic ordinal identity
/// (file + ordinal), independent of source offsets.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalDefIndex {
    /// All definitions sorted by `(name, id)`. Modules and packages
    /// share the same namespace per IEEE 1800-2023 section 3.13(a).
    definitions: Box<[(SmolStr, GlobalDefId, DefinitionKind)]>,
    /// Parallel table sorted by `GlobalDefId` for O(log n) kind lookup.
    kinds_sorted: Box<[(GlobalDefId, DefinitionKind)]>,
}

impl GlobalDefIndex {
    /// Look up a definition by name. Returns the first match.
    pub fn resolve_definition(&self, name: &str) -> Option<(GlobalDefId, DefinitionKind)> {
        let idx = self
            .definitions
            .binary_search_by(|(n, _, _)| n.as_str().cmp(name))
            .ok()?;
        let entry = &self.definitions[idx];
        Some((entry.1, entry.2))
    }

    /// Look up a module by name. Returns the first match with `Module` kind.
    pub fn resolve_module(&self, name: &str) -> Option<GlobalDefId> {
        let (id, kind) = self.resolve_definition(name)?;
        if kind == DefinitionKind::Module {
            Some(id)
        } else {
            None
        }
    }

    /// Look up a package by name. Returns the first match with `Package` kind.
    pub fn resolve_package(&self, name: &str) -> Option<GlobalDefId> {
        let (id, kind) = self.resolve_definition(name)?;
        if kind == DefinitionKind::Package {
            Some(id)
        } else {
            None
        }
    }

    /// O(log n) kind lookup by `GlobalDefId`.
    pub fn def_kind(&self, def: GlobalDefId) -> Option<DefinitionKind> {
        let idx = self
            .kinds_sorted
            .binary_search_by_key(&def, |(id, _)| *id)
            .ok()?;
        Some(self.kinds_sorted[idx].1)
    }

    /// Iterate over all definitions. Used for duplicate detection.
    pub fn definitions(&self) -> &[(SmolStr, GlobalDefId, DefinitionKind)] {
        &self.definitions
    }
}

/// Build a `GlobalDefIndex` from pre-extracted definition entries.
///
/// Pure function. Sorts by `(name, id)` for deterministic binary search.
pub fn build_global_def_index(
    entries: &[(SmolStr, GlobalDefId, DefinitionKind)],
) -> GlobalDefIndex {
    let mut sorted: Vec<(SmolStr, GlobalDefId, DefinitionKind)> = entries.to_vec();
    sorted.sort_by(|(na, ida, _), (nb, idb, _)| na.cmp(nb).then_with(|| ida.cmp(idb)));
    let mut kinds: Vec<(GlobalDefId, DefinitionKind)> =
        sorted.iter().map(|(_, id, k)| (*id, *k)).collect();
    kinds.sort_by_key(|(id, _)| *id);
    kinds.dedup_by_key(|(id, _)| *id);
    GlobalDefIndex {
        definitions: sorted.into_boxed_slice(),
        kinds_sorted: kinds.into_boxed_slice(),
    }
}

/// Namespace-aware index of symbols exported from packages.
///
/// Used by import resolution and qualified name resolution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageScopeIndex {
    packages: Box<[PackageScope]>,
}

/// Exported symbols from a single package, split by namespace.
///
/// Maps exported names to their `name_site` anchors (the unique
/// name-introducing AST node for each symbol).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageScope {
    pub(crate) name: SmolStr,
    pub(crate) value_ns: Box<[(SmolStr, Site)]>,
    pub(crate) type_ns: Box<[(SmolStr, Site)]>,
}

impl PackageScope {
    pub fn new(
        name: SmolStr,
        value_ns: Box<[(SmolStr, Site)]>,
        type_ns: Box<[(SmolStr, Site)]>,
    ) -> Self {
        Self {
            name,
            value_ns,
            type_ns,
        }
    }

    pub fn pkg_name(&self) -> &SmolStr {
        &self.name
    }

    pub fn value_iter(&self) -> &[(SmolStr, Site)] {
        &self.value_ns
    }

    pub fn type_iter(&self) -> &[(SmolStr, Site)] {
        &self.type_ns
    }
}

impl PackageScopeIndex {
    /// Resolve a symbol in a package by namespace.
    pub fn resolve(&self, pkg: &str, name: &str, ns: Namespace) -> Option<Site> {
        let pkg_idx = self
            .packages
            .binary_search_by(|p| p.name.as_str().cmp(pkg))
            .ok()?;
        let pkg_scope = &self.packages[pkg_idx];
        let entries = match ns {
            Namespace::Value => &pkg_scope.value_ns,
            Namespace::Type => &pkg_scope.type_ns,
            Namespace::Definition => return None,
        };
        let idx = entries
            .binary_search_by(|(n, _)| n.as_str().cmp(name))
            .ok()?;
        Some(entries[idx].1)
    }

    /// Check if a package exists in the index.
    pub fn has_package(&self, pkg: &str) -> bool {
        self.packages
            .binary_search_by(|p| p.name.as_str().cmp(pkg))
            .is_ok()
    }

    /// Full package surface for iterating all exported names.
    pub fn public_surface(&self, pkg: &str) -> Option<&PackageScope> {
        let idx = self
            .packages
            .binary_search_by(|p| p.name.as_str().cmp(pkg))
            .ok()?;
        Some(&self.packages[idx])
    }

    /// Resolve a symbol in a package across all namespaces.
    /// Used for wildcard import validation.
    pub fn resolve_any_ns(&self, pkg: &str, name: &str) -> Option<Site> {
        let pkg_idx = self
            .packages
            .binary_search_by(|p| p.name.as_str().cmp(pkg))
            .ok()?;
        let pkg_scope = &self.packages[pkg_idx];
        if let Ok(idx) = pkg_scope
            .value_ns
            .binary_search_by(|(n, _)| n.as_str().cmp(name))
        {
            return Some(pkg_scope.value_ns[idx].1);
        }
        if let Ok(idx) = pkg_scope
            .type_ns
            .binary_search_by(|(n, _)| n.as_str().cmp(name))
        {
            return Some(pkg_scope.type_ns[idx].1);
        }
        None
    }
}

/// Build a `PackageScopeIndex` from package scope entries.
pub fn build_package_scope_index(mut packages: Vec<PackageScope>) -> PackageScopeIndex {
    packages.sort_by(|a, b| a.name.cmp(&b.name));
    PackageScopeIndex {
        packages: packages.into_boxed_slice(),
    }
}

/// Per-package collected facts for computing the public surface.
pub struct PackageLocalFacts {
    pub name: SmolStr,
    pub value_ns: Vec<(SmolStr, Site)>,
    pub type_ns: Vec<(SmolStr, Site)>,
    pub imports: Vec<crate::def_index::ImportName>,
    pub import_packages: Vec<SmolStr>,
    pub export_decls: Vec<crate::def_index::ExportKey>,
}

/// Compute the public surface for a single package, resolving exports.
///
/// Merge precedence:
/// 1. Local defs always win
/// 2. Explicit single-name exports (`export P::name`)
/// 3. Package wildcard exports (`export P::*`)
/// 4. All-wildcard exports (`export *::*`)
///
/// `get_dep_surface` fetches the public surface of a dependency package.
/// It should return `None` for unknown packages or to break cycles.
pub fn compute_public_surface(
    facts: &PackageLocalFacts,
    get_dep_surface: &dyn Fn(&str) -> Option<PackageScope>,
) -> PackageScope {
    use crate::def_index::ExportKey;

    let mut value_ns: Vec<(SmolStr, Site)> = facts.value_ns.clone();
    let mut type_ns: Vec<(SmolStr, Site)> = facts.type_ns.clone();

    // Track names already present from local defs (owned for borrow checker)
    let local_value_names: std::collections::HashSet<SmolStr> =
        value_ns.iter().map(|(n, _)| n.clone()).collect();
    let local_type_names: std::collections::HashSet<SmolStr> =
        type_ns.iter().map(|(n, _)| n.clone()).collect();

    // Process exports in order: explicit first, then package wildcards, then all-wildcards
    let mut explicit_exports = Vec::new();
    let mut wildcard_exports = Vec::new();
    let mut has_all_wildcard = false;

    for entry in &facts.export_decls {
        match entry {
            ExportKey::Explicit { package, name } => {
                explicit_exports.push((package.clone(), name.clone()));
            }
            ExportKey::PackageWildcard { package } => {
                wildcard_exports.push(package.clone());
            }
            ExportKey::AllWildcard => {
                has_all_wildcard = true;
            }
        }
    }

    // Explicit exports: add specific names from specific packages
    for (pkg_name, member_name) in &explicit_exports {
        if let Some(dep) = get_dep_surface(pkg_name) {
            if !local_value_names.contains(member_name)
                && let Ok(idx) = dep
                    .value_ns
                    .binary_search_by(|(n, _)| n.as_str().cmp(member_name.as_str()))
            {
                value_ns.push(dep.value_ns[idx].clone());
            }
            if !local_type_names.contains(member_name)
                && let Ok(idx) = dep
                    .type_ns
                    .binary_search_by(|(n, _)| n.as_str().cmp(member_name.as_str()))
            {
                type_ns.push(dep.type_ns[idx].clone());
            }
        }
    }

    // Package wildcard exports: add all symbols from specific packages
    for pkg_name in &wildcard_exports {
        if let Some(dep) = get_dep_surface(pkg_name) {
            merge_ns_entries(&mut value_ns, &dep.value_ns, &local_value_names);
            merge_ns_entries(&mut type_ns, &dep.type_ns, &local_type_names);
        }
    }

    // All-wildcard exports: re-export all imported symbols
    if has_all_wildcard {
        // Re-export from all imported packages
        let mut seen_pkgs: std::collections::HashSet<SmolStr> = std::collections::HashSet::new();
        for pkg_name in &facts.import_packages {
            if seen_pkgs.insert(pkg_name.clone())
                && let Some(dep) = get_dep_surface(pkg_name)
            {
                merge_ns_entries(&mut value_ns, &dep.value_ns, &local_value_names);
                merge_ns_entries(&mut type_ns, &dep.type_ns, &local_type_names);
            }
        }
    }

    value_ns.sort_by(|(a, _), (b, _)| a.cmp(b));
    value_ns.dedup_by(|(a, _), (b, _)| a == b);
    type_ns.sort_by(|(a, _), (b, _)| a.cmp(b));
    type_ns.dedup_by(|(a, _), (b, _)| a == b);

    PackageScope::new(
        facts.name.clone(),
        value_ns.into_boxed_slice(),
        type_ns.into_boxed_slice(),
    )
}

fn merge_ns_entries(
    target: &mut Vec<(SmolStr, Site)>,
    source: &[(SmolStr, Site)],
    local_names: &std::collections::HashSet<SmolStr>,
) {
    for (name, id) in source {
        if !local_names.contains(name) && !target.iter().any(|(n, _)| n == name) {
            target.push((name.clone(), *id));
        }
    }
}

/// File-level declarative symbols collected in compilation-unit scope
/// for one source file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileScopeIndex {
    pub value_ns: Box<[(SmolStr, Site)]>,
    pub type_ns: Box<[(SmolStr, Site)]>,
}

impl FileScopeIndex {
    /// True if no file-scope declarations exist.
    pub fn is_empty(&self) -> bool {
        self.value_ns.is_empty() && self.type_ns.is_empty()
    }
}

/// Build a `FileScopeIndex` from a single file's definition index.
pub fn build_file_scope_index(def: &DefIndex) -> FileScopeIndex {
    let Some(file_scope) = def.file_scope() else {
        return FileScopeIndex {
            value_ns: Box::new([]),
            type_ns: Box::new([]),
        };
    };

    let scope = def.scopes.get(file_scope);

    let mut value_ns = Vec::new();
    for &sym_id in &*scope.value_ns {
        let sym = def.symbols.get(sym_id);
        value_ns.push((sym.name.clone(), sym.name_site));
    }
    value_ns.sort_by(|a, b| a.0.cmp(&b.0));

    let mut type_ns = Vec::new();
    for &sym_id in &*scope.type_ns {
        let sym = def.symbols.get(sym_id);
        type_ns.push((sym.name.clone(), sym.name_site));
    }
    type_ns.sort_by(|a, b| a.0.cmp(&b.0));

    FileScopeIndex {
        value_ns: value_ns.into(),
        type_ns: type_ns.into(),
    }
}

/// Compilation-unit scope aggregated across all files in the unit.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CuScopeIndex {
    pub value_ns: Box<[(SmolStr, Site)]>,
    pub type_ns: Box<[(SmolStr, Site)]>,
}

/// Result of a name lookup in a scope summary that preserves ambiguity.
///
/// Borrows from the underlying sorted slice to avoid per-lookup allocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeLookup<'a> {
    /// Name not found in this scope.
    Missing,
    /// Exactly one declaration with this name.
    Unique(Site),
    /// Multiple declarations share this name (e.g. across files).
    /// Borrows the matching range from the sorted index.
    Ambiguous(&'a [(SmolStr, Site)]),
}

impl CuScopeIndex {
    /// Resolve a name in the compilation-unit scope by namespace.
    ///
    /// Preserves ambiguity: if multiple files declare the same name,
    /// returns `Ambiguous` with all sites.
    pub fn resolve(&self, name: &str, ns: Namespace) -> ScopeLookup<'_> {
        let entries = match ns {
            Namespace::Value => &self.value_ns,
            Namespace::Type => &self.type_ns,
            Namespace::Definition => return ScopeLookup::Missing,
        };
        scope_lookup_in_sorted(entries, name)
    }

    /// True if no compilation-unit scope declarations exist.
    pub fn is_empty(&self) -> bool {
        self.value_ns.is_empty() && self.type_ns.is_empty()
    }
}

/// Shared lookup in a sorted `(name, site)` slice that preserves multiplicity.
///
/// Returns a borrowed range for the ambiguous case -- no per-lookup allocation.
fn scope_lookup_in_sorted<'a>(entries: &'a [(SmolStr, Site)], name: &str) -> ScopeLookup<'a> {
    let start = entries.partition_point(|(n, _)| n.as_str() < name);
    let end = entries[start..].partition_point(|(n, _)| n.as_str() == name) + start;
    match end - start {
        0 => ScopeLookup::Missing,
        1 => ScopeLookup::Unique(entries[start].1),
        _ => ScopeLookup::Ambiguous(&entries[start..end]),
    }
}

/// Build a `CuScopeIndex` from per-file summaries.
pub fn build_cu_scope_index(files: &[&FileScopeIndex]) -> CuScopeIndex {
    let mut value_ns = Vec::new();
    let mut type_ns = Vec::new();

    for file in files {
        value_ns.extend(file.value_ns.iter().cloned());
        type_ns.extend(file.type_ns.iter().cloned());
    }

    value_ns.sort_by(|a, b| a.0.cmp(&b.0));
    type_ns.sort_by(|a, b| a.0.cmp(&b.0));

    CuScopeIndex {
        value_ns: value_ns.into(),
        type_ns: type_ns.into(),
    }
}
