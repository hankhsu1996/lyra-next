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
    /// Derive `DefinitionKind` from a `SymbolKind` for definition-namespace symbols.
    /// Returns `None` for symbol kinds that are not in the definition namespace.
    pub fn from_symbol_kind(kind: crate::symbols::SymbolKind) -> Option<Self> {
        match kind {
            crate::symbols::SymbolKind::Module => Some(Self::Module),
            crate::symbols::SymbolKind::Package => Some(Self::Package),
            crate::symbols::SymbolKind::Interface => Some(Self::Interface),
            crate::symbols::SymbolKind::Program => Some(Self::Program),
            crate::symbols::SymbolKind::Primitive => Some(Self::Primitive),
            crate::symbols::SymbolKind::Config => Some(Self::Config),
            _ => None,
        }
    }
}

/// Global definition index for the definitions name space
/// (IEEE 1800-2023 section 3.13(a)), scoped to one compilation unit.
///
/// Modules and packages share the same namespace per IEEE 3.13(a).
/// Stores only stable, offset-independent data: names and
/// `GlobalDefId`s. Derived `PartialEq` backdates correctly across
/// whitespace edits because `GlobalDefId` wraps topology-based
/// `ErasedAstId`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalDefIndex {
    /// All definitions sorted by `(name, id)`. Modules and packages
    /// share the same namespace per IEEE 1800-2023 section 3.13(a).
    definitions: Box<[(SmolStr, GlobalDefId, DefinitionKind)]>,
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
    GlobalDefIndex {
        definitions: sorted.into_boxed_slice(),
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageScope {
    pub name: SmolStr,
    pub value_ns: Box<[(SmolStr, GlobalDefId)]>,
    pub type_ns: Box<[(SmolStr, GlobalDefId)]>,
}

impl PackageScopeIndex {
    /// Resolve a symbol in a package by namespace.
    pub fn resolve(&self, pkg: &str, name: &str, ns: Namespace) -> Option<GlobalDefId> {
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

    /// Resolve a symbol in a package across all namespaces.
    /// Used for wildcard import validation.
    pub fn resolve_any_ns(&self, pkg: &str, name: &str) -> Option<GlobalDefId> {
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
    pub value_ns: Vec<(SmolStr, GlobalDefId)>,
    pub type_ns: Vec<(SmolStr, GlobalDefId)>,
    pub imports: Vec<crate::def_index::ImportName>,
    pub import_packages: Vec<SmolStr>,
    pub export_decls: Vec<crate::def_index::ExportEntry>,
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
    use crate::def_index::ExportEntry;

    let mut value_ns: Vec<(SmolStr, GlobalDefId)> = facts.value_ns.clone();
    let mut type_ns: Vec<(SmolStr, GlobalDefId)> = facts.type_ns.clone();

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
            ExportEntry::Explicit { package, name } => {
                explicit_exports.push((package.clone(), name.clone()));
            }
            ExportEntry::PackageWildcard { package } => {
                wildcard_exports.push(package.clone());
            }
            ExportEntry::AllWildcard => {
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

    PackageScope {
        name: facts.name.clone(),
        value_ns: value_ns.into_boxed_slice(),
        type_ns: type_ns.into_boxed_slice(),
    }
}

fn merge_ns_entries(
    target: &mut Vec<(SmolStr, GlobalDefId)>,
    source: &[(SmolStr, GlobalDefId)],
    local_names: &std::collections::HashSet<SmolStr>,
) {
    for (name, id) in source {
        if !local_names.contains(name) && !target.iter().any(|(n, _)| n == name) {
            target.push((name.clone(), *id));
        }
    }
}
