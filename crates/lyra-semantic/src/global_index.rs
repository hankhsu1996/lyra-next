use smol_str::SmolStr;

use crate::symbols::{GlobalDefId, Namespace};

/// Whether a global definition is a module or package.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefinitionKind {
    Module,
    Package,
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
