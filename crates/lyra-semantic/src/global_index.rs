use lyra_ast::ErasedAstId;
use smol_str::SmolStr;

/// Global definition index for the definitions name space
/// (IEEE 1800-2023 section 3.13(a)), scoped to one compilation unit.
///
/// Stores only stable, offset-independent data: module names and
/// `ErasedAstId`s. Derived `PartialEq` backdates correctly across
/// whitespace edits because `ErasedAstId` is topology-based.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalDefIndex {
    /// Module name -> `ErasedAstId` of the `ModuleDecl` node.
    /// Sorted by `(name, ast_id)` for binary search. Duplicate names
    /// are kept (multiple definitions produce a diagnostic).
    modules: Box<[(SmolStr, ErasedAstId)]>,
}

impl GlobalDefIndex {
    /// Look up a module by name. Returns the first match (stable:
    /// sorted by name, then by `ErasedAstId`).
    pub fn resolve_module(&self, name: &str) -> Option<ErasedAstId> {
        let idx = self
            .modules
            .binary_search_by(|(n, _)| n.as_str().cmp(name))
            .ok()?;
        Some(self.modules[idx].1)
    }

    /// Iterate over all `(name, ast_id)` entries. Used for duplicate
    /// detection in `unit_diagnostics`.
    pub fn modules(&self) -> &[(SmolStr, ErasedAstId)] {
        &self.modules
    }
}

/// Build a `GlobalDefIndex` from pre-extracted `(name, ast_id)` pairs.
///
/// Pure function. Sorts by `(name, ast_id)` for deterministic binary search.
pub fn build_global_def_index(entries: &[(SmolStr, ErasedAstId)]) -> GlobalDefIndex {
    let mut sorted: Vec<(SmolStr, ErasedAstId)> = entries.to_vec();
    sorted.sort_by(|(na, ida), (nb, idb)| na.cmp(nb).then_with(|| ida.cmp(idb)));
    GlobalDefIndex {
        modules: sorted.into_boxed_slice(),
    }
}
