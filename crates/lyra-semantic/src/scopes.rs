use crate::symbols::{SymbolId, SymbolTable};

/// Scope identifier, per-file index.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub(crate) u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    File,
    Module,
    Block,
}

/// A single scope in the scope tree.
///
/// `bindings` stores `SymbolId`s sorted by the corresponding
/// `Symbol.name`. Names live once in `Symbol`; scope entries
/// reference by id. Resolve does binary search with a custom
/// comparator that looks up `symbols[mid].name`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
    pub kind: ScopeKind,
    pub parent: Option<ScopeId>,
    pub bindings: Box<[SymbolId]>,
}

/// Frozen scope tree, indexed by `ScopeId`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScopeTree {
    scopes: Box<[Scope]>,
}

/// Builder for accumulating scopes before freezing.
pub(crate) struct ScopeTreeBuilder {
    scopes: Vec<ScopeBuilderEntry>,
}

pub(crate) struct ScopeBuilderEntry {
    pub(crate) kind: ScopeKind,
    pub(crate) parent: Option<ScopeId>,
    pub(crate) bindings: Vec<SymbolId>,
}

impl ScopeTreeBuilder {
    pub(crate) fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    pub(crate) fn push(&mut self, kind: ScopeKind, parent: Option<ScopeId>) -> ScopeId {
        let id = ScopeId(self.scopes.len() as u32);
        self.scopes.push(ScopeBuilderEntry {
            kind,
            parent,
            bindings: Vec::new(),
        });
        id
    }

    pub(crate) fn add_binding(&mut self, scope: ScopeId, sym: SymbolId) {
        self.scopes[scope.0 as usize].bindings.push(sym);
    }

    pub(crate) fn freeze(self, symbols: &SymbolTable) -> ScopeTree {
        let scopes: Vec<Scope> = self
            .scopes
            .into_iter()
            .map(|entry| {
                let mut bindings = entry.bindings;
                bindings.sort_by(|a, b| symbols.get(*a).name.cmp(&symbols.get(*b).name));
                Scope {
                    kind: entry.kind,
                    parent: entry.parent,
                    bindings: bindings.into_boxed_slice(),
                }
            })
            .collect();
        ScopeTree {
            scopes: scopes.into_boxed_slice(),
        }
    }
}

impl ScopeTree {
    /// Resolve a name starting from `scope`, walking parent chain.
    ///
    /// Binary search within each scope's sorted bindings using the
    /// `symbols` table for name comparisons. Returns the first match
    /// (for duplicates, the first in sorted order).
    pub fn resolve(&self, symbols: &SymbolTable, scope: ScopeId, name: &str) -> Option<SymbolId> {
        let s = &self.scopes[scope.0 as usize];
        let result = s
            .bindings
            .binary_search_by(|id| symbols.get(*id).name.as_str().cmp(name));
        if let Ok(mut idx) = result {
            // Walk back to find the first entry with this name
            while idx > 0 && symbols.get(s.bindings[idx - 1]).name == name {
                idx -= 1;
            }
            return Some(s.bindings[idx]);
        }
        if let Some(parent) = s.parent {
            return self.resolve(symbols, parent, name);
        }
        None
    }

    pub fn get(&self, id: ScopeId) -> &Scope {
        &self.scopes[id.0 as usize]
    }

    pub fn len(&self) -> usize {
        self.scopes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.scopes.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use lyra_source::{TextRange, TextSize};
    use smol_str::SmolStr;

    use super::*;
    use crate::symbols::{Symbol, SymbolKind, SymbolTableBuilder};

    #[test]
    fn resolve_finds_binding() {
        let mut sym_builder = SymbolTableBuilder::new();
        let mut scope_builder = ScopeTreeBuilder::new();

        let scope = scope_builder.push(ScopeKind::Module, None);
        let range = TextRange::new(TextSize::new(0), TextSize::new(1));
        let id = sym_builder.push(Symbol {
            name: SmolStr::new("x"),
            kind: SymbolKind::Variable,
            def_range: range,
            scope,
        });
        scope_builder.add_binding(scope, id);

        let symbols = sym_builder.freeze();
        let scopes = scope_builder.freeze(&symbols);

        assert_eq!(scopes.resolve(&symbols, scope, "x"), Some(id));
        assert_eq!(scopes.resolve(&symbols, scope, "y"), None);
    }

    #[test]
    fn resolve_walks_parent_chain() {
        let mut sym_builder = SymbolTableBuilder::new();
        let mut scope_builder = ScopeTreeBuilder::new();

        let parent = scope_builder.push(ScopeKind::Module, None);
        let child = scope_builder.push(ScopeKind::Block, Some(parent));
        let range = TextRange::new(TextSize::new(0), TextSize::new(1));
        let id = sym_builder.push(Symbol {
            name: SmolStr::new("a"),
            kind: SymbolKind::Port,
            def_range: range,
            scope: parent,
        });
        scope_builder.add_binding(parent, id);

        let symbols = sym_builder.freeze();
        let scopes = scope_builder.freeze(&symbols);

        assert_eq!(
            scopes.resolve(&symbols, child, "a"),
            Some(id),
            "child scope should find binding in parent"
        );
    }

    #[test]
    fn resolve_returns_first_duplicate() {
        let mut sym_builder = SymbolTableBuilder::new();
        let mut scope_builder = ScopeTreeBuilder::new();

        let scope = scope_builder.push(ScopeKind::Module, None);
        let range1 = TextRange::new(TextSize::new(0), TextSize::new(1));
        let range2 = TextRange::new(TextSize::new(5), TextSize::new(6));
        let id1 = sym_builder.push(Symbol {
            name: SmolStr::new("x"),
            kind: SymbolKind::Variable,
            def_range: range1,
            scope,
        });
        let _id2 = sym_builder.push(Symbol {
            name: SmolStr::new("x"),
            kind: SymbolKind::Variable,
            def_range: range2,
            scope,
        });
        scope_builder.add_binding(scope, id1);
        scope_builder.add_binding(scope, _id2);

        let symbols = sym_builder.freeze();
        let scopes = scope_builder.freeze(&symbols);

        assert_eq!(
            scopes.resolve(&symbols, scope, "x"),
            Some(id1),
            "should return first definition"
        );
    }
}
