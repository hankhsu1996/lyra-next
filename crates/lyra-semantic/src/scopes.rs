use crate::symbols::{Namespace, SymbolId, SymbolKind, SymbolTable};

/// Trait for looking up symbol names by id during resolution.
///
/// `ScopeTree::resolve` needs symbol names for binary search but does
/// not need the full `Symbol` struct. This trait allows resolution to
/// work against both `SymbolTable` (full data) and `NameGraph`
/// (offset-independent projection).
pub trait SymbolNameLookup {
    fn name(&self, id: SymbolId) -> &str;
}

impl SymbolNameLookup for SymbolTable {
    fn name(&self, id: SymbolId) -> &str {
        self.get(id).name.as_str()
    }
}

/// Scope identifier, per-file index.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ScopeId(pub(crate) u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    File,
    Module,
    Block,
    Generate,
    Function,
    Task,
    Package,
    Interface,
    Program,
    Class,
}

/// A single scope in the scope tree.
///
/// `value_ns` and `type_ns` store `SymbolId`s sorted by the
/// corresponding `Symbol.name`. Names live once in `Symbol`; scope
/// entries reference by id. Resolve does binary search with a custom
/// comparator that looks up `symbols[mid].name`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
    pub kind: ScopeKind,
    pub parent: Option<ScopeId>,
    pub value_ns: Box<[SymbolId]>,
    pub type_ns: Box<[SymbolId]>,
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
    pub(crate) value_bindings: Vec<SymbolId>,
    pub(crate) type_bindings: Vec<SymbolId>,
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
            value_bindings: Vec::new(),
            type_bindings: Vec::new(),
        });
        id
    }

    pub(crate) fn add_binding(&mut self, scope: ScopeId, sym: SymbolId, kind: SymbolKind) {
        // Modport symbols are resolved via DefIndex.modport_name_map, not
        // lexical scope bindings.
        if kind == SymbolKind::Modport {
            return;
        }
        let entry = &mut self.scopes[scope.0 as usize];
        match kind.namespace() {
            Namespace::Value => entry.value_bindings.push(sym),
            Namespace::Type => entry.type_bindings.push(sym),
            // Definition-namespace symbols (modules) live in GlobalDefIndex,
            // not lexical scopes.
            Namespace::Definition => {}
        }
    }

    pub(crate) fn freeze(self, symbols: &SymbolTable) -> ScopeTree {
        let scopes: Vec<Scope> = self
            .scopes
            .into_iter()
            .map(|entry| {
                let mut value_bindings = entry.value_bindings;
                value_bindings.sort_by(|a, b| symbols.get(*a).name.cmp(&symbols.get(*b).name));
                let mut type_bindings = entry.type_bindings;
                type_bindings.sort_by(|a, b| symbols.get(*a).name.cmp(&symbols.get(*b).name));
                Scope {
                    kind: entry.kind,
                    parent: entry.parent,
                    value_ns: value_bindings.into_boxed_slice(),
                    type_ns: type_bindings.into_boxed_slice(),
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
    /// `names` lookup for name comparisons. Returns the first match
    /// (for duplicates, the first in sorted order).
    pub fn resolve(
        &self,
        names: &dyn SymbolNameLookup,
        scope: ScopeId,
        ns: Namespace,
        name: &str,
    ) -> Option<SymbolId> {
        let s = &self.scopes[scope.0 as usize];
        let bindings = match ns {
            Namespace::Value => &s.value_ns,
            Namespace::Type => &s.type_ns,
            // Definition-namespace names are resolved via GlobalDefIndex,
            // not lexical scope chains.
            Namespace::Definition => return None,
        };
        let result = bindings.binary_search_by(|id| names.name(*id).cmp(name));
        if let Ok(mut idx) = result {
            // Walk back to find the first entry with this name
            while idx > 0 && names.name(bindings[idx - 1]) == name {
                idx -= 1;
            }
            return Some(bindings[idx]);
        }
        if let Some(parent) = s.parent {
            return self.resolve(names, parent, ns, name);
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
    use crate::Site;
    use lyra_source::{FileId, NameSpan};
    use smol_str::SmolStr;

    use super::*;
    use crate::record::SymbolOrigin;
    use crate::symbols::{Symbol, SymbolKind, SymbolTableBuilder};

    #[test]
    fn resolve_finds_binding() {
        let mut sym_builder = SymbolTableBuilder::new();
        let mut scope_builder = ScopeTreeBuilder::new();

        let scope = scope_builder.push(ScopeKind::Module, None);
        let placeholder = Site::placeholder(FileId(0));
        let id = sym_builder.push(Symbol {
            name: SmolStr::new("x"),
            kind: SymbolKind::Variable,
            decl_site: placeholder,
            name_site: placeholder,
            type_site: None,
            name_span: NameSpan::INVALID,
            scope,
            origin: SymbolOrigin::TypeSpec,
        });
        scope_builder.add_binding(scope, id, SymbolKind::Variable);

        let symbols = sym_builder.freeze();
        let scopes = scope_builder.freeze(&symbols);

        assert_eq!(
            scopes.resolve(&symbols, scope, Namespace::Value, "x"),
            Some(id)
        );
        assert_eq!(scopes.resolve(&symbols, scope, Namespace::Value, "y"), None);
    }

    #[test]
    fn resolve_walks_parent_chain() {
        let mut sym_builder = SymbolTableBuilder::new();
        let mut scope_builder = ScopeTreeBuilder::new();

        let parent = scope_builder.push(ScopeKind::Module, None);
        let child = scope_builder.push(ScopeKind::Block, Some(parent));
        let placeholder = Site::placeholder(FileId(0));
        let id = sym_builder.push(Symbol {
            name: SmolStr::new("a"),
            kind: SymbolKind::PortAnsi,
            decl_site: placeholder,
            name_site: placeholder,
            type_site: None,
            name_span: NameSpan::INVALID,
            scope: parent,
            origin: SymbolOrigin::TypeSpec,
        });
        scope_builder.add_binding(parent, id, SymbolKind::PortAnsi);

        let symbols = sym_builder.freeze();
        let scopes = scope_builder.freeze(&symbols);

        assert_eq!(
            scopes.resolve(&symbols, child, Namespace::Value, "a"),
            Some(id),
            "child scope should find binding in parent"
        );
    }

    #[test]
    fn resolve_returns_first_duplicate() {
        let mut sym_builder = SymbolTableBuilder::new();
        let mut scope_builder = ScopeTreeBuilder::new();

        let scope = scope_builder.push(ScopeKind::Module, None);
        let placeholder = Site::placeholder(FileId(0));
        let id1 = sym_builder.push(Symbol {
            name: SmolStr::new("x"),
            kind: SymbolKind::Variable,
            decl_site: placeholder,
            name_site: placeholder,
            type_site: None,
            name_span: NameSpan::INVALID,
            scope,
            origin: SymbolOrigin::TypeSpec,
        });
        let id2 = sym_builder.push(Symbol {
            name: SmolStr::new("x"),
            kind: SymbolKind::Variable,
            decl_site: placeholder,
            name_site: placeholder,
            type_site: None,
            name_span: NameSpan::INVALID,
            scope,
            origin: SymbolOrigin::TypeSpec,
        });
        scope_builder.add_binding(scope, id1, SymbolKind::Variable);
        scope_builder.add_binding(scope, id2, SymbolKind::Variable);

        let symbols = sym_builder.freeze();
        let scopes = scope_builder.freeze(&symbols);

        assert_eq!(
            scopes.resolve(&symbols, scope, Namespace::Value, "x"),
            Some(id1),
            "should return first definition"
        );
    }
}
