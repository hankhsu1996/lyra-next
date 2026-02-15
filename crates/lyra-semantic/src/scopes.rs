use crate::symbols::SymbolId;
use std::collections::HashMap;

/// Scope identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub u32);

/// A single scope in the scope tree.
#[derive(Debug)]
pub struct Scope {
    pub id: ScopeId,
    pub parent: Option<ScopeId>,
    pub names: HashMap<String, SymbolId>,
}

/// Frozen scope tree.
#[derive(Debug, Default)]
pub struct ScopeTree {
    scopes: Vec<Scope>,
}

impl ScopeTree {
    pub fn push(&mut self, scope: Scope) {
        self.scopes.push(scope);
    }

    pub fn get(&self, id: ScopeId) -> Option<&Scope> {
        self.scopes.get(id.0 as usize)
    }

    pub fn resolve(&self, scope: ScopeId, name: &str) -> Option<SymbolId> {
        let s = self.get(scope)?;
        if let Some(&sym) = s.names.get(name) {
            return Some(sym);
        }
        if let Some(parent) = s.parent {
            return self.resolve(parent, name);
        }
        None
    }
}
