//! Shared test utilities for `lyra-semantic` tests.
//!
//! Provides classification helpers used across multiple test modules
//! (e.g., `builder_tests`, `resolve_tests`). Kept separate from
//! production modules so core files like `def_index.rs` stay focused
//! on production concepts.

use crate::global_index::DefinitionKind;
use crate::symbols::SymbolKind;

/// Semantic role of a scope-owning declaration (test classification).
///
/// Unifies `DefinitionKind` (containers) and `SymbolKind` (callables)
/// into a single enum for test helpers that need role-qualified scope
/// lookup without caring about the namespace split.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeOwnerRole {
    Module,
    Package,
    Interface,
    Program,
    Primitive,
    Config,
    Function,
    Task,
}

impl ScopeOwnerRole {
    pub fn from_definition_kind(dk: DefinitionKind) -> Self {
        match dk {
            DefinitionKind::Module => Self::Module,
            DefinitionKind::Package => Self::Package,
            DefinitionKind::Interface => Self::Interface,
            DefinitionKind::Program => Self::Program,
            DefinitionKind::Primitive => Self::Primitive,
            DefinitionKind::Config => Self::Config,
        }
    }

    pub fn from_symbol_kind(sk: SymbolKind) -> Option<Self> {
        match sk {
            SymbolKind::Function => Some(Self::Function),
            SymbolKind::Task => Some(Self::Task),
            _ => None,
        }
    }
}
