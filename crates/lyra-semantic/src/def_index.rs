use std::collections::HashMap;

use lyra_ast::ErasedAstId;
use lyra_source::{FileId, TextRange};
use smol_str::SmolStr;

use crate::diagnostic::SemanticDiag;
use crate::scopes::{ScopeId, ScopeTree};
use crate::symbols::{Namespace, SymbolId, SymbolTable};

/// Lookup strategy for a use-site.
///
/// Determines which namespace(s) to search and in what order.
/// Lives on use-sites and resolution requests only -- never on
/// symbol storage or scope bindings (those stay `Namespace`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExpectedNs {
    /// Look up in exactly one namespace.
    Exact(Namespace),
    /// Type position: try Type first, fall back to Value.
    /// Used for user-defined type names in `TypeSpec`.
    TypeThenValue,
}

/// Per-file definition index: symbols, scopes, exports.
///
/// Depends only on the parse result. Does NOT resolve uses.
/// All ranges are in expanded-text coordinate space within `file`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefIndex {
    pub file: FileId,
    pub symbols: SymbolTable,
    pub scopes: ScopeTree,
    pub exports: Exports,
    pub use_sites: Box<[UseSite]>,
    pub imports: Box<[Import]>,
    /// Reverse map from exported declaration `ErasedAstId` to `SymbolId`.
    /// Used by cross-file resolution to convert `ErasedAstId` (from
    /// `GlobalDefIndex`) back to a `SymbolId` in this file.
    pub decl_to_symbol: HashMap<ErasedAstId, SymbolId>,
    pub diagnostics: Box<[SemanticDiag]>,
}

/// A name path: simple identifier or qualified path.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NamePath {
    /// Simple identifier: `foo`
    Simple(SmolStr),
    /// Qualified path: `a::b::c`. Segments are the identifiers.
    Qualified { segments: Box<[SmolStr]> },
}

impl NamePath {
    pub fn as_simple(&self) -> Option<&str> {
        match self {
            Self::Simple(s) => Some(s.as_str()),
            Self::Qualified { .. } => None,
        }
    }

    pub fn as_qualified(&self) -> Option<&[SmolStr]> {
        match self {
            Self::Simple(_) => None,
            Self::Qualified { segments } => Some(segments),
        }
    }

    pub fn display_name(&self) -> String {
        match self {
            Self::Simple(s) => s.to_string(),
            Self::Qualified { segments } => segments.join("::"),
        }
    }
}

/// A recorded name-use site awaiting resolution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseSite {
    pub path: NamePath,
    pub expected_ns: ExpectedNs,
    pub range: TextRange,
    pub scope: ScopeId,
    pub ast_id: ErasedAstId,
}

/// An import declaration record.
///
/// Carries no namespace: the namespace is determined at the use-site
/// when the import is consulted during resolution. This matches LRM
/// semantics where `import pkg::x` makes `x` visible for whatever
/// namespace the use-site needs.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Import {
    pub package: SmolStr,
    pub name: ImportName,
    pub scope: ScopeId,
    pub range: TextRange,
}

/// Whether an import is explicit (`pkg::sym`) or wildcard (`pkg::*`).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ImportName {
    Explicit(SmolStr),
    Wildcard,
}

/// Definition-namespace and package names exported to compilation-unit namespace.
///
/// Separate from lexical `ScopeTree`. Entries sorted by
/// `symbols[id].name` for binary-search lookup.
///
/// `definitions` holds all definition-namespace constructs (module, interface,
/// program, primitive, config) per LRM 3.13(a). Packages stay separate since
/// they are in a distinct namespace (LRM 3.13(b)) and are used differently
/// (package scope index).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Exports {
    pub definitions: Box<[SymbolId]>,
    pub packages: Box<[SymbolId]>,
}
