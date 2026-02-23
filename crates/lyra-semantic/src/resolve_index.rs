use std::collections::HashMap;

use lyra_source::FileId;
use smol_str::SmolStr;

use smallvec::SmallVec;

use lyra_ast::ErasedAstId;

use crate::def_index::{ExportDeclId, ImportDeclId, LocalDeclId};
use crate::diagnostic::SemanticDiag;
use crate::enum_def::EnumVariantTarget;
use crate::scopes::ScopeId;
use crate::symbols::{GlobalDefId, GlobalSymbolId, Namespace, NsMask, SymbolId};

/// Offset-independent resolution result from `build_resolve_core`.
///
/// Local resolutions carry a `SymbolId` (per-file); global resolutions
/// carry either a `GlobalDefId` (definition-namespace) or an `ErasedAstId`
/// (package-scope). The mapping to `GlobalSymbolId` happens in
/// `build_resolve_index`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CoreResolution {
    /// Resolved within the same file's lexical scopes.
    Local {
        symbol: SymbolId,
        namespace: Namespace,
    },
    /// Resolved via the global definitions namespace (`GlobalDefIndex`).
    /// Namespace is always `Definition` (enforced by omission).
    Def { def: GlobalDefId },
    /// Resolved via package scope (`PackageScopeIndex`).
    /// The anchor is a `name_ast` site. Namespace is `Value` or `Type`
    /// (never `Definition`).
    Pkg {
        ast: ErasedAstId,
        namespace: Namespace,
    },
    /// Resolved as a range-generated enum variant name.
    EnumVariant(EnumVariantTarget),
}

impl CoreResolution {
    /// Construct a Pkg resolution.
    ///
    /// Invariant: namespace must be `Value` or `Type`, never `Definition`.
    /// Structurally guaranteed by `PackageScopeIndex::resolve()` returning
    /// `None` for `Definition`, and `resolve_qualified` remapping `Definition`
    /// to `Value` before querying package scope. Validated at the consumer
    /// in `build_resolve_index` via `SemanticDiagKind::InternalError`.
    pub(crate) fn pkg(ast: ErasedAstId, namespace: Namespace) -> Self {
        Self::Pkg { ast, namespace }
    }
}

/// Result of resolving a single use-site.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CoreResolveResult {
    Resolved(CoreResolution),
    Unresolved(UnresolvedReason),
}

/// Why a name could not be resolved.
///
/// All fields are offset-independent (`SmolStr`, `usize`), so
/// `PartialEq` backdates correctly.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnresolvedReason {
    /// Name not found in any scope or import.
    NotFound,
    /// Package in qualified name or import not found.
    PackageNotFound { package: SmolStr },
    /// Member not found in package.
    MemberNotFound { package: SmolStr, member: SmolStr },
    /// Multiple wildcard imports provide the same name.
    AmbiguousWildcardImport { candidates: Box<[SmolStr]> },
    /// Qualified path has more than 2 segments (not yet supported).
    UnsupportedQualifiedPath { len: usize },
}

/// An error from validating an import declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportError {
    /// Index into `NameGraph.imports` / `DefIndex.imports`.
    pub import_idx: u32,
    pub reason: UnresolvedReason,
}

/// Output of `build_resolve_core`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CoreResolveOutput {
    /// One entry per use-site, same order as `NameGraph.use_entries`.
    pub resolutions: Box<[CoreResolveResult]>,
    /// Errors from validating imports (package not found, member not found).
    pub import_errors: Box<[ImportError]>,
    /// LRM 26.3: local declarations that conflict with wildcard-realized names.
    pub wildcard_local_conflicts: Box<[WildcardLocalConflict]>,
}

/// A local declaration that conflicts with a previously realized wildcard
/// import in the same scope (LRM 26.3).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WildcardLocalConflict {
    pub local_decl_id: LocalDeclId,
    pub namespace: Namespace,
    pub import_id: ImportDeclId,
    pub use_site_idx: u32,
}

/// The resolved target of a name: either a declared symbol or a range-generated enum variant.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedTarget {
    Symbol(GlobalSymbolId),
    EnumVariant(EnumVariantTarget),
}

/// A resolved name: the target and the namespace it was found in.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Resolution {
    pub target: ResolvedTarget,
    pub namespace: Namespace,
}

/// Per-file resolution results. Depends on `DefIndex`.
///
/// Maps use-site `ErasedAstId` to resolved `Resolution`.
/// Uses `HashMap` because the access pattern is point lookup
/// by `AstId` (from cursor queries).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolveIndex {
    pub file: FileId,
    pub resolutions: HashMap<lyra_ast::ErasedAstId, Resolution>,
    pub diagnostics: Box<[SemanticDiag]>,
}

/// An import conflict detected by LRM 26.5 rules.
///
/// Keyed by (name, scope, packages) -- no positional indices.
/// Stable across import reordering for Salsa backdating.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportConflict {
    pub scope: ScopeId,
    pub name: SmolStr,
    pub kind: ImportConflictKind,
    /// Export declarations that triggered this conflict (empty for pure explicit imports).
    pub export_sources: SmallVec<[ExportDeclId; 1]>,
}

/// The kind of import conflict (LRM 26.5).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportConflictKind {
    /// Explicit import of a name already locally declared (LRM 26.5a).
    ExplicitVsLocal { package: SmolStr, ns: NsMask },
    /// Explicit import conflicts with a wildcard-provided name from
    /// a different package (different `GlobalDefId`) (LRM 26.5b).
    ExplicitVsWildcard {
        explicit_package: SmolStr,
        wildcard_package: SmolStr,
        ns: NsMask,
    },
}
