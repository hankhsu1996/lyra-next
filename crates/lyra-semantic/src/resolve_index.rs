use std::collections::HashMap;

use lyra_source::FileId;
use smol_str::SmolStr;

use smallvec::SmallVec;

use crate::Site;

use crate::def_index::{ExpectedNs, ExportDeclId, ImportDeclId, LocalDeclId, NamePath, UseSite};
use crate::diagnostic::{DiagSpan, SemanticDiag, SemanticDiagKind};
use crate::enum_def::EnumVariantTarget;
use crate::scopes::ScopeId;
use crate::symbols::{GlobalDefId, GlobalSymbolId, Namespace, NsMask, SymbolId};
use crate::types::NetKind;

/// Offset-independent resolution result from `build_resolve_core`.
///
/// Local resolutions carry a `SymbolId` (per-file); global resolutions
/// carry either a `GlobalDefId` (definition-namespace) or an `Site`
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
    /// The anchor is a `name_site` site. Namespace is `Value` or `Type`
    /// (never `Definition`).
    Pkg {
        name_site: Site,
        namespace: Namespace,
    },
    /// Resolved as a range-generated enum variant name.
    EnumVariant(EnumVariantTarget),
}

impl CoreResolution {
    /// Construct a site-based resolution (package scope or CU file scope).
    ///
    /// Returns `Err` if namespace is `Definition` (structurally prevented
    /// by `PackageScopeIndex::resolve()` returning `None` for `Definition`,
    /// and `resolve_qualified` remapping `Definition` to `Value`).
    pub(crate) fn from_site(name_site: Site, namespace: Namespace) -> Result<Self, SmolStr> {
        if namespace == Namespace::Definition {
            return Err(SmolStr::new(format!(
                "site-based resolution carries Definition namespace (anchor={name_site:?})"
            )));
        }
        Ok(Self::Pkg {
            name_site,
            namespace,
        })
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
    /// Member not found in qualified root (package or `$unit`).
    MemberNotFound {
        root: crate::def_index::QualifiedRoot,
        member: SmolStr,
    },
    /// Multiple wildcard imports provide the same name.
    AmbiguousWildcardImport { candidates: Box<[SmolStr]> },
    /// Multiple compilation-unit file-scope declarations share this name.
    AmbiguousCuScope { sites: Box<[Site]> },
    /// Qualified path has more than 2 segments (not yet supported).
    UnsupportedQualifiedPath { len: usize },
    /// Internal invariant violation (e.g. Pkg with Definition namespace).
    InternalError { detail: SmolStr },
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

/// File-local ordinal identity for an implicit net created by resolve (LRM 6.10).
///
/// Indexes into `ImplicitNetIndex::nets`. Scoped to a single `ResolveIndex`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ImplicitNetId(pub u32);

/// A resolve-created implicit net entity (LRM 6.10).
///
/// One entity per unique `(ScopeId, name)` pair within a file.
/// Multiple use-sites resolve to the same `ImplicitNetId`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplicitNet {
    pub owner_scope: ScopeId,
    pub name: SmolStr,
    pub net_kind: NetKind,
    /// Canonical creation site: the first use-site that triggered creation.
    pub decl_site: Site,
}

/// Ordered storage for implicit nets created during resolution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplicitNetIndex {
    nets: Box<[ImplicitNet]>,
}

impl ImplicitNetIndex {
    pub fn new(nets: Vec<ImplicitNet>) -> Self {
        Self {
            nets: nets.into_boxed_slice(),
        }
    }

    pub fn empty() -> Self {
        Self { nets: Box::new([]) }
    }

    pub fn get(&self, id: ImplicitNetId) -> Option<&ImplicitNet> {
        self.nets.get(id.0 as usize)
    }

    pub fn len(&self) -> usize {
        self.nets.len()
    }

    pub fn is_empty(&self) -> bool {
        self.nets.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (ImplicitNetId, &ImplicitNet)> {
        self.nets
            .iter()
            .enumerate()
            .map(|(i, net)| (ImplicitNetId(i as u32), net))
    }
}

/// The resolved target of a name: a symbol, a definition-namespace entry,
/// a range-generated enum variant, or an implicit net.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedTarget {
    Symbol(GlobalSymbolId),
    Def(GlobalDefId),
    EnumVariant(EnumVariantTarget),
    /// Resolve-created implicit net (LRM 6.10).
    ImplicitNet(ImplicitNetId),
}

/// A resolved name: the target and the namespace it was found in.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Resolution {
    pub target: ResolvedTarget,
    pub namespace: Namespace,
}

/// Per-file resolution results. Depends on `DefIndex`.
///
/// Maps use-site `Site` to resolved `Resolution`.
/// `resolutions` is a lookup-only table: keyed point lookup by `Site`;
/// do not iterate for observable output.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolveIndex {
    pub file: FileId,
    pub resolutions: HashMap<Site, Resolution>,
    pub implicit_nets: ImplicitNetIndex,
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

pub(crate) fn reason_to_diagnostic(
    reason: &UnresolvedReason,
    expected_ns: ExpectedNs,
    path: &NamePath,
    primary: DiagSpan,
) -> SemanticDiag {
    match reason {
        UnresolvedReason::NotFound => {
            let name = SmolStr::new(path.display_name());
            let kind = if matches!(
                expected_ns,
                ExpectedNs::TypeThenValue | ExpectedNs::Exact(Namespace::Type)
            ) {
                SemanticDiagKind::UndeclaredType { name }
            } else {
                SemanticDiagKind::UnresolvedName { name }
            };
            SemanticDiag {
                kind,
                primary,
                label: None,
            }
        }
        UnresolvedReason::PackageNotFound { package } => SemanticDiag {
            kind: SemanticDiagKind::PackageNotFound {
                package: package.clone(),
            },
            primary,
            label: None,
        },
        UnresolvedReason::MemberNotFound { root, member } => SemanticDiag {
            kind: SemanticDiagKind::MemberNotFound {
                package: SmolStr::new(root.display_name()),
                member: member.clone(),
            },
            primary,
            label: None,
        },
        UnresolvedReason::AmbiguousWildcardImport { candidates } => SemanticDiag {
            kind: SemanticDiagKind::AmbiguousWildcardImport {
                name: SmolStr::new(path.display_name()),
                candidates: candidates.clone(),
            },
            primary,
            label: None,
        },
        UnresolvedReason::AmbiguousCuScope { sites } => SemanticDiag {
            kind: SemanticDiagKind::AmbiguousCuScope {
                name: SmolStr::new(path.display_name()),
                sites: sites.clone(),
            },
            primary,
            label: None,
        },
        UnresolvedReason::UnsupportedQualifiedPath { .. } => SemanticDiag {
            kind: SemanticDiagKind::UnsupportedQualifiedPath {
                path: SmolStr::new(path.display_name()),
            },
            primary,
            label: None,
        },
        UnresolvedReason::InternalError { detail } => SemanticDiag {
            kind: SemanticDiagKind::InternalError {
                detail: detail.clone(),
            },
            primary,
            label: None,
        },
    }
}

pub(crate) fn check_type_mismatch(
    expected_ns: ExpectedNs,
    actual_ns: Namespace,
    path: &NamePath,
    primary: DiagSpan,
) -> Option<SemanticDiag> {
    if expected_ns == ExpectedNs::TypeThenValue && actual_ns == Namespace::Value {
        Some(SemanticDiag {
            kind: SemanticDiagKind::NotAType {
                name: SmolStr::new(path.display_name()),
            },
            primary,
            label: None,
        })
    } else {
        None
    }
}

/// Resolve a cross-file (package-scope) name site and insert into the
/// resolution map, emitting type-mismatch diagnostics if needed.
pub(crate) fn resolve_cross_file(
    anchor: crate::Site,
    namespace: Namespace,
    use_site: &UseSite,
    lookup_decl: &dyn Fn(crate::Site) -> Option<SymbolId>,
    resolutions: &mut HashMap<crate::Site, Resolution>,
    diagnostics: &mut Vec<SemanticDiag>,
) {
    if let Some(local) = lookup_decl(anchor) {
        resolutions.insert(
            use_site.name_ref_site,
            Resolution {
                target: ResolvedTarget::Symbol(GlobalSymbolId {
                    file: anchor.file(),
                    local,
                }),
                namespace,
            },
        );
        if let Some(diag) = check_type_mismatch(
            use_site.expected_ns,
            namespace,
            &use_site.path,
            DiagSpan::Site(use_site.name_ref_site),
        ) {
            diagnostics.push(diag);
        }
    }
}
