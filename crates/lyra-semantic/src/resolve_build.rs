//! Assembly of per-file `ResolveIndex` from core resolution results.
//!
//! `resolve.rs` implements the core name-resolution algorithms (lexical
//! lookup, import resolution, scope traversal). This module takes those
//! core results and assembles the final per-file `ResolveIndex`:
//! zipping use-sites with core results, creating implicit net entities,
//! deduplicating by (scope, name), and collecting diagnostics.

use std::collections::HashMap;

use smol_str::SmolStr;

use crate::def_index::{DefIndex, ExpectedNs, ImportName, NamePath};
use crate::diagnostic::{DiagSpan, SemanticDiag, SemanticDiagKind};
use crate::resolve_index::{
    self, CoreResolution, CoreResolveOutput, CoreResolveResult, ImplicitNet, ImplicitNetId,
    ImplicitNetIndex, Resolution, ResolveIndex, ResolvedTarget, UnresolvedReason,
};
use crate::scopes::ScopeId;
use crate::symbols::{GlobalSymbolId, Namespace, SymbolId};

/// Mutable builder state for assembling a `ResolveIndex`.
///
/// Owns all four output collections: resolutions, diagnostics,
/// implicit net entities, and the dedup map. Unresolved-name
/// classification is a method on this builder.
pub(crate) struct ResolveBuildCtx {
    pub(crate) resolutions: HashMap<crate::Site, Resolution>,
    pub(crate) diagnostics: Vec<SemanticDiag>,
    implicit_nets: Vec<ImplicitNet>,
    implicit_net_dedup: HashMap<(ScopeId, SmolStr), ImplicitNetId>,
}

impl ResolveBuildCtx {
    pub(crate) fn new() -> Self {
        Self {
            resolutions: HashMap::new(),
            diagnostics: Vec::new(),
            implicit_nets: Vec::new(),
            implicit_net_dedup: HashMap::new(),
        }
    }

    pub(crate) fn into_parts(
        self,
    ) -> (
        HashMap<crate::Site, Resolution>,
        Vec<SemanticDiag>,
        Vec<ImplicitNet>,
    ) {
        (self.resolutions, self.diagnostics, self.implicit_nets)
    }

    /// Classify an unresolved use-site and either emit a diagnostic or
    /// create/reuse an implicit net entity.
    ///
    /// At implicit-net candidate sites (where `UseSite.implicit_net_site`
    /// is `Some`), consults the active `default_nettype` policy:
    ///
    /// - If the policy allows net creation (`wire`, `tri`, etc.), creates
    ///   or reuses an implicit net entity (deduped by scope + name) and
    ///   records a `ResolvedTarget::ImplicitNet` resolution.
    /// - If the policy is `none`, emits `ImplicitNetForbidden`.
    /// - Otherwise (non-candidate site, or reason other than `NotFound`),
    ///   emits the standard `UnresolvedName` diagnostic.
    pub(crate) fn classify_and_record_unresolved(
        &mut self,
        reason: &UnresolvedReason,
        use_site: &crate::def_index::UseSite,
        default_nettype: &crate::default_nettype::DefaultNettypePolicy,
    ) {
        if matches!(reason, UnresolvedReason::NotFound)
            && use_site.implicit_net_site.is_some()
            && let Some(simple_name) = use_site.path.as_simple()
        {
            let active = default_nettype.active_at_site(use_site.name_ref_site);
            if let Some(net_kind) = active.to_implicit_net_kind() {
                let name = SmolStr::new(simple_name);
                let key = (use_site.scope, name.clone());
                let id = *self.implicit_net_dedup.entry(key).or_insert_with(|| {
                    let id = ImplicitNetId(self.implicit_nets.len() as u32);
                    self.implicit_nets.push(ImplicitNet {
                        owner_scope: use_site.scope,
                        name,
                        net_kind,
                        decl_site: use_site.name_ref_site,
                    });
                    id
                });
                self.resolutions.insert(
                    use_site.name_ref_site,
                    Resolution {
                        target: ResolvedTarget::ImplicitNet(id),
                        namespace: Namespace::Value,
                    },
                );
                return;
            }
        }
        let diag = classify_unresolved(reason, use_site, default_nettype);
        self.diagnostics.push(diag);
    }
}

/// Build the per-file resolution index from pre-computed core results.
///
/// Zips `def.use_sites` with `core.resolutions`, builds the `HashMap`
/// and diagnostics. Import errors are also mapped to diagnostics.
///
/// `lookup_decl` maps a `Site` (`name_site` anchor from
/// `CoreResolution::Def` or `CoreResolution::Pkg`) to a `SymbolId`
/// in the target file.
///
/// `instance_filter` is called when core resolution resolves a use-site
/// to an `Instance` symbol. Returns `true` if the instance should be
/// kept as a valid resolution (e.g., interface instances). Returns
/// `false` to reject the resolution and emit an unresolved diagnostic
/// (e.g., module instances are not valid in value expressions).
pub fn build_resolve_index(
    def: &DefIndex,
    core: &CoreResolveOutput,
    lookup_decl: &dyn Fn(crate::Site) -> Option<SymbolId>,
    instance_filter: &dyn Fn(crate::instance_decl::InstanceDeclIdx) -> bool,
    default_nettype: &crate::default_nettype::DefaultNettypePolicy,
) -> ResolveIndex {
    let mut ctx = ResolveBuildCtx::new();

    for (use_site, result) in def.use_sites.iter().zip(core.resolutions.iter()) {
        match result {
            CoreResolveResult::Resolved(CoreResolution::Local { symbol, namespace }) => {
                let site_span = DiagSpan::Site(use_site.name_ref_site);
                if let crate::record::SymbolOrigin::Instance(idx) = def.symbols.get(*symbol).origin
                    && !instance_filter(idx)
                {
                    ctx.diagnostics.push(resolve_index::reason_to_diagnostic(
                        &UnresolvedReason::NotFound,
                        use_site.expected_ns,
                        &use_site.path,
                        site_span,
                    ));
                    continue;
                }
                let target = ResolvedTarget::Symbol(GlobalSymbolId {
                    file: def.file,
                    local: *symbol,
                });
                ctx.resolutions.insert(
                    use_site.name_ref_site,
                    Resolution {
                        target,
                        namespace: *namespace,
                    },
                );
                if let Some(diag) = resolve_index::check_type_mismatch(
                    use_site.expected_ns,
                    *namespace,
                    &use_site.path,
                    site_span,
                ) {
                    ctx.diagnostics.push(diag);
                }
            }
            CoreResolveResult::Resolved(CoreResolution::Def { def }) => {
                ctx.resolutions.insert(
                    use_site.name_ref_site,
                    Resolution {
                        target: ResolvedTarget::Def(*def),
                        namespace: Namespace::Definition,
                    },
                );
            }
            CoreResolveResult::Resolved(CoreResolution::Pkg {
                name_site,
                namespace,
            }) => {
                resolve_index::resolve_cross_file(
                    *name_site,
                    *namespace,
                    use_site,
                    lookup_decl,
                    &mut ctx.resolutions,
                    &mut ctx.diagnostics,
                );
            }
            CoreResolveResult::Resolved(CoreResolution::EnumVariant(target)) => {
                ctx.resolutions.insert(
                    use_site.name_ref_site,
                    Resolution {
                        target: ResolvedTarget::EnumVariant(target.clone()),
                        namespace: Namespace::Value,
                    },
                );
            }
            CoreResolveResult::Unresolved(reason) => {
                ctx.classify_and_record_unresolved(reason, use_site, default_nettype);
            }
        }
    }

    let (resolutions, mut diagnostics, implicit_nets) = ctx.into_parts();
    map_import_errors(def, core, &mut diagnostics);

    ResolveIndex {
        file: def.file,
        resolutions,
        implicit_nets: ImplicitNetIndex::new(implicit_nets),
        diagnostics: diagnostics.into_boxed_slice(),
    }
}

/// Classify an unresolved use-site into the appropriate diagnostic.
///
/// At implicit-net candidate sites where the active `default_nettype` is
/// `none`, produces `ImplicitNetForbidden` instead of the generic
/// `UnresolvedName`. All other unresolved cases produce the standard
/// diagnostic from `reason_to_diagnostic`.
pub(crate) fn classify_unresolved(
    reason: &UnresolvedReason,
    use_site: &crate::def_index::UseSite,
    default_nettype: &crate::default_nettype::DefaultNettypePolicy,
) -> SemanticDiag {
    let site_span = DiagSpan::Site(use_site.name_ref_site);
    if matches!(reason, UnresolvedReason::NotFound)
        && let Some(site_kind) = use_site.implicit_net_site
        && default_nettype
            .active_at_site(use_site.name_ref_site)
            .is_none()
    {
        let name = SmolStr::new(use_site.path.display_name());
        let kind = SemanticDiagKind::ImplicitNetForbidden { name, site_kind };
        SemanticDiag {
            kind,
            primary: site_span,
            label: None,
        }
    } else {
        resolve_index::reason_to_diagnostic(reason, use_site.expected_ns, &use_site.path, site_span)
    }
}

fn map_import_errors(
    def: &DefIndex,
    core: &CoreResolveOutput,
    diagnostics: &mut Vec<SemanticDiag>,
) {
    for err in &core.import_errors {
        let imp = &def.imports[err.import_idx as usize];
        let path = match &imp.name {
            ImportName::Explicit(member) => {
                NamePath::Qualified(crate::name_lowering::QualifiedPath {
                    root: crate::def_index::QualifiedRoot::Package(imp.package.clone()),
                    member: member.clone(),
                })
            }
            ImportName::Wildcard => NamePath::Simple(SmolStr::new(format!("{}::*", imp.package))),
        };
        let diag = resolve_index::reason_to_diagnostic(
            &err.reason,
            ExpectedNs::Exact(Namespace::Value),
            &path,
            DiagSpan::Site(imp.import_stmt_site),
        );
        diagnostics.push(diag);
    }
}
