use std::collections::HashMap;

use lyra_ast::ErasedAstId;
use lyra_source::{FileId, TextSize};
use smol_str::SmolStr;

use crate::def_index::{ExpectedNs, ImplicitNetSiteKind, NamePath, QualifiedRoot, UseSite};
use crate::default_nettype::{ActiveNetType, DefaultNettypePolicy};
use crate::diagnostic::SemanticDiagKind;
use crate::name_lowering::QualifiedPath;
use crate::resolve_build::{ResolveBuildCtx, classify_unresolved};
use crate::resolve_index::{
    ImplicitNet, ImplicitNetId, Resolution, ResolvedTarget, UnresolvedReason,
};
use crate::scopes::ScopeId;
use crate::symbols::Namespace;

fn site_at(offset: u32) -> crate::Site {
    let src = format!("{}x = 1;", " ".repeat(offset as usize));
    let tokens = lyra_lexer::lex(&src);
    let parse = lyra_parser::parse(&tokens, &src);
    let map = lyra_ast::AstIdMap::from_root(FileId(0), &parse.syntax());
    for node in parse.syntax().descendants() {
        if node.kind() == lyra_lexer::SyntaxKind::NameRef
            && let Some(id) = map.erased_ast_id(&node)
            && id.text_range().start() == TextSize::new(offset)
        {
            return id;
        }
    }
    ErasedAstId::placeholder(FileId(0))
}

fn make_use_site(name: &str, net: Option<ImplicitNetSiteKind>) -> UseSite {
    UseSite {
        path: NamePath::Simple(SmolStr::new(name)),
        expected_ns: ExpectedNs::Exact(Namespace::Value),
        scope: ScopeId(0),
        name_ref_site: site_at(10),
        order_key: 0,
        implicit_net_site: net,
    }
}

fn none_policy() -> DefaultNettypePolicy {
    DefaultNettypePolicy::new(vec![(TextSize::new(0), ActiveNetType::None)])
}

const LHS: Option<ImplicitNetSiteKind> = Some(ImplicitNetSiteKind::ContinuousAssignLhs);

#[test]
fn none_policy_at_lhs_produces_implicit_net_forbidden() {
    let us = make_use_site("y", LHS);
    let diag = classify_unresolved(&UnresolvedReason::NotFound, &us, &none_policy());
    match &diag.kind {
        SemanticDiagKind::ImplicitNetForbidden { name, site_kind } => {
            assert_eq!(name.as_str(), "y");
            assert_eq!(*site_kind, ImplicitNetSiteKind::ContinuousAssignLhs);
        }
        other => panic!("expected ImplicitNetForbidden, got {other:?}"),
    }
}

#[test]
fn wire_policy_at_lhs_produces_unresolved_name() {
    let us = make_use_site("y", LHS);
    let policy = DefaultNettypePolicy::new(vec![(TextSize::new(0), ActiveNetType::Wire)]);
    let diag = classify_unresolved(&UnresolvedReason::NotFound, &us, &policy);
    assert!(
        matches!(&diag.kind, SemanticDiagKind::UnresolvedName { .. }),
        "{:?}",
        diag.kind
    );
}

#[test]
fn non_candidate_site_with_none_policy_produces_unresolved_name() {
    let us = make_use_site("x", None);
    let diag = classify_unresolved(&UnresolvedReason::NotFound, &us, &none_policy());
    assert!(
        matches!(&diag.kind, SemanticDiagKind::UnresolvedName { .. }),
        "{:?}",
        diag.kind
    );
}

#[test]
fn qualified_name_never_produces_implicit_net_forbidden() {
    let us = UseSite {
        path: NamePath::Qualified(QualifiedPath {
            root: QualifiedRoot::Package(SmolStr::new("pkg")),
            member: SmolStr::new("y"),
        }),
        expected_ns: ExpectedNs::Exact(Namespace::Value),
        scope: ScopeId(0),
        name_ref_site: site_at(10),
        order_key: 0,
        implicit_net_site: None,
    };
    let diag = classify_unresolved(&UnresolvedReason::NotFound, &us, &none_policy());
    assert!(!matches!(
        &diag.kind,
        SemanticDiagKind::ImplicitNetForbidden { .. }
    ));
}

#[test]
fn default_policy_produces_unresolved_name() {
    let us = make_use_site("y", LHS);
    let policy = DefaultNettypePolicy::default();
    let diag = classify_unresolved(&UnresolvedReason::NotFound, &us, &policy);
    assert!(
        matches!(&diag.kind, SemanticDiagKind::UnresolvedName { .. }),
        "{:?}",
        diag.kind
    );
}

#[test]
fn to_implicit_net_kind_none_returns_none() {
    assert!(ActiveNetType::None.to_implicit_net_kind().is_none());
}

#[test]
fn to_implicit_net_kind_wire_returns_wire() {
    use crate::types::NetKind;
    assert_eq!(
        ActiveNetType::Wire.to_implicit_net_kind(),
        Some(NetKind::Wire)
    );
}

#[test]
fn to_implicit_net_kind_tri_returns_tri() {
    use crate::types::NetKind;
    assert_eq!(
        ActiveNetType::Tri.to_implicit_net_kind(),
        Some(NetKind::Tri)
    );
}

#[test]
fn to_implicit_net_kind_triand_maps_to_wand() {
    use crate::types::NetKind;
    assert_eq!(
        ActiveNetType::Triand.to_implicit_net_kind(),
        Some(NetKind::Wand)
    );
}

#[test]
fn to_implicit_net_kind_trior_maps_to_wor() {
    use crate::types::NetKind;
    assert_eq!(
        ActiveNetType::Trior.to_implicit_net_kind(),
        Some(NetKind::Wor)
    );
}

fn wire_policy() -> DefaultNettypePolicy {
    DefaultNettypePolicy::new(vec![(TextSize::new(0), ActiveNetType::Wire)])
}

fn make_use_site_in_scope(
    name: &str,
    scope: ScopeId,
    offset: u32,
    net: Option<ImplicitNetSiteKind>,
) -> UseSite {
    UseSite {
        path: NamePath::Simple(SmolStr::new(name)),
        expected_ns: ExpectedNs::Exact(Namespace::Value),
        scope,
        name_ref_site: site_at(offset),
        order_key: 0,
        implicit_net_site: net,
    }
}

fn run_classify(
    use_sites: &[UseSite],
    policy: &DefaultNettypePolicy,
) -> (
    HashMap<crate::Site, Resolution>,
    Vec<crate::diagnostic::SemanticDiag>,
    Vec<ImplicitNet>,
) {
    let mut ctx = ResolveBuildCtx::new();
    for us in use_sites {
        ctx.classify_and_record_unresolved(&UnresolvedReason::NotFound, us, policy);
    }
    ctx.into_parts()
}

#[test]
fn wire_policy_creates_implicit_net() {
    let us = make_use_site_in_scope("y", ScopeId(0), 10, LHS);
    let (resolutions, diagnostics, nets) = run_classify(std::slice::from_ref(&us), &wire_policy());
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    assert_eq!(nets.len(), 1);
    assert_eq!(nets[0].name.as_str(), "y");
    assert_eq!(nets[0].net_kind, crate::types::NetKind::Wire);
    assert_eq!(nets[0].owner_scope, ScopeId(0));
    let res = resolutions.get(&us.name_ref_site);
    assert!(
        matches!(
            res,
            Some(Resolution {
                target: ResolvedTarget::ImplicitNet(ImplicitNetId(0)),
                ..
            })
        ),
        "{res:?}"
    );
}

#[test]
fn dedup_same_scope_reuses_id() {
    let us1 = make_use_site_in_scope("y", ScopeId(0), 10, LHS);
    let us2 = make_use_site_in_scope("y", ScopeId(0), 20, LHS);
    let (resolutions, diagnostics, nets) =
        run_classify(&[us1.clone(), us2.clone()], &wire_policy());
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    assert_eq!(
        nets.len(),
        1,
        "expected one implicit net, got {}",
        nets.len()
    );
    let r1 = resolutions.get(&us1.name_ref_site);
    let r2 = resolutions.get(&us2.name_ref_site);
    assert!(
        matches!(
            r1,
            Some(Resolution {
                target: ResolvedTarget::ImplicitNet(ImplicitNetId(0)),
                ..
            })
        ),
        "{r1:?}"
    );
    assert!(
        matches!(
            r2,
            Some(Resolution {
                target: ResolvedTarget::ImplicitNet(ImplicitNetId(0)),
                ..
            })
        ),
        "{r2:?}"
    );
}

#[test]
fn distinct_scopes_create_distinct_nets() {
    let us1 = make_use_site_in_scope("y", ScopeId(0), 10, LHS);
    let us2 = make_use_site_in_scope("y", ScopeId(1), 20, LHS);
    let (_, diagnostics, nets) = run_classify(&[us1, us2], &wire_policy());
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    assert_eq!(
        nets.len(),
        2,
        "expected two implicit nets, got {}",
        nets.len()
    );
    assert_eq!(nets[0].owner_scope, ScopeId(0));
    assert_eq!(nets[1].owner_scope, ScopeId(1));
}

#[test]
fn tri_policy_creates_implicit_net_with_tri_kind() {
    let policy = DefaultNettypePolicy::new(vec![(TextSize::new(0), ActiveNetType::Tri)]);
    let us = make_use_site_in_scope("z", ScopeId(0), 10, LHS);
    let (_, diagnostics, nets) = run_classify(&[us], &policy);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    assert_eq!(nets.len(), 1);
    assert_eq!(nets[0].net_kind, crate::types::NetKind::Tri);
}

#[test]
fn none_policy_emits_forbidden_no_implicit_net() {
    let us = make_use_site_in_scope("y", ScopeId(0), 10, LHS);
    let (resolutions, diagnostics, nets) = run_classify(std::slice::from_ref(&us), &none_policy());
    assert!(nets.is_empty(), "no implicit net should be created");
    assert!(resolutions.is_empty(), "no resolution should be recorded");
    assert_eq!(diagnostics.len(), 1);
    assert!(matches!(
        &diagnostics[0].kind,
        SemanticDiagKind::ImplicitNetForbidden { .. }
    ));
}

#[test]
fn non_candidate_site_emits_unresolved_no_implicit_net() {
    let us = make_use_site_in_scope("y", ScopeId(0), 10, None);
    let (_, diagnostics, nets) = run_classify(&[us], &wire_policy());
    assert!(nets.is_empty());
    assert_eq!(diagnostics.len(), 1);
    assert!(matches!(
        &diagnostics[0].kind,
        SemanticDiagKind::UnresolvedName { .. }
    ));
}
