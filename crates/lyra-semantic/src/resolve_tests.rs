use lyra_ast::ErasedAstId;
use lyra_source::{FileId, TextSize};
use smol_str::SmolStr;

use crate::def_index::{ExpectedNs, ImplicitNetSiteKind, NamePath, QualifiedRoot, UseSite};
use crate::default_nettype::{ActiveNetType, DefaultNettypePolicy};
use crate::diagnostic::SemanticDiagKind;
use crate::name_lowering::QualifiedPath;
use crate::resolve::classify_unresolved;
use crate::resolve_index::UnresolvedReason;
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
