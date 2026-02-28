use lyra_ast::{AstNode, HasSyntax, TypeNameRef, TypeSpec, UnpackedDimKind};
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;

fn parse_module(src: &str) -> SyntaxNode {
    let tokens = lyra_lexer::lex(src);
    let pp = lyra_preprocess::preprocess_identity(lyra_source::FileId(0), &tokens, src);
    lyra_parser::parse(&pp.tokens, &pp.expanded_text).syntax()
}

fn find_kind(node: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxNode> {
    if node.kind() == kind {
        return Some(node.clone());
    }
    for child in node.children() {
        if let Some(found) = find_kind(&child, kind) {
            return Some(found);
        }
    }
    None
}

fn find_typespec(src: &str) -> TypeSpec {
    let root = parse_module(src);
    let node = find_kind(&root, SyntaxKind::TypeSpec).expect("should find TypeSpec");
    TypeSpec::cast(node).expect("should cast to TypeSpec")
}

// TypeSpec tests

#[test]
fn typespec_keyword_logic() {
    let ts = find_typespec("module m; logic [7:0] x; endmodule");
    let kw = ts.keyword().expect("should have keyword");
    assert_eq!(kw.kind(), SyntaxKind::LogicKw);
    assert_eq!(ts.packed_dimensions().count(), 1);
}

#[test]
fn typespec_type_name_ref_simple() {
    let ts = find_typespec("module m; my_type x; endmodule");
    match ts.type_name_ref() {
        Some(TypeNameRef::Simple(nr)) => {
            let ident = nr.ident().expect("ident");
            assert_eq!(ident.text(), "my_type");
        }
        other => panic!("expected Simple, got {other:?}"),
    }
}

#[test]
fn typespec_type_name_ref_qualified() {
    let ts = find_typespec("module m; pkg::my_type x; endmodule");
    assert!(matches!(
        ts.type_name_ref(),
        Some(TypeNameRef::Qualified(_))
    ));
}

#[test]
fn typespec_type_name_ref_dotted() {
    let ts = find_typespec("module m; my_bus.master x; endmodule");
    assert!(matches!(ts.type_name_ref(), Some(TypeNameRef::Dotted(_))));
}

#[test]
fn typespec_signed_token() {
    let ts = find_typespec("module m; logic signed [7:0] x; endmodule");
    let tok = ts.signed_token().expect("should have signed");
    assert_eq!(tok.kind(), SyntaxKind::SignedKw);
}

#[test]
fn typespec_unsigned_token() {
    let ts = find_typespec("module m; logic unsigned [7:0] x; endmodule");
    let tok = ts.signed_token().expect("should have unsigned");
    assert_eq!(tok.kind(), SyntaxKind::UnsignedKw);
}

#[test]
fn typespec_no_signed_token() {
    let ts = find_typespec("module m; logic [7:0] x; endmodule");
    assert!(ts.signed_token().is_none());
}

// DottedName tests

#[test]
fn dotted_name_parts() {
    let root = parse_module("module m; my_bus.master x; endmodule");
    let dn_node = find_kind(&root, SyntaxKind::DottedName).expect("DottedName");
    let dn = lyra_ast::DottedName::cast(dn_node).expect("cast");

    let iface = dn.interface_ref().expect("interface_ref");
    let iface_ident = iface.ident().expect("ident");
    assert_eq!(iface_ident.text(), "my_bus");

    let mp = dn.modport_ident().expect("modport_ident");
    assert_eq!(mp.text(), "master");
}

// PackedDimension tests

#[test]
fn packed_dim_msb_lsb() {
    let root = parse_module("module m; logic [7:0] x; endmodule");
    let pd_node = find_kind(&root, SyntaxKind::PackedDimension).expect("PackedDimension");
    let pd = lyra_ast::PackedDimension::cast(pd_node).expect("cast");

    let msb = pd.msb().expect("should have msb");
    assert!(msb.syntax().text().to_string().contains('7'));

    let lsb = pd.lsb().expect("should have lsb");
    assert!(lsb.syntax().text().to_string().contains('0'));
}

// UnpackedDimension classify tests

fn parse_unpacked_dim(dim_syntax: &str) -> lyra_ast::UnpackedDimension {
    let src = format!("module m; logic x {dim_syntax}; endmodule");
    let root = parse_module(&src);
    let node = find_kind(&root, SyntaxKind::UnpackedDimension).expect("UnpackedDimension");
    lyra_ast::UnpackedDimension::cast(node).expect("cast")
}

#[test]
fn unpacked_dim_unsized() {
    let dim = parse_unpacked_dim("[]");
    assert!(matches!(dim.classify(), UnpackedDimKind::Unsized));
    assert!(dim.assoc_type_spec().is_none());
}

#[test]
fn unpacked_dim_size() {
    let dim = parse_unpacked_dim("[8]");
    match dim.classify() {
        UnpackedDimKind::Size { expr } => {
            assert!(expr.syntax().text().to_string().contains('8'));
        }
        other => panic!("expected Size, got {other:?}"),
    }
    assert!(dim.assoc_type_spec().is_none());
}

#[test]
fn unpacked_dim_range() {
    let dim = parse_unpacked_dim("[7:0]");
    match dim.classify() {
        UnpackedDimKind::Range { msb, lsb } => {
            assert!(msb.syntax().text().to_string().contains('7'));
            assert!(lsb.syntax().text().to_string().contains('0'));
        }
        other => panic!("expected Range, got {other:?}"),
    }
}

#[test]
fn unpacked_dim_queue_no_bound() {
    let dim = parse_unpacked_dim("[$]");
    match dim.classify() {
        UnpackedDimKind::Queue { bound } => {
            assert!(bound.is_none());
        }
        other => panic!("expected Queue, got {other:?}"),
    }
}

#[test]
fn unpacked_dim_queue_with_bound() {
    let dim = parse_unpacked_dim("[$:100]");
    match dim.classify() {
        UnpackedDimKind::Queue { bound } => {
            let b = bound.expect("should have bound");
            assert!(b.syntax().text().to_string().contains("100"));
        }
        other => panic!("expected Queue, got {other:?}"),
    }
}

#[test]
fn unpacked_dim_wildcard() {
    let dim = parse_unpacked_dim("[*]");
    assert!(matches!(dim.classify(), UnpackedDimKind::Wildcard));
}

#[test]
fn unpacked_dim_assoc() {
    let dim = parse_unpacked_dim("[int]");
    assert!(matches!(dim.classify(), UnpackedDimKind::Assoc));
    assert!(dim.assoc_type_spec().is_some());
}

#[test]
fn unpacked_dim_assoc_invariant() {
    let dim = parse_unpacked_dim("[int]");
    assert!(matches!(dim.classify(), UnpackedDimKind::Assoc));
    assert!(dim.assoc_type_spec().is_some());

    let non_assoc = parse_unpacked_dim("[8]");
    assert!(!matches!(non_assoc.classify(), UnpackedDimKind::Assoc));
    assert!(non_assoc.assoc_type_spec().is_none());
}

// ParamDecl tests

fn find_param_decl(src: &str) -> lyra_ast::ParamDecl {
    let root = parse_module(src);
    let node = find_kind(&root, SyntaxKind::ParamDecl).expect("ParamDecl");
    lyra_ast::ParamDecl::cast(node).expect("cast")
}

#[test]
fn param_decl_no_type_keyword_with_typespec() {
    let pd = find_param_decl("module m; parameter int P = 0; endmodule");
    assert!(pd.type_keyword().is_none());
}

#[test]
fn param_decl_no_type_keyword_without_typespec() {
    let pd = find_param_decl("module m; parameter P = 8; endmodule");
    assert!(pd.type_keyword().is_none());
}

// Declarator unpacked_dimensions test

#[test]
fn declarator_unpacked_dims() {
    let root = parse_module("module m; logic x [3:0][7:0]; endmodule");
    let decl_node = find_kind(&root, SyntaxKind::Declarator).expect("Declarator");
    let decl = lyra_ast::Declarator::cast(decl_node).expect("cast");
    assert_eq!(decl.unpacked_dimensions().count(), 2);
}

// Port unpacked_dimensions test

#[test]
fn port_unpacked_dims() {
    let root = parse_module("module m(input logic a [3:0]); endmodule");
    let port_node = find_kind(&root, SyntaxKind::Port).expect("Port");
    let port = lyra_ast::Port::cast(port_node).expect("cast");
    assert_eq!(port.unpacked_dimensions().count(), 1);
}

// TypedefDecl unpacked_dimensions test

#[test]
fn typedef_unpacked_dims() {
    let root = parse_module("module m; typedef logic byte_arr [8]; endmodule");
    let td_node = find_kind(&root, SyntaxKind::TypedefDecl).expect("TypedefDecl");
    let td = lyra_ast::TypedefDecl::cast(td_node).expect("cast");
    assert_eq!(td.unpacked_dimensions().count(), 1);
}

// Recovery: partial range falls through to Size or Unsized

#[test]
fn unpacked_dim_partial_range_recovery() {
    let root = parse_module("module m; logic x [7:]; endmodule");
    let dim_node = find_kind(&root, SyntaxKind::UnpackedDimension);
    if let Some(node) = dim_node {
        let dim = lyra_ast::UnpackedDimension::cast(node).expect("cast");
        // With Colon but missing lsb, classify should NOT return Range.
        // It falls through to Size (if msb expr survives) or Unsized.
        let kind = dim.classify();
        assert!(!matches!(kind, UnpackedDimKind::Range { .. }));
    }
}
