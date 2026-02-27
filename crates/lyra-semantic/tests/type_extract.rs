use lyra_ast::{
    AstIdMap, AstNode, Declarator, NetDecl, SourceFile, TypeDeclSite, TypeNameRef, TypeSpec,
    VarDecl,
};
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;
use lyra_semantic::types::{ConstInt, IntegralKw, NetKind, SymbolType, Ty};
use lyra_semantic::{
    Site, UserTypeRef, extract_type_from_container, normalize_symbol_type, user_type_ref,
};
use lyra_source::FileId;

fn parse_source(src: &str) -> (lyra_parser::Parse, AstIdMap) {
    let tokens = lyra_lexer::lex(src);
    let pp = lyra_preprocess::preprocess_identity(FileId(0), &tokens, src);
    let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
    let map = AstIdMap::from_root(FileId(0), &parse.syntax());
    (parse, map)
}

fn find_in_module(root: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxNode> {
    let sf = SourceFile::cast(root.clone())?;
    let module = sf.modules().next()?;
    let body = module.body()?;
    let body_node: &SyntaxNode = body.syntax();
    body_node.children().find(|c| c.kind() == kind)
}

fn first_declarator(container: &SyntaxNode) -> Option<Declarator> {
    VarDecl::cast(container.clone())
        .and_then(|v| v.declarators().next())
        .or_else(|| NetDecl::cast(container.clone()).and_then(|n| n.declarators().next()))
}

fn cast_site(node: &SyntaxNode) -> TypeDeclSite {
    TypeDeclSite::cast(node).expect("expected TypeDeclSite")
}

fn find_typespec(container: &SyntaxNode) -> Option<SyntaxNode> {
    match container.kind() {
        SyntaxKind::VarDecl => VarDecl::cast(container.clone())?
            .type_spec()
            .map(|ts| ts.syntax().clone()),
        SyntaxKind::NetDecl => NetDecl::cast(container.clone())?
            .type_spec()
            .map(|ts| ts.syntax().clone()),
        SyntaxKind::ParamDecl => lyra_ast::ParamDecl::cast(container.clone())?
            .type_spec()
            .map(|ts| ts.syntax().clone()),
        _ => None,
    }
}

fn find_first_port(root: &SyntaxNode) -> SyntaxNode {
    let sf = SourceFile::cast(root.clone()).expect("SourceFile");
    let module = sf.modules().next().expect("module");
    let port_list = module.port_list().expect("port list");
    let port = port_list.ports().next().expect("port");
    port.syntax().clone()
}

#[test]
fn extract_logic_simple() {
    let (parse, map) = parse_source("module m; logic x; endmodule");
    let root = parse.syntax();
    let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
    let decl = first_declarator(&var_decl);
    let result = extract_type_from_container(&cast_site(&var_decl), decl.as_ref(), &map);
    assert_eq!(result, SymbolType::Value(Ty::simple_logic()));
}

#[test]
fn extract_int() {
    let (parse, map) = parse_source("module m; int x; endmodule");
    let root = parse.syntax();
    let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
    let decl = first_declarator(&var_decl);
    let result = extract_type_from_container(&cast_site(&var_decl), decl.as_ref(), &map);
    assert_eq!(result, SymbolType::Value(Ty::int()));
}

#[test]
fn extract_logic_packed_has_unevaluated() {
    let (parse, map) = parse_source("module m; logic [7:0] x; endmodule");
    let root = parse.syntax();
    let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
    let decl = first_declarator(&var_decl);
    let result = extract_type_from_container(&cast_site(&var_decl), decl.as_ref(), &map);
    if let SymbolType::Value(Ty::Integral(i)) = &result {
        assert_eq!(i.keyword, IntegralKw::Logic);
        assert!(!i.signed);
        assert_eq!(i.packed.len(), 1);
        assert!(matches!(i.packed[0].msb, ConstInt::Unevaluated(_)));
        assert!(matches!(i.packed[0].lsb, ConstInt::Unevaluated(_)));
    } else {
        panic!("expected Value(Integral), got {result:?}");
    }
}

#[test]
fn extract_wire_simple() {
    let (parse, map) = parse_source("module m; wire w; endmodule");
    let root = parse.syntax();
    let net_decl = find_in_module(&root, SyntaxKind::NetDecl).expect("NetDecl");
    let decl = first_declarator(&net_decl);
    let result = extract_type_from_container(&cast_site(&net_decl), decl.as_ref(), &map);
    if let SymbolType::Net(nt) = &result {
        assert_eq!(nt.kind, NetKind::Wire);
        assert_eq!(nt.data, Ty::simple_logic());
    } else {
        panic!("expected Net, got {result:?}");
    }
}

#[test]
fn extract_param_no_typespec_defaults_int() {
    let (parse, map) = parse_source("module m; parameter W = 8; endmodule");
    let root = parse.syntax();
    let param_decl = find_in_module(&root, SyntaxKind::ParamDecl).expect("ParamDecl");
    let result = extract_type_from_container(&cast_site(&param_decl), None, &map);
    assert_eq!(result, SymbolType::Value(Ty::int()));
}

#[test]
fn extract_param_with_typespec() {
    let (parse, map) = parse_source("module m; parameter int W = 8; endmodule");
    let root = parse.syntax();
    let param_decl = find_in_module(&root, SyntaxKind::ParamDecl).expect("ParamDecl");
    let result = extract_type_from_container(&cast_site(&param_decl), None, &map);
    assert_eq!(result, SymbolType::Value(Ty::int()));
}

#[test]
fn extract_port_with_typespec() {
    let (parse, map) = parse_source("module m(input logic [7:0] a); endmodule");
    let root = parse.syntax();
    let port = find_first_port(&root);
    let result = extract_type_from_container(&cast_site(&port), None, &map);
    if let SymbolType::Value(Ty::Integral(i)) = &result {
        assert_eq!(i.keyword, IntegralKw::Logic);
        assert_eq!(i.packed.len(), 1);
    } else {
        panic!("expected Value(Integral), got {result:?}");
    }
}

#[test]
fn extract_port_no_typespec_defaults_logic() {
    let (parse, map) = parse_source("module m(input a); endmodule");
    let root = parse.syntax();
    let port = find_first_port(&root);
    let result = extract_type_from_container(&cast_site(&port), None, &map);
    assert_eq!(result, SymbolType::Value(Ty::simple_logic()));
}

#[test]
fn extract_typedef() {
    let (parse, map) = parse_source("module m; typedef logic [7:0] byte_t; endmodule");
    let root = parse.syntax();
    let td = find_in_module(&root, SyntaxKind::TypedefDecl).expect("TypedefDecl");
    let result = extract_type_from_container(&cast_site(&td), None, &map);
    if let SymbolType::TypeAlias(Ty::Integral(i)) = &result {
        assert_eq!(i.keyword, IntegralKw::Logic);
        assert_eq!(i.packed.len(), 1);
    } else {
        panic!("expected TypeAlias(Integral), got {result:?}");
    }
}

#[test]
fn normalize_resolves_unevaluated() {
    let (parse, map) = parse_source("module m; logic [7:0] x; endmodule");
    let root = parse.syntax();
    let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
    let decl = first_declarator(&var_decl);
    let raw = extract_type_from_container(&cast_site(&var_decl), decl.as_ref(), &map);

    let call_count = std::sync::atomic::AtomicU32::new(0);
    let eval = |_id: Site| -> ConstInt {
        let n = call_count.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        if n.is_multiple_of(2) {
            ConstInt::Known(7)
        } else {
            ConstInt::Known(0)
        }
    };
    let normalized = normalize_symbol_type(&raw, &eval);
    if let SymbolType::Value(Ty::Integral(i)) = &normalized {
        assert_eq!(i.packed[0].msb, ConstInt::Known(7));
        assert_eq!(i.packed[0].lsb, ConstInt::Known(0));
    } else {
        panic!("expected Value(Integral), got {normalized:?}");
    }
}

#[test]
fn type_name_ref_detects_user_type() {
    let (parse, _map) = parse_source("module m; byte_t x; endmodule");
    let root = parse.syntax();
    let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
    let typespec = find_typespec(&var_decl).expect("TypeSpec");
    let ts = TypeSpec::cast(typespec).expect("cast");
    assert!(ts.type_name_ref().is_some());
}

#[test]
fn type_name_ref_none_for_builtin() {
    let (parse, _map) = parse_source("module m; logic x; endmodule");
    let root = parse.syntax();
    let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
    let typespec = find_typespec(&var_decl).expect("TypeSpec");
    let ts = TypeSpec::cast(typespec).expect("cast");
    assert!(ts.type_name_ref().is_none());
}

#[test]
fn user_type_ref_dotted_name() {
    let (parse, _map) = parse_source("module m; my_bus.master v; endmodule");
    let root = parse.syntax();
    let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
    let typespec = find_typespec(&var_decl).expect("TypeSpec");

    let ts = TypeSpec::cast(typespec.clone()).expect("cast");
    let tnr = ts.type_name_ref().expect("type_name_ref");
    let dn = match tnr {
        TypeNameRef::Dotted(d) => d,
        other => panic!("expected Dotted, got {other:?}"),
    };
    let iface = dn.interface_ref().expect("interface_ref");
    assert_eq!(iface.ident().expect("ident").text(), "my_bus");
    assert_eq!(dn.modport_ident().expect("modport").text(), "master");

    let utr = user_type_ref(&typespec).expect("should extract UserTypeRef");
    match utr {
        UserTypeRef::InterfaceModport { modport_name, .. } => {
            assert_eq!(modport_name.as_str(), "master");
        }
        other => panic!("expected InterfaceModport, got {other:?}"),
    }
}

#[test]
fn user_type_ref_simple_name() {
    let (parse, _map) = parse_source("module m; byte_t x; endmodule");
    let root = parse.syntax();
    let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
    let typespec = find_typespec(&var_decl).expect("TypeSpec");
    let utr = user_type_ref(&typespec).expect("should extract UserTypeRef");
    assert!(matches!(utr, UserTypeRef::Simple(_)));
}

#[test]
fn user_type_ref_none_for_keyword() {
    let (parse, _map) = parse_source("module m; logic x; endmodule");
    let root = parse.syntax();
    let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
    let typespec = find_typespec(&var_decl).expect("TypeSpec");
    assert!(user_type_ref(&typespec).is_none());
}
