use lyra_ast::{AstNode, Expr};
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;

fn parse_expr(src: &str) -> SyntaxNode {
    let full = format!("module m; parameter P = {src}; endmodule");
    let tokens = lyra_lexer::lex(&full);
    let pp = lyra_preprocess::preprocess_identity(lyra_source::FileId(0), &tokens, &full);
    let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
    find_init_expr(&parse.syntax()).expect("should find init expression")
}

fn find_init_expr(node: &SyntaxNode) -> Option<SyntaxNode> {
    if node.kind() == SyntaxKind::Declarator {
        return node
            .children()
            .find(|c| lyra_ast::is_expression_kind(c.kind()));
    }
    for child in node.children() {
        if let Some(found) = find_init_expr(&child) {
            return Some(found);
        }
    }
    None
}

fn parse_stmt(src: &str) -> SyntaxNode {
    let full = format!("module m; initial begin {src} end endmodule");
    let tokens = lyra_lexer::lex(&full);
    let pp = lyra_preprocess::preprocess_identity(lyra_source::FileId(0), &tokens, &full);
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

#[test]
fn bin_expr_lhs_rhs_op() {
    let node = parse_expr("a + b * c");
    let bin = lyra_ast::BinExpr::cast(node).expect("should be BinExpr");
    assert!(bin.lhs().is_some());
    assert!(bin.rhs().is_some());
    assert_eq!(bin.binary_op(), Some(lyra_ast::SyntaxBinaryOp::Add));
}

#[test]
fn prefix_expr_operand() {
    let node = parse_expr("~x");
    let pfx = lyra_ast::PrefixExpr::cast(node).expect("should be PrefixExpr");
    let op = pfx.op_token().expect("should have op");
    assert_eq!(op.kind(), SyntaxKind::Tilde);
    let operand = pfx.operand().expect("should have operand");
    assert_eq!(operand.kind(), SyntaxKind::NameRef);
}

#[test]
fn cond_expr_parts() {
    let node = parse_expr("a ? b : c");
    let cond = lyra_ast::CondExpr::cast(node).expect("should be CondExpr");
    assert!(cond.condition().is_some());
    assert!(cond.then_expr().is_some());
    assert!(cond.else_expr().is_some());
}

#[test]
fn concat_expr_operands() {
    let node = parse_expr("{a, b, c}");
    let concat = lyra_ast::ConcatExpr::cast(node).expect("should be ConcatExpr");
    assert_eq!(concat.operands().count(), 3);
}

#[test]
fn replic_expr_count_body() {
    let node = parse_expr("{3{a, b}}");
    let replic = lyra_ast::ReplicExpr::cast(node).expect("should be ReplicExpr");
    let count = replic.count().expect("should have count");
    assert_eq!(count.kind(), SyntaxKind::Literal);
}

#[test]
fn call_expr_callee() {
    let root = parse_stmt("foo(x);");
    let call = find_kind(&root, SyntaxKind::CallExpr).expect("should find CallExpr");
    let ce = lyra_ast::CallExpr::cast(call).expect("cast");
    let callee = ce.callee().expect("should have callee");
    assert_eq!(callee.kind(), SyntaxKind::NameRef);
    assert!(ce.arg_list().is_some());
}

#[test]
fn system_tf_call_name() {
    let root = parse_stmt("$clog2(x);");
    let call = find_kind(&root, SyntaxKind::SystemTfCall).expect("should find SystemTfCall");
    let stf = lyra_ast::SystemTfCall::cast(call).expect("cast");
    let name = stf.system_name().expect("should have name");
    assert_eq!(name.text(), "$clog2");
    let al = stf.arg_list().expect("should have arg_list");
    assert_eq!(al.args().count(), 1);
}

#[test]
fn system_tf_arg_list_type_arg() {
    let full = "module m; parameter P = $bits(logic [7:0]); endmodule";
    let tokens = lyra_lexer::lex(full);
    let pp = lyra_preprocess::preprocess_identity(lyra_source::FileId(0), &tokens, full);
    let root = lyra_parser::parse(&pp.tokens, &pp.expanded_text).syntax();
    let call = find_kind(&root, SyntaxKind::SystemTfCall).expect("SystemTfCall");
    let stf = lyra_ast::SystemTfCall::cast(call).expect("cast");
    let al = stf.arg_list().expect("arg_list");
    let args: Vec<_> = al.args().collect();
    assert_eq!(args.len(), 1);
    assert!(matches!(args[0], lyra_ast::TfArg::Type(_)));
}

#[test]
fn system_tf_arg_list_expr_arg() {
    let full = "module m; parameter P = $bits(a[3:0]); endmodule";
    let tokens = lyra_lexer::lex(full);
    let pp = lyra_preprocess::preprocess_identity(lyra_source::FileId(0), &tokens, full);
    let root = lyra_parser::parse(&pp.tokens, &pp.expanded_text).syntax();
    let call = find_kind(&root, SyntaxKind::SystemTfCall).expect("SystemTfCall");
    let stf = lyra_ast::SystemTfCall::cast(call).expect("cast");
    let al = stf.arg_list().expect("arg_list");
    let args: Vec<_> = al.args().collect();
    assert_eq!(args.len(), 1);
    assert!(matches!(args[0], lyra_ast::TfArg::Expr(_)));
}

#[test]
fn assign_stmt_blocking() {
    let root = parse_stmt("x = y;");
    let assign = find_kind(&root, SyntaxKind::AssignStmt).expect("AssignStmt");
    let a = lyra_ast::AssignStmt::cast(assign).expect("cast");
    assert_eq!(a.assign_op(), Some(lyra_ast::SyntaxAssignOp::Blocking));
    assert!(!a.assign_op().expect("op").is_compound());
    assert!(a.lhs().is_some());
    assert!(a.rhs().is_some());
}

#[test]
fn assign_stmt_compound() {
    let root = parse_stmt("x += 1;");
    let assign = find_kind(&root, SyntaxKind::AssignStmt).expect("AssignStmt");
    let a = lyra_ast::AssignStmt::cast(assign).expect("cast");
    assert_eq!(a.assign_op(), Some(lyra_ast::SyntaxAssignOp::AddAssign));
    assert!(a.assign_op().expect("op").is_compound());
}

#[test]
fn assign_stmt_nonblocking() {
    let root = parse_stmt("x <= y;");
    let assign = find_kind(&root, SyntaxKind::AssignStmt).expect("AssignStmt");
    let a = lyra_ast::AssignStmt::cast(assign).expect("cast");
    assert_eq!(a.assign_op(), Some(lyra_ast::SyntaxAssignOp::NonBlocking));
}

#[test]
fn continuous_assign_lhs_rhs() {
    let full = "module m; assign a = b; endmodule";
    let tokens = lyra_lexer::lex(full);
    let pp = lyra_preprocess::preprocess_identity(lyra_source::FileId(0), &tokens, full);
    let root = lyra_parser::parse(&pp.tokens, &pp.expanded_text).syntax();
    let ca = find_kind(&root, SyntaxKind::ContinuousAssign).expect("ContinuousAssign");
    let ca = lyra_ast::ContinuousAssign::cast(ca).expect("cast");
    assert!(ca.lhs().is_some());
    assert!(ca.rhs().is_some());
}

#[test]
fn declarator_init_expr() {
    let full = "module m; logic x = 5; endmodule";
    let tokens = lyra_lexer::lex(full);
    let pp = lyra_preprocess::preprocess_identity(lyra_source::FileId(0), &tokens, full);
    let root = lyra_parser::parse(&pp.tokens, &pp.expanded_text).syntax();
    let decl = find_kind(&root, SyntaxKind::Declarator).expect("Declarator");
    let d = lyra_ast::Declarator::cast(decl).expect("cast");
    let init = d.init_expr().expect("should have init");
    assert_eq!(init.kind(), SyntaxKind::Literal);
}

#[test]
fn literal_kind_based() {
    let node = parse_expr("8'hFF");
    let lit = lyra_ast::Literal::cast(node).expect("Literal");
    let lk = lit.literal_kind().expect("should classify");
    match lk {
        lyra_ast::LiteralKind::Based {
            size_token,
            base_token,
        } => {
            assert!(size_token.is_some());
            assert_eq!(size_token.expect("size").text(), "8");
            assert_eq!(base_token.text(), "'hFF");
        }
        other => panic!("expected Based, got {other:?}"),
    }
}

#[test]
fn literal_kind_int() {
    let node = parse_expr("42");
    let lit = lyra_ast::Literal::cast(node).expect("Literal");
    let lk = lit.literal_kind().expect("should classify");
    assert!(matches!(lk, lyra_ast::LiteralKind::Int { .. }));
}

#[test]
fn expr_peel_parens() {
    let node = parse_expr("((x))");
    let expr = Expr::cast(node).expect("should cast");
    let peeled = expr.peeled().expect("should peel");
    assert_eq!(peeled.kind(), SyntaxKind::NameRef);
}

#[test]
fn expr_peel_expression_wrapper() {
    let src = "module m; typedef enum { A = 42 } e; endmodule";
    let tokens = lyra_lexer::lex(src);
    let pp = lyra_preprocess::preprocess_identity(lyra_source::FileId(0), &tokens, src);
    let root = lyra_parser::parse(&pp.tokens, &pp.expanded_text).syntax();
    let expr_node = find_expression_wrapper(&root).expect("should find Expression wrapper");
    let expr = Expr::cast(expr_node).expect("should cast");
    let peeled = expr.peeled().expect("should peel");
    assert_eq!(peeled.kind(), SyntaxKind::Literal);
}

fn find_expression_wrapper(node: &SyntaxNode) -> Option<SyntaxNode> {
    node.descendants()
        .find(|n| n.kind() == SyntaxKind::Expression)
}
