use lyra_ast::{AstNode, NameRef, QualifiedName};
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;
use smol_str::SmolStr;

use crate::builder::{DefContext, collect_declarators, collect_name_refs, is_expression_kind};
use crate::def_index::{ExpectedNs, NamePath, UseSite};
use crate::scopes::ScopeKind;
use crate::symbols::{Namespace, SymbolKind};

pub(crate) fn collect_procedural_block(
    ctx: &mut DefContext<'_>,
    node: &SyntaxNode,
    scope: crate::scopes::ScopeId,
) {
    for child in node.children() {
        collect_statement(ctx, &child, scope);
    }
}

pub(crate) fn collect_statement(
    ctx: &mut DefContext<'_>,
    node: &SyntaxNode,
    scope: crate::scopes::ScopeId,
) {
    match node.kind() {
        SyntaxKind::BlockStmt => {
            let block_scope = ctx.scopes.push(ScopeKind::Block, Some(scope));
            for child in node.children() {
                match child.kind() {
                    SyntaxKind::VarDecl => {
                        collect_declarators(ctx, &child, SymbolKind::Variable, block_scope);
                    }
                    _ => {
                        collect_statement(ctx, &child, block_scope);
                    }
                }
            }
        }
        SyntaxKind::VarDecl => {
            collect_declarators(ctx, node, SymbolKind::Variable, scope);
        }
        SyntaxKind::IfStmt
        | SyntaxKind::CaseStmt
        | SyntaxKind::CaseItem
        | SyntaxKind::ForStmt
        | SyntaxKind::WhileStmt
        | SyntaxKind::RepeatStmt
        | SyntaxKind::ForeverStmt
        | SyntaxKind::TimingControl => {
            for child in node.children() {
                collect_statement(ctx, &child, scope);
            }
            collect_direct_name_refs(ctx, node, scope);
        }
        SyntaxKind::AssignStmt | SyntaxKind::EventExpr | SyntaxKind::EventItem => {
            collect_name_refs(ctx, node, scope);
        }
        _ => {
            if is_expression_kind(node.kind()) {
                collect_name_refs(ctx, node, scope);
            } else {
                for child in node.children() {
                    collect_statement(ctx, &child, scope);
                }
            }
        }
    }
}

fn collect_direct_name_refs(
    ctx: &mut DefContext<'_>,
    node: &SyntaxNode,
    scope: crate::scopes::ScopeId,
) {
    for child in node.children() {
        if is_expression_kind(child.kind())
            || child.kind() == SyntaxKind::NameRef
            || child.kind() == SyntaxKind::QualifiedName
        {
            collect_name_refs_from_expr(ctx, &child, scope);
        }
    }
}

fn collect_name_refs_from_expr(
    ctx: &mut DefContext<'_>,
    node: &SyntaxNode,
    scope: crate::scopes::ScopeId,
) {
    if node.kind() == SyntaxKind::NameRef {
        if let Some(name_ref) = NameRef::cast(node.clone())
            && let Some(ident) = name_ref.ident()
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&name_ref)
        {
            ctx.use_sites.push(UseSite {
                path: NamePath::Simple(SmolStr::new(ident.text())),
                expected_ns: ExpectedNs::Exact(Namespace::Value),
                range: name_ref.text_range(),
                scope,
                ast_id: ast_id.erase(),
            });
        }
        return;
    }
    if node.kind() == SyntaxKind::QualifiedName {
        if let Some(qn) = QualifiedName::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&qn)
        {
            let segments: Box<[SmolStr]> = qn
                .segments()
                .map(|ident| SmolStr::new(ident.text()))
                .collect();
            if !segments.is_empty() {
                ctx.use_sites.push(UseSite {
                    path: NamePath::Qualified { segments },
                    expected_ns: ExpectedNs::Exact(Namespace::Value),
                    range: qn.text_range(),
                    scope,
                    ast_id: ast_id.erase(),
                });
            }
        }
        return;
    }
    for child in node.children() {
        collect_name_refs_from_expr(ctx, &child, scope);
    }
}
