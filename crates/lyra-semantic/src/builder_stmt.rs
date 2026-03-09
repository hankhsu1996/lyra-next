use lyra_ast::{
    AssignmentPatternItem, AstNode, ForStmt, ForeachStmt, HasSyntax, NameRef, QualifiedName,
    semantic_spelling,
};
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;

use crate::builder::{DeclaratorContext, DefContext, expect_typed, is_expression_kind};
use crate::builder_items::{collect_foreach_vars, collect_var_decl};
use crate::builder_types::{
    collect_assignment_pattern_item_refs, collect_name_refs, try_push_qualified_use_site,
};
use crate::def_index::{ExpectedNs, NamePath, UseSite};
use crate::scopes::ScopeKind;
use crate::symbols::Namespace;

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
    // Blocks inherit surrounding procedural lifetime context; no independent
    // block lifetime declaration is modeled.
    match node.kind() {
        SyntaxKind::BlockStmt => {
            let block_scope = ctx.scopes.push(ScopeKind::Block, Some(scope));
            for child in node.children() {
                match child.kind() {
                    SyntaxKind::VarDecl => {
                        let vd = expect_typed::<lyra_ast::VarDecl>(&child);
                        collect_var_decl(ctx, &vd, block_scope, DeclaratorContext::ProceduralLocal);
                    }
                    _ => {
                        collect_statement(ctx, &child, block_scope);
                    }
                }
            }
        }
        SyntaxKind::VarDecl => {
            let vd = expect_typed::<lyra_ast::VarDecl>(node);
            collect_var_decl(ctx, &vd, scope, DeclaratorContext::ProceduralLocal);
        }
        SyntaxKind::ForeachStmt => {
            // Name refs in the array expression resolve in the parent scope.
            collect_direct_name_refs(ctx, node, scope);
            // Create foreach body scope + define foreach vars in the index.
            let fs = expect_typed::<ForeachStmt>(node);
            let foreach_scope = collect_foreach_vars(ctx, &fs, scope);
            if let Some(body) = fs.body() {
                collect_statement(ctx, body.syntax(), foreach_scope);
            }
        }
        SyntaxKind::ForStmt => {
            let for_stmt = expect_typed::<ForStmt>(node);
            collect_for_stmt(ctx, &for_stmt, scope);
        }
        SyntaxKind::IfStmt
        | SyntaxKind::CaseStmt
        | SyntaxKind::CaseItem
        | SyntaxKind::CaseInsideItem
        | SyntaxKind::WhileStmt
        | SyntaxKind::RepeatStmt
        | SyntaxKind::ForeverStmt
        | SyntaxKind::DoWhileStmt
        | SyntaxKind::TimingControl => {
            for child in node.children() {
                collect_statement(ctx, &child, scope);
            }
            collect_direct_name_refs(ctx, node, scope);
        }
        SyntaxKind::ReturnStmt
        | SyntaxKind::AssignStmt
        | SyntaxKind::EventExpr
        | SyntaxKind::EventItem => {
            collect_name_refs(ctx, node, scope, None);
        }
        _ => {
            if is_expression_kind(node.kind()) {
                collect_name_refs(ctx, node, scope, None);
            } else {
                for child in node.children() {
                    collect_statement(ctx, &child, scope);
                }
            }
        }
    }
}

fn collect_for_stmt(ctx: &mut DefContext<'_>, for_stmt: &ForStmt, scope: crate::scopes::ScopeId) {
    // Init: variable declaration with forced automatic lifetime (LRM 6.21)
    if let Some(var_decl) = for_stmt.init_var_decl() {
        collect_var_decl(ctx, &var_decl, scope, DeclaratorContext::ForInit);
    }
    // Init/condition/step expressions: collect name refs
    collect_direct_name_refs(ctx, for_stmt.syntax(), scope);
    // Body
    if let Some(body) = for_stmt.body() {
        collect_statement(ctx, body.syntax(), scope);
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
                path: NamePath::Simple(semantic_spelling(&ident)),
                expected_ns: ExpectedNs::Exact(Namespace::Value),
                scope,
                name_ref_site: ast_id.erase(),
                order_key: 0,
                implicit_net_site: None,
            });
        }
        return;
    }
    if node.kind() == SyntaxKind::QualifiedName {
        if let Some(qn) = QualifiedName::cast(node.clone()) {
            try_push_qualified_use_site(ctx, &qn, scope, ExpectedNs::Exact(Namespace::Value));
        }
        return;
    }
    for child in node.children() {
        if child.kind() == SyntaxKind::AssignmentPatternItem {
            if let Some(item) = AssignmentPatternItem::cast(child.clone()) {
                collect_assignment_pattern_item_refs(&item, &child, |node| {
                    collect_name_refs_from_expr(ctx, node, scope);
                });
            } else {
                collect_name_refs_from_expr(ctx, &child, scope);
            }
        } else {
            collect_name_refs_from_expr(ctx, &child, scope);
        }
    }
}
