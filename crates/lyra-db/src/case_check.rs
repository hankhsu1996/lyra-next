// Case statement legality checks (LRM 12.5.4).
//
// Validates that `inside` is only used with plain `case`, not
// `casex` or `casez`. Items carry `TokenSpan` anchors so lowering
// is a trivial span mapping.

use lyra_ast::{
    AstNode, BlockStmt, CaseKind, CaseStmt, DoWhileStmt, ForStmt, ForeachStmt, ForeverStmt,
    FunctionDecl, HasSyntax, IfStmt, RepeatStmt, SourceFile, StmtNode, TaskDecl, TimingControl,
    WhileStmt,
};
use lyra_parser::SyntaxNode;
use lyra_semantic::case_check::{CaseCheckIndex, CaseCheckItem};
use lyra_source::TokenSpan;

use crate::pipeline::parse_file;

/// Salsa-cached case-check index for a file.
#[salsa::tracked(return_ref)]
pub fn case_check_index(db: &dyn salsa::Database, file: crate::SourceFile) -> CaseCheckIndex {
    let parse = parse_file(db, file);
    build_case_check_index(&parse.syntax())
}

fn build_case_check_index(root: &SyntaxNode) -> CaseCheckIndex {
    let mut items = Vec::new();
    let Some(sf) = SourceFile::cast(root.clone()) else {
        return CaseCheckIndex {
            items: Box::new([]),
        };
    };
    for module in sf.modules() {
        if let Some(body) = module.body() {
            walk_module_body(&body, &mut items);
        }
    }
    CaseCheckIndex {
        items: items.into_boxed_slice(),
    }
}

fn walk_module_body(body: &lyra_ast::ModuleBody, items: &mut Vec<CaseCheckItem>) {
    for always in body.always_blocks() {
        if let Some(stmt) = always.body() {
            walk_stmt(&stmt, items);
        }
    }
    for initial in body.initial_blocks() {
        if let Some(stmt) = initial.body() {
            walk_stmt(&stmt, items);
        }
    }
    for func in body.function_decls() {
        walk_function(&func, items);
    }
    for task in body.task_decls() {
        walk_task(&task, items);
    }
    for gi in body.generate_items() {
        walk_generate_item(&gi, items);
    }
}

fn walk_function(func: &FunctionDecl, items: &mut Vec<CaseCheckItem>) {
    for stmt in func.statements() {
        walk_stmt(&stmt, items);
    }
}

fn walk_task(task: &TaskDecl, items: &mut Vec<CaseCheckItem>) {
    for stmt in task.statements() {
        walk_stmt(&stmt, items);
    }
}

fn walk_generate_item(item: &lyra_ast::GenerateItem, items: &mut Vec<CaseCheckItem>) {
    match item {
        lyra_ast::GenerateItem::ModuleInstantiation(_) => {}
        lyra_ast::GenerateItem::IfStmt(if_s) => {
            if let Some(t) = if_s.then_body() {
                walk_stmt(&t, items);
            }
            if let Some(e) = if_s.else_body() {
                walk_stmt(&e, items);
            }
        }
        lyra_ast::GenerateItem::ForStmt(for_s) => {
            if let Some(b) = for_s.body() {
                walk_stmt(&b, items);
            }
        }
        lyra_ast::GenerateItem::CaseStmt(case) => {
            check_case_stmt(case, items);
            for ci in case.items() {
                let body = match &ci {
                    lyra_ast::CaseItemLike::Normal(n) => n.body(),
                    lyra_ast::CaseItemLike::Inside(i) => i.body(),
                };
                if let Some(b) = body {
                    walk_stmt(&b, items);
                }
            }
        }
        lyra_ast::GenerateItem::GenerateRegion(region) => {
            for gi in region.generate_items() {
                walk_generate_item(&gi, items);
            }
        }
        lyra_ast::GenerateItem::BlockStmt(block) => {
            for s in block.statements() {
                walk_stmt(&s, items);
            }
        }
    }
}

fn walk_stmt(stmt: &StmtNode, items: &mut Vec<CaseCheckItem>) {
    let node = stmt.syntax();
    if let Some(block) = BlockStmt::cast(node.clone()) {
        for s in block.statements() {
            walk_stmt(&s, items);
        }
    } else if let Some(if_s) = IfStmt::cast(node.clone()) {
        if let Some(t) = if_s.then_body() {
            walk_stmt(&t, items);
        }
        if let Some(e) = if_s.else_body() {
            walk_stmt(&e, items);
        }
    } else if let Some(case) = CaseStmt::cast(node.clone()) {
        check_case_stmt(&case, items);
        for ci in case.items() {
            let body = match &ci {
                lyra_ast::CaseItemLike::Normal(n) => n.body(),
                lyra_ast::CaseItemLike::Inside(i) => i.body(),
            };
            if let Some(b) = body {
                walk_stmt(&b, items);
            }
        }
    } else if let Some(for_s) = ForStmt::cast(node.clone()) {
        if let Some(b) = for_s.body() {
            walk_stmt(&b, items);
        }
    } else if let Some(while_s) = WhileStmt::cast(node.clone()) {
        if let Some(b) = while_s.body() {
            walk_stmt(&b, items);
        }
    } else if let Some(repeat_s) = RepeatStmt::cast(node.clone()) {
        if let Some(b) = repeat_s.body() {
            walk_stmt(&b, items);
        }
    } else if let Some(forever_s) = ForeverStmt::cast(node.clone()) {
        if let Some(b) = forever_s.body() {
            walk_stmt(&b, items);
        }
    } else if let Some(do_while) = DoWhileStmt::cast(node.clone()) {
        if let Some(b) = do_while.body() {
            walk_stmt(&b, items);
        }
    } else if let Some(foreach_s) = ForeachStmt::cast(node.clone()) {
        if let Some(b) = foreach_s.body() {
            walk_stmt(&b, items);
        }
    } else if let Some(tc) = TimingControl::cast(node.clone()) {
        if let Some(b) = tc.body() {
            walk_stmt(&b, items);
        }
    } else if let Some(func) = FunctionDecl::cast(node.clone()) {
        walk_function(&func, items);
    } else if let Some(task) = TaskDecl::cast(node.clone()) {
        walk_task(&task, items);
    }
}

fn check_case_stmt(case: &CaseStmt, items: &mut Vec<CaseCheckItem>) {
    if !case.is_inside() {
        return;
    }
    let Some(kind) = case.case_kind() else {
        return;
    };
    if matches!(kind, CaseKind::CaseX | CaseKind::CaseZ) {
        let kw_span = case
            .case_keyword_token()
            .map_or(TokenSpan::INVALID, |t| TokenSpan::new(t.text_range()));
        items.push(CaseCheckItem::IllegalInsideCaseKind { kind, kw_span });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lyra_semantic::case_check::CaseCheckItem;
    use lyra_source::FileId;

    fn check(src: &str) -> CaseCheckIndex {
        let file = FileId(0);
        let tokens = lyra_lexer::lex(src);
        let pp = lyra_preprocess::preprocess_identity(file, &tokens, src);
        let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
        build_case_check_index(&parse.syntax())
    }

    #[test]
    fn case_inside_ok() {
        let idx = check("module m; initial begin case (x) inside 1: ; endcase end endmodule");
        assert!(idx.items.is_empty());
    }

    #[test]
    fn casex_inside_illegal() {
        let idx = check("module m; initial begin casex (x) inside 1: ; endcase end endmodule");
        assert_eq!(idx.items.len(), 1);
        assert!(matches!(
            idx.items[0],
            CaseCheckItem::IllegalInsideCaseKind {
                kind: CaseKind::CaseX,
                ..
            }
        ));
    }

    #[test]
    fn casez_inside_illegal() {
        let idx = check("module m; initial begin casez (x) inside 1: ; endcase end endmodule");
        assert_eq!(idx.items.len(), 1);
        assert!(matches!(
            idx.items[0],
            CaseCheckItem::IllegalInsideCaseKind {
                kind: CaseKind::CaseZ,
                ..
            }
        ));
    }

    #[test]
    fn plain_casex_no_inside_ok() {
        let idx = check("module m; initial begin casex (x) 1: ; endcase end endmodule");
        assert!(idx.items.is_empty());
    }

    #[test]
    fn nested_case_inside_in_function() {
        let idx = check(
            "module m; function void foo(); casex (x) inside 1: ; endcase endfunction endmodule",
        );
        assert_eq!(idx.items.len(), 1);
    }
}
