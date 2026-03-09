// Jump statement legality checks (LRM 12.8).
//
// Validates that `break`/`continue` appear only inside loops and that
// `return` appears only inside functions/tasks with correct value form.
//
// Items carry `TokenSpan` anchors (keyword token ranges) so lowering
// is a trivial span mapping with no CST reconstruction needed.

use lyra_ast::{
    AstIdMap, AstNode, BlockStmt, BreakStmt, CaseStmt, ContinueStmt, DoWhileStmt, ForStmt,
    ForeachStmt, ForeverStmt, FunctionDecl, HasSyntax, IfStmt, RepeatStmt, ReturnStmt, SourceFile,
    StmtNode, TaskDecl, TimingControl, WhileStmt,
};
use lyra_parser::SyntaxNode;
use lyra_source::TokenSpan;

use crate::pipeline::{ast_id_map, parse_file};

/// A single jump-legality finding.
///
/// Each item anchors a jump statement by its keyword `TokenSpan`.
/// Lowering maps the span directly through the `SourceMap`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JumpCheckItem {
    BreakOutsideLoop { kw: TokenSpan },
    ContinueOutsideLoop { kw: TokenSpan },
    ReturnOutsideCallable { kw: TokenSpan },
    ReturnValueInVoid { kw: TokenSpan },
    ReturnMissingValue { kw: TokenSpan },
}

impl JumpCheckItem {
    pub fn kw(&self) -> TokenSpan {
        match self {
            Self::BreakOutsideLoop { kw }
            | Self::ContinueOutsideLoop { kw }
            | Self::ReturnOutsideCallable { kw }
            | Self::ReturnValueInVoid { kw }
            | Self::ReturnMissingValue { kw } => *kw,
        }
    }
}

/// Per-file jump-legality product.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JumpCheckIndex {
    pub items: Box<[JumpCheckItem]>,
}

/// Salsa-cached jump-check index for a file.
#[salsa::tracked(return_ref)]
pub fn jump_check_index(db: &dyn salsa::Database, file: crate::SourceFile) -> JumpCheckIndex {
    let parse = parse_file(db, file);
    let id_map = ast_id_map(db, file);
    build_jump_check_index(&parse.syntax(), id_map)
}

fn build_jump_check_index(root: &SyntaxNode, id_map: &AstIdMap) -> JumpCheckIndex {
    let mut ctx = JumpCtx {
        loop_depth: 0,
        callable: None,
        id_map,
        items: Vec::new(),
    };
    let Some(sf) = SourceFile::cast(root.clone()) else {
        return JumpCheckIndex {
            items: Box::new([]),
        };
    };
    for module in sf.modules() {
        if let Some(body) = module.body() {
            ctx.walk_module_body(&body);
        }
    }
    JumpCheckIndex {
        items: ctx.items.into_boxed_slice(),
    }
}

/// Extract a `TokenSpan` from a typed keyword accessor result.
fn token_span(tok: Option<lyra_parser::SyntaxToken>) -> TokenSpan {
    tok.map_or(TokenSpan::INVALID, |t| TokenSpan::new(t.text_range()))
}

struct CallableInfo {
    is_void: bool,
}

struct JumpCtx<'a> {
    loop_depth: u32,
    callable: Option<CallableInfo>,
    id_map: &'a AstIdMap,
    items: Vec<JumpCheckItem>,
}

impl JumpCtx<'_> {
    fn walk_module_body(&mut self, body: &lyra_ast::ModuleBody) {
        for always in body.always_blocks() {
            if let Some(stmt) = always.body() {
                self.walk_stmt(&stmt);
            }
        }
        for initial in body.initial_blocks() {
            if let Some(stmt) = initial.body() {
                self.walk_stmt(&stmt);
            }
        }
        for func in body.function_decls() {
            self.walk_function(&func);
        }
        for task in body.task_decls() {
            self.walk_task(&task);
        }
        for gi in body.generate_items() {
            self.walk_generate_item(&gi);
        }
    }

    fn walk_generate_item(&mut self, item: &lyra_ast::GenerateItem) {
        match item {
            lyra_ast::GenerateItem::ModuleInstantiation(_) => {}
            lyra_ast::GenerateItem::IfStmt(if_s) => {
                if let Some(t) = if_s.then_body() {
                    self.walk_stmt(&t);
                }
                if let Some(e) = if_s.else_body() {
                    self.walk_stmt(&e);
                }
            }
            lyra_ast::GenerateItem::ForStmt(for_s) => {
                if let Some(b) = for_s.body() {
                    self.walk_stmt(&b);
                }
            }
            lyra_ast::GenerateItem::CaseStmt(case) => {
                for ci in case.items() {
                    let body = match &ci {
                        lyra_ast::CaseItemLike::Normal(n) => n.body(),
                        lyra_ast::CaseItemLike::Inside(i) => i.body(),
                        lyra_ast::CaseItemLike::Pattern(p) => p.body(),
                    };
                    if let Some(b) = body {
                        self.walk_stmt(&b);
                    }
                }
            }
            lyra_ast::GenerateItem::GenerateRegion(region) => {
                for gi in region.generate_items() {
                    self.walk_generate_item(&gi);
                }
            }
            lyra_ast::GenerateItem::BlockStmt(block) => {
                for s in block.statements() {
                    self.walk_stmt(&s);
                }
            }
        }
    }

    fn walk_function(&mut self, func: &FunctionDecl) {
        let prev = self.callable.take();
        let prev_depth = self.loop_depth;
        self.loop_depth = 0;
        let is_void = func.type_spec().is_some_and(|ts| {
            lyra_semantic::extract_base_ty_from_typespec(&ts, self.id_map)
                == lyra_semantic::types::Ty::Void
        });
        self.callable = Some(CallableInfo { is_void });
        for stmt in func.statements() {
            self.walk_stmt(&stmt);
        }
        self.callable = prev;
        self.loop_depth = prev_depth;
    }

    fn walk_task(&mut self, task: &TaskDecl) {
        let prev = self.callable.take();
        let prev_depth = self.loop_depth;
        self.loop_depth = 0;
        self.callable = Some(CallableInfo { is_void: true });
        for stmt in task.statements() {
            self.walk_stmt(&stmt);
        }
        self.callable = prev;
        self.loop_depth = prev_depth;
    }

    fn walk_stmt(&mut self, stmt: &StmtNode) {
        let node = stmt.syntax();
        if let Some(block) = BlockStmt::cast(node.clone()) {
            for s in block.statements() {
                self.walk_stmt(&s);
            }
        } else if let Some(if_s) = IfStmt::cast(node.clone()) {
            if let Some(t) = if_s.then_body() {
                self.walk_stmt(&t);
            }
            if let Some(e) = if_s.else_body() {
                self.walk_stmt(&e);
            }
        } else if let Some(case) = CaseStmt::cast(node.clone()) {
            for item in case.items() {
                let body = match &item {
                    lyra_ast::CaseItemLike::Normal(n) => n.body(),
                    lyra_ast::CaseItemLike::Inside(i) => i.body(),
                    lyra_ast::CaseItemLike::Pattern(p) => p.body(),
                };
                if let Some(b) = body {
                    self.walk_stmt(&b);
                }
            }
        } else if let Some(for_s) = ForStmt::cast(node.clone()) {
            self.walk_loop_body(for_s.body());
        } else if let Some(while_s) = WhileStmt::cast(node.clone()) {
            self.walk_loop_body(while_s.body());
        } else if let Some(repeat_s) = RepeatStmt::cast(node.clone()) {
            self.walk_loop_body(repeat_s.body());
        } else if let Some(forever_s) = ForeverStmt::cast(node.clone()) {
            self.walk_loop_body(forever_s.body());
        } else if let Some(do_while) = DoWhileStmt::cast(node.clone()) {
            self.walk_loop_body(do_while.body());
        } else if let Some(foreach_s) = ForeachStmt::cast(node.clone()) {
            self.walk_loop_body(foreach_s.body());
        } else if let Some(tc) = TimingControl::cast(node.clone()) {
            if let Some(b) = tc.body() {
                self.walk_stmt(&b);
            }
        } else if let Some(brk) = BreakStmt::cast(node.clone()) {
            self.check_break(&brk);
        } else if let Some(cont) = ContinueStmt::cast(node.clone()) {
            self.check_continue(&cont);
        } else if let Some(ret) = ReturnStmt::cast(node.clone()) {
            self.check_return(&ret);
        } else if let Some(func) = FunctionDecl::cast(node.clone()) {
            self.walk_function(&func);
        } else if let Some(task) = TaskDecl::cast(node.clone()) {
            self.walk_task(&task);
        }
    }

    fn walk_loop_body(&mut self, body: Option<StmtNode>) {
        self.loop_depth += 1;
        if let Some(b) = body {
            self.walk_stmt(&b);
        }
        self.loop_depth -= 1;
    }

    fn check_break(&mut self, brk: &BreakStmt) {
        if self.loop_depth == 0 {
            let kw = token_span(brk.break_kw());
            self.items.push(JumpCheckItem::BreakOutsideLoop { kw });
        }
    }

    fn check_continue(&mut self, cont: &ContinueStmt) {
        if self.loop_depth == 0 {
            let kw = token_span(cont.continue_kw());
            self.items.push(JumpCheckItem::ContinueOutsideLoop { kw });
        }
    }

    fn check_return(&mut self, ret: &ReturnStmt) {
        let kw = token_span(ret.return_kw());
        match &self.callable {
            None => {
                self.items.push(JumpCheckItem::ReturnOutsideCallable { kw });
            }
            Some(info) => {
                let has_value = ret.value().is_some();
                if info.is_void && has_value {
                    self.items.push(JumpCheckItem::ReturnValueInVoid { kw });
                } else if !info.is_void && !has_value {
                    self.items.push(JumpCheckItem::ReturnMissingValue { kw });
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lyra_source::FileId;

    fn check(src: &str) -> JumpCheckIndex {
        let file = FileId(0);
        let tokens = lyra_lexer::lex(src);
        let pp = lyra_preprocess::preprocess_identity(file, &tokens, src);
        let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
        let map = AstIdMap::from_root(file, &parse.syntax());
        build_jump_check_index(&parse.syntax(), &map)
    }

    #[test]
    fn break_inside_loop_ok() {
        let idx = check("module m; initial begin while (1) break; end endmodule");
        assert!(idx.items.is_empty());
    }

    #[test]
    fn break_outside_loop() {
        let idx = check("module m; initial begin break; end endmodule");
        assert_eq!(idx.items.len(), 1);
        assert!(matches!(
            idx.items[0],
            JumpCheckItem::BreakOutsideLoop { .. }
        ));
    }

    #[test]
    fn continue_outside_loop() {
        let idx = check("module m; initial begin continue; end endmodule");
        assert_eq!(idx.items.len(), 1);
        assert!(matches!(
            idx.items[0],
            JumpCheckItem::ContinueOutsideLoop { .. }
        ));
    }

    #[test]
    fn return_outside_callable() {
        let idx = check("module m; initial begin return; end endmodule");
        assert_eq!(idx.items.len(), 1);
        assert!(matches!(
            idx.items[0],
            JumpCheckItem::ReturnOutsideCallable { .. }
        ));
    }

    #[test]
    fn return_value_in_void_function() {
        let idx = check("module m; function void foo(); return 1; endfunction endmodule");
        assert_eq!(idx.items.len(), 1);
        assert!(matches!(
            idx.items[0],
            JumpCheckItem::ReturnValueInVoid { .. }
        ));
    }

    #[test]
    fn return_value_in_task() {
        let idx = check("module m; task foo(); return 1; endtask endmodule");
        assert_eq!(idx.items.len(), 1);
        assert!(matches!(
            idx.items[0],
            JumpCheckItem::ReturnValueInVoid { .. }
        ));
    }

    #[test]
    fn return_missing_value_in_nonvoid() {
        let idx = check("module m; function int foo(); return; endfunction endmodule");
        assert_eq!(idx.items.len(), 1);
        assert!(matches!(
            idx.items[0],
            JumpCheckItem::ReturnMissingValue { .. }
        ));
    }

    #[test]
    fn return_ok_in_void_function() {
        let idx = check("module m; function void foo(); return; endfunction endmodule");
        assert!(idx.items.is_empty());
    }

    #[test]
    fn return_ok_in_nonvoid_function() {
        let idx = check("module m; function int foo(); return 42; endfunction endmodule");
        assert!(idx.items.is_empty());
    }

    #[test]
    fn break_in_nested_loop_ok() {
        let idx = check(
            "module m; initial begin \
             for (int i = 0; i < 10; i = i + 1) begin \
               while (1) break; \
             end \
             end endmodule",
        );
        assert!(idx.items.is_empty());
    }

    #[test]
    fn do_while_counts_as_loop() {
        let idx = check("module m; initial begin do break; while (1); end endmodule");
        assert!(idx.items.is_empty());
    }

    #[test]
    fn foreach_counts_as_loop() {
        let idx = check("module m; initial begin int a[10]; foreach (a[i]) break; end endmodule");
        assert!(idx.items.is_empty());
    }
}
