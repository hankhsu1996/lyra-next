// Declaration legality checks (LRM 6.21).
//
// Validates that explicit `automatic` lifetime qualifiers appear only
// in procedural declaration contexts. Non-procedural contexts include
// module, package, interface, and program bodies.
//
// Items carry `TokenSpan` anchors (keyword token ranges) so lowering
// is a trivial span mapping with no CST reconstruction needed.

use lyra_ast::{
    AstNode, BlockStmt, DeclLifetimeSyntax, FunctionDecl, HasSyntax, SourceFile, StmtNode,
    TaskDecl, VarDecl,
};
use lyra_parser::SyntaxNode;
use lyra_source::TokenSpan;

use crate::pipeline::parse_file;

/// A single declaration-legality finding.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeclCheckItem {
    AutomaticVarNonProcedural { kw: TokenSpan },
}

/// Per-file declaration-legality product.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclCheckIndex {
    pub items: Box<[DeclCheckItem]>,
}

/// Salsa-cached declaration-check index for a file.
#[salsa::tracked(return_ref)]
pub fn decl_check_index(db: &dyn salsa::Database, file: crate::SourceFile) -> DeclCheckIndex {
    let parse = parse_file(db, file);
    build_decl_check_index(&parse.syntax())
}

fn build_decl_check_index(root: &SyntaxNode) -> DeclCheckIndex {
    let mut ctx = DeclCtx {
        procedural: false,
        items: Vec::new(),
    };
    let Some(sf) = SourceFile::cast(root.clone()) else {
        return DeclCheckIndex {
            items: Box::new([]),
        };
    };
    for module in sf.modules() {
        if let Some(body) = module.body() {
            ctx.walk_module_body(&body);
        }
    }
    for pkg in sf.packages() {
        if let Some(body) = pkg.body() {
            ctx.walk_non_procedural_items(
                body.var_decls(),
                body.function_decls(),
                body.task_decls(),
            );
        }
    }
    for iface in sf.interfaces() {
        if let Some(body) = iface.body() {
            ctx.walk_non_procedural_items(
                body.var_decls(),
                body.function_decls(),
                body.task_decls(),
            );
        }
    }
    for prog in sf.programs() {
        if let Some(body) = prog.body() {
            ctx.walk_non_procedural_items(
                body.var_decls(),
                body.function_decls(),
                body.task_decls(),
            );
        }
    }
    DeclCheckIndex {
        items: ctx.items.into_boxed_slice(),
    }
}

struct DeclCtx {
    procedural: bool,
    items: Vec<DeclCheckItem>,
}

impl DeclCtx {
    fn walk_module_body(&mut self, body: &lyra_ast::ModuleBody) {
        // Module body is non-procedural
        for vd in body.var_decls() {
            self.check_var_decl(&vd);
        }
        for always in body.always_blocks() {
            if let Some(stmt) = always.body() {
                self.walk_procedural_stmt(&stmt);
            }
        }
        for initial in body.initial_blocks() {
            if let Some(stmt) = initial.body() {
                self.walk_procedural_stmt(&stmt);
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

    fn walk_non_procedural_items(
        &mut self,
        var_decls: impl Iterator<Item = VarDecl>,
        function_decls: impl Iterator<Item = FunctionDecl>,
        task_decls: impl Iterator<Item = TaskDecl>,
    ) {
        for vd in var_decls {
            self.check_var_decl(&vd);
        }
        for func in function_decls {
            self.walk_function(&func);
        }
        for task in task_decls {
            self.walk_task(&task);
        }
    }

    fn walk_generate_item(&mut self, item: &lyra_ast::GenerateItem) {
        match item {
            lyra_ast::GenerateItem::ModuleInstantiation(_) => {}
            lyra_ast::GenerateItem::IfStmt(if_s) => {
                if let Some(t) = if_s.then_body() {
                    self.walk_generate_stmt(&t);
                }
                if let Some(e) = if_s.else_body() {
                    self.walk_generate_stmt(&e);
                }
            }
            lyra_ast::GenerateItem::ForStmt(for_s) => {
                if let Some(b) = for_s.body() {
                    self.walk_generate_stmt(&b);
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
                        self.walk_generate_stmt(&b);
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
                    self.walk_generate_stmt(&s);
                }
            }
        }
    }

    fn walk_generate_stmt(&mut self, stmt: &StmtNode) {
        let node = stmt.syntax();
        if let Some(block) = BlockStmt::cast(node.clone()) {
            for s in block.statements() {
                self.walk_generate_stmt(&s);
            }
        } else if let Some(vd) = VarDecl::cast(node.clone()) {
            self.check_var_decl(&vd);
        } else if let Some(func) = FunctionDecl::cast(node.clone()) {
            self.walk_function(&func);
        } else if let Some(task) = TaskDecl::cast(node.clone()) {
            self.walk_task(&task);
        }
    }

    fn walk_function(&mut self, func: &FunctionDecl) {
        let prev = self.procedural;
        self.procedural = true;
        for stmt in func.statements() {
            self.walk_procedural_stmt(&stmt);
        }
        self.procedural = prev;
    }

    fn walk_task(&mut self, task: &TaskDecl) {
        let prev = self.procedural;
        self.procedural = true;
        for stmt in task.statements() {
            self.walk_procedural_stmt(&stmt);
        }
        self.procedural = prev;
    }

    fn walk_procedural_stmt(&mut self, stmt: &StmtNode) {
        let prev = self.procedural;
        self.procedural = true;
        let node = stmt.syntax();
        if let Some(block) = BlockStmt::cast(node.clone()) {
            for s in block.statements() {
                self.walk_procedural_stmt(&s);
            }
        } else if let Some(vd) = VarDecl::cast(node.clone()) {
            self.check_var_decl(&vd);
        } else if let Some(func) = FunctionDecl::cast(node.clone()) {
            self.walk_function(&func);
        } else if let Some(task) = TaskDecl::cast(node.clone()) {
            self.walk_task(&task);
        }
        self.procedural = prev;
    }

    fn check_var_decl(&mut self, vd: &VarDecl) {
        if self.procedural {
            return;
        }
        if let Some(DeclLifetimeSyntax::Automatic(auto_tok)) = vd.lifetime() {
            self.items.push(DeclCheckItem::AutomaticVarNonProcedural {
                kw: TokenSpan::new(auto_tok.text_range()),
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lyra_source::FileId;

    fn check(src: &str) -> DeclCheckIndex {
        let file = FileId(0);
        let tokens = lyra_lexer::lex(src);
        let pp = lyra_preprocess::preprocess_identity(file, &tokens, src);
        let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
        build_decl_check_index(&parse.syntax())
    }

    #[test]
    fn automatic_in_module_body() {
        let idx = check("module m; automatic int x; endmodule");
        assert_eq!(idx.items.len(), 1);
        assert!(matches!(
            idx.items[0],
            DeclCheckItem::AutomaticVarNonProcedural { .. }
        ));
    }

    #[test]
    fn static_in_module_body_ok() {
        let idx = check("module m; static int x; endmodule");
        assert!(idx.items.is_empty());
    }

    #[test]
    fn no_qualifier_in_module_body_ok() {
        let idx = check("module m; int x; endmodule");
        assert!(idx.items.is_empty());
    }

    #[test]
    fn automatic_in_package_body() {
        let idx = check("package p; automatic int x; endpackage");
        assert_eq!(idx.items.len(), 1);
    }

    #[test]
    fn automatic_in_interface_body() {
        let idx = check("interface i; automatic int x; endinterface");
        assert_eq!(idx.items.len(), 1);
    }

    #[test]
    fn automatic_in_procedural_block_ok() {
        let idx = check("module m; initial begin automatic int x; end endmodule");
        assert!(idx.items.is_empty());
    }

    #[test]
    fn automatic_in_function_body_ok() {
        let idx = check("module m; function void f(); automatic int x; endfunction endmodule");
        assert!(idx.items.is_empty());
    }

    #[test]
    fn automatic_in_task_body_ok() {
        let idx = check("module m; task t(); automatic int x; endtask endmodule");
        assert!(idx.items.is_empty());
    }

    #[test]
    fn mixed_procedural_and_non_procedural() {
        let idx = check(
            "module m;\n\
             automatic int bad;\n\
             initial begin automatic int ok; end\n\
             endmodule",
        );
        assert_eq!(idx.items.len(), 1);
    }
}
