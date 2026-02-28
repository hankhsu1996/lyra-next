use std::collections::HashSet;

use lyra_ast::{
    AssignStmt, AstIdMap, AstNode, BlockStmt, CaseStmt, ContinuousAssign, ErasedAstId, Expr,
    ExprKind, ForStmt, ForeverStmt, FunctionDecl, GenerateItem, GenerateRegion, HasSyntax, IfStmt,
    ModuleBody, RepeatStmt, SourceFile, StmtNode, SystemTfCall, TaskDecl, TimingControl, VarDecl,
    WhileStmt, expr_children,
};
use lyra_parser::SyntaxNode;
use lyra_semantic::type_check::AccessMode;

use crate::pipeline::{ast_id_map, parse_file};

/// A deterministic, sorted list of type-check sites for a file.
///
/// Built from the parse tree using typed AST APIs. Each entry
/// anchors a check-worthy node by its `ErasedAstId` plus minimal
/// context (check kind, access mode). The index is independent of
/// semantic queries and designed for incremental stability.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ChecksIndex {
    pub entries: Box<[CheckEntry]>,
}

/// A single check site in the index.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CheckEntry {
    pub site: ErasedAstId,
    pub kind: CheckKind,
    pub access: AccessMode,
}

/// What kind of check to dispatch for this entry.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CheckKind {
    ContinuousAssign,
    AssignStmt,
    VarDecl,
    SystemTfCall,
    FieldExpr,
    CastExpr,
    StreamOperandItem,
    CallExpr,
}

/// Salsa-cached checks index for a file.
#[salsa::tracked(return_ref)]
pub fn checks_index(db: &dyn salsa::Database, file: crate::SourceFile) -> ChecksIndex {
    let parse = parse_file(db, file);
    let id_map = ast_id_map(db, file);
    build_checks_index(&parse.syntax(), id_map)
}

fn build_checks_index(root: &SyntaxNode, id_map: &AstIdMap) -> ChecksIndex {
    let mut builder = IndexBuilder {
        id_map,
        entries: Vec::new(),
        seen: HashSet::new(),
    };
    if let Some(sf) = SourceFile::cast(root.clone()) {
        for module in sf.modules() {
            if let Some(body) = module.body() {
                builder.collect_module_body(&body);
            }
        }
    }
    builder.entries.sort_by_key(|e| e.site);
    ChecksIndex {
        entries: builder.entries.into_boxed_slice(),
    }
}

struct IndexBuilder<'a> {
    id_map: &'a AstIdMap,
    entries: Vec<CheckEntry>,
    seen: HashSet<ErasedAstId>,
}

impl IndexBuilder<'_> {
    fn push(&mut self, node: &SyntaxNode, kind: CheckKind, access: AccessMode) {
        let Some(site) = self.id_map.erased_ast_id(node) else {
            return;
        };
        if self.seen.insert(site) {
            self.entries.push(CheckEntry { site, kind, access });
        }
    }

    fn collect_module_body(&mut self, body: &ModuleBody) {
        for ca in body.continuous_assigns() {
            self.collect_continuous_assign(&ca);
        }
        for vd in body.var_decls() {
            self.push(vd.syntax(), CheckKind::VarDecl, AccessMode::Read);
        }
        for always in body.always_blocks() {
            if let Some(stmt) = always.body() {
                self.collect_stmt(&stmt, AccessMode::Read);
            }
        }
        for initial in body.initial_blocks() {
            if let Some(stmt) = initial.body() {
                self.collect_stmt(&stmt, AccessMode::Read);
            }
        }
        for func in body.function_decls() {
            self.collect_function_body(&func);
        }
        for task in body.task_decls() {
            self.collect_task_body(&task);
        }
        for gi in body.generate_items() {
            self.collect_generate_item(&gi);
        }
    }

    fn collect_function_body(&mut self, func: &FunctionDecl) {
        for stmt in func.statements() {
            self.collect_stmt(&stmt, AccessMode::Read);
        }
    }

    fn collect_task_body(&mut self, task: &TaskDecl) {
        for stmt in task.statements() {
            self.collect_stmt(&stmt, AccessMode::Read);
        }
    }

    fn collect_generate_item(&mut self, item: &GenerateItem) {
        match item {
            GenerateItem::ModuleInstantiation(_) => {}
            GenerateItem::IfStmt(if_s) => {
                if let Some(c) = if_s.condition() {
                    self.collect_from_expr(&c, AccessMode::Read);
                }
                if let Some(t) = if_s.then_body() {
                    self.collect_stmt(&t, AccessMode::Read);
                }
                if let Some(e) = if_s.else_body() {
                    self.collect_stmt(&e, AccessMode::Read);
                }
            }
            GenerateItem::ForStmt(for_s) => {
                for expr in expr_children(for_s.syntax()) {
                    self.collect_from_expr(&expr, AccessMode::Read);
                }
                if let Some(b) = for_s.body() {
                    self.collect_stmt(&b, AccessMode::Read);
                }
            }
            GenerateItem::CaseStmt(case) => {
                if let Some(sel) = case.selector() {
                    self.collect_from_expr(&sel, AccessMode::Read);
                }
                for ci in case.items() {
                    for expr in expr_children(ci.syntax()) {
                        self.collect_from_expr(&expr, AccessMode::Read);
                    }
                    if let Some(b) = ci.body() {
                        self.collect_stmt(&b, AccessMode::Read);
                    }
                }
            }
            GenerateItem::GenerateRegion(region) => {
                self.collect_generate_region(region);
            }
            GenerateItem::BlockStmt(block) => {
                for s in block.statements() {
                    self.collect_stmt(&s, AccessMode::Read);
                }
            }
        }
    }

    fn collect_generate_region(&mut self, region: &GenerateRegion) {
        for gi in region.generate_items() {
            self.collect_generate_item(&gi);
        }
    }

    fn collect_continuous_assign(&mut self, ca: &ContinuousAssign) {
        self.push(ca.syntax(), CheckKind::ContinuousAssign, AccessMode::Read);
        if let Some(lhs) = ca.lhs() {
            self.collect_from_expr(&lhs, AccessMode::Write);
        }
        if let Some(rhs) = ca.rhs() {
            self.collect_from_expr(&rhs, AccessMode::Read);
        }
        if let Some(tc) = ca.timing_control() {
            for expr in expr_children(tc.syntax()) {
                self.collect_from_expr(&expr, AccessMode::Read);
            }
        }
    }

    fn collect_assign_stmt(&mut self, assign: &AssignStmt) {
        let compound = assign.assign_op().is_some_and(|op| op.is_compound());
        let lhs_access = if compound {
            AccessMode::ReadWrite
        } else {
            AccessMode::Write
        };
        self.push(assign.syntax(), CheckKind::AssignStmt, AccessMode::Read);
        if let Some(lhs) = assign.lhs() {
            self.collect_from_expr(&lhs, lhs_access);
        }
        if let Some(rhs) = assign.rhs() {
            self.collect_from_expr(&rhs, AccessMode::Read);
        }
        if let Some(tc) = assign.timing_control() {
            for expr in expr_children(tc.syntax()) {
                self.collect_from_expr(&expr, AccessMode::Read);
            }
        }
    }

    fn collect_stmt(&mut self, stmt: &StmtNode, access: AccessMode) {
        let node = stmt.syntax();
        if let Some(block) = BlockStmt::cast(node.clone()) {
            for s in block.statements() {
                self.collect_stmt(&s, access);
            }
        } else if let Some(assign) = AssignStmt::cast(node.clone()) {
            self.collect_assign_stmt(&assign);
        } else if let Some(if_s) = IfStmt::cast(node.clone()) {
            if let Some(c) = if_s.condition() {
                self.collect_from_expr(&c, access);
            }
            if let Some(t) = if_s.then_body() {
                self.collect_stmt(&t, access);
            }
            if let Some(e) = if_s.else_body() {
                self.collect_stmt(&e, access);
            }
        } else if let Some(case) = CaseStmt::cast(node.clone()) {
            if let Some(sel) = case.selector() {
                self.collect_from_expr(&sel, access);
            }
            for item in case.items() {
                for expr in expr_children(item.syntax()) {
                    self.collect_from_expr(&expr, access);
                }
                if let Some(b) = item.body() {
                    self.collect_stmt(&b, access);
                }
            }
        } else if let Some(for_s) = ForStmt::cast(node.clone()) {
            for expr in expr_children(for_s.syntax()) {
                self.collect_from_expr(&expr, access);
            }
            if let Some(b) = for_s.body() {
                self.collect_stmt(&b, access);
            }
        } else if let Some(while_s) = WhileStmt::cast(node.clone()) {
            for expr in expr_children(while_s.syntax()) {
                self.collect_from_expr(&expr, access);
            }
            if let Some(b) = while_s.body() {
                self.collect_stmt(&b, access);
            }
        } else if let Some(repeat_s) = RepeatStmt::cast(node.clone()) {
            for expr in expr_children(repeat_s.syntax()) {
                self.collect_from_expr(&expr, access);
            }
            if let Some(b) = repeat_s.body() {
                self.collect_stmt(&b, access);
            }
        } else if let Some(forever_s) = ForeverStmt::cast(node.clone()) {
            if let Some(b) = forever_s.body() {
                self.collect_stmt(&b, access);
            }
        } else if let Some(tc) = TimingControl::cast(node.clone()) {
            for expr in expr_children(tc.syntax()) {
                self.collect_from_expr(&expr, access);
            }
            if let Some(b) = tc.body() {
                self.collect_stmt(&b, access);
            }
        } else if let Some(ca) = ContinuousAssign::cast(node.clone()) {
            self.collect_continuous_assign(&ca);
        } else if SystemTfCall::cast(node.clone()).is_some() {
            self.push(node, CheckKind::SystemTfCall, access);
        } else if VarDecl::cast(node.clone()).is_some() {
            self.push(node, CheckKind::VarDecl, access);
        } else {
            for expr in expr_children(node) {
                self.collect_from_expr(&expr, access);
            }
        }
    }

    fn collect_from_expr(&mut self, expr: &Expr, access: AccessMode) {
        if let Some(ek) = expr.classify() {
            match ek {
                ExprKind::FieldExpr(_) => {
                    self.push(ek.syntax(), CheckKind::FieldExpr, access);
                }
                ExprKind::CastExpr(_) => {
                    self.push(ek.syntax(), CheckKind::CastExpr, access);
                }
                ExprKind::CallExpr(_) => {
                    self.push(ek.syntax(), CheckKind::CallExpr, access);
                }
                ExprKind::SystemTfCall(_) => {
                    self.push(ek.syntax(), CheckKind::SystemTfCall, access);
                }
                ExprKind::StreamExpr(stream) => {
                    if let Some(operands) = stream.stream_operands() {
                        for item in operands.items() {
                            self.push(item.syntax(), CheckKind::StreamOperandItem, access);
                            for e in expr_children(item.syntax()) {
                                self.collect_from_expr(&e, access);
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        for child in expr_children(expr.syntax()) {
            self.collect_from_expr(&child, access);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lyra_source::FileId;

    fn parse_and_index(src: &str) -> ChecksIndex {
        let file = FileId(0);
        let tokens = lyra_lexer::lex(src);
        let pp = lyra_preprocess::preprocess_identity(file, &tokens, src);
        let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
        let root = parse.syntax();
        let map = AstIdMap::from_root(file, &root);
        build_checks_index(&root, &map)
    }

    fn has_kind(index: &ChecksIndex, kind: CheckKind) -> bool {
        index.entries.iter().any(|e| e.kind == kind)
    }

    #[test]
    fn function_body_var_decl_and_assign() {
        let idx =
            parse_and_index("module m; function void foo(); logic x; x = 1; endfunction endmodule");
        assert!(
            has_kind(&idx, CheckKind::VarDecl),
            "VarDecl inside function body"
        );
        assert!(
            has_kind(&idx, CheckKind::AssignStmt),
            "AssignStmt inside function body"
        );
    }

    #[test]
    fn task_body_system_call() {
        let idx = parse_and_index("module m; task t(); $display(\"hi\"); endtask endmodule");
        assert!(
            has_kind(&idx, CheckKind::SystemTfCall),
            "SystemTfCall inside task body"
        );
    }

    #[test]
    fn generate_if_with_assign() {
        let idx = parse_and_index("module m; if (1) begin : g logic x; end endmodule");
        assert!(
            has_kind(&idx, CheckKind::VarDecl),
            "VarDecl inside generate if"
        );
    }

    #[test]
    fn generate_for_with_nested_region() {
        let idx = parse_and_index(
            "module m;\n\
             generate\n\
               genvar i;\n\
               for (i = 0; i < 4; i = i + 1) begin : gen\n\
                 assign x = i;\n\
               end\n\
             endgenerate\n\
             endmodule",
        );
        assert!(
            has_kind(&idx, CheckKind::ContinuousAssign),
            "ContinuousAssign inside generate for"
        );
    }

    #[test]
    fn while_condition_system_call() {
        let idx =
            parse_and_index("module m; initial begin while ($bits(logic)) begin end end endmodule");
        assert!(
            has_kind(&idx, CheckKind::SystemTfCall),
            "$bits in while condition"
        );
    }

    #[test]
    fn for_stmt_condition_system_call() {
        let idx = parse_and_index(
            "module m; initial begin\n\
             integer i;\n\
             for (i = 0; i < $bits(logic); i = i + 1) begin end\n\
             end endmodule",
        );
        assert!(
            has_kind(&idx, CheckKind::SystemTfCall),
            "$bits in for-loop condition"
        );
    }

    #[test]
    fn repeat_condition_collected() {
        let idx = parse_and_index(
            "module m; initial begin repeat ($bits(logic)) begin end end endmodule",
        );
        assert!(
            has_kind(&idx, CheckKind::SystemTfCall),
            "$bits in repeat count"
        );
    }

    #[test]
    fn timing_control_expr_collected() {
        let idx = parse_and_index("module m; initial begin #($bits(logic)) x = 1; end endmodule");
        assert!(
            has_kind(&idx, CheckKind::SystemTfCall),
            "$bits in timing control delay"
        );
    }

    #[test]
    fn generate_for_genvar_init_expr() {
        let idx = parse_and_index(
            "module m;\n\
             generate\n\
               genvar i;\n\
               for (i = $bits(logic); i < 4; i = i + 1) begin : g end\n\
             endgenerate\n\
             endmodule",
        );
        assert!(
            has_kind(&idx, CheckKind::SystemTfCall),
            "$bits in generate-for init expression"
        );
    }

    #[test]
    fn generate_case_with_condition() {
        let idx = parse_and_index(
            "module m;\n\
             case ($bits(logic))\n\
               1: logic x;\n\
             endcase\n\
             endmodule",
        );
        assert!(
            has_kind(&idx, CheckKind::SystemTfCall),
            "$bits in generate case condition"
        );
    }

    #[test]
    fn procedural_case_selector_collected() {
        let idx = parse_and_index(
            "module m; initial begin case ($bits(logic)) default: ; endcase end endmodule",
        );
        assert!(
            has_kind(&idx, CheckKind::SystemTfCall),
            "$bits in procedural case selector"
        );
    }

    #[test]
    fn continuous_assign_inside_generate_block() {
        // Verify the parser produces ContinuousAssign (not AssignStmt)
        // inside generate blocks, and checks_index indexes it.
        let idx = parse_and_index(
            "module m;\n\
             generate\n\
               begin : g\n\
                 assign x = 1;\n\
               end\n\
             endgenerate\n\
             endmodule",
        );
        assert!(
            has_kind(&idx, CheckKind::ContinuousAssign),
            "assign inside generate block is ContinuousAssign"
        );
        assert!(
            !has_kind(&idx, CheckKind::AssignStmt),
            "assign inside generate block is not AssignStmt"
        );
    }

    #[test]
    fn entries_strictly_ordered_and_deterministic() {
        let src = "module m;\n\
                    assign a = b;\n\
                    assign c = d;\n\
                    always @(*) begin\n\
                      if ($bits(logic)) x = 1;\n\
                      case (y) 1: z = 2; endcase\n\
                    end\n\
                    function void f(); w = 3; endfunction\n\
                    endmodule";
        let idx1 = parse_and_index(src);

        assert!(
            idx1.entries.len() >= 4,
            "expected multiple entries, got {}",
            idx1.entries.len()
        );

        // Strictly increasing order by ErasedAstId.
        for pair in idx1.entries.windows(2) {
            assert!(
                pair[0].site < pair[1].site,
                "entries not strictly ordered: {:?} >= {:?}",
                pair[0].site,
                pair[1].site
            );
        }

        // Identical output on second build.
        let idx2 = parse_and_index(src);
        assert_eq!(idx1, idx2, "non-deterministic: two builds differ");
    }
}
