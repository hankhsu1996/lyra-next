use std::collections::HashSet;

use lyra_ast::{
    AssignStmt, AstIdMap, AstNode, BlockStmt, CaseStmt, ContinuousAssign, ErasedAstId, ForStmt,
    ForeverStmt, IfStmt, ModuleBody, RepeatStmt, SourceFile, StmtNode, StreamExpr, TimingControl,
    WhileStmt, expr_children,
};
use lyra_lexer::SyntaxKind;
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
    }

    fn collect_continuous_assign(&mut self, ca: &ContinuousAssign) {
        self.push(ca.syntax(), CheckKind::ContinuousAssign, AccessMode::Read);
        if let Some(lhs) = ca.lhs() {
            self.collect_from_expr(lhs.syntax(), AccessMode::Write);
        }
        if let Some(rhs) = ca.rhs() {
            self.collect_from_expr(rhs.syntax(), AccessMode::Read);
        }
        if let Some(tc) = ca.timing_control() {
            for expr in expr_children(tc.syntax()) {
                self.collect_from_expr(expr.syntax(), AccessMode::Read);
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
            self.collect_from_expr(lhs.syntax(), lhs_access);
        }
        if let Some(rhs) = assign.rhs() {
            self.collect_from_expr(rhs.syntax(), AccessMode::Read);
        }
        if let Some(tc) = assign.timing_control() {
            for expr in expr_children(tc.syntax()) {
                self.collect_from_expr(expr.syntax(), AccessMode::Read);
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
                self.collect_from_expr(c.syntax(), access);
            }
            if let Some(t) = if_s.then_body() {
                self.collect_stmt(&t, access);
            }
            if let Some(e) = if_s.else_body() {
                self.collect_stmt(&e, access);
            }
        } else if let Some(case) = CaseStmt::cast(node.clone()) {
            for item in case.items() {
                for expr in expr_children(item.syntax()) {
                    self.collect_from_expr(expr.syntax(), access);
                }
                if let Some(b) = item.body() {
                    self.collect_stmt(&b, access);
                }
            }
        } else if let Some(for_s) = ForStmt::cast(node.clone()) {
            if let Some(b) = for_s.body() {
                self.collect_stmt(&b, access);
            }
        } else if let Some(while_s) = WhileStmt::cast(node.clone()) {
            if let Some(b) = while_s.body() {
                self.collect_stmt(&b, access);
            }
        } else if let Some(repeat_s) = RepeatStmt::cast(node.clone()) {
            if let Some(b) = repeat_s.body() {
                self.collect_stmt(&b, access);
            }
        } else if let Some(forever_s) = ForeverStmt::cast(node.clone()) {
            if let Some(b) = forever_s.body() {
                self.collect_stmt(&b, access);
            }
        } else if let Some(tc) = TimingControl::cast(node.clone()) {
            if let Some(b) = tc.body() {
                self.collect_stmt(&b, access);
            }
        } else if node.kind() == SyntaxKind::SystemTfCall {
            self.push(node, CheckKind::SystemTfCall, access);
        } else if node.kind() == SyntaxKind::VarDecl {
            self.push(node, CheckKind::VarDecl, access);
        } else {
            for expr in expr_children(node) {
                self.collect_from_expr(expr.syntax(), access);
            }
        }
    }

    fn collect_from_expr(&mut self, node: &SyntaxNode, access: AccessMode) {
        match node.kind() {
            SyntaxKind::FieldExpr => {
                self.push(node, CheckKind::FieldExpr, access);
            }
            SyntaxKind::CastExpr => {
                self.push(node, CheckKind::CastExpr, access);
            }
            SyntaxKind::CallExpr => {
                self.push(node, CheckKind::CallExpr, access);
            }
            SyntaxKind::SystemTfCall => {
                self.push(node, CheckKind::SystemTfCall, access);
            }
            SyntaxKind::StreamExpr => {
                // StreamOperandItem is not an expression kind, so
                // expr_children won't reach it. Descend via typed API.
                if let Some(stream) = StreamExpr::cast(node.clone())
                    && let Some(operands) = stream.stream_operands()
                {
                    for item in operands.items() {
                        self.push(item.syntax(), CheckKind::StreamOperandItem, access);
                        for e in expr_children(item.syntax()) {
                            self.collect_from_expr(e.syntax(), access);
                        }
                    }
                }
            }
            _ => {}
        }
        for child in expr_children(node) {
            self.collect_from_expr(child.syntax(), access);
        }
    }
}
