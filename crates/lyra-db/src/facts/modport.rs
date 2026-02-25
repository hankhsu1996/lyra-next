use lyra_ast::{AstNode, FieldExpr};
use lyra_lexer::SyntaxKind;
use lyra_semantic::modport_facts::{FieldAccessFact, FieldAccessFacts, FieldAccessTarget};
use lyra_semantic::types::{ModportViewTarget, Ty};

use crate::expr_queries::{ExprRef, type_of_expr};
use crate::pipeline::{ast_id_map, parse_file};
use crate::record_queries::{ModportRef, modport_sem};
use crate::{CompilationUnit, SourceFile};

/// Pre-compute modport direction facts for all `FieldExpr` nodes in a file.
#[salsa::tracked(return_ref)]
pub fn field_access_facts(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> FieldAccessFacts {
    let parse = parse_file(db, file);
    let map = ast_id_map(db, file);
    let mut facts = FieldAccessFacts::new();

    collect_field_exprs(&parse.syntax(), db, unit, map, &mut facts);

    facts
}

fn collect_field_exprs(
    node: &lyra_parser::SyntaxNode,
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    map: &lyra_ast::AstIdMap,
    facts: &mut FieldAccessFacts,
) {
    if node.kind() == SyntaxKind::FieldExpr
        && let Some(fact) = compute_field_fact(node, db, unit, map)
        && let Some(ast_id) = map.erased_ast_id(node)
    {
        facts.insert(ast_id, fact);
    }
    for child in node.children() {
        collect_field_exprs(&child, db, unit, map, facts);
    }
}

fn compute_field_fact(
    node: &lyra_parser::SyntaxNode,
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    map: &lyra_ast::AstIdMap,
) -> Option<FieldAccessFact> {
    let field_expr = FieldExpr::cast(node.clone())?;
    let base = field_expr.base_expr()?;
    let field_tok = field_expr.field_name()?;
    let member_name = field_tok.text();

    let base_ast_id = map.erased_ast_id(&base)?;
    let expr_ref = ExprRef::new(db, unit, base_ast_id);
    let base_type = type_of_expr(db, expr_ref);

    let Ty::Interface(ref iface_ty) = base_type.ty else {
        return None;
    };
    let mp_id = iface_ty.modport?;

    let mref = ModportRef::new(db, unit, mp_id);
    let sem = modport_sem(db, mref);
    let entry = sem.view.lookup(member_name)?;

    let target = match &entry.target {
        ModportViewTarget::Member(_) => FieldAccessTarget::Member,
        ModportViewTarget::Expr(expr_id) => FieldAccessTarget::Expr(*expr_id),
        ModportViewTarget::Empty => FieldAccessTarget::Empty,
    };

    Some(FieldAccessFact {
        member_name_span: lyra_source::NameSpan::new(field_tok.text_range()),
        port_id: entry.port_id,
        direction: entry.direction,
        target,
    })
}
