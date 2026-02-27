use lyra_ast::{AstNode, FieldExpr};
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

    for field_expr in lyra_ast::field_exprs(&parse.syntax()) {
        if let Some(fact) = compute_field_fact(&field_expr, db, unit, map)
            && let Some(ast_id) = map.erased_ast_id(field_expr.syntax())
        {
            facts.insert(ast_id, fact);
        }
    }

    facts
}

fn compute_field_fact(
    field_expr: &FieldExpr,
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    map: &lyra_ast::AstIdMap,
) -> Option<FieldAccessFact> {
    let base = field_expr.base_expr()?;
    let field_tok = field_expr.field_name()?;
    let member_name = field_tok.text();

    let base_ast_id = map.erased_ast_id(base.syntax())?;
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
