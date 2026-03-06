use lyra_ast::{FieldExpr, HasSyntax};
use lyra_semantic::modport_facts::{FieldAccessFact, FieldAccessTarget};
use lyra_semantic::types::{ModportViewTarget, Ty};

use crate::expr_queries::{ExprRef, type_of_expr};
use crate::modport_queries::{ModportRef, modport_sem};
use crate::pipeline::{ast_id_map, parse_file};
use crate::{CompilationUnit, SourceFile};

/// Pre-compute direction-access facts for all `FieldExpr` nodes in a file.
///
/// Returns facts for members listed in the modport view. Restriction
/// detection (members not in the modport) is handled by the expression
/// inference path (`ExprTypeErrorKind::MemberNotInModport`).
#[salsa::tracked(return_ref)]
pub fn field_access_facts(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> lyra_semantic::modport_facts::FieldAccessFacts {
    let parse = parse_file(db, file);
    let map = ast_id_map(db, file);
    let mut facts = lyra_semantic::modport_facts::FieldAccessFacts::default();

    for field_expr in lyra_ast::field_exprs(&parse.syntax()) {
        let Some(ast_id) = map.erased_ast_id(field_expr.syntax()) else {
            continue;
        };
        if let Some(fact) = classify_field_access(&field_expr, db, unit, map) {
            facts.insert(ast_id, fact);
        }
    }

    facts
}

fn classify_field_access(
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
