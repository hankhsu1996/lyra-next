use lyra_ast::{AstNode, FieldExpr};
use lyra_lexer::SyntaxKind;
use lyra_semantic::modport_def::PortDirection;
use lyra_semantic::modport_facts::{FieldAccessFact, FieldAccessFacts};
use lyra_semantic::symbols::Namespace;
use lyra_semantic::types::{InterfaceType, Ty};

use crate::expr_queries::{ExprRef, type_of_expr};
use crate::pipeline::{ast_id_map, parse_file};
use crate::record_queries::{ModportRef, modport_sem};
use crate::semantic::{def_index_file, def_symbol};
use crate::{CompilationUnit, SourceFile, source_file_by_id};

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
    iface_ty.modport?;

    let direction = modport_member_direction(db, unit, iface_ty, member_name)?;
    Some(FieldAccessFact {
        member_range: field_tok.text_range(),
        direction,
    })
}

/// Resolve the modport direction for a member of an interface.
fn modport_member_direction(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    iface_ty: &InterfaceType,
    member_name: &str,
) -> Option<PortDirection> {
    let gsym = def_symbol(db, unit, iface_ty.iface.global_def())?;
    let src = source_file_by_id(db, unit, gsym.file)?;
    let def = def_index_file(db, src);
    let iface_scope = def.symbols.get(gsym.local).scope;
    let member_sym =
        def.scopes
            .resolve(&def.symbols, iface_scope, Namespace::Value, member_name)?;

    let mp_id = iface_ty.modport?;
    let mref = ModportRef::new(db, unit, mp_id);
    let sem = modport_sem(db, mref);
    sem.view.direction_of(member_sym)
}
