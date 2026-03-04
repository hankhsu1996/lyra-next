// Foreach loop legality checks (LRM 12.7.3).
//
// Validates:
// - Loop variables are not assigned to (read-only)
// - Loop variable names do not shadow the iterated array name
// - Number of loop variables does not exceed iterable dimensions
//
// Items carry `TokenSpan` anchors so lowering is a trivial span mapping.

use std::collections::HashSet;

use lyra_ast::{AssignStmt, AstIdMap, AstNode, Expr, ExprKind, ForeachStmt, HasSyntax, SourceFile};
use lyra_parser::SyntaxNode;
use lyra_source::TokenSpan;
use smol_str::SmolStr;

use lyra_semantic::foreach_check::{ForeachCheckIndex, ForeachCheckItem};
use lyra_semantic::symbols::GlobalSymbolId;
use lyra_semantic::types::foreach_dim::ForeachDims;

use crate::expr_queries::{ExprRef, type_of_expr};
use crate::pipeline::{ast_id_map, parse_file};
use crate::semantic::{def_index_file, resolve_index_file};
use crate::{CompilationUnit, SourceFile as DbSourceFile};

/// Salsa-cached foreach-check index for a file.
#[salsa::tracked(return_ref)]
pub fn foreach_check_index(
    db: &dyn salsa::Database,
    file: DbSourceFile,
    unit: CompilationUnit,
) -> ForeachCheckIndex {
    let parse = parse_file(db, file);
    let id_map = ast_id_map(db, file);
    let def = def_index_file(db, file);
    let resolve = resolve_index_file(db, file, unit);

    let root = parse.syntax();
    if SourceFile::cast(root.clone()).is_none() {
        return ForeachCheckIndex {
            items: Box::new([]),
        };
    }

    let mut items = Vec::new();

    for node in root.descendants() {
        let Some(foreach_stmt) = ForeachStmt::cast(node) else {
            continue;
        };
        check_foreach_stmt(db, unit, &foreach_stmt, id_map, &mut items);
    }

    check_assign_to_foreach_vars(db, file, id_map, def, resolve, &mut items);

    ForeachCheckIndex {
        items: items.into_boxed_slice(),
    }
}

fn check_foreach_stmt(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    foreach_stmt: &ForeachStmt,
    id_map: &AstIdMap,
    items: &mut Vec<ForeachCheckItem>,
) {
    let Some(var_list) = foreach_stmt.var_list() else {
        return;
    };
    let Some(array_expr) = foreach_stmt.array_expr() else {
        return;
    };

    let array_root_name = extract_root_name(&array_expr);
    let dim_count = compute_dim_count(db, unit, id_map, &array_expr);

    let mut var_count: u32 = 0;
    for slot in var_list.slots() {
        var_count += 1;
        let Some(decl) = slot.declarator() else {
            continue;
        };
        let Some(name_tok) = decl.name() else {
            continue;
        };
        let var_name: SmolStr = SmolStr::new(name_tok.text());
        let var_span = TokenSpan::new(name_tok.text_range());

        if let Some(ref arr_name) = array_root_name
            && var_name == *arr_name
        {
            items.push(ForeachCheckItem::VarSameNameAsArray {
                var_name_span: var_span,
                array_name: arr_name.clone(),
            });
        }
    }

    if let Some(dc) = dim_count
        && var_count > dc
    {
        let mut slot_idx: u32 = 0;
        for slot in var_list.slots() {
            slot_idx += 1;
            if slot_idx > dc
                && let Some(decl) = slot.declarator()
                && let Some(name_tok) = decl.name()
            {
                items.push(ForeachCheckItem::TooManyVars {
                    excess_var_span: TokenSpan::new(name_tok.text_range()),
                    dim_count: dc,
                    var_count,
                });
            }
        }
    }
}

/// Compute the number of iterable dimensions for the array expression.
fn compute_dim_count(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    id_map: &AstIdMap,
    array_expr: &Expr,
) -> Option<u32> {
    let expr_site = id_map.erased_ast_id(array_expr.syntax())?;
    let expr_ref = ExprRef::new(db, unit, expr_site);
    let expr_type = type_of_expr(db, expr_ref);
    let dims = ForeachDims::from_iterated_type(&expr_type.ty);
    Some(dims.len() as u32)
}

/// Extract the root name from an iterated expression.
///
/// Walks the expression structure to find the base `NameRef`:
/// - `arr` -> "arr"
/// - `s.arr` -> "s"
/// - `arr[0]` -> "arr"
/// - `pkg::name` -> last segment
fn extract_root_name(expr: &Expr) -> Option<SmolStr> {
    match expr.classify()? {
        ExprKind::NameRef(nr) => nr.ident().map(|t| SmolStr::new(t.text())),
        ExprKind::QualifiedName(qn) => qn.segments().last().map(|t| SmolStr::new(t.text())),
        ExprKind::FieldExpr(fe) => extract_root_name_from_base(&fe.base_expr()?),
        ExprKind::IndexExpr(ie) => extract_root_name_from_base(&ie.base_expr()?),
        _ => None,
    }
}

fn extract_root_name_from_base(expr: &Expr) -> Option<SmolStr> {
    match expr.classify()? {
        ExprKind::NameRef(nr) => nr.ident().map(|t| SmolStr::new(t.text())),
        ExprKind::QualifiedName(qn) => qn.segments().last().map(|t| SmolStr::new(t.text())),
        ExprKind::FieldExpr(fe) => extract_root_name_from_base(&fe.base_expr()?),
        ExprKind::IndexExpr(ie) => extract_root_name_from_base(&ie.base_expr()?),
        _ => None,
    }
}

/// Check all resolutions for assignment to foreach loop variables.
fn check_assign_to_foreach_vars(
    db: &dyn salsa::Database,
    file: DbSourceFile,
    id_map: &AstIdMap,
    def: &lyra_semantic::def_index::DefIndex,
    resolve: &lyra_semantic::resolve_index::ResolveIndex,
    items: &mut Vec<ForeachCheckItem>,
) {
    if def.foreach_var_defs.is_empty() {
        return;
    }

    let file_id = file.file_id(db);

    let foreach_var_syms: HashSet<_> = def
        .foreach_var_defs
        .keys()
        .map(|local| GlobalSymbolId {
            file: file_id,
            local: *local,
        })
        .collect();

    let parse = parse_file(db, file);
    let root = parse.syntax();

    for (use_site, resolution) in &resolve.resolutions {
        let lyra_semantic::resolve_index::ResolvedTarget::Symbol(gsym) = &resolution.target else {
            continue;
        };
        if !foreach_var_syms.contains(gsym) {
            continue;
        }

        let Some(use_node) = id_map.get_node(&root, *use_site) else {
            continue;
        };
        if !is_assignment_lhs(&use_node) {
            continue;
        }

        let var_name = def.symbols.get(gsym.local).name.clone();
        let span = lyra_ast::NameRef::cast(use_node.clone())
            .and_then(|nr| nr.ident())
            .map_or_else(
                || TokenSpan::new(use_node.text_range()),
                |t| TokenSpan::new(t.text_range()),
            );
        items.push(ForeachCheckItem::AssignToForeachVar {
            lhs_name_span: span,
            var_name,
        });
    }
}

/// Check whether a `NameRef` node is the direct assignment target.
///
/// Returns true only when the `NameRef` IS the LHS expression of an
/// `AssignStmt` (e.g. `i = 0;`). Using the variable as an array
/// index (`arr[i] = 1;`) does NOT count -- there the LHS is
/// `arr[i]`, not `i`.
fn is_assignment_lhs(node: &SyntaxNode) -> bool {
    let Some(parent) = node.parent() else {
        return false;
    };
    if let Some(assign) = AssignStmt::cast(parent)
        && let Some(lhs) = assign.lhs()
    {
        return *lhs.syntax() == *node;
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{LyraDatabase, new_compilation_unit};
    use lyra_source::FileId;

    fn new_file(db: &dyn salsa::Database, id: u32, text: &str) -> DbSourceFile {
        DbSourceFile::new(
            db,
            FileId(id),
            text.to_owned(),
            crate::IncludeMap::default(),
        )
    }

    fn single_file_unit(db: &dyn salsa::Database, file: DbSourceFile) -> CompilationUnit {
        new_compilation_unit(db, vec![file])
    }

    fn check(src: &str) -> (LyraDatabase, ForeachCheckIndex) {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, src);
        let unit = single_file_unit(&db, file);
        let idx = foreach_check_index(&db, file, unit).clone();
        (db, idx)
    }

    #[test]
    fn no_issues_simple_foreach() {
        let (_, idx) = check(
            "module m;\
             int arr[4];\
             initial foreach (arr[i]) begin end\
             endmodule",
        );
        assert!(idx.items.is_empty());
    }

    #[test]
    fn too_many_vars_unpacked() {
        let (_, idx) = check(
            "module m;\
             int arr[4];\
             initial foreach (arr[i,j]) begin end\
             endmodule",
        );
        assert_eq!(idx.items.len(), 1);
        assert!(matches!(
            &idx.items[0],
            ForeachCheckItem::TooManyVars {
                dim_count: 1,
                var_count: 2,
                ..
            }
        ));
    }

    #[test]
    fn too_many_vars_multi_excess() {
        let (_, idx) = check(
            "module m;\
             int arr[4];\
             initial foreach (arr[i,j,k]) begin end\
             endmodule",
        );
        assert_eq!(idx.items.len(), 2);
    }

    #[test]
    fn same_name_as_array() {
        let (_, idx) = check(
            "module m;\
             int arr[4];\
             initial foreach (arr[arr]) begin end\
             endmodule",
        );
        let same_name_items: Vec<_> = idx
            .items
            .iter()
            .filter(|i| matches!(i, ForeachCheckItem::VarSameNameAsArray { .. }))
            .collect();
        assert_eq!(same_name_items.len(), 1);
    }

    #[test]
    fn assign_to_foreach_var() {
        let (_, idx) = check(
            "module m;\
             int arr[4];\
             initial foreach (arr[i]) begin i = 0; end\
             endmodule",
        );
        let assign_items: Vec<_> = idx
            .items
            .iter()
            .filter(|i| matches!(i, ForeachCheckItem::AssignToForeachVar { .. }))
            .collect();
        assert_eq!(assign_items.len(), 1);
    }

    #[test]
    fn assign_compound_to_foreach_var() {
        let (_, idx) = check(
            "module m;\
             int arr[4];\
             initial foreach (arr[i]) begin i += 1; end\
             endmodule",
        );
        let assign_items: Vec<_> = idx
            .items
            .iter()
            .filter(|i| matches!(i, ForeachCheckItem::AssignToForeachVar { .. }))
            .collect();
        assert_eq!(assign_items.len(), 1);
    }

    #[test]
    fn read_foreach_var_is_ok() {
        let (_, idx) = check(
            "module m;\
             int arr[4];\
             int x;\
             initial foreach (arr[i]) begin x = i; end\
             endmodule",
        );
        let assign_items: Vec<_> = idx
            .items
            .iter()
            .filter(|i| matches!(i, ForeachCheckItem::AssignToForeachVar { .. }))
            .collect();
        assert!(assign_items.is_empty());
    }

    #[test]
    fn packed_dim_foreach() {
        let (_, idx) = check(
            "module m;\
             logic [7:0] v;\
             initial foreach (v[i]) begin end\
             endmodule",
        );
        assert!(idx.items.is_empty());
    }

    #[test]
    fn mixed_unpacked_packed_foreach() {
        let (_, idx) = check(
            "module m;\
             logic [7:0] arr [3];\
             initial foreach (arr[i,j]) begin end\
             endmodule",
        );
        assert!(idx.items.is_empty());
    }

    #[test]
    fn string_foreach_one_dim() {
        let (_, idx) = check(
            "module m;\
             string s;\
             initial foreach (s[i]) begin end\
             endmodule",
        );
        assert!(idx.items.is_empty());
    }

    #[test]
    fn string_foreach_too_many() {
        let (_, idx) = check(
            "module m;\
             string s;\
             initial foreach (s[i,j]) begin end\
             endmodule",
        );
        assert_eq!(idx.items.len(), 1);
        assert!(matches!(
            &idx.items[0],
            ForeachCheckItem::TooManyVars {
                dim_count: 1,
                var_count: 2,
                ..
            }
        ));
    }
}
