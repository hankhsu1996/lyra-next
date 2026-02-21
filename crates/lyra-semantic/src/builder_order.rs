use std::collections::HashMap;

use lyra_ast::AstIdMap;
use lyra_parser::SyntaxNode;
use smallvec::SmallVec;

use crate::builder::DefContext;
use crate::diagnostic::{SemanticDiag, SemanticDiagKind};
use crate::symbols::SymbolId;

enum OrderItem {
    UseSite(usize),
    Import(usize),
    LocalDecl(usize),
}

/// Assign monotonic order keys to imports and use-sites via preorder syntax walk.
///
/// Walks the file's syntax tree in preorder. When encountering a node whose
/// `ErasedAstId` matches a collected import or use-site, assigns the next
/// sequential order key. This establishes LRM 26.3 positional visibility:
/// an import is only a candidate when `import.order_key < use_site.order_key`.
pub(crate) fn assign_order_keys(ctx: &mut DefContext<'_>, root: &SyntaxNode) {
    let mut items_by_ast_id: HashMap<lyra_ast::ErasedAstId, SmallVec<[OrderItem; 1]>> =
        HashMap::new();
    for (i, site) in ctx.use_sites.iter().enumerate() {
        items_by_ast_id
            .entry(site.ast_id)
            .or_default()
            .push(OrderItem::UseSite(i));
    }
    for (i, imp) in ctx.imports.iter().enumerate() {
        items_by_ast_id
            .entry(imp.ast_id)
            .or_default()
            .push(OrderItem::Import(i));
    }
    for (i, decl) in ctx.local_decls.iter().enumerate() {
        let entry = items_by_ast_id.entry(decl.ast_id).or_default();
        debug_assert!(
            !entry
                .iter()
                .any(|item| matches!(item, OrderItem::LocalDecl(_))),
            "duplicate LocalDecl AstId"
        );
        entry.push(OrderItem::LocalDecl(i));
    }
    let total_items = ctx.use_sites.len() + ctx.imports.len() + ctx.local_decls.len();

    // Collect (OrderItem, order_key) assignments via preorder walk
    let mut assignments: Vec<(OrderItem, u32)> = Vec::with_capacity(total_items);
    let mut order_key = 0u32;
    preorder_collect(
        ctx.ast_id_map,
        root,
        &mut items_by_ast_id,
        &mut order_key,
        &mut assignments,
    );

    // Apply collected assignments. Items not matched during the walk
    // (e.g. due to parse error recovery) keep order_key 0 and remain
    // visible to all use-sites -- a safe conservative default.
    for (item, key) in &assignments {
        match item {
            OrderItem::UseSite(i) => ctx.use_sites[*i].order_key = *key,
            OrderItem::Import(i) => ctx.imports[*i].order_key = *key,
            OrderItem::LocalDecl(i) => ctx.local_decls[*i].order_key = *key,
        }
    }
}

fn preorder_collect(
    ast_id_map: &AstIdMap,
    node: &SyntaxNode,
    items_by_ast_id: &mut HashMap<lyra_ast::ErasedAstId, SmallVec<[OrderItem; 1]>>,
    order_key: &mut u32,
    out: &mut Vec<(OrderItem, u32)>,
) {
    if let Some(ast_id) = ast_id_map.erased_ast_id(node)
        && let Some(items) = items_by_ast_id.remove(&ast_id)
    {
        for item in items {
            out.push((item, *order_key));
            *order_key += 1;
        }
    }
    for child in node.children() {
        preorder_collect(ast_id_map, &child, items_by_ast_id, order_key, out);
    }
}

pub(crate) fn detect_duplicates(
    symbols: &crate::symbols::SymbolTable,
    bindings: &[SymbolId],
    diagnostics: &mut Vec<SemanticDiag>,
) {
    for i in 1..bindings.len() {
        let prev = symbols.get(bindings[i - 1]);
        let curr = symbols.get(bindings[i]);
        if prev.name == curr.name {
            diagnostics.push(SemanticDiag {
                kind: SemanticDiagKind::DuplicateDefinition {
                    name: curr.name.clone(),
                    original: prev.def_range,
                },
                range: curr.def_range,
            });
        }
    }
}
