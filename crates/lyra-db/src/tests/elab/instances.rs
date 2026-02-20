use super::*;

#[test]
fn unresolved_module() {
    let diags = elab_diags(&["module top; nonexistent u1(); endmodule"], "top");
    let unresolved: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::UNRESOLVED_MODULE_INST)
        .collect();
    assert_eq!(unresolved.len(), 1);
}

#[test]
fn not_a_module_instantiation() {
    let diags = elab_diags(
        &["package pkg; endpackage", "module top; pkg u1(); endmodule"],
        "top",
    );
    let not_mod: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::NOT_INSTANTIABLE)
        .collect();
    assert_eq!(not_mod.len(), 1);
}

#[test]
fn instance_tree_two_levels() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(input logic a); endmodule",
            "module mid(input logic x); leaf l1(.a(x)); endmodule",
            "module top; logic w; mid m1(.x(w)); endmodule",
        ],
        "top",
    );

    let top_id = tree.top.expect("top should exist");
    let top_node = tree.inst(top_id);
    assert_eq!(top_node.instance_name, "top");
    assert!(top_node.parent.is_none(), "top has no parent");

    let top_children = child_instance_names(&tree, top_id);
    assert_eq!(top_children, vec!["m1"]);

    let m1_id = find_child_inst_by_name(&tree, top_id, "m1");
    let m1_node = tree.inst(m1_id);
    assert_eq!(m1_node.parent, Some(ElabNodeId::Inst(top_id)));
    let m1_children = child_instance_names(&tree, m1_id);
    assert_eq!(m1_children, vec!["l1"]);

    let l1_id = find_child_inst_by_name(&tree, m1_id, "l1");
    let l1_node = tree.inst(l1_id);
    assert_eq!(l1_node.parent, Some(ElabNodeId::Inst(m1_id)));
    assert!(l1_node.children.is_empty(), "leaf has no children");

    assert_eq!(tree.instances.len(), 3);
    assert_no_duplicate_origins(&tree);
}

#[test]
fn instance_tree_diamond_legal() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(input logic a); endmodule",
            "module top; logic x, y; leaf u1(.a(x)); leaf u2(.a(y)); endmodule",
        ],
        "top",
    );

    let top_id = tree.top.expect("top should exist");
    let top_children = child_instance_names(&tree, top_id);
    assert_eq!(top_children, vec!["u1", "u2"]);

    for child in &tree.inst(top_id).children {
        if let ElabNodeId::Inst(cid) = child {
            assert_eq!(tree.inst(*cid).parent, Some(ElabNodeId::Inst(top_id)));
            assert!(tree.inst(*cid).children.is_empty());
        }
    }

    let recursion: Vec<_> = tree
        .diagnostics
        .iter()
        .filter(|d| matches!(d, crate::elaboration::ElabDiag::RecursionLimit { .. }))
        .collect();
    assert!(recursion.is_empty(), "diamond should not trigger recursion");

    assert_eq!(tree.instances.len(), 3);
    assert_no_duplicate_origins(&tree);
}

#[test]
fn instance_tree_multi_instance_statement() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(input logic a); endmodule",
            "module top; logic x, y; leaf u1(.a(x)), u2(.a(y)); endmodule",
        ],
        "top",
    );

    let top_id = tree.top.expect("top should exist");
    let top_children = child_instance_names(&tree, top_id);
    assert_eq!(
        top_children,
        vec!["u1", "u2"],
        "multi-instance statement produces two distinct children"
    );
    assert_eq!(tree.instances.len(), 3);
    assert_no_duplicate_origins(&tree);
}

#[test]
fn cycle_detection() {
    let (_, tree) = elab_tree(
        &["module a; b b1(); endmodule", "module b; a a1(); endmodule"],
        "a",
    );

    let recursion: Vec<_> = tree
        .diagnostics
        .iter()
        .filter(|d| matches!(d, crate::elaboration::ElabDiag::RecursionLimit { .. }))
        .collect();
    assert!(
        !recursion.is_empty(),
        "expected recursion limit diag for cycle"
    );

    let top_id = tree.top.expect("top should exist");
    let top_children = child_instance_names(&tree, top_id);
    assert_eq!(top_children, vec!["b1"], "top has one child before cycle");
}

#[test]
fn multi_file_instantiation() {
    let diags = elab_diags(
        &[
            "module leaf(input logic [7:0] data); endmodule",
            "module top; logic [7:0] d; leaf u1(.data(d)); endmodule",
        ],
        "top",
    );
    let elab_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.code.namespace == "lyra.elab")
        .collect();
    assert!(
        elab_diags.is_empty(),
        "multi-file should have no elab errors, got: {elab_diags:?}"
    );
}
