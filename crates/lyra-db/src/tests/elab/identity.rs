use super::*;

#[test]
fn two_instances_of_same_module_children_disjoint() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module mid(); leaf u(); endmodule",
            "module top; mid m1(); mid m2(); endmodule",
        ],
        "top",
    );

    let top_id = tree.top.expect("top should exist");
    let m1_id = find_child_inst_by_name(&tree, top_id, "m1");
    let m2_id = find_child_inst_by_name(&tree, top_id, "m2");

    let m1_children = child_instance_names(&tree, m1_id);
    let m2_children = child_instance_names(&tree, m2_id);
    assert_eq!(m1_children, vec!["u"], "m1 has its own leaf");
    assert_eq!(m2_children, vec!["u"], "m2 has its own leaf");

    let m1_leaf = find_child_inst_by_name(&tree, m1_id, "u");
    let m2_leaf = find_child_inst_by_name(&tree, m2_id, "u");
    assert_ne!(m1_leaf, m2_leaf, "leaves are distinct nodes");
    assert_eq!(tree.inst(m1_leaf).parent, Some(ElabNodeId::Inst(m1_id)));
    assert_eq!(tree.inst(m2_leaf).parent, Some(ElabNodeId::Inst(m2_id)));

    assert_eq!(tree.instances.len(), 5);
    assert_no_duplicate_origins(&tree);
}

#[test]
fn same_module_with_generate_scopes_disjoint() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module mid(); if (1) begin leaf u(); end endmodule",
            "module top; mid m1(); mid m2(); endmodule",
        ],
        "top",
    );

    let top_id = tree.top.expect("top should exist");
    let m1_all = all_instance_names_under(&tree, find_child_inst_by_name(&tree, top_id, "m1"));
    let m2_all = all_instance_names_under(&tree, find_child_inst_by_name(&tree, top_id, "m2"));
    assert_eq!(m1_all, vec!["u"]);
    assert_eq!(m2_all, vec!["u"]);

    assert_eq!(tree.instances.len(), 5);
    assert_no_duplicate_origins(&tree);
}

#[test]
fn three_level_diamond() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module mid(); leaf u(); endmodule",
            "module top; mid m1(); mid m2(); endmodule",
        ],
        "top",
    );

    assert_eq!(tree.instances.len(), 5, "top + m1 + m2 + 2 leaves");

    let top_id = tree.top.expect("top should exist");
    let m1_id = find_child_inst_by_name(&tree, top_id, "m1");
    let m2_id = find_child_inst_by_name(&tree, top_id, "m2");
    let m1_leaf = find_child_inst_by_name(&tree, m1_id, "u");
    let m2_leaf = find_child_inst_by_name(&tree, m2_id, "u");

    assert_eq!(tree.inst(m1_leaf).parent, Some(ElabNodeId::Inst(m1_id)));
    assert_eq!(tree.inst(m2_leaf).parent, Some(ElabNodeId::Inst(m2_id)));
    assert_no_duplicate_origins(&tree);
}

#[test]
fn multi_instance_statement_identity() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module mid(); leaf u(); endmodule",
            "module top; mid m1(), m2(); endmodule",
        ],
        "top",
    );

    let top_id = tree.top.expect("top should exist");
    let m1_id = find_child_inst_by_name(&tree, top_id, "m1");
    let m2_id = find_child_inst_by_name(&tree, top_id, "m2");

    let m1_children = child_instance_names(&tree, m1_id);
    let m2_children = child_instance_names(&tree, m2_id);
    assert_eq!(m1_children, vec!["u"]);
    assert_eq!(m2_children, vec!["u"]);
    assert_eq!(tree.instances.len(), 5);

    let m1_origin = tree.inst(m1_id).origin;
    let m2_origin = tree.inst(m2_id).origin;
    assert_eq!(
        m1_origin.inst_stmt_ast, m2_origin.inst_stmt_ast,
        "same statement"
    );
    assert_ne!(
        m1_origin.inst_ordinal, m2_origin.inst_ordinal,
        "different ordinals"
    );
    assert_no_duplicate_origins(&tree);
}

#[test]
fn source_order_children_lock() {
    let (_, tree1) = elab_tree(
        &[
            "module a(); endmodule",
            "module b(); endmodule",
            "module c(); endmodule",
            "module top; a u_a(); b u_b(); c u_c(); endmodule",
        ],
        "top",
    );
    let top1 = tree1.top.expect("top should exist");
    let names1 = child_instance_names(&tree1, top1);
    assert_eq!(names1, vec!["u_a", "u_b", "u_c"]);

    let (_, tree2) = elab_tree(
        &[
            "module a(); endmodule",
            "module b(); endmodule",
            "module c(); endmodule",
            "module top; a u_a(); b u_b(); c u_c(); endmodule",
        ],
        "top",
    );
    let top2 = tree2.top.expect("top should exist");
    let names2 = child_instance_names(&tree2, top2);
    assert_eq!(names1, names2, "deterministic ordering across runs");
}

#[test]
fn generate_for_at_limit_emits_diagnostic() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module top; for (genvar i = 0; i < 100; i = i + 1) begin leaf u(); end endmodule",
        ],
        "top",
    );

    let limit_diags: Vec<_> = tree
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d,
                crate::elaboration::ElabDiag::GenerateIterationLimit { .. }
            )
        })
        .collect();
    assert_eq!(
        limit_diags.len(),
        1,
        "expected 1 iteration limit diagnostic"
    );

    let top_id = tree.top.expect("top should exist");
    let all = all_instance_names_under(&tree, top_id);
    assert_eq!(
        all.len(),
        32,
        "expected exactly 32 iterations (test cap) preserved as children"
    );
}

#[test]
fn generate_for_exact_cap_no_false_positive() {
    // Loop with trip count == test cap (32). Should NOT emit a diagnostic.
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module top; for (genvar i = 0; i < 32; i = i + 1) begin leaf u(); end endmodule",
        ],
        "top",
    );

    let limit_diags: Vec<_> = tree
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d,
                crate::elaboration::ElabDiag::GenerateIterationLimit { .. }
            )
        })
        .collect();
    assert_eq!(
        limit_diags.len(),
        0,
        "loop terminating exactly at cap should not emit iteration limit diagnostic"
    );

    let top_id = tree.top.expect("top should exist");
    let all = all_instance_names_under(&tree, top_id);
    assert_eq!(all.len(), 32, "all 32 iterations should be present");
}
