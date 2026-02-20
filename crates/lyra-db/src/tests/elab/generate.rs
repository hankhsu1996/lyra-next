use super::*;

#[test]
fn generate_if_true_branch() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module top; if (1) begin leaf u1(); end else begin leaf u2(); end endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    let all = all_instance_names_under(&tree, top_id);
    assert!(
        all.contains(&"u1".to_string()),
        "true branch should be elaborated: {all:?}"
    );
    assert!(
        !all.contains(&"u2".to_string()),
        "false branch should not be elaborated: {all:?}"
    );
    assert_no_duplicate_origins(&tree);
}

#[test]
fn generate_if_false_branch() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module top; if (0) begin leaf u1(); end else begin leaf u2(); end endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    let all = all_instance_names_under(&tree, top_id);
    assert!(
        !all.contains(&"u1".to_string()),
        "true branch should not be elaborated: {all:?}"
    );
    assert!(
        all.contains(&"u2".to_string()),
        "false branch should be elaborated: {all:?}"
    );
}

#[test]
fn generate_for_four_iterations() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module top; for (genvar i = 0; i < 4; i = i + 1) begin leaf u(); end endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    let all = all_instance_names_under(&tree, top_id);
    assert_eq!(all.len(), 4, "expected 4 instances from for-loop: {all:?}");
    assert_no_duplicate_origins(&tree);
}

#[test]
fn generate_for_zero_iterations() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module top; for (genvar i = 0; i < 0; i = i + 1) begin leaf u(); end endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    let all = all_instance_names_under(&tree, top_id);
    assert!(
        all.is_empty(),
        "expected 0 instances for zero-iteration loop: {all:?}"
    );
}

#[test]
fn generate_case_match() {
    let (_, tree) = elab_tree(
        &[
            "module leaf_a(); endmodule",
            "module leaf_b(); endmodule",
            "module top; case (1) 0: begin leaf_a u1(); end 1: begin leaf_b u2(); end endcase endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    let all = all_instance_names_under(&tree, top_id);
    assert!(
        all.contains(&"u2".to_string()),
        "case 1 should match: {all:?}"
    );
    assert!(
        !all.contains(&"u1".to_string()),
        "case 0 should not match: {all:?}"
    );
    assert_no_duplicate_origins(&tree);
}

#[test]
fn generate_case_default() {
    let (_, tree) = elab_tree(
        &[
            "module leaf_a(); endmodule",
            "module leaf_b(); endmodule",
            "module top; case (99) 0: begin leaf_a u1(); end default: begin leaf_b u2(); end endcase endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    let all = all_instance_names_under(&tree, top_id);
    assert!(
        all.contains(&"u2".to_string()),
        "default should match: {all:?}"
    );
    assert!(
        !all.contains(&"u1".to_string()),
        "case 0 should not match: {all:?}"
    );
}

#[test]
fn param_dependent_generate_if() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module inner #(parameter int USE_A = 1)(); if (USE_A) begin leaf u_a(); end else begin leaf u_b(); end endmodule",
            "module top; inner u1(); endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    let all = all_instance_names_under(&tree, top_id);
    assert!(all.contains(&"u1".to_string()), "inner instance: {all:?}");
    assert!(
        all.contains(&"u_a".to_string()),
        "USE_A=1 default should pick true branch: {all:?}"
    );
}

#[test]
fn nested_generate_for_inside_if() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module top; if (1) begin for (genvar i = 0; i < 2; i = i + 1) begin leaf u(); end end endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    let all = all_instance_names_under(&tree, top_id);
    assert_eq!(
        all.len(),
        2,
        "expected 2 instances from nested for-in-if: {all:?}"
    );
    assert_no_duplicate_origins(&tree);
}

#[test]
fn for_loop_bound_from_param() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module top #(parameter int N = 4)(); for (genvar i = 0; i < N; i = i + 1) begin leaf u(); end endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    let all = all_instance_names_under(&tree, top_id);
    assert_eq!(
        all.len(),
        4,
        "expected 4 instances from for-loop with param N=4: {all:?}"
    );
}

#[test]
fn for_loop_bound_from_param_expression() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module top #(parameter int N = 4)(); for (genvar i = 0; i < N - 1; i = i + 1) begin leaf u(); end endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    let all = all_instance_names_under(&tree, top_id);
    assert_eq!(
        all.len(),
        3,
        "expected 3 instances from for-loop with param N-1=3: {all:?}"
    );
}

#[test]
fn for_loop_flipped_condition() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module top; for (genvar i = 0; 4 > i; i = i + 1) begin leaf u(); end endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    let all = all_instance_names_under(&tree, top_id);
    assert_eq!(
        all.len(),
        4,
        "expected 4 instances from flipped condition '4 > i': {all:?}"
    );
}

#[test]
fn nested_for_loops_see_both_genvars() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module top; for (genvar i = 0; i < 2; i = i + 1) begin for (genvar j = 0; j < 3; j = j + 1) begin leaf u(); end end endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    let all = all_instance_names_under(&tree, top_id);
    assert_eq!(
        all.len(),
        6,
        "expected 2*3=6 instances from nested for-loops: {all:?}"
    );
    assert_no_duplicate_origins(&tree);
}

#[test]
fn genvar_dependent_generate_if_inside_for() {
    let (_, tree) = elab_tree(
        &[
            "module leaf_a(); endmodule",
            "module leaf_b(); endmodule",
            "module top; for (genvar i = 0; i < 3; i = i + 1) begin if (i) begin leaf_a u_a(); end else begin leaf_b u_b(); end end endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    let all = all_instance_names_under(&tree, top_id);
    let a_count = all.iter().filter(|n| *n == "u_a").count();
    let b_count = all.iter().filter(|n| *n == "u_b").count();
    assert_eq!(a_count, 2, "i=1,2 pick true branch: {all:?}");
    assert_eq!(b_count, 1, "i=0 picks false branch: {all:?}");
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
