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
fn param_default_sees_override_of_earlier_param() {
    let (_, tree) = elab_tree(
        &[
            "module leaf #(parameter int W = 8, parameter int DEPTH = 1 << W)(); endmodule",
            "module top; leaf #(.W(16)) u1(); endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    let vals = child_param_values(&tree, top_id, "u1");
    assert_eq!(vals.len(), 2);
    assert_eq!(vals[0], ConstInt::Known(16), "W should be overridden to 16");
    assert_eq!(
        vals[1],
        ConstInt::Known(65536),
        "DEPTH should be 1<<16 = 65536"
    );
}

#[test]
fn param_forward_reference_errors() {
    let (_, tree) = elab_tree(
        &[
            "module leaf #(parameter int A = B, parameter int B = 8)(); endmodule",
            "module top; leaf u1(); endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    let vals = child_param_values(&tree, top_id, "u1");
    assert_eq!(vals.len(), 2);
    assert!(
        matches!(vals[0], ConstInt::Error(_)),
        "A should be error (forward ref to B), got: {:?}",
        vals[0]
    );
    assert_eq!(vals[1], ConstInt::Known(8), "B should be 8");
}

#[test]
fn param_env_dedup() {
    let (_, tree) = elab_tree(
        &[
            "module leaf #(parameter int W = 8)(); endmodule",
            "module top; leaf u1(); leaf u2(); endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    let ids: Vec<_> = tree
        .inst(top_id)
        .children
        .iter()
        .filter_map(|ck| match ck {
            ElabNodeId::Inst(iid) => Some(tree.inst(*iid).param_env),
            ElabNodeId::GenScope(_) => None,
        })
        .collect();
    assert_eq!(ids.len(), 2);
    assert_eq!(
        ids[0], ids[1],
        "identical param envs should share the same ParamEnvId"
    );
}

#[test]
fn override_uses_instantiator_param() {
    let (_, tree) = elab_tree(
        &[
            "module inner #(parameter int W = 1)(); endmodule",
            "module outer #(parameter int X = 4)(); inner #(.W(X)) u1(); endmodule",
        ],
        "outer",
    );
    let top_id = tree.top.expect("top should exist");
    let all = all_instance_names_under(&tree, top_id);
    assert_eq!(all, vec!["u1"]);
    let vals = child_param_values(&tree, top_id, "u1");
    assert_eq!(vals.len(), 1);
    assert_eq!(
        vals[0],
        ConstInt::Known(4),
        "W should be 4 from outer's X default"
    );
}

#[test]
fn override_uses_localparam() {
    let (_, tree) = elab_tree(
        &[
            "module inner #(parameter int W = 1)(); endmodule",
            "module outer; localparam int Y = 16; inner #(.W(Y)) u1(); endmodule",
        ],
        "outer",
    );
    let top_id = tree.top.expect("top should exist");
    let vals = child_param_values(&tree, top_id, "u1");
    assert_eq!(vals.len(), 1);
    assert_eq!(
        vals[0],
        ConstInt::Known(16),
        "W should be 16 from localparam Y"
    );
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
