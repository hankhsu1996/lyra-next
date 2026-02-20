mod generate;
mod identity;
mod instances;
mod interface;
mod params;
mod ports;
mod signature;

use lyra_diag::DiagnosticCode;
use lyra_semantic::types::ConstInt;
use smol_str::SmolStr;

use super::*;

use crate::elaboration::{ElabNodeId, ElabTree, InstId};

pub(super) fn elab_diags(files: &[&str], top: &str) -> Vec<lyra_diag::Diagnostic> {
    let db = LyraDatabase::default();
    let mut source_files = Vec::new();
    for (i, src) in files.iter().enumerate() {
        source_files.push(new_file(&db, i as u32, src));
    }
    let unit = new_compilation_unit(&db, source_files);
    let top_mod = TopModule::new(&db, unit, SmolStr::new(top));
    elab_diagnostics(&db, top_mod).clone()
}

pub(super) fn elab_tree(files: &[&str], top: &str) -> (LyraDatabase, ElabTree) {
    let db = LyraDatabase::default();
    let mut source_files = Vec::new();
    for (i, src) in files.iter().enumerate() {
        source_files.push(new_file(&db, i as u32, src));
    }
    let unit = new_compilation_unit(&db, source_files);
    let top_mod = TopModule::new(&db, unit, SmolStr::new(top));
    let tree = elaborate_top(&db, top_mod).clone();
    (db, tree)
}

pub(super) fn sig_ports(src: &str, module_name: &str) -> Vec<String> {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, src);
    let unit = single_file_unit(&db, file);
    let global = crate::semantic::global_def_index(&db, unit);
    let def_id = global
        .resolve_module(module_name)
        .expect("module not found");
    let mref = crate::elab_queries::DesignUnitRef::new(&db, unit, def_id);
    let sig = design_unit_signature(&db, mref);
    sig.ports
        .iter()
        .map(|p| format!("{}: {}", p.name, p.ty.pretty()))
        .collect()
}

pub(super) fn child_instance_names(tree: &ElabTree, id: InstId) -> Vec<String> {
    tree.inst(id)
        .children
        .iter()
        .filter_map(|ck| match ck {
            ElabNodeId::Inst(iid) => Some(tree.inst(*iid).instance_name.to_string()),
            ElabNodeId::GenScope(_) => None,
        })
        .collect()
}

pub(super) fn all_instance_names_under(tree: &ElabTree, id: InstId) -> Vec<String> {
    let mut names = Vec::new();
    collect_inst_names(tree, ElabNodeId::Inst(id), &mut names);
    names
}

pub(super) fn collect_inst_names(tree: &ElabTree, node: ElabNodeId, names: &mut Vec<String>) {
    let children: &[ElabNodeId] = match node {
        ElabNodeId::Inst(id) => &tree.inst(id).children,
        ElabNodeId::GenScope(id) => &tree.gen_scope(id).children,
    };
    for child in children {
        if let ElabNodeId::Inst(cid) = child {
            names.push(tree.inst(*cid).instance_name.to_string());
        }
        collect_inst_names(tree, *child, names);
    }
}

pub(super) fn find_child_inst_by_name(tree: &ElabTree, parent: InstId, name: &str) -> InstId {
    for child in &tree.inst(parent).children {
        if let ElabNodeId::Inst(cid) = child
            && tree.inst(*cid).instance_name == name
        {
            return *cid;
        }
    }
    panic!("child instance '{name}' not found under parent");
}

pub(super) fn child_param_values(
    tree: &ElabTree,
    parent: InstId,
    child_name: &str,
) -> Vec<ConstInt> {
    let cid = find_child_inst_by_name(tree, parent, child_name);
    let env_id = tree.inst(cid).param_env;
    tree.envs.values(env_id).to_vec()
}

pub(super) fn assert_no_duplicate_origins(tree: &ElabTree) {
    use std::collections::HashSet;
    for inst in &tree.instances {
        let mut seen_inst_origins = HashSet::new();
        let mut seen_gen_origins = HashSet::new();
        for child in &inst.children {
            match child {
                ElabNodeId::Inst(cid) => {
                    let origin = tree.inst(*cid).origin;
                    assert!(
                        seen_inst_origins.insert(origin),
                        "duplicate InstOrigin under instance '{}'",
                        inst.instance_name
                    );
                }
                ElabNodeId::GenScope(gid) => {
                    let origin = tree.gen_scope(*gid).origin.clone();
                    assert!(
                        seen_gen_origins.insert(origin),
                        "duplicate GenScopeOrigin under instance '{}'",
                        inst.instance_name
                    );
                }
            }
        }
    }
}
