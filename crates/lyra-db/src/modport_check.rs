use lyra_ast::AstNode;
use lyra_semantic::diagnostic::{
    DiagSpan, PrototypeMismatchDetail, SemanticDiag, SemanticDiagKind,
};
use lyra_semantic::modport_def::{ModportTfEntry, ModportTfForm};
use lyra_semantic::types::{ConstInt, Ty};

use crate::callable_queries::{CallableRef, callable_signature, extract_tf_ports};
use crate::const_eval::{ConstExprRef, eval_const_int};
use crate::modport_queries::ModportRef;
use crate::module_sig::CallableKind;
use crate::pipeline::{ast_id_map, parse_file};
use crate::semantic::{def_index_file, resolve_index_file};
use crate::source_file_by_id;

/// Validate prototype signatures in a modport's TF entries (Salsa-tracked).
///
/// Produces diagnostics for each prototype-form TF entry whose signature
/// does not match the actual callable declaration. Separate from `modport_sem`
/// to keep view construction independent of validation.
#[salsa::tracked(return_ref)]
pub fn modport_prototype_diagnostics<'db>(
    db: &'db dyn salsa::Database,
    mref: ModportRef<'db>,
) -> Box<[SemanticDiag]> {
    let unit = mref.unit(db);
    let modport_id = mref.modport_id(db);

    let iface_def_id = modport_id.owner.global_def();
    let file_id = iface_def_id.ast_id().file();
    let Some(src) = source_file_by_id(db, unit, file_id) else {
        return Box::new([]);
    };
    let def = def_index_file(db, src);

    let Some(modport_def) = def.modport_def(modport_id) else {
        return Box::new([]);
    };

    let Some(entry) = def.def_entry(iface_def_id) else {
        return Box::new([]);
    };
    let lyra_semantic::def_entry::DefScope::Owned(iface_scope) = entry.scope else {
        return Box::new([]);
    };

    let parse = parse_file(db, src);
    let map = ast_id_map(db, src);
    let resolve = resolve_index_file(db, src, unit);

    let resolve_ty = |ts: &lyra_ast::TypeSpec| -> Ty {
        crate::callable_queries::resolve_typespec_ty_with(
            db,
            unit,
            map,
            ts,
            resolve,
            &|db, unit, target| {
                let sym_ref = crate::type_queries::SymbolRef::new(db, unit, target);
                crate::type_queries::type_of_symbol(db, sym_ref)
            },
        )
    };

    let eval = |site: lyra_semantic::Site| -> ConstInt {
        let expr_ref = ConstExprRef::new(db, unit, site);
        eval_const_int(db, expr_ref)
    };

    let mut diags = Vec::new();

    for tf in &*modport_def.tf_entries {
        if tf.form != ModportTfForm::Prototype {
            continue;
        }
        let Some(sym_id) = def.scopes.resolve(
            &def.symbols,
            iface_scope,
            lyra_semantic::symbols::Namespace::Value,
            &tf.name,
        ) else {
            continue;
        };
        let sym = def.symbols.get(sym_id);
        if !matches!(
            sym.kind,
            lyra_semantic::symbols::SymbolKind::Function | lyra_semantic::symbols::SymbolKind::Task
        ) {
            continue;
        }
        validate_one_prototype(
            db,
            unit,
            tf,
            file_id,
            sym_id,
            parse,
            map,
            &resolve_ty,
            &eval,
            &mut diags,
        );
    }

    diags.into_boxed_slice()
}

#[allow(clippy::too_many_arguments)]
fn validate_one_prototype(
    db: &dyn salsa::Database,
    unit: crate::CompilationUnit,
    tf: &ModportTfEntry,
    file_id: lyra_source::FileId,
    sym_id: lyra_semantic::symbols::SymbolId,
    parse: &lyra_parser::Parse,
    map: &lyra_ast::AstIdMap,
    resolve_ty: &dyn Fn(&lyra_ast::TypeSpec) -> Ty,
    eval: &dyn Fn(lyra_semantic::Site) -> ConstInt,
    diags: &mut Vec<SemanticDiag>,
) {
    let Some(proto_node) = map.get_node(&parse.syntax(), tf.port_site) else {
        return;
    };
    let Some(entry_node) = lyra_ast::ModportTfPortEntry::cast(proto_node) else {
        return;
    };

    let (proto_kind, proto_return_ty, proto_ports) =
        if let Some(task_proto) = entry_node.task_prototype() {
            let ports = extract_tf_ports(resolve_ty, task_proto.tf_port_decls());
            (CallableKind::Task, Ty::Void, ports)
        } else if let Some(func_proto) = entry_node.function_prototype() {
            let return_ty = func_proto
                .type_spec()
                .map_or_else(Ty::simple_logic, |ts| resolve_ty(&ts));
            let ports = extract_tf_ports(resolve_ty, func_proto.tf_port_decls());
            (CallableKind::Function, return_ty, ports)
        } else {
            return;
        };

    let proto_return_ty = lyra_semantic::normalize_ty(&proto_return_ty, eval);
    let proto_ports: Vec<_> = proto_ports
        .into_iter()
        .map(|mut p| {
            p.ty = lyra_semantic::normalize_ty(&p.ty, eval);
            p
        })
        .collect();

    let gsym = lyra_semantic::symbols::GlobalSymbolId {
        file: file_id,
        local: sym_id,
    };
    let callable_ref = CallableRef::new(db, unit, gsym);
    let actual_sig = callable_signature(db, callable_ref);
    let actual_return_ty = lyra_semantic::normalize_ty(&actual_sig.return_ty, eval);
    let actual_ports: Vec<_> = actual_sig
        .ports
        .iter()
        .map(|p| (p.direction, lyra_semantic::normalize_ty(&p.ty, eval)))
        .collect();

    compare_signatures(
        tf,
        proto_kind,
        &proto_return_ty,
        &proto_ports,
        actual_sig.kind,
        &actual_return_ty,
        &actual_ports,
        diags,
    );
}

#[allow(clippy::too_many_arguments)]
fn compare_signatures(
    tf: &ModportTfEntry,
    proto_kind: CallableKind,
    proto_return_ty: &Ty,
    proto_ports: &[crate::module_sig::TfPortSig],
    actual_kind: CallableKind,
    actual_return_ty: &Ty,
    actual_ports: &[(lyra_ast::PortDirection, Ty)],
    diags: &mut Vec<SemanticDiag>,
) {
    let tf_primary = DiagSpan::Site(tf.port_site);
    let tf_label = DiagSpan::Name(tf.name_span);

    if proto_kind != actual_kind {
        return;
    }

    if proto_kind == CallableKind::Function && proto_return_ty != actual_return_ty {
        diags.push(SemanticDiag {
            kind: SemanticDiagKind::PrototypeMismatch {
                name: tf.name.clone(),
                mismatch: PrototypeMismatchDetail::ReturnType,
            },
            primary: tf_primary,
            label: Some(tf_label),
        });
    }

    if proto_ports.len() != actual_ports.len() {
        diags.push(SemanticDiag {
            kind: SemanticDiagKind::PrototypeMismatch {
                name: tf.name.clone(),
                mismatch: PrototypeMismatchDetail::PortCount {
                    proto: proto_ports.len(),
                    actual: actual_ports.len(),
                },
            },
            primary: tf_primary,
            label: Some(tf_label),
        });
        return;
    }

    for (i, (proto_port, (actual_dir, actual_ty))) in
        proto_ports.iter().zip(actual_ports.iter()).enumerate()
    {
        if proto_port.direction != *actual_dir {
            diags.push(SemanticDiag {
                kind: SemanticDiagKind::PrototypeMismatch {
                    name: tf.name.clone(),
                    mismatch: PrototypeMismatchDetail::PortDirection {
                        index: i,
                        proto_dir: proto_port.direction,
                        actual_dir: *actual_dir,
                    },
                },
                primary: tf_primary,
                label: Some(tf_label),
            });
        }
        if proto_port.ty != *actual_ty {
            diags.push(SemanticDiag {
                kind: SemanticDiagKind::PrototypeMismatch {
                    name: tf.name.clone(),
                    mismatch: PrototypeMismatchDetail::PortType { index: i },
                },
                primary: tf_primary,
                label: Some(tf_label),
            });
        }
    }
}
