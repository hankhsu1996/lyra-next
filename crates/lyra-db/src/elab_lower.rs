use std::collections::HashSet;

use lyra_ast::{AstNode, ErasedAstId, Expr, HasSyntax, SyntaxBinaryOp};
use lyra_semantic::symbols::GlobalDefId;
use lyra_semantic::types::{InterfaceType, Ty};
use lyra_source::{FileId, Span, TextRange};
use smol_str::SmolStr;

use crate::elab_queries::{TopModule, elaborate_top};
use crate::elaboration::ElabDiag;
use crate::expr_queries::{ExprRef, type_of_expr};
use crate::module_sig::DesignUnitSig;
use crate::pipeline::preprocess_file;
use crate::{CompilationUnit, source_file_by_id};

pub(crate) enum PortActual {
    Expr(ErasedAstId),
    Resolved(Ty),
}

pub(crate) struct PortBinding {
    pub formal_idx: u32,
    pub actual: PortActual,
    pub conn_span: Span,
}

pub(crate) fn resolve_port_connections(
    port_list: &lyra_ast::InstancePortList,
    sig: &DesignUnitSig,
    module_name: &SmolStr,
    file_id: FileId,
    inst_range: TextRange,
    id_map: Option<&lyra_ast::AstIdMap>,
    diags: &mut Vec<ElabDiag>,
) -> Vec<PortBinding> {
    let ports: Vec<_> = port_list.ports().collect();
    if ports.is_empty() {
        return Vec::new();
    }

    let inst_span = Span {
        file: file_id,
        range: inst_range,
    };

    let has_named = ports.iter().any(|p| p.is_named());
    let has_wildcard = ports.iter().any(|p| p.is_wildcard());

    if has_named || has_wildcard {
        resolve_named_ports(
            &ports,
            sig,
            module_name,
            inst_span,
            has_wildcard,
            id_map,
            diags,
        )
    } else {
        resolve_positional_ports(&ports, sig, module_name, inst_span, id_map, diags)
    }
}

fn resolve_named_ports(
    ports: &[lyra_ast::InstancePort],
    sig: &DesignUnitSig,
    module_name: &SmolStr,
    inst_span: Span,
    has_wildcard: bool,
    id_map: Option<&lyra_ast::AstIdMap>,
    diags: &mut Vec<ElabDiag>,
) -> Vec<PortBinding> {
    let file_id = inst_span.file;
    let mut bindings = Vec::new();
    let mut connected: HashSet<u32> = HashSet::new();
    for port in ports {
        if port.is_wildcard() {
            continue;
        }
        let Some(port_name_tok) = port.port_name() else {
            continue;
        };
        let port_name_str = port_name_tok.text();
        let conn_span = Span {
            file: file_id,
            range: port.text_range(),
        };

        match sig.port_by_name(port_name_str) {
            Some((idx, _)) => {
                if !connected.insert(idx) {
                    diags.push(ElabDiag::DuplicatePortConn {
                        port: SmolStr::new(port_name_str),
                        span: conn_span,
                    });
                } else if let Some(actual_node) = port.actual_expr()
                    && let Some(ast_id) = id_map.and_then(|m| m.erased_ast_id(&actual_node))
                {
                    bindings.push(PortBinding {
                        formal_idx: idx,
                        actual: PortActual::Expr(ast_id),
                        conn_span,
                    });
                }
            }
            None => {
                diags.push(ElabDiag::UnknownPort {
                    port: SmolStr::new(port_name_str),
                    module: module_name.clone(),
                    span: conn_span,
                });
            }
        }
    }

    for (i, port_sig) in sig.ports.iter().enumerate() {
        if !connected.contains(&(i as u32)) && !has_wildcard {
            diags.push(ElabDiag::MissingPortConn {
                port: port_sig.name.clone(),
                module: module_name.clone(),
                span: inst_span,
            });
        }
    }
    bindings
}

fn resolve_positional_ports(
    ports: &[lyra_ast::InstancePort],
    sig: &DesignUnitSig,
    module_name: &SmolStr,
    inst_span: Span,
    id_map: Option<&lyra_ast::AstIdMap>,
    diags: &mut Vec<ElabDiag>,
) -> Vec<PortBinding> {
    let actual_count = ports.len();
    let formal_count = sig.ports.len();
    let mut bindings = Vec::new();

    if actual_count > formal_count {
        diags.push(ElabDiag::TooManyPositionalPorts {
            expected: formal_count,
            got: actual_count,
            span: inst_span,
        });
    }

    let matched = actual_count.min(formal_count);
    for (idx, port) in ports.iter().take(matched).enumerate() {
        if let Some(actual_node) = port.actual_expr()
            && let Some(ast_id) = id_map.and_then(|m| m.erased_ast_id(&actual_node))
        {
            bindings.push(PortBinding {
                formal_idx: idx as u32,
                actual: PortActual::Expr(ast_id),
                conn_span: Span {
                    file: inst_span.file,
                    range: port.text_range(),
                },
            });
        }
    }

    if actual_count < formal_count {
        for port_sig in &sig.ports[actual_count..] {
            diags.push(ElabDiag::MissingPortConn {
                port: port_sig.name.clone(),
                module: module_name.clone(),
                span: inst_span,
            });
        }
    }
    bindings
}

pub(crate) fn check_modport_conflicts(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    bindings: &[PortBinding],
    sig: &DesignUnitSig,
    target_def: GlobalDefId,
    diags: &mut Vec<ElabDiag>,
) {
    for binding in bindings {
        let Some(formal) = sig.ports.get(binding.formal_idx as usize) else {
            continue;
        };
        let (formal_iface, formal_mp) = match &formal.ty {
            Ty::Interface(InterfaceType {
                iface,
                modport: Some(mp),
            }) => (*iface, *mp),
            _ => continue,
        };

        let actual_ty = match &binding.actual {
            PortActual::Expr(ast_id) => type_of_expr(db, ExprRef::new(db, unit, *ast_id)).ty,
            PortActual::Resolved(ty) => ty.clone(),
        };
        if matches!(actual_ty, Ty::Error) {
            continue;
        }
        let (actual_iface, actual_mp) = match &actual_ty {
            Ty::Interface(InterfaceType {
                iface,
                modport: Some(mp),
            }) => (*iface, *mp),
            _ => continue,
        };
        if actual_iface != formal_iface {
            continue;
        }
        if actual_mp != formal_mp {
            diags.push(ElabDiag::ModportConflict {
                target_def,
                formal_port_ordinal: binding.formal_idx,
                formal_mp,
                actual_mp,
                span: binding.conn_span,
            });
        }
    }
}

/// Lower elaboration diagnostics into structured `lyra_diag::Diagnostic` (Salsa-cached).
#[salsa::tracked(return_ref)]
pub fn elab_diagnostics<'db>(
    db: &'db dyn salsa::Database,
    top: TopModule<'db>,
) -> Vec<lyra_diag::Diagnostic> {
    let unit = top.unit(db);
    let tree = elaborate_top(db, top);
    let mut diags = Vec::new();

    for elab_diag in &tree.diagnostics {
        diags.push(lower_elab_diag(db, unit, elab_diag));
    }

    diags
}

fn lower_elab_diag(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    diag: &ElabDiag,
) -> lyra_diag::Diagnostic {
    use lyra_diag::{
        Arg, Diagnostic, DiagnosticCode as C, Label, LabelKind, Message, MessageId as M, Severity,
    };

    let span = map_elab_span(db, unit, *diag.span());

    if let ElabDiag::ModportConflict {
        target_def,
        formal_port_ordinal,
        formal_mp,
        actual_mp,
        ..
    } = diag
    {
        let sig = crate::elab_queries::design_unit_signature(
            db,
            crate::elab_queries::DesignUnitRef::new(db, unit, *target_def),
        );
        let port_name = sig
            .ports
            .get(*formal_port_ordinal as usize)
            .map(|p| p.name.clone())
            .unwrap_or_default();
        let formal_name = crate::semantic::modport_name(db, unit, *formal_mp);
        let actual_name = crate::semantic::modport_name(db, unit, *actual_mp);
        let msg = Message::new(
            M::ModportConflict,
            vec![
                Arg::Name(port_name),
                Arg::Name(formal_name),
                Arg::Name(actual_name),
            ],
        );
        return Diagnostic::new(Severity::Error, C::MODPORT_CONFLICT, msg.clone()).with_label(
            Label {
                kind: LabelKind::Primary,
                span,
                message: msg,
            },
        );
    }

    let (severity, code, msg) = elab_diag_code_msg(diag);

    Diagnostic::new(severity, code, msg.clone()).with_label(Label {
        kind: LabelKind::Primary,
        span,
        message: msg,
    })
}

fn elab_diag_code_msg(
    diag: &ElabDiag,
) -> (
    lyra_diag::Severity,
    lyra_diag::DiagnosticCode,
    lyra_diag::Message,
) {
    use lyra_diag::{Arg, DiagnosticCode as C, Message, MessageId as M, Severity};
    let e = Severity::Error;
    let n = |d: &ElabDiag| match d {
        ElabDiag::UnresolvedModuleInst { name, .. }
        | ElabDiag::NotInstantiable { name, .. }
        | ElabDiag::UnknownParam { name, .. }
        | ElabDiag::ParamNotConst { name, .. }
        | ElabDiag::DuplicateParamOverride { name, .. } => name.clone(),
        ElabDiag::DuplicatePortConn { port, .. } => port.clone(),
        _ => SmolStr::default(),
    };
    match diag {
        ElabDiag::UnresolvedModuleInst { .. } => (
            e,
            C::UNRESOLVED_MODULE_INST,
            Message::new(M::UnresolvedModuleInst, vec![Arg::Name(n(diag))]),
        ),
        ElabDiag::NotInstantiable { .. } => (
            e,
            C::NOT_INSTANTIABLE,
            Message::new(M::NotInstantiable, vec![Arg::Name(n(diag))]),
        ),
        ElabDiag::UnknownPort { port, module, .. } => (
            e,
            C::UNKNOWN_PORT,
            Message::new(
                M::UnknownPort,
                vec![Arg::Name(port.clone()), Arg::Name(module.clone())],
            ),
        ),
        ElabDiag::DuplicatePortConn { .. } => (
            e,
            C::DUPLICATE_PORT_CONN,
            Message::new(M::DuplicatePortConn, vec![Arg::Name(n(diag))]),
        ),
        ElabDiag::TooManyPositionalPorts { expected, got, .. } => (
            e,
            C::TOO_MANY_POSITIONAL_PORTS,
            Message::new(
                M::TooManyPositionalPorts,
                vec![Arg::Count(*expected), Arg::Count(*got)],
            ),
        ),
        ElabDiag::MissingPortConn { port, module, .. } => (
            Severity::Warning,
            C::MISSING_PORT_CONN,
            Message::new(
                M::MissingPortConn,
                vec![Arg::Name(port.clone()), Arg::Name(module.clone())],
            ),
        ),
        ElabDiag::RecursionLimit { .. } => (
            e,
            C::ELAB_RECURSION_LIMIT,
            Message::simple(M::ElabRecursionLimit),
        ),
        ElabDiag::UnknownParam { name, module, .. } => (
            e,
            C::UNKNOWN_PARAM,
            Message::new(
                M::UnknownParam,
                vec![Arg::Name(name.clone()), Arg::Name(module.clone())],
            ),
        ),
        ElabDiag::DuplicateParamOverride { .. } => (
            e,
            C::DUPLICATE_PARAM_OVERRIDE,
            Message::new(M::DuplicateParamOverride, vec![Arg::Name(n(diag))]),
        ),
        ElabDiag::TooManyPositionalParams { expected, got, .. } => (
            e,
            C::TOO_MANY_POSITIONAL_PARAMS,
            Message::new(
                M::TooManyPositionalParams,
                vec![Arg::Count(*expected), Arg::Count(*got)],
            ),
        ),
        ElabDiag::ParamNotConst { .. } => (
            e,
            C::PARAM_NOT_CONST,
            Message::new(M::ParamNotConst, vec![Arg::Name(n(diag))]),
        ),
        ElabDiag::GenCondNotConst { .. } => (
            e,
            C::GEN_COND_NOT_CONST,
            Message::simple(M::GenCondNotConst),
        ),
        ElabDiag::GenvarNotConst { .. } => {
            (e, C::GENVAR_NOT_CONST, Message::simple(M::GenvarNotConst))
        }
        ElabDiag::GenerateIterationLimit { limit, .. } => (
            e,
            C::GENERATE_ITERATION_LIMIT,
            Message::new(M::GenerateIterationLimit, vec![Arg::Count(*limit)]),
        ),
        ElabDiag::ModportConflict { .. } => {
            (e, C::MODPORT_CONFLICT, Message::simple(M::ModportConflict))
        }
    }
}

fn map_elab_span(db: &dyn salsa::Database, unit: CompilationUnit, raw: Span) -> Span {
    let Some(source_file) = source_file_by_id(db, unit, raw.file) else {
        return raw;
    };
    let pp = preprocess_file(db, source_file);
    pp.source_map.map_span(raw.range).unwrap_or(raw)
}

pub(crate) fn extract_block_name(block: &lyra_ast::BlockStmt) -> Option<SmolStr> {
    block.block_name().map(|tok| SmolStr::new(tok.text()))
}

pub(crate) enum ForLimit {
    Lt(i64),
    Le(i64),
    Gt(i64),
    Ge(i64),
    Ne(i64),
}

pub(crate) struct ForParts {
    pub(crate) genvar_name: SmolStr,
    pub(crate) init: i64,
    pub(crate) limit: ForLimit,
    pub(crate) step: i64,
}

pub(crate) fn extract_for_parts(
    for_stmt: &lyra_ast::ForStmt,
    eval_expr: &dyn Fn(&Expr) -> Option<i64>,
) -> Option<ForParts> {
    let init_name_ref = for_stmt.init_name()?;
    let genvar_name = SmolStr::new(init_name_ref.ident()?.text());

    let init_expr = for_stmt.init_value()?;
    let init = eval_expr(&init_expr)?;

    let cond_bin = for_stmt.condition_bin()?;
    let (limit, flipped) = extract_condition(&genvar_name, &cond_bin, eval_expr)?;

    let step_bin = for_stmt.step_bin()?;
    let step = extract_step(&genvar_name, &step_bin, eval_expr)?;

    let _ = flipped;
    Some(ForParts {
        genvar_name,
        init,
        limit,
        step,
    })
}

fn extract_condition(
    genvar_name: &str,
    cond_bin: &lyra_ast::BinExpr,
    eval_expr: &dyn Fn(&Expr) -> Option<i64>,
) -> Option<(ForLimit, bool)> {
    let lhs = cond_bin.lhs()?;
    let rhs = cond_bin.rhs()?;
    let op = cond_bin.binary_op()?;

    if !op.is_comparison() {
        return None;
    }

    let lhs_is_genvar = expr_is_name(genvar_name, &lhs);
    let rhs_is_genvar = expr_is_name(genvar_name, &rhs);

    let (limit_expr, effective_op, flipped) = if lhs_is_genvar {
        (&rhs, op, false)
    } else if rhs_is_genvar {
        (&lhs, op.flip()?, true)
    } else {
        return None;
    };

    let limit_val = eval_expr(limit_expr)?;
    let limit = match effective_op {
        SyntaxBinaryOp::Lt => ForLimit::Lt(limit_val),
        SyntaxBinaryOp::LtEq => ForLimit::Le(limit_val),
        SyntaxBinaryOp::Gt => ForLimit::Gt(limit_val),
        SyntaxBinaryOp::GtEq => ForLimit::Ge(limit_val),
        SyntaxBinaryOp::Neq => ForLimit::Ne(limit_val),
        _ => return None,
    };
    Some((limit, flipped))
}

fn extract_step(
    genvar_name: &str,
    step_bin: &lyra_ast::BinExpr,
    eval_expr: &dyn Fn(&Expr) -> Option<i64>,
) -> Option<i64> {
    let lhs = step_bin.lhs()?;
    let rhs = step_bin.rhs()?;
    let op = step_bin.binary_op()?;

    if !expr_is_name(genvar_name, &lhs) {
        return None;
    }

    let step_val = eval_expr(&rhs)?;
    match op {
        SyntaxBinaryOp::Add => Some(step_val),
        SyntaxBinaryOp::Sub => Some(-step_val),
        _ => None,
    }
}

fn expr_is_name(name: &str, expr: &Expr) -> bool {
    lyra_ast::NameRef::cast(expr.syntax().clone())
        .and_then(|nr| nr.ident())
        .is_some_and(|tok| tok.text() == name)
}
