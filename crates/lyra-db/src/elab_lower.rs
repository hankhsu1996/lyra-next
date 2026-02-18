use std::collections::HashSet;

use lyra_ast::AstNode;
use lyra_lexer::SyntaxKind;
use lyra_parser::{SyntaxElement, SyntaxNode};
use lyra_source::{FileId, Span, TextRange};
use smol_str::SmolStr;

use crate::elab_queries::{TopModule, elaborate_top};
use crate::elaboration::ElabDiag;
use crate::module_sig::ModuleSig;
use crate::pipeline::preprocess_file;
use crate::{CompilationUnit, source_file_by_id};

pub(crate) fn resolve_port_connections(
    port_list: &lyra_ast::InstancePortList,
    sig: &ModuleSig,
    module_name: &SmolStr,
    file_id: FileId,
    inst_range: TextRange,
    diags: &mut Vec<ElabDiag>,
) {
    let ports: Vec<_> = port_list.ports().collect();
    if ports.is_empty() {
        return;
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
            file_id,
            inst_span,
            has_wildcard,
            diags,
        );
    } else {
        resolve_positional_ports(&ports, sig, module_name, inst_span, diags);
    }
}

fn resolve_named_ports(
    ports: &[lyra_ast::InstancePort],
    sig: &ModuleSig,
    module_name: &SmolStr,
    file_id: FileId,
    inst_span: Span,
    has_wildcard: bool,
    diags: &mut Vec<ElabDiag>,
) {
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
}

fn resolve_positional_ports(
    ports: &[lyra_ast::InstancePort],
    sig: &ModuleSig,
    module_name: &SmolStr,
    inst_span: Span,
    diags: &mut Vec<ElabDiag>,
) {
    let actual_count = ports.len();
    let formal_count = sig.ports.len();

    if actual_count > formal_count {
        diags.push(ElabDiag::TooManyPositionalPorts {
            expected: formal_count,
            got: actual_count,
            span: inst_span,
        });
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
    use lyra_diag::{Diagnostic, Label, LabelKind};

    let (severity, code, msg) = elab_diag_code_msg(diag);
    let span = map_elab_span(db, unit, *diag.span());

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
        | ElabDiag::NotAModule { name, .. }
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
        ElabDiag::NotAModule { .. } => (
            e,
            C::NOT_A_MODULE,
            Message::new(M::NotAModule, vec![Arg::Name(n(diag))]),
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
    }
}

fn map_elab_span(db: &dyn salsa::Database, unit: CompilationUnit, raw: Span) -> Span {
    let Some(source_file) = source_file_by_id(db, unit, raw.file) else {
        return raw;
    };
    let pp = preprocess_file(db, source_file);
    pp.source_map.map_span(raw.range).unwrap_or(raw)
}

pub(crate) fn extract_block_name(node: &SyntaxNode) -> Option<SmolStr> {
    let mut saw_begin = false;
    let mut saw_colon = false;
    for el in node.children_with_tokens() {
        match el {
            SyntaxElement::Token(tok) => match tok.kind() {
                SyntaxKind::BeginKw => saw_begin = true,
                SyntaxKind::Colon if saw_begin => saw_colon = true,
                SyntaxKind::Ident if saw_colon => return Some(SmolStr::new(tok.text())),
                _ => {
                    if saw_begin && !saw_colon {
                        return None;
                    }
                }
            },
            SyntaxElement::Node(_) => {
                if saw_begin {
                    return None;
                }
            }
        }
    }
    None
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
    node: &SyntaxNode,
    eval_expr: &dyn Fn(&SyntaxNode) -> Option<i64>,
) -> Option<ForParts> {
    // ForStmt child structure (from the parser):
    //   NameRef       -- genvar name (init LHS)
    //   Expression    -- init value
    //   BinExpr       -- condition (genvar OP limit)
    //   NameRef       -- genvar name (step LHS)
    //   BinExpr       -- step expression (genvar +/- step_val)
    //   Body          -- loop body (BlockStmt)
    let children: Vec<SyntaxNode> = node.children().collect();

    // Extract genvar name from first NameRef
    let genvar_name_node = children.iter().find(|c| c.kind() == SyntaxKind::NameRef)?;
    let genvar_name = SmolStr::new(
        genvar_name_node
            .children_with_tokens()
            .filter_map(|el| el.into_token())
            .find(|t| t.kind() == SyntaxKind::Ident)?
            .text(),
    );

    // Find the init expression (first non-NameRef expression-kind node)
    let init_node = children
        .iter()
        .find(|c| c.kind() != SyntaxKind::NameRef && is_expr_kind(c.kind()))?;
    let init = eval_expr(init_node)?;

    // Find the first BinExpr (condition)
    let cond_node = children.iter().find(|c| c.kind() == SyntaxKind::BinExpr)?;
    let (limit, flipped) = extract_condition(&genvar_name, cond_node, eval_expr)?;

    // Find the second BinExpr (step), after the condition
    let cond_idx = children.iter().position(|c| std::ptr::eq(c, cond_node))?;
    let step_node = children[cond_idx + 1..]
        .iter()
        .find(|c| c.kind() == SyntaxKind::BinExpr)?;
    let step = extract_step(&genvar_name, step_node, eval_expr)?;

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
    cond_node: &SyntaxNode,
    eval_expr: &dyn Fn(&SyntaxNode) -> Option<i64>,
) -> Option<(ForLimit, bool)> {
    let cond_children: Vec<SyntaxNode> = cond_node.children().collect();
    if cond_children.len() < 2 {
        return None;
    }

    let op_kind = cond_node
        .children_with_tokens()
        .filter_map(|el| el.into_token())
        .find(|t| is_comparison_op(t.kind()))?
        .kind();

    let lhs_is_genvar = node_is_name(genvar_name, &cond_children[0]);
    let rhs_is_genvar = node_is_name(genvar_name, &cond_children[1]);

    let (limit_node, effective_op, flipped) = if lhs_is_genvar {
        (&cond_children[1], op_kind, false)
    } else if rhs_is_genvar {
        (&cond_children[0], flip_comparison(op_kind)?, true)
    } else {
        return None;
    };

    let limit_val = eval_expr(limit_node)?;
    let limit = match effective_op {
        SyntaxKind::Lt => ForLimit::Lt(limit_val),
        SyntaxKind::LtEq => ForLimit::Le(limit_val),
        SyntaxKind::Gt => ForLimit::Gt(limit_val),
        SyntaxKind::GtEq => ForLimit::Ge(limit_val),
        SyntaxKind::BangEq => ForLimit::Ne(limit_val),
        _ => return None,
    };
    Some((limit, flipped))
}

fn extract_step(
    genvar_name: &str,
    step_node: &SyntaxNode,
    eval_expr: &dyn Fn(&SyntaxNode) -> Option<i64>,
) -> Option<i64> {
    let step_children: Vec<SyntaxNode> = step_node.children().collect();
    if step_children.len() < 2 {
        return None;
    }

    let op_kind = step_node
        .children_with_tokens()
        .filter_map(|el| el.into_token())
        .find(|t| matches!(t.kind(), SyntaxKind::Plus | SyntaxKind::Minus))?
        .kind();

    let lhs_is_genvar = node_is_name(genvar_name, &step_children[0]);

    if !lhs_is_genvar {
        return None;
    }

    let step_val = eval_expr(&step_children[1])?;
    match op_kind {
        SyntaxKind::Plus => Some(step_val),
        SyntaxKind::Minus => Some(-step_val),
        _ => None,
    }
}

fn node_is_name(name: &str, node: &SyntaxNode) -> bool {
    if node.kind() != SyntaxKind::NameRef {
        return false;
    }
    node.children_with_tokens()
        .filter_map(|el| el.into_token())
        .any(|t| t.kind() == SyntaxKind::Ident && t.text() == name)
}

fn is_comparison_op(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Lt | SyntaxKind::LtEq | SyntaxKind::Gt | SyntaxKind::GtEq | SyntaxKind::BangEq
    )
}

fn flip_comparison(op: SyntaxKind) -> Option<SyntaxKind> {
    match op {
        SyntaxKind::Lt => Some(SyntaxKind::Gt),
        SyntaxKind::Gt => Some(SyntaxKind::Lt),
        SyntaxKind::LtEq => Some(SyntaxKind::GtEq),
        SyntaxKind::GtEq => Some(SyntaxKind::LtEq),
        SyntaxKind::BangEq => Some(SyntaxKind::BangEq),
        _ => None,
    }
}

pub(crate) fn is_expr_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Literal
            | SyntaxKind::BinExpr
            | SyntaxKind::PrefixExpr
            | SyntaxKind::ParenExpr
            | SyntaxKind::CondExpr
            | SyntaxKind::ConcatExpr
            | SyntaxKind::ReplicExpr
            | SyntaxKind::IndexExpr
            | SyntaxKind::RangeExpr
            | SyntaxKind::FieldExpr
            | SyntaxKind::CallExpr
            | SyntaxKind::NameRef
            | SyntaxKind::QualifiedName
            | SyntaxKind::Expression
    )
}
