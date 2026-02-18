use std::collections::HashSet;

use lyra_ast::AstNode;
use lyra_lexer::SyntaxKind;
use lyra_parser::{SyntaxElement, SyntaxNode};
use lyra_source::{FileId, Span, TextRange};
use smol_str::SmolStr;

use crate::elab_queries::{TopModule, elaborate_top};
use crate::elaboration::{ElabDiag, ParamEnvId};
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
    _param_env: ParamEnvId,
    _sig: &ModuleSig,
    _genvar_binding: Option<(&SmolStr, i64)>,
) -> Option<ForParts> {
    let tokens: Vec<_> = node
        .descendants_with_tokens()
        .filter_map(|el| el.into_token())
        .filter(|t| t.kind() != SyntaxKind::Whitespace)
        .collect();

    let lparen_pos = tokens.iter().position(|t| t.kind() == SyntaxKind::LParen)?;

    let mut pos = lparen_pos + 1;
    if pos < tokens.len() && tokens[pos].kind() == SyntaxKind::GenvarKw {
        pos += 1;
    }

    if pos >= tokens.len() || tokens[pos].kind() != SyntaxKind::Ident {
        return None;
    }
    let genvar_name = SmolStr::new(tokens[pos].text());

    pos += 1;
    if pos >= tokens.len() || tokens[pos].kind() != SyntaxKind::Assign {
        return None;
    }
    pos += 1;

    let first_semi = tokens[pos..]
        .iter()
        .position(|t| t.kind() == SyntaxKind::Semicolon)?;
    let init_end = pos + first_semi;
    let init = parse_simple_int_from_tokens(&tokens[pos..init_end])?;

    pos = init_end + 1;

    let second_semi_offset = tokens[pos..]
        .iter()
        .position(|t| t.kind() == SyntaxKind::Semicolon)?;
    let cond_end = pos + second_semi_offset;

    if pos >= cond_end {
        return None;
    }
    if tokens[pos].kind() == SyntaxKind::Ident && tokens[pos].text() == genvar_name.as_str() {
        pos += 1;
    } else {
        return None;
    }

    if pos >= cond_end {
        return None;
    }

    let op_kind = tokens[pos].kind();
    pos += 1;

    let limit_val = parse_simple_int_from_tokens(&tokens[pos..cond_end])?;

    let limit = match op_kind {
        SyntaxKind::Lt => ForLimit::Lt(limit_val),
        SyntaxKind::LtEq => ForLimit::Le(limit_val),
        SyntaxKind::Gt => ForLimit::Gt(limit_val),
        SyntaxKind::GtEq => ForLimit::Ge(limit_val),
        SyntaxKind::BangEq => ForLimit::Ne(limit_val),
        _ => return None,
    };

    pos = cond_end + 1;

    let rparen_pos = tokens[pos..]
        .iter()
        .position(|t| t.kind() == SyntaxKind::RParen)?;
    let step_end = pos + rparen_pos;

    if pos < step_end && tokens[pos].kind() == SyntaxKind::Ident {
        pos += 1;
    }
    if pos < step_end && tokens[pos].kind() == SyntaxKind::Assign {
        pos += 1;
    }
    if pos < step_end && tokens[pos].kind() == SyntaxKind::Ident {
        pos += 1;
    }

    if pos >= step_end {
        return None;
    }

    let step_op = tokens[pos].kind();
    pos += 1;

    let step_val = parse_simple_int_from_tokens(&tokens[pos..step_end])?;

    let step = match step_op {
        SyntaxKind::Plus => step_val,
        SyntaxKind::Minus => -step_val,
        _ => return None,
    };

    Some(ForParts {
        genvar_name,
        init,
        limit,
        step,
    })
}

fn parse_simple_int_from_tokens(tokens: &[lyra_parser::SyntaxToken]) -> Option<i64> {
    if tokens.len() == 1 && tokens[0].kind() == SyntaxKind::IntLiteral {
        return tokens[0].text().parse::<i64>().ok();
    }
    None
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
