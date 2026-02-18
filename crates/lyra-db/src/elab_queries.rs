use lyra_ast::AstNode;
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxElement;
use lyra_semantic::global_index::DefinitionKind;
use lyra_semantic::symbols::GlobalDefId;
use lyra_semantic::types::{SymbolType, Ty};
use lyra_source::{FileId, Span, TextRange};
use smol_str::SmolStr;
use std::collections::{HashMap, HashSet};

use crate::elaboration::{ElabDiag, ElabTree, InstanceKey, InstanceNode};
use crate::module_sig::{ModuleSig, ParamKind, ParamSig, PortDirection, PortSig};
use crate::pipeline::{ast_id_map, parse_file, preprocess_file};
use crate::semantic::{def_index_file, global_def_index};
use crate::type_queries::{SymbolRef, type_of_symbol};
use crate::{CompilationUnit, source_file_by_id};

const ELAB_RECURSION_LIMIT: usize = 256;

/// Identifies a top module for elaboration.
#[salsa::interned]
pub struct TopModule<'db> {
    pub unit: CompilationUnit,
    pub name: SmolStr,
}

/// Identifies a module definition for signature extraction.
#[salsa::interned]
pub struct ModuleRef<'db> {
    pub unit: CompilationUnit,
    pub def_id: GlobalDefId,
}

/// Build a module signature from the module header AST (Salsa-cached).
#[salsa::tracked(return_ref)]
pub fn module_signature<'db>(db: &'db dyn salsa::Database, module: ModuleRef<'db>) -> ModuleSig {
    let unit = module.unit(db);
    let def_id = module.def_id(db);
    let file_id = def_id.file();

    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return ModuleSig::new(SmolStr::default(), Vec::new(), Vec::new());
    };

    let parse = parse_file(db, source_file);
    let id_map = ast_id_map(db, source_file);
    let def = def_index_file(db, source_file);

    let Some(module_node) = id_map.get_node(&parse.syntax(), def_id.ast_id()) else {
        return ModuleSig::new(SmolStr::default(), Vec::new(), Vec::new());
    };

    let Some(module_decl) = lyra_ast::ModuleDecl::cast(module_node) else {
        return ModuleSig::new(SmolStr::default(), Vec::new(), Vec::new());
    };

    let name = module_decl
        .name()
        .map(|t| SmolStr::new(t.text()))
        .unwrap_or_default();

    let mut ports = Vec::new();
    if let Some(port_list) = module_decl.port_list() {
        for port in port_list.ports() {
            let port_name = port
                .name()
                .map(|t| SmolStr::new(t.text()))
                .unwrap_or_default();

            let direction = port.direction().and_then(|tok| match tok.kind() {
                SyntaxKind::InputKw => Some(PortDirection::Input),
                SyntaxKind::OutputKw => Some(PortDirection::Output),
                SyntaxKind::InoutKw => Some(PortDirection::Inout),
                SyntaxKind::RefKw => Some(PortDirection::Ref),
                _ => None,
            });

            let port_ast_id = id_map.erased_ast_id(port.syntax());
            let ty = port_ast_id
                .and_then(|ast_id| {
                    let sym_id = def.decl_to_symbol.get(&ast_id).copied()?;
                    let gsym = lyra_semantic::symbols::GlobalSymbolId {
                        file: file_id,
                        local: sym_id,
                    };
                    let sym_ref = SymbolRef::new(db, unit, gsym);
                    let st = type_of_symbol(db, sym_ref);
                    match st {
                        SymbolType::Value(ty) => Some(ty),
                        _ => None,
                    }
                })
                .unwrap_or(Ty::Error);

            let name_range = port.name().map(|t| t.text_range()).unwrap_or_default();
            let decl_range = port.text_range();

            ports.push(PortSig {
                name: port_name,
                direction,
                ty,
                name_range,
                decl_range,
            });
        }
    }

    let mut params = Vec::new();
    if let Some(param_port_list) = module_decl.param_port_list() {
        for param_decl in param_port_list.params() {
            let is_type_param = param_decl.syntax().children_with_tokens().any(|el| {
                matches!(el, SyntaxElement::Token(ref tok) if tok.kind() == SyntaxKind::TypeKw)
            });

            for declarator in param_decl.declarators() {
                let param_name = declarator
                    .name()
                    .map(|t| SmolStr::new(t.text()))
                    .unwrap_or_default();
                let name_range = declarator
                    .name()
                    .map(|t| t.text_range())
                    .unwrap_or_default();
                let has_default = declarator
                    .syntax()
                    .children_with_tokens()
                    .any(|el| {
                        matches!(el, SyntaxElement::Token(ref tok) if tok.kind() == SyntaxKind::Assign)
                    });

                params.push(ParamSig {
                    name: param_name,
                    kind: if is_type_param {
                        ParamKind::Type
                    } else {
                        ParamKind::Value
                    },
                    has_default,
                    name_range,
                });
            }
        }
    }

    ModuleSig::new(name, ports, params)
}

/// Elaborate a top module into an instance tree (Salsa-cached).
#[salsa::tracked(return_ref)]
pub fn elaborate_top<'db>(db: &'db dyn salsa::Database, top: TopModule<'db>) -> ElabTree {
    let unit = top.unit(db);
    let name = top.name(db);
    let global = global_def_index(db, unit);

    let mut diags = Vec::new();

    let Some((top_def_id, top_kind)) = global.resolve_definition(&name) else {
        diags.push(ElabDiag::UnresolvedModuleInst {
            name: name.clone(),
            span: Span {
                file: FileId(0),
                range: TextRange::default(),
            },
        });
        return ElabTree {
            top: None,
            nodes: HashMap::new(),
            diagnostics: diags,
        };
    };

    if top_kind != DefinitionKind::Module {
        diags.push(ElabDiag::NotAModule {
            name: name.clone(),
            span: Span {
                file: FileId(0),
                range: TextRange::default(),
            },
        });
        return ElabTree {
            top: Some(top_def_id),
            nodes: HashMap::new(),
            diagnostics: diags,
        };
    }

    let mut nodes = HashMap::new();
    let mut active_stack = Vec::new();

    elaborate_module(
        db,
        unit,
        top_def_id,
        None,
        &mut nodes,
        &mut diags,
        &mut active_stack,
    );

    ElabTree {
        top: Some(top_def_id),
        nodes,
        diagnostics: diags,
    }
}

fn elaborate_module(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    module_def: GlobalDefId,
    parent_key: Option<InstanceKey>,
    nodes: &mut HashMap<InstanceKey, InstanceNode>,
    diags: &mut Vec<ElabDiag>,
    active_stack: &mut Vec<GlobalDefId>,
) {
    let file_id = module_def.file();

    if active_stack.len() >= ELAB_RECURSION_LIMIT || active_stack.contains(&module_def) {
        diags.push(ElabDiag::RecursionLimit {
            span: Span {
                file: file_id,
                range: TextRange::default(),
            },
        });
        return;
    }

    active_stack.push(module_def);
    elaborate_module_body(db, unit, module_def, parent_key, nodes, diags, active_stack);
    active_stack.pop();
}

fn elaborate_module_body(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    module_def: GlobalDefId,
    parent_key: Option<InstanceKey>,
    nodes: &mut HashMap<InstanceKey, InstanceNode>,
    diags: &mut Vec<ElabDiag>,
    active_stack: &mut Vec<GlobalDefId>,
) {
    let file_id = module_def.file();
    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return;
    };
    let parse = parse_file(db, source_file);
    let id_map = ast_id_map(db, source_file);
    let global = global_def_index(db, unit);

    let Some(module_node) = id_map.get_node(&parse.syntax(), module_def.ast_id()) else {
        return;
    };
    let Some(module_decl) = lyra_ast::ModuleDecl::cast(module_node) else {
        return;
    };
    let Some(body) = module_decl.body() else {
        return;
    };

    for inst_node in body.module_instantiations() {
        let Some(name_tok) = inst_node.module_name() else {
            continue;
        };
        let inst_module_name = SmolStr::new(name_tok.text());
        let name_span = Span {
            file: file_id,
            range: name_tok.text_range(),
        };

        let Some((target_def_id, target_kind)) = global.resolve_definition(&inst_module_name)
        else {
            diags.push(ElabDiag::UnresolvedModuleInst {
                name: inst_module_name,
                span: name_span,
            });
            continue;
        };

        if target_kind != DefinitionKind::Module {
            diags.push(ElabDiag::NotAModule {
                name: inst_module_name,
                span: name_span,
            });
            continue;
        }

        let sig = module_signature(db, ModuleRef::new(db, unit, target_def_id));

        for (name_tok, port_list) in inst_node.instances() {
            let child_key = InstanceKey {
                file: file_id,
                name_range: name_tok.text_range(),
            };

            if let Some(ref pl) = port_list {
                resolve_port_connections(
                    pl,
                    sig,
                    &inst_module_name,
                    file_id,
                    name_tok.text_range(),
                    diags,
                );
            }

            nodes.entry(child_key).or_insert_with(|| InstanceNode {
                key: child_key,
                parent: parent_key,
                module_def: target_def_id,
                instance_name: SmolStr::new(name_tok.text()),
                children: Vec::new(),
            });

            elaborate_module(
                db,
                unit,
                target_def_id,
                Some(child_key),
                nodes,
                diags,
                active_stack,
            );
        }
    }
}

fn resolve_port_connections(
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
        let mut connected: HashSet<u32> = HashSet::new();
        for port in &ports {
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
    } else {
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
        Arg, Diagnostic, DiagnosticCode, Label, LabelKind, Message, MessageId, Severity,
    };

    let (severity, code, msg, raw_span) = match diag {
        ElabDiag::UnresolvedModuleInst { name, span } => (
            Severity::Error,
            DiagnosticCode::UNRESOLVED_MODULE_INST,
            Message::new(
                MessageId::UnresolvedModuleInst,
                vec![Arg::Name(name.clone())],
            ),
            *span,
        ),
        ElabDiag::NotAModule { name, span } => (
            Severity::Error,
            DiagnosticCode::NOT_A_MODULE,
            Message::new(MessageId::NotAModule, vec![Arg::Name(name.clone())]),
            *span,
        ),
        ElabDiag::UnknownPort { port, module, span } => (
            Severity::Error,
            DiagnosticCode::UNKNOWN_PORT,
            Message::new(
                MessageId::UnknownPort,
                vec![Arg::Name(port.clone()), Arg::Name(module.clone())],
            ),
            *span,
        ),
        ElabDiag::DuplicatePortConn { port, span } => (
            Severity::Error,
            DiagnosticCode::DUPLICATE_PORT_CONN,
            Message::new(MessageId::DuplicatePortConn, vec![Arg::Name(port.clone())]),
            *span,
        ),
        ElabDiag::TooManyPositionalPorts {
            expected,
            got,
            span,
        } => (
            Severity::Error,
            DiagnosticCode::TOO_MANY_POSITIONAL_PORTS,
            Message::new(
                MessageId::TooManyPositionalPorts,
                vec![Arg::Count(*expected), Arg::Count(*got)],
            ),
            *span,
        ),
        ElabDiag::MissingPortConn { port, module, span } => (
            Severity::Warning,
            DiagnosticCode::MISSING_PORT_CONN,
            Message::new(
                MessageId::MissingPortConn,
                vec![Arg::Name(port.clone()), Arg::Name(module.clone())],
            ),
            *span,
        ),
        ElabDiag::RecursionLimit { span } => (
            Severity::Error,
            DiagnosticCode::ELAB_RECURSION_LIMIT,
            Message::simple(MessageId::ElabRecursionLimit),
            *span,
        ),
    };

    let span = map_elab_span(db, unit, raw_span);

    Diagnostic::new(severity, code, msg.clone()).with_label(Label {
        kind: LabelKind::Primary,
        span,
        message: msg,
    })
}

/// Map an expanded-text span to source coordinates using the file's source map.
fn map_elab_span(db: &dyn salsa::Database, unit: CompilationUnit, raw: Span) -> Span {
    let Some(source_file) = source_file_by_id(db, unit, raw.file) else {
        return raw;
    };
    let pp = preprocess_file(db, source_file);
    pp.source_map.map_span(raw.range).unwrap_or(raw)
}
