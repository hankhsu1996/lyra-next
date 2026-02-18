use lyra_ast::AstNode;
use lyra_lexer::SyntaxKind;
use lyra_parser::{SyntaxElement, SyntaxNode};
use lyra_semantic::const_eval::eval_const_expr;
use lyra_semantic::global_index::DefinitionKind;
use lyra_semantic::symbols::GlobalDefId;
use lyra_semantic::types::{ConstEvalError, ConstInt, SymbolType, Ty};
use lyra_source::{FileId, Span, TextRange};
use smol_str::SmolStr;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::elaboration::{
    ElabDiag, ElabItemKey, ElabTree, GenScopeKey, GenScopeKind, GenScopeNode, InstanceKey,
    InstanceNode, ParamEnv, ScopeKey,
};
use crate::module_sig::{ModuleSig, ParamKind, ParamSig, PortDirection, PortSig};
use crate::pipeline::{ast_id_map, parse_file};
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

    let ports = extract_port_sigs(db, unit, file_id, &module_decl, id_map, def);
    let params = extract_param_sigs(&module_decl, id_map);

    ModuleSig::new(name, ports, params)
}

fn extract_port_sigs(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    file_id: FileId,
    module_decl: &lyra_ast::ModuleDecl,
    id_map: &lyra_ast::AstIdMap,
    def: &lyra_semantic::def_index::DefIndex,
) -> Vec<PortSig> {
    let Some(port_list) = module_decl.port_list() else {
        return Vec::new();
    };

    let mut ports = Vec::new();
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
    ports
}

fn extract_param_sigs(
    module_decl: &lyra_ast::ModuleDecl,
    id_map: &lyra_ast::AstIdMap,
) -> Vec<ParamSig> {
    let Some(param_port_list) = module_decl.param_port_list() else {
        return Vec::new();
    };

    let mut params = Vec::new();
    for param_decl in param_port_list.params() {
        let is_type_param = param_decl.syntax().children_with_tokens().any(
            |el| matches!(el, SyntaxElement::Token(ref tok) if tok.kind() == SyntaxKind::TypeKw),
        );

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

            let default_expr = if has_default {
                declarator
                    .syntax()
                    .children()
                    .find(|c| is_expr_kind(c.kind()))
                    .and_then(|expr_node| id_map.erased_ast_id(&expr_node))
            } else {
                None
            };

            params.push(ParamSig {
                name: param_name,
                kind: if is_type_param {
                    ParamKind::Type
                } else {
                    ParamKind::Value
                },
                has_default,
                default_expr,
                name_range,
            });
        }
    }
    params
}

/// Build a `ParamEnv` by evaluating defaults and applying overrides.
fn build_param_env(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    sig: &ModuleSig,
    overrides: &[ParamOverride],
    file_id: FileId,
    inst_range: TextRange,
    diags: &mut Vec<ElabDiag>,
) -> Arc<ParamEnv> {
    let param_count = sig.params.len();
    if param_count == 0 {
        return ParamEnv::empty();
    }

    // Evaluate defaults in declaration order
    let mut values = Vec::with_capacity(param_count);
    for (i, param) in sig.params.iter().enumerate() {
        if param.kind == ParamKind::Type {
            values.push(ConstInt::Error(ConstEvalError::Unsupported));
            continue;
        }

        let val = if let Some(expr_id) = &param.default_expr {
            let expr_ref = crate::const_eval::ConstExprRef::new(db, unit, *expr_id);
            crate::const_eval::eval_const_int(db, expr_ref)
        } else {
            ConstInt::Error(ConstEvalError::Unresolved)
        };
        let _ = i; // future: forward reference detection
        values.push(val);
    }

    // Apply overrides
    let has_named = overrides.iter().any(|o| o.name.is_some());
    if has_named {
        let mut seen: HashSet<u32> = HashSet::new();
        for ovr in overrides {
            let Some(ref name) = ovr.name else {
                continue;
            };
            match sig.param_by_name(name) {
                Some((idx, _)) => {
                    if !seen.insert(idx) {
                        diags.push(ElabDiag::DuplicateParamOverride {
                            name: SmolStr::new(name.as_str()),
                            span: Span {
                                file: file_id,
                                range: ovr.span,
                            },
                        });
                        continue;
                    }
                    if matches!(ovr.value, ConstInt::Error(_)) {
                        diags.push(ElabDiag::ParamNotConst {
                            name: sig.params[idx as usize].name.clone(),
                            span: Span {
                                file: file_id,
                                range: ovr.span,
                            },
                        });
                    }
                    values[idx as usize] = ovr.value.clone();
                }
                None => {
                    diags.push(ElabDiag::UnknownParam {
                        name: SmolStr::new(name.as_str()),
                        module: sig.name.clone(),
                        span: Span {
                            file: file_id,
                            range: ovr.span,
                        },
                    });
                }
            }
        }
    } else {
        let value_param_count = sig
            .params
            .iter()
            .filter(|p| p.kind == ParamKind::Value)
            .count();
        if overrides.len() > value_param_count {
            diags.push(ElabDiag::TooManyPositionalParams {
                expected: value_param_count,
                got: overrides.len(),
                span: Span {
                    file: file_id,
                    range: inst_range,
                },
            });
        }
        let mut ovr_idx = 0;
        for (i, param) in sig.params.iter().enumerate() {
            if param.kind == ParamKind::Type {
                continue;
            }
            if ovr_idx < overrides.len() {
                if matches!(overrides[ovr_idx].value, ConstInt::Error(_)) {
                    diags.push(ElabDiag::ParamNotConst {
                        name: param.name.clone(),
                        span: Span {
                            file: file_id,
                            range: overrides[ovr_idx].span,
                        },
                    });
                }
                values[i] = overrides[ovr_idx].value.clone();
                ovr_idx += 1;
            }
        }
    }

    Arc::new(ParamEnv::new(values))
}

struct ParamOverride {
    name: Option<SmolStr>,
    value: ConstInt,
    span: TextRange,
}

/// Extract parameter overrides from a `ModuleInstantiation` node.
fn extract_param_overrides(
    inst_node: &lyra_ast::ModuleInstantiation,
    file_id: FileId,
    db: &dyn salsa::Database,
    unit: CompilationUnit,
) -> Vec<ParamOverride> {
    let Some(param_port_list) = inst_node.param_overrides() else {
        return Vec::new();
    };

    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return Vec::new();
    };
    let id_map = ast_id_map(db, source_file);

    let mut overrides = Vec::new();
    for param_port in param_port_list.params() {
        // ParamPortList contains ParamDecl nodes in the header,
        // but in instantiation context it contains InstancePort nodes.
        // The parser wraps overrides as InstancePort nodes.
        // We need to work at the raw syntax level.
        for declarator in param_port.declarators() {
            let _ = declarator;
        }
    }

    // Work at the raw syntax level: the ParamPortList in instantiation
    // context contains InstancePort children (not ParamDecl).
    for child in param_port_list.syntax().children() {
        if child.kind() != SyntaxKind::InstancePort {
            continue;
        }
        let Some(ip) = lyra_ast::InstancePort::cast(child) else {
            continue;
        };

        let name = ip.port_name().map(|t| SmolStr::new(t.text()));
        let span = ip.syntax().text_range();

        let value = if let Some(expr_node) = ip.actual_expr() {
            if let Some(expr_ast_id) = id_map.erased_ast_id(&expr_node) {
                let expr_ref = crate::const_eval::ConstExprRef::new(db, unit, expr_ast_id);
                crate::const_eval::eval_const_int(db, expr_ref)
            } else {
                ConstInt::Error(ConstEvalError::Unsupported)
            }
        } else {
            ConstInt::Error(ConstEvalError::Unresolved)
        };

        overrides.push(ParamOverride { name, value, span });
    }

    overrides
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
            gen_scopes: HashMap::new(),
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
            top: None,
            nodes: HashMap::new(),
            gen_scopes: HashMap::new(),
            diagnostics: diags,
        };
    }

    let file_id = top_def_id.file();
    let top_name_range = module_name_range(db, unit, top_def_id);
    let top_key = InstanceKey {
        file: file_id,
        name_range: top_name_range,
        parent_gen: None,
    };

    // Top module: defaults only, no overrides
    let sig = module_signature(db, ModuleRef::new(db, unit, top_def_id));
    let param_env = build_param_env(db, unit, sig, &[], file_id, top_name_range, &mut diags);

    let mut nodes = HashMap::new();
    let mut gen_scopes = HashMap::new();
    nodes.insert(
        top_key.clone(),
        InstanceNode {
            key: top_key.clone(),
            parent: None,
            module_def: top_def_id,
            instance_name: name.clone(),
            param_env,
            children: Vec::new(),
        },
    );

    let mut active_stack = Vec::new();
    let mut ctx = ElabCtx {
        db,
        unit,
        nodes: &mut nodes,
        gen_scopes: &mut gen_scopes,
        diags: &mut diags,
        active_stack: &mut active_stack,
    };

    elaborate_module(
        &mut ctx,
        top_def_id,
        &ScopeKey::Instance(top_key.clone()),
        None,
    );

    ElabTree {
        top: Some(top_key),
        nodes,
        gen_scopes,
        diagnostics: diags,
    }
}

struct ElabCtx<'a> {
    db: &'a dyn salsa::Database,
    unit: CompilationUnit,
    nodes: &'a mut HashMap<InstanceKey, InstanceNode>,
    gen_scopes: &'a mut HashMap<GenScopeKey, GenScopeNode>,
    diags: &'a mut Vec<ElabDiag>,
    active_stack: &'a mut Vec<GlobalDefId>,
}

/// Immutable context for expanding children within a scope.
struct ScopeEnv<'a> {
    file_id: FileId,
    parent_scope: &'a ScopeKey,
    sig: &'a ModuleSig,
    param_env: &'a Arc<ParamEnv>,
    genvar_binding: Option<(&'a SmolStr, i64)>,
    global: &'a lyra_semantic::global_index::GlobalDefIndex,
}

fn module_name_range(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    def_id: GlobalDefId,
) -> TextRange {
    let Some(source_file) = source_file_by_id(db, unit, def_id.file()) else {
        return TextRange::default();
    };
    let parse = parse_file(db, source_file);
    let id_map = ast_id_map(db, source_file);
    let Some(node) = id_map.get_node(&parse.syntax(), def_id.ast_id()) else {
        return TextRange::default();
    };
    let Some(module_decl) = lyra_ast::ModuleDecl::cast(node) else {
        return TextRange::default();
    };
    module_decl
        .name()
        .map(|t| t.text_range())
        .unwrap_or_default()
}

fn elaborate_module(
    ctx: &mut ElabCtx<'_>,
    module_def: GlobalDefId,
    parent_scope: &ScopeKey,
    genvar_binding: Option<(&SmolStr, i64)>,
) {
    let file_id = module_def.file();

    if ctx.active_stack.len() >= ELAB_RECURSION_LIMIT || ctx.active_stack.contains(&module_def) {
        ctx.diags.push(ElabDiag::RecursionLimit {
            span: Span {
                file: file_id,
                range: TextRange::default(),
            },
        });
        return;
    }

    ctx.active_stack.push(module_def);
    elaborate_module_body(ctx, module_def, parent_scope, genvar_binding);
    ctx.active_stack.pop();
}

fn elaborate_module_body(
    ctx: &mut ElabCtx<'_>,
    module_def: GlobalDefId,
    parent_scope: &ScopeKey,
    genvar_binding: Option<(&SmolStr, i64)>,
) {
    let file_id = module_def.file();
    let Some(source_file) = source_file_by_id(ctx.db, ctx.unit, file_id) else {
        return;
    };
    let parse = parse_file(ctx.db, source_file);
    let id_map = ast_id_map(ctx.db, source_file);
    let global = global_def_index(ctx.db, ctx.unit);

    let Some(module_node) = id_map.get_node(&parse.syntax(), module_def.ast_id()) else {
        return;
    };
    let Some(module_decl) = lyra_ast::ModuleDecl::cast(module_node) else {
        return;
    };
    let Some(body) = module_decl.body() else {
        return;
    };

    let sig = module_signature(ctx.db, ModuleRef::new(ctx.db, ctx.unit, module_def));

    // Get the param_env for the current instance scope
    let param_env = match parent_scope {
        ScopeKey::Instance(ik) => ctx.nodes.get(ik).map(|n| Arc::clone(&n.param_env)),
        ScopeKey::GenScope(_) => None,
    }
    .unwrap_or_else(ParamEnv::empty);

    let env = ScopeEnv {
        file_id,
        parent_scope,
        sig,
        param_env: &param_env,
        genvar_binding,
        global,
    };
    expand_body_children(ctx, body.syntax(), &env);
}

fn expand_body_children(ctx: &mut ElabCtx<'_>, body_node: &SyntaxNode, env: &ScopeEnv<'_>) {
    for child in body_node.children() {
        match child.kind() {
            SyntaxKind::ModuleInstantiation => {
                process_instantiation(ctx, &child, env.file_id, env.parent_scope, env.global);
            }
            SyntaxKind::IfStmt => process_generate_if(ctx, &child, env),
            SyntaxKind::ForStmt => process_generate_for(ctx, &child, env),
            SyntaxKind::CaseStmt => process_generate_case(ctx, &child, env),
            SyntaxKind::GenerateRegion => expand_body_children(ctx, &child, env),
            SyntaxKind::BlockStmt => process_generate_block(ctx, &child, env),
            _ => {}
        }
    }
}

fn process_instantiation(
    ctx: &mut ElabCtx<'_>,
    node: &SyntaxNode,
    file_id: FileId,
    parent_scope: &ScopeKey,
    global: &lyra_semantic::global_index::GlobalDefIndex,
) {
    let Some(inst_node) = lyra_ast::ModuleInstantiation::cast(node.clone()) else {
        return;
    };
    let Some(name_tok) = inst_node.module_name() else {
        return;
    };
    let inst_module_name = SmolStr::new(name_tok.text());
    let name_span = Span {
        file: file_id,
        range: name_tok.text_range(),
    };

    let Some((target_def_id, target_kind)) = global.resolve_definition(&inst_module_name) else {
        ctx.diags.push(ElabDiag::UnresolvedModuleInst {
            name: inst_module_name,
            span: name_span,
        });
        return;
    };

    if target_kind != DefinitionKind::Module {
        ctx.diags.push(ElabDiag::NotAModule {
            name: inst_module_name,
            span: name_span,
        });
        return;
    }

    let target_sig = module_signature(ctx.db, ModuleRef::new(ctx.db, ctx.unit, target_def_id));

    let overrides = extract_param_overrides(&inst_node, file_id, ctx.db, ctx.unit);

    for (inst_name_tok, port_list) in inst_node.instances() {
        let parent_gen = match parent_scope {
            ScopeKey::GenScope(gsk) => Some(gsk.clone()),
            ScopeKey::Instance(_) => None,
        };
        let child_key = InstanceKey {
            file: file_id,
            name_range: inst_name_tok.text_range(),
            parent_gen,
        };

        let child_param_env = build_param_env(
            ctx.db,
            ctx.unit,
            target_sig,
            &overrides,
            file_id,
            inst_name_tok.text_range(),
            ctx.diags,
        );

        if let Some(ref pl) = port_list {
            crate::elab_lower::resolve_port_connections(
                pl,
                target_sig,
                &inst_module_name,
                file_id,
                inst_name_tok.text_range(),
                ctx.diags,
            );
        }

        let child_item = ElabItemKey::Inst(child_key.clone());

        ctx.nodes
            .entry(child_key.clone())
            .or_insert_with(|| InstanceNode {
                key: child_key.clone(),
                parent: Some(parent_scope.clone()),
                module_def: target_def_id,
                instance_name: SmolStr::new(inst_name_tok.text()),
                param_env: child_param_env,
                children: Vec::new(),
            });

        add_child_to_scope(ctx, parent_scope, child_item);

        elaborate_module(ctx, target_def_id, &ScopeKey::Instance(child_key), None);
    }
}

fn process_generate_if(ctx: &mut ElabCtx<'_>, node: &SyntaxNode, env: &ScopeEnv<'_>) {
    let children: Vec<SyntaxNode> = node.children().collect();
    if children.is_empty() {
        return;
    }

    let cond_val = eval_gen_condition(
        ctx,
        &children[0],
        env.file_id,
        env.sig,
        env.param_env,
        env.genvar_binding,
    );

    let cond_true = if let Some(v) = cond_val {
        v != 0
    } else {
        ctx.diags.push(ElabDiag::GenCondNotConst {
            span: Span {
                file: env.file_id,
                range: children[0].text_range(),
            },
        });
        return;
    };

    let has_else = node
        .children_with_tokens()
        .any(|el| matches!(el, SyntaxElement::Token(ref t) if t.kind() == SyntaxKind::ElseKw));

    let true_body = children.get(1);
    let false_body = if has_else { children.get(2) } else { None };
    let Some(body) = (if cond_true { true_body } else { false_body }) else {
        return;
    };

    let scope_key = GenScopeKey {
        file: env.file_id,
        offset: node.text_range(),
        iter: None,
    };
    let block_name = extract_block_name(body);

    ctx.gen_scopes
        .entry(scope_key.clone())
        .or_insert_with(|| GenScopeNode {
            key: scope_key.clone(),
            parent: env.parent_scope.clone(),
            name: block_name,
            kind: GenScopeKind::If,
            children: Vec::new(),
        });

    let scope_item = ElabItemKey::GenScope(scope_key.clone());
    add_child_to_scope(ctx, env.parent_scope, scope_item);

    let child_scope = ScopeKey::GenScope(scope_key);
    let child_env = ScopeEnv {
        parent_scope: &child_scope,
        ..*env
    };
    expand_body_children(ctx, body, &child_env);
}

fn process_generate_for(ctx: &mut ElabCtx<'_>, node: &SyntaxNode, env: &ScopeEnv<'_>) {
    let for_parts = extract_for_parts(node, env.param_env, env.sig, env.genvar_binding);

    let Some(parts) = for_parts else {
        ctx.diags.push(ElabDiag::GenvarNotConst {
            span: Span {
                file: env.file_id,
                range: node.text_range(),
            },
        });
        return;
    };

    let Some(body) = node.children().last() else {
        return;
    };

    let genvar_name = parts.genvar_name;
    let mut current = parts.init;
    let limit = parts.limit;
    let step = parts.step;

    if step == 0 {
        return;
    }

    let max_iterations = 10000;
    let mut iteration = 0;

    while iteration < max_iterations {
        let cond_met = match limit {
            ForLimit::Lt(n) => current < n,
            ForLimit::Le(n) => current <= n,
            ForLimit::Gt(n) => current > n,
            ForLimit::Ge(n) => current >= n,
            ForLimit::Ne(n) => current != n,
        };

        if !cond_met {
            break;
        }

        let scope_key = GenScopeKey {
            file: env.file_id,
            offset: node.text_range(),
            iter: Some(current),
        };

        let block_name = extract_block_name(&body);

        ctx.gen_scopes
            .entry(scope_key.clone())
            .or_insert_with(|| GenScopeNode {
                key: scope_key.clone(),
                parent: env.parent_scope.clone(),
                name: block_name,
                kind: GenScopeKind::ForIteration {
                    genvar_name: genvar_name.clone(),
                    genvar_value: current,
                },
                children: Vec::new(),
            });

        let scope_item = ElabItemKey::GenScope(scope_key.clone());
        add_child_to_scope(ctx, env.parent_scope, scope_item);

        let child_scope = ScopeKey::GenScope(scope_key);
        let child_env = ScopeEnv {
            parent_scope: &child_scope,
            genvar_binding: Some((&genvar_name, current)),
            ..*env
        };
        expand_body_children(ctx, &body, &child_env);

        current = current.wrapping_add(step);
        iteration += 1;
    }
}

fn process_generate_case(ctx: &mut ElabCtx<'_>, node: &SyntaxNode, env: &ScopeEnv<'_>) {
    let child_nodes: Vec<SyntaxNode> = node.children().collect();
    if child_nodes.is_empty() {
        return;
    }

    let case_val = eval_gen_condition(
        ctx,
        &child_nodes[0],
        env.file_id,
        env.sig,
        env.param_env,
        env.genvar_binding,
    );
    let Some(case_val) = case_val else {
        ctx.diags.push(ElabDiag::GenCondNotConst {
            span: Span {
                file: env.file_id,
                range: child_nodes[0].text_range(),
            },
        });
        return;
    };

    let mut matched_body: Option<SyntaxNode> = None;
    let mut default_body: Option<SyntaxNode> = None;

    for child in &child_nodes[1..] {
        if child.kind() != SyntaxKind::CaseItem {
            continue;
        }

        let is_default = child.children_with_tokens().any(
            |el| matches!(el, SyntaxElement::Token(ref t) if t.kind() == SyntaxKind::DefaultKw),
        );

        if is_default {
            default_body = child.children().last();
            continue;
        }

        let item_exprs: Vec<SyntaxNode> = child
            .children()
            .take_while(|c| is_expr_kind(c.kind()))
            .collect();

        for expr in &item_exprs {
            let item_val = eval_gen_condition(
                ctx,
                expr,
                env.file_id,
                env.sig,
                env.param_env,
                env.genvar_binding,
            );
            if let Some(v) = item_val
                && v == case_val
            {
                matched_body = child.children().last();
                break;
            }
        }

        if matched_body.is_some() {
            break;
        }
    }

    let Some(body) = matched_body.or(default_body) else {
        return;
    };

    let scope_key = GenScopeKey {
        file: env.file_id,
        offset: node.text_range(),
        iter: None,
    };

    let block_name = extract_block_name(&body);

    ctx.gen_scopes
        .entry(scope_key.clone())
        .or_insert_with(|| GenScopeNode {
            key: scope_key.clone(),
            parent: env.parent_scope.clone(),
            name: block_name,
            kind: GenScopeKind::CaseItem,
            children: Vec::new(),
        });

    let scope_item = ElabItemKey::GenScope(scope_key.clone());
    add_child_to_scope(ctx, env.parent_scope, scope_item);

    let child_scope = ScopeKey::GenScope(scope_key);
    let child_env = ScopeEnv {
        parent_scope: &child_scope,
        ..*env
    };
    expand_body_children(ctx, &body, &child_env);
}

fn process_generate_block(ctx: &mut ElabCtx<'_>, node: &SyntaxNode, env: &ScopeEnv<'_>) {
    let block_name = extract_block_name(node);

    let scope_key = GenScopeKey {
        file: env.file_id,
        offset: node.text_range(),
        iter: None,
    };

    ctx.gen_scopes
        .entry(scope_key.clone())
        .or_insert_with(|| GenScopeNode {
            key: scope_key.clone(),
            parent: env.parent_scope.clone(),
            name: block_name,
            kind: GenScopeKind::If,
            children: Vec::new(),
        });

    let scope_item = ElabItemKey::GenScope(scope_key.clone());
    add_child_to_scope(ctx, env.parent_scope, scope_item);

    let child_scope = ScopeKey::GenScope(scope_key);
    let child_env = ScopeEnv {
        parent_scope: &child_scope,
        ..*env
    };
    expand_body_children(ctx, node, &child_env);
}

fn add_child_to_scope(ctx: &mut ElabCtx<'_>, scope: &ScopeKey, item: ElabItemKey) {
    match scope {
        ScopeKey::Instance(ik) => {
            if let Some(node) = ctx.nodes.get_mut(ik) {
                node.children.push(item);
            }
        }
        ScopeKey::GenScope(gk) => {
            if let Some(node) = ctx.gen_scopes.get_mut(gk) {
                node.children.push(item);
            }
        }
    }
}

fn eval_gen_condition(
    ctx: &mut ElabCtx<'_>,
    expr_node: &SyntaxNode,
    file_id: FileId,
    sig: &ModuleSig,
    param_env: &Arc<ParamEnv>,
    genvar_binding: Option<(&SmolStr, i64)>,
) -> Option<i64> {
    let source_file = source_file_by_id(ctx.db, ctx.unit, file_id)?;
    let id_map = ast_id_map(ctx.db, source_file);

    // Try DB-backed evaluation first for simple literal cases (no genvar, no params)
    if genvar_binding.is_none()
        && sig.params.is_empty()
        && let Some(expr_ast_id) = id_map.erased_ast_id(expr_node)
    {
        let expr_ref = crate::const_eval::ConstExprRef::new(ctx.db, ctx.unit, expr_ast_id);
        let result = crate::const_eval::eval_const_int(ctx.db, expr_ref);
        if let ConstInt::Known(v) = result {
            return Some(v);
        }
    }

    // Inline evaluation with genvar and param_env lookup in the closure
    let genvar_name = genvar_binding.map(|(n, _)| n.clone());
    let genvar_value = genvar_binding.map(|(_, v)| v);
    let env_values = param_env.values_slice();

    let resolve_name = |name_node: &SyntaxNode| -> Result<i64, ConstEvalError> {
        let name_text = extract_name_text(name_node)?;

        // Check genvar binding first
        if let (Some(gn), Some(gv)) = (&genvar_name, genvar_value)
            && name_text == gn.as_str()
        {
            return Ok(gv);
        }

        // Check param env by name via sig
        if let Some((idx, _)) = sig.param_by_name(&name_text)
            && let Some(val) = env_values.get(idx as usize)
        {
            return match val {
                ConstInt::Known(v) => Ok(*v),
                ConstInt::Error(e) => Err(*e),
                ConstInt::Unevaluated(_) => Err(ConstEvalError::Unsupported),
            };
        }

        Err(ConstEvalError::Unresolved)
    };

    eval_const_expr(expr_node, &resolve_name).ok()
}

fn extract_name_text(name_node: &SyntaxNode) -> Result<String, ConstEvalError> {
    match name_node.kind() {
        SyntaxKind::NameRef => name_node
            .children_with_tokens()
            .filter_map(|el| el.into_token())
            .find(|t| t.kind() == SyntaxKind::Ident)
            .map(|t| t.text().to_string())
            .ok_or(ConstEvalError::Unresolved),
        SyntaxKind::QualifiedName => name_node
            .children_with_tokens()
            .filter_map(|el| el.into_token())
            .filter(|t| t.kind() == SyntaxKind::Ident)
            .last()
            .map(|t| t.text().to_string())
            .ok_or(ConstEvalError::Unresolved),
        _ => Err(ConstEvalError::Unresolved),
    }
}

use crate::elab_lower::{ForLimit, extract_block_name, extract_for_parts, is_expr_kind};
