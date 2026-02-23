use std::collections::HashMap;

use lyra_ast::{AstNode, ErasedAstId};
use lyra_lexer::SyntaxKind;
use lyra_parser::{SyntaxElement, SyntaxNode, SyntaxToken};
use lyra_semantic::global_index::DefinitionKind;
use lyra_semantic::symbols::GlobalDefId;
use lyra_semantic::types::ConstInt;
use lyra_source::{FileId, Span, TextRange};
use smol_str::SmolStr;

use crate::elab_eval::{
    EvalEnv, build_param_env, cond_site_key, eval_env_expr, eval_gen_condition,
    extract_param_overrides,
};
use crate::elaboration::{
    CondCacheKey, CondSiteKind, ElabDiag, ElabNodeId, ElabTree, GenScopeKind, GenScopeNode,
    GenScopeOrigin, GenvarEnvId, GenvarEnvInterner, InstId, InstOrigin, InstanceNode, ParamEnvId,
    ParamEnvInterner,
};
use crate::module_sig::{DesignUnitSig, ParamKind, PortDirection, PortSig};
use crate::pipeline::{ast_id_map, parse_file};
use crate::semantic::{def_index_file, global_def_index};
use crate::type_queries::{SymbolRef, type_of_symbol};
use crate::{CompilationUnit, source_file_by_id};

const ELAB_RECURSION_LIMIT: usize = 256;

#[salsa::interned]
pub struct TopModule<'db> {
    pub unit: CompilationUnit,
    pub name: SmolStr,
}

#[salsa::interned]
pub struct DesignUnitRef<'db> {
    pub unit: CompilationUnit,
    pub def_id: GlobalDefId,
}

enum DesignUnitDeclNode {
    Module(lyra_ast::ModuleDecl),
    Interface(lyra_ast::InterfaceDecl),
}

impl DesignUnitDeclNode {
    fn cast(node: &SyntaxNode) -> Option<Self> {
        if let Some(m) = lyra_ast::ModuleDecl::cast(node.clone()) {
            return Some(Self::Module(m));
        }
        if let Some(i) = lyra_ast::InterfaceDecl::cast(node.clone()) {
            return Some(Self::Interface(i));
        }
        None
    }

    fn name(&self) -> Option<SyntaxToken> {
        match self {
            Self::Module(m) => m.name(),
            Self::Interface(i) => i.name(),
        }
    }

    fn port_list(&self) -> Option<lyra_ast::PortList> {
        match self {
            Self::Module(m) => m.port_list(),
            Self::Interface(i) => i.port_list(),
        }
    }

    fn param_port_list(&self) -> Option<lyra_ast::ParamPortList> {
        match self {
            Self::Module(m) => m.param_port_list(),
            Self::Interface(i) => i.param_port_list(),
        }
    }

    fn body_syntax(&self) -> Option<SyntaxNode> {
        match self {
            Self::Module(m) => m.body().map(|b| b.syntax().clone()),
            Self::Interface(i) => i.body().map(|b| b.syntax().clone()),
        }
    }
}

#[salsa::tracked(return_ref)]
pub fn design_unit_signature<'db>(
    db: &'db dyn salsa::Database,
    module: DesignUnitRef<'db>,
) -> DesignUnitSig {
    let unit = module.unit(db);
    let def_id = module.def_id(db);
    let file_id = def_id.file();

    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return DesignUnitSig::new(SmolStr::default(), Vec::new(), Vec::new());
    };

    let parse = parse_file(db, source_file);
    let id_map = ast_id_map(db, source_file);
    let def = def_index_file(db, source_file);

    let Some(node) = id_map.get_node(&parse.syntax(), def_id.ast_id()) else {
        return DesignUnitSig::new(SmolStr::default(), Vec::new(), Vec::new());
    };

    let Some(decl) = DesignUnitDeclNode::cast(&node) else {
        return DesignUnitSig::new(SmolStr::default(), Vec::new(), Vec::new());
    };

    let name = decl
        .name()
        .map(|t| SmolStr::new(t.text()))
        .unwrap_or_default();

    let ports = extract_port_sigs(db, unit, file_id, decl.port_list(), id_map, def);
    let params = extract_param_sigs(decl.param_port_list(), id_map);

    DesignUnitSig::new(name, ports, params)
}

fn extract_port_sigs(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    file_id: FileId,
    port_list: Option<lyra_ast::PortList>,
    id_map: &lyra_ast::AstIdMap,
    def: &lyra_semantic::def_index::DefIndex,
) -> Vec<PortSig> {
    let Some(port_list) = port_list else {
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
                    lyra_semantic::types::SymbolType::Value(ty) => Some(ty),
                    _ => None,
                }
            })
            .unwrap_or(lyra_semantic::types::Ty::Error);

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
    param_port_list: Option<lyra_ast::ParamPortList>,
    id_map: &lyra_ast::AstIdMap,
) -> Vec<crate::module_sig::ParamSig> {
    let Some(param_port_list) = param_port_list else {
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

            params.push(crate::module_sig::ParamSig {
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

#[salsa::tracked(return_ref)]
pub fn elaborate_top<'db>(db: &'db dyn salsa::Database, top: TopModule<'db>) -> ElabTree {
    let unit = top.unit(db);
    let name = top.name(db);
    let global = global_def_index(db, unit);

    let mut diags = Vec::new();
    let mut interner = ParamEnvInterner::new();
    let genvar_interner = GenvarEnvInterner::new();

    let empty_tree = |diags, interner, genvar_interner| ElabTree {
        top: None,
        instances: Vec::new(),
        gen_scopes: Vec::new(),
        diagnostics: diags,
        envs: interner,
        genvar_envs: genvar_interner,
    };

    let Some((top_def_id, top_kind)) = global.resolve_definition(&name) else {
        diags.push(ElabDiag::UnresolvedModuleInst {
            name: name.clone(),
            span: Span {
                file: FileId(0),
                range: TextRange::default(),
            },
        });
        return empty_tree(diags, interner, genvar_interner);
    };

    if top_kind != DefinitionKind::Module {
        diags.push(ElabDiag::NotInstantiable {
            name: name.clone(),
            span: Span {
                file: FileId(0),
                range: TextRange::default(),
            },
        });
        return empty_tree(diags, interner, genvar_interner);
    }

    let file_id = top_def_id.file();
    let top_name_range = unit_name_range(db, unit, top_def_id);

    let sig = design_unit_signature(db, DesignUnitRef::new(db, unit, top_def_id));
    let param_env = build_param_env(
        db,
        unit,
        sig,
        &[],
        Span {
            file: file_id,
            range: top_name_range,
        },
        &mut diags,
        &mut interner,
    );

    let top_origin = InstOrigin {
        parent_inst: None,
        inst_stmt_ast: ErasedAstId::placeholder(file_id),
        inst_ordinal: 0,
    };

    let mut tree = ElabTree {
        top: None,
        instances: Vec::new(),
        gen_scopes: Vec::new(),
        diagnostics: Vec::new(),
        envs: interner,
        genvar_envs: genvar_interner,
    };

    let top_id = tree.push_instance(InstanceNode {
        origin: top_origin,
        parent: None,
        module_def: top_def_id,
        instance_name: name.clone(),
        param_env,
        children: Vec::new(),
        source_file: file_id,
        name_range: top_name_range,
    });
    tree.top = Some(top_id);

    let mut active_stack = Vec::new();
    let mut cond_cache = HashMap::new();
    let max_generate_iters = max_generate_iterations();

    let mut ctx = ElabCtx {
        db,
        unit,
        tree: &mut tree,
        diags: &mut diags,
        active_stack: &mut active_stack,
        cond_cache: &mut cond_cache,
        max_generate_iters,
    };

    elaborate_module(&mut ctx, top_def_id, top_id, ElabNodeId::Inst(top_id));

    tree.diagnostics = diags;
    tree
}

fn max_generate_iterations() -> usize {
    #[cfg(test)]
    {
        32
    }
    #[cfg(not(test))]
    {
        10_000
    }
}

struct ElabCtx<'a> {
    db: &'a dyn salsa::Database,
    unit: CompilationUnit,
    tree: &'a mut ElabTree,
    diags: &'a mut Vec<ElabDiag>,
    active_stack: &'a mut Vec<GlobalDefId>,
    cond_cache: &'a mut HashMap<CondCacheKey, ConstInt>,
    max_generate_iters: usize,
}

struct ScopeEnv<'a> {
    file_id: FileId,
    parent_scope: ElabNodeId,
    enclosing_inst: InstId,
    sig: &'a DesignUnitSig,
    param_env: ParamEnvId,
    genvar_env: GenvarEnvId,
    global: &'a lyra_semantic::global_index::GlobalDefIndex,
}

fn make_eval_env<'a>(ctx: &'a ElabCtx<'_>, env: &'a ScopeEnv<'a>) -> EvalEnv<'a> {
    let genvar_values = ctx.tree.genvar_envs.values(env.genvar_env);
    EvalEnv {
        file_id: env.file_id,
        sig: env.sig,
        param_env: env.param_env,
        genvar_values,
        interner: &ctx.tree.envs,
    }
}

fn unit_name_range(
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
    let Some(decl) = DesignUnitDeclNode::cast(&node) else {
        return TextRange::default();
    };
    decl.name().map(|t| t.text_range()).unwrap_or_default()
}

fn elaborate_module(
    ctx: &mut ElabCtx<'_>,
    module_def: GlobalDefId,
    inst_id: InstId,
    parent_scope: ElabNodeId,
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
    elaborate_unit_body(ctx, module_def, inst_id, parent_scope);
    ctx.active_stack.pop();
}

fn elaborate_unit_body(
    ctx: &mut ElabCtx<'_>,
    module_def: GlobalDefId,
    inst_id: InstId,
    parent_scope: ElabNodeId,
) {
    let file_id = module_def.file();
    let Some(source_file) = source_file_by_id(ctx.db, ctx.unit, file_id) else {
        return;
    };
    let parse = parse_file(ctx.db, source_file);
    let id_map = ast_id_map(ctx.db, source_file);
    let global = global_def_index(ctx.db, ctx.unit);

    let Some(node) = id_map.get_node(&parse.syntax(), module_def.ast_id()) else {
        return;
    };
    let Some(decl) = DesignUnitDeclNode::cast(&node) else {
        return;
    };
    // Elaboration currently only builds the instance tree and evaluates
    // generate constructs. Non-elaboration items (signal declarations,
    // always blocks, modport declarations, etc.) are intentionally
    // skipped -- they are handled by per-file semantic queries, not
    // the elaboration pass.
    let Some(body) = decl.body_syntax() else {
        return;
    };

    let sig = design_unit_signature(ctx.db, DesignUnitRef::new(ctx.db, ctx.unit, module_def));
    let param_env = ctx.tree.inst(inst_id).param_env;

    let env = ScopeEnv {
        file_id,
        parent_scope,
        enclosing_inst: inst_id,
        sig,
        param_env,
        genvar_env: GenvarEnvId::EMPTY,
        global,
    };
    expand_body_children(ctx, &body, &env);
}

fn expand_body_children(ctx: &mut ElabCtx<'_>, body_node: &SyntaxNode, env: &ScopeEnv<'_>) {
    for child in body_node.children() {
        match child.kind() {
            SyntaxKind::ModuleInstantiation => {
                process_instantiation(ctx, &child, env);
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

fn process_instantiation(ctx: &mut ElabCtx<'_>, node: &SyntaxNode, env: &ScopeEnv<'_>) {
    let Some(inst_node) = lyra_ast::ModuleInstantiation::cast(node.clone()) else {
        return;
    };
    let Some(name_tok) = inst_node.module_name() else {
        return;
    };
    let inst_module_name = SmolStr::new(name_tok.text());
    let name_span = Span {
        file: env.file_id,
        range: name_tok.text_range(),
    };

    let Some((target_def_id, target_kind)) = env.global.resolve_definition(&inst_module_name)
    else {
        ctx.diags.push(ElabDiag::UnresolvedModuleInst {
            name: inst_module_name,
            span: name_span,
        });
        return;
    };

    if !target_kind.is_instantiable() {
        ctx.diags.push(ElabDiag::NotInstantiable {
            name: inst_module_name,
            span: name_span,
        });
        return;
    }

    let target_sig =
        design_unit_signature(ctx.db, DesignUnitRef::new(ctx.db, ctx.unit, target_def_id));

    let source_file = source_file_by_id(ctx.db, ctx.unit, env.file_id);
    let id_map = source_file.map(|sf| ast_id_map(ctx.db, sf));

    let inst_stmt_ast = id_map
        .as_ref()
        .and_then(|m| m.erased_ast_id(node))
        .unwrap_or_else(|| ErasedAstId::placeholder(env.file_id));

    let eval_env = make_eval_env(ctx, env);
    let overrides = extract_param_overrides(&inst_node, ctx.db, ctx.unit, &eval_env);

    #[cfg(debug_assertions)]
    let mut debug_origins = std::collections::HashSet::new();

    for (idx, (inst_name_tok, port_list)) in inst_node.instances().enumerate() {
        let origin = InstOrigin {
            parent_inst: Some(env.enclosing_inst),
            inst_stmt_ast,
            inst_ordinal: idx as u32,
        };

        #[cfg(debug_assertions)]
        {
            assert!(
                debug_origins.insert(origin),
                "duplicate instance origin under parent {:?}",
                env.enclosing_inst
            );
        }

        let child_param_env = build_param_env(
            ctx.db,
            ctx.unit,
            target_sig,
            &overrides,
            Span {
                file: env.file_id,
                range: inst_name_tok.text_range(),
            },
            ctx.diags,
            &mut ctx.tree.envs,
        );

        if let Some(ref pl) = port_list {
            let mut bindings = crate::elab_lower::resolve_port_connections(
                pl,
                target_sig,
                &inst_module_name,
                env.file_id,
                inst_name_tok.text_range(),
                id_map,
                ctx.diags,
            );
            add_implicit_port_bindings(pl, target_sig, env, &mut bindings);
            crate::elab_lower::check_modport_conflicts(
                ctx.db,
                ctx.unit,
                &bindings,
                target_sig,
                target_def_id,
                ctx.diags,
            );
        }

        let child_id = ctx.tree.push_instance(InstanceNode {
            origin,
            parent: Some(env.parent_scope),
            module_def: target_def_id,
            instance_name: SmolStr::new(inst_name_tok.text()),
            param_env: child_param_env,
            children: Vec::new(),
            source_file: env.file_id,
            name_range: inst_name_tok.text_range(),
        });

        ctx.tree
            .add_child(env.parent_scope, ElabNodeId::Inst(child_id));

        elaborate_module(ctx, target_def_id, child_id, ElabNodeId::Inst(child_id));
    }
}

fn add_implicit_port_bindings(
    port_list: &lyra_ast::InstancePortList,
    target_sig: &DesignUnitSig,
    env: &ScopeEnv<'_>,
    bindings: &mut Vec<crate::elab_lower::PortBinding>,
) {
    use crate::elab_lower::{PortActual, PortBinding};

    let mut has_wildcard = false;
    for port in port_list.ports() {
        if port.is_wildcard() {
            has_wildcard = true;
            continue;
        }
        if !port.is_named() || port.actual_expr().is_some() {
            continue;
        }
        let has_parens = port
            .syntax()
            .children_with_tokens()
            .any(|c| c.kind() == SyntaxKind::LParen);
        if has_parens {
            continue;
        }
        let Some(name_tok) = port.port_name() else {
            continue;
        };
        let name = name_tok.text();
        let Some((formal_idx, _)) = target_sig.port_by_name(name) else {
            continue;
        };
        let Some(enclosing_port) = env.sig.ports.iter().find(|p| p.name == name) else {
            continue;
        };
        bindings.push(PortBinding {
            formal_idx,
            actual: PortActual::Resolved(enclosing_port.ty.clone()),
            conn_span: Span {
                file: env.file_id,
                range: port.text_range(),
            },
        });
    }

    if !has_wildcard {
        return;
    }
    let bound: std::collections::HashSet<u32> = bindings.iter().map(|b| b.formal_idx).collect();
    let wildcard_span = Span {
        file: env.file_id,
        range: port_list.text_range(),
    };
    for (idx, formal) in target_sig.ports.iter().enumerate() {
        let idx = idx as u32;
        if bound.contains(&idx) {
            continue;
        }
        let Some(enclosing_port) = env.sig.ports.iter().find(|p| p.name == formal.name) else {
            continue;
        };
        bindings.push(PortBinding {
            formal_idx: idx,
            actual: PortActual::Resolved(enclosing_port.ty.clone()),
            conn_span: wildcard_span,
        });
    }
}

fn process_generate_if(ctx: &mut ElabCtx<'_>, node: &SyntaxNode, env: &ScopeEnv<'_>) {
    let children: Vec<SyntaxNode> = node.children().collect();
    if children.is_empty() {
        return;
    }

    let source_file = source_file_by_id(ctx.db, ctx.unit, env.file_id);
    let id_map = source_file.map(|sf| ast_id_map(ctx.db, sf));
    let scope_ast = id_map
        .as_ref()
        .and_then(|m| m.erased_ast_id(node))
        .unwrap_or_else(|| ErasedAstId::placeholder(env.file_id));

    let cache_key = Some(cond_site_key(
        scope_ast,
        CondSiteKind::GenIf,
        env.param_env,
        env.genvar_env,
    ));

    let genvar_values = ctx.tree.genvar_envs.values(env.genvar_env);
    let eval_env = EvalEnv {
        file_id: env.file_id,
        sig: env.sig,
        param_env: env.param_env,
        genvar_values,
        interner: &ctx.tree.envs,
    };
    let cond_val = eval_gen_condition(
        ctx.db,
        ctx.unit,
        &children[0],
        &eval_env,
        ctx.cond_cache,
        cache_key,
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

    let origin = GenScopeOrigin {
        parent_inst: env.enclosing_inst,
        scope_ast,
        iter: None,
    };

    let block_name = extract_block_name(body);

    let scope_id = ctx.tree.push_gen_scope(GenScopeNode {
        origin,
        parent: env.parent_scope,
        name: block_name,
        kind: GenScopeKind::If,
        children: Vec::new(),
        source_file: env.file_id,
        offset: node.text_range(),
    });

    ctx.tree
        .add_child(env.parent_scope, ElabNodeId::GenScope(scope_id));

    let child_env = ScopeEnv {
        parent_scope: ElabNodeId::GenScope(scope_id),
        ..*env
    };
    expand_body_children(ctx, body, &child_env);
}

fn process_generate_for(ctx: &mut ElabCtx<'_>, node: &SyntaxNode, env: &ScopeEnv<'_>) {
    let eval_env = make_eval_env(ctx, env);
    let eval_fn = |expr_node: &SyntaxNode| -> Option<i64> {
        eval_env_expr(ctx.db, ctx.unit, expr_node, &eval_env).ok()
    };
    let for_parts = extract_for_parts(node, &eval_fn);

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

    let source_file = source_file_by_id(ctx.db, ctx.unit, env.file_id);
    let id_map = source_file.map(|sf| ast_id_map(ctx.db, sf));
    let scope_ast = id_map
        .as_ref()
        .and_then(|m| m.erased_ast_id(node))
        .unwrap_or_else(|| ErasedAstId::placeholder(env.file_id));

    let genvar_name = parts.genvar_name;
    let mut current = parts.init;
    let limit = parts.limit;
    let step = parts.step;

    if step == 0 {
        return;
    }

    let max_iters = ctx.max_generate_iters;
    let mut iteration = 0;

    while iteration < max_iters {
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

        let iter_value = ConstInt::Known(current);
        let origin = GenScopeOrigin {
            parent_inst: env.enclosing_inst,
            scope_ast,
            iter: Some(iter_value.clone()),
        };

        let block_name = extract_block_name(&body);

        let scope_id = ctx.tree.push_gen_scope(GenScopeNode {
            origin,
            parent: env.parent_scope,
            name: block_name,
            kind: GenScopeKind::ForIteration {
                genvar_name: genvar_name.clone(),
                genvar_value: iter_value.clone(),
            },
            children: Vec::new(),
            source_file: env.file_id,
            offset: node.text_range(),
        });

        ctx.tree
            .add_child(env.parent_scope, ElabNodeId::GenScope(scope_id));

        let child_genvar_env =
            ctx.tree
                .genvar_envs
                .push(env.genvar_env, genvar_name.clone(), iter_value);

        let child_env = ScopeEnv {
            parent_scope: ElabNodeId::GenScope(scope_id),
            genvar_env: child_genvar_env,
            ..*env
        };
        expand_body_children(ctx, &body, &child_env);

        current = current.wrapping_add(step);
        iteration += 1;
    }

    if iteration >= max_iters {
        let still_going = match limit {
            ForLimit::Lt(n) => current < n,
            ForLimit::Le(n) => current <= n,
            ForLimit::Gt(n) => current > n,
            ForLimit::Ge(n) => current >= n,
            ForLimit::Ne(n) => current != n,
        };
        if still_going {
            ctx.diags.push(ElabDiag::GenerateIterationLimit {
                span: Span {
                    file: env.file_id,
                    range: node.text_range(),
                },
                limit: max_iters,
            });
        }
    }
}

fn match_case_body(
    child_nodes: &[SyntaxNode],
    case_val: i64,
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    cond_cache: &mut HashMap<CondCacheKey, ConstInt>,
    eval_env: &EvalEnv<'_>,
) -> Option<(Option<SyntaxNode>, SyntaxNode)> {
    let mut matched_body: Option<(SyntaxNode, SyntaxNode)> = None;
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
            let item_val = eval_gen_condition(db, unit, expr, eval_env, cond_cache, None);
            if let Some(v) = item_val
                && v == case_val
            {
                if let Some(body_node) = child.children().last() {
                    matched_body = Some((child.clone(), body_node));
                }
                break;
            }
        }

        if matched_body.is_some() {
            break;
        }
    }

    if let Some((item, body)) = matched_body {
        Some((Some(item), body))
    } else {
        default_body.map(|body| (None, body))
    }
}

fn process_generate_case(ctx: &mut ElabCtx<'_>, node: &SyntaxNode, env: &ScopeEnv<'_>) {
    let child_nodes: Vec<SyntaxNode> = node.children().collect();
    if child_nodes.is_empty() {
        return;
    }

    let source_file = source_file_by_id(ctx.db, ctx.unit, env.file_id);
    let id_map = source_file.map(|sf| ast_id_map(ctx.db, sf));
    let case_stmt_ast = id_map
        .as_ref()
        .and_then(|m| m.erased_ast_id(node))
        .unwrap_or_else(|| ErasedAstId::placeholder(env.file_id));

    let cache_key = Some(cond_site_key(
        case_stmt_ast,
        CondSiteKind::GenCase,
        env.param_env,
        env.genvar_env,
    ));

    let genvar_values = ctx.tree.genvar_envs.values(env.genvar_env);
    let eval_env = EvalEnv {
        file_id: env.file_id,
        sig: env.sig,
        param_env: env.param_env,
        genvar_values,
        interner: &ctx.tree.envs,
    };
    let case_val = eval_gen_condition(
        ctx.db,
        ctx.unit,
        &child_nodes[0],
        &eval_env,
        ctx.cond_cache,
        cache_key,
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

    let Some((case_item_node, body)) = match_case_body(
        &child_nodes,
        case_val,
        ctx.db,
        ctx.unit,
        ctx.cond_cache,
        &eval_env,
    ) else {
        return;
    };

    let scope_ast = case_item_node
        .as_ref()
        .and_then(|item| id_map.as_ref().and_then(|m| m.erased_ast_id(item)))
        .unwrap_or(case_stmt_ast);

    let origin = GenScopeOrigin {
        parent_inst: env.enclosing_inst,
        scope_ast,
        iter: None,
    };

    let block_name = extract_block_name(&body);

    let scope_id = ctx.tree.push_gen_scope(GenScopeNode {
        origin,
        parent: env.parent_scope,
        name: block_name,
        kind: GenScopeKind::CaseItem,
        children: Vec::new(),
        source_file: env.file_id,
        offset: node.text_range(),
    });

    ctx.tree
        .add_child(env.parent_scope, ElabNodeId::GenScope(scope_id));

    let child_env = ScopeEnv {
        parent_scope: ElabNodeId::GenScope(scope_id),
        ..*env
    };
    expand_body_children(ctx, &body, &child_env);
}

fn process_generate_block(ctx: &mut ElabCtx<'_>, node: &SyntaxNode, env: &ScopeEnv<'_>) {
    let block_name = extract_block_name(node);

    let source_file = source_file_by_id(ctx.db, ctx.unit, env.file_id);
    let id_map = source_file.map(|sf| ast_id_map(ctx.db, sf));
    let scope_ast = id_map
        .as_ref()
        .and_then(|m| m.erased_ast_id(node))
        .unwrap_or_else(|| ErasedAstId::placeholder(env.file_id));

    let origin = GenScopeOrigin {
        parent_inst: env.enclosing_inst,
        scope_ast,
        iter: None,
    };

    let scope_id = ctx.tree.push_gen_scope(GenScopeNode {
        origin,
        parent: env.parent_scope,
        name: block_name,
        kind: GenScopeKind::If,
        children: Vec::new(),
        source_file: env.file_id,
        offset: node.text_range(),
    });

    ctx.tree
        .add_child(env.parent_scope, ElabNodeId::GenScope(scope_id));

    let child_env = ScopeEnv {
        parent_scope: ElabNodeId::GenScope(scope_id),
        ..*env
    };
    expand_body_children(ctx, node, &child_env);
}

use crate::elab_lower::{ForLimit, extract_block_name, extract_for_parts, is_expr_kind};
