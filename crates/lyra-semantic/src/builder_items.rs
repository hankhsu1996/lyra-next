use lyra_ast::{
    AstNode, DeclLifetimeSyntax, Declarator, ExportDecl, ExportItem, ForeachStmt, FunctionDecl,
    HasSyntax, ImportItem, ModportDecl, ModportPortKind, ModportTfPortsGroup, ModuleInstantiation,
    TaskDecl, TfPortDecl, TimeprecisionDecl, TimeunitDecl, TypeSpec,
};
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;
use lyra_source::NameSpan;
use smol_str::SmolStr;

use crate::builder::DefContext;
use crate::builder_types::{collect_name_refs, collect_type_spec_refs, detect_aggregate_type};
use crate::def_index::{
    ExpectedNs, ExportDeclId, ExportKey, ForeachVarDef, Import, ImportDeclId, ImportName, NamePath,
    UseSite,
};
use crate::instance_decl::{InstanceDecl, InstanceDeclIdx};
use crate::interface_id::InterfaceDefId;
use crate::modport_def::{
    ModportDef, ModportDefId, ModportEntry, ModportTarget, ModportTfEntry, ModportTfForm,
    PortDirection, TfPortKind,
};
use crate::record::SymbolOrigin;
use crate::scopes::{ScopeId, ScopeKind};
use crate::symbols::{Constness, Lifetime, Namespace, Symbol, SymbolKind};
use crate::time_scale::{TimeLiteral, TimeUnitsDecl};

use crate::builder::RawModportEntry;

pub(crate) fn collect_module_instantiation(
    ctx: &mut DefContext<'_>,
    node: &SyntaxNode,
    scope: ScopeId,
) {
    // Record the module type name as a Definition-namespace use-site
    if let Some(inst) = ModuleInstantiation::cast(node.clone())
        && let Some(name_tok) = inst.module_name()
        && let Some(ast_id) = ctx.ast_id_map.ast_id(&inst)
    {
        let type_use_site_idx = ctx.use_sites.len() as u32;
        ctx.use_sites.push(UseSite {
            path: NamePath::Simple(SmolStr::new(name_tok.text())),
            expected_ns: ExpectedNs::Exact(Namespace::Definition),
            scope,
            name_ref_site: ast_id.erase(),
            order_key: 0,
        });
        // Register each instance name as an Instance symbol
        let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
            ctx.emit_internal_error_unanchored(&format!(
                "erased_ast_id returned None for {:?} in collect_module_instantiation",
                node.kind()
            ));
            collect_name_refs(ctx, node, scope);
            return;
        };
        for hier_inst in inst.instances() {
            let Some(inst_name_tok) = hier_inst.name() else {
                continue;
            };
            let Some(inst_name_site) = ctx.ast_id_map.erased_ast_id(hier_inst.syntax()) else {
                ctx.emit_internal_error_unanchored(&format!(
                    "erased_ast_id returned None for {:?} in collect_module_instantiation hier_inst",
                    hier_inst.syntax().kind()
                ));
                continue;
            };
            let idx = InstanceDeclIdx(ctx.instance_decls.len() as u32);
            let sym_id = ctx.push_symbol(Symbol {
                name: SmolStr::new(inst_name_tok.text()),
                kind: SymbolKind::Instance,
                constness: Constness::Mutable,
                lifetime: Lifetime::Static,
                decl_site,
                name_site: inst_name_site,
                type_site: None,
                name_span: NameSpan::new(inst_name_tok.text_range()),
                scope,
                origin: SymbolOrigin::Instance(idx),
            });
            ctx.instance_decls.push(InstanceDecl {
                type_use_site_idx,
                sym_id,
                name: SmolStr::new(inst_name_tok.text()),
                scope,
            });
        }
    }
    // Still collect NameRefs in port connection expressions
    collect_name_refs(ctx, node, scope);
}

pub(crate) fn collect_foreach_vars(
    ctx: &mut DefContext<'_>,
    node: &SyntaxNode,
    parent_scope: ScopeId,
) -> ScopeId {
    let foreach_scope = ctx.scopes.push(ScopeKind::Block, Some(parent_scope));
    let Some(fs) = ForeachStmt::cast(node.clone()) else {
        return foreach_scope;
    };
    let Some(foreach_stmt_site) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error_unanchored(&format!(
            "erased_ast_id returned None for {:?} in collect_foreach_vars",
            node.kind()
        ));
        return foreach_scope;
    };
    let Some(var_list) = fs.var_list() else {
        return foreach_scope;
    };
    for (slot_index, slot) in var_list.slots().enumerate() {
        let Some(decl) = slot.declarator() else {
            continue; // skipped slot
        };
        let Some(name_tok) = decl.name() else {
            continue;
        };
        let Some(decl_name_site) = ctx.ast_id_map.erased_ast_id(decl.syntax()) else {
            ctx.emit_internal_error_unanchored(&format!(
                "erased_ast_id returned None for {:?} in collect_foreach_vars declarator",
                decl.syntax().kind()
            ));
            continue;
        };
        let sym_id = ctx.push_symbol(Symbol {
            name: SmolStr::new(name_tok.text()),
            kind: SymbolKind::Variable,
            constness: Constness::Mutable,
            lifetime: Lifetime::Automatic,
            decl_site: foreach_stmt_site,
            name_site: decl_name_site,
            type_site: None,
            name_span: NameSpan::new(name_tok.text_range()),
            scope: foreach_scope,
            origin: SymbolOrigin::TypeSpec,
        });
        ctx.register_binding(sym_id);
        ctx.foreach_var_defs.insert(
            sym_id,
            ForeachVarDef {
                foreach_stmt: foreach_stmt_site,
                slot: slot_index as u32,
            },
        );
    }
    foreach_scope
}

pub(crate) fn collect_callable_decl(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    // Cast once; extract all header data from the typed node.
    let (kind, scope_kind, name_tok, lifetime_tok, type_spec, tf_port_decls) =
        if let Some(f) = FunctionDecl::cast(node.clone()) {
            (
                SymbolKind::Function,
                ScopeKind::Function,
                f.name(),
                f.lifetime_token(),
                f.type_spec(),
                f.tf_port_decls().collect::<Box<[_]>>(),
            )
        } else if let Some(t) = TaskDecl::cast(node.clone()) {
            (
                SymbolKind::Task,
                ScopeKind::Task,
                t.name(),
                t.lifetime_token(),
                None,
                t.tf_port_decls().collect::<Box<[_]>>(),
            )
        } else {
            return;
        };

    let Some(name_tok) = name_tok else { return };
    let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error_unanchored(&format!(
            "erased_ast_id returned None for {:?} in collect_callable_decl",
            node.kind()
        ));
        return;
    };
    let name = SmolStr::new(name_tok.text());
    let callable_scope = ctx.scopes.push(scope_kind, Some(scope));
    let callable_type_site = type_spec
        .as_ref()
        .and_then(|ts| ctx.ast_id_map.erased_ast_id(ts.syntax()));
    let lifetime = match lifetime_tok.as_ref().map(|t| t.kind()) {
        Some(SyntaxKind::AutomaticKw) => Lifetime::Automatic,
        Some(SyntaxKind::StaticKw) => Lifetime::Static,
        _ => ctx.lifetime_env.callable_default,
    };
    let sym_id = ctx.push_symbol(Symbol {
        name: name.clone(),
        kind,
        constness: Constness::Mutable,
        lifetime,
        decl_site,
        name_site: decl_site,
        type_site: callable_type_site,
        name_span: NameSpan::new(name_tok.text_range()),
        scope,
        origin: SymbolOrigin::TypeSpec,
    });

    ctx.register_binding(sym_id);

    if let Some(ts) = &type_spec {
        collect_type_spec_refs(ctx, ts, scope);
    }

    collect_tf_ports(ctx, &tf_port_decls, callable_scope);

    // Lower callable body under the callable's procedural lifetime context.
    ctx.with_local_default(lifetime, |ctx| {
        if let Some(f) = FunctionDecl::cast(node.clone()) {
            collect_callable_body(ctx, f.statements(), callable_scope);
        } else if let Some(t) = TaskDecl::cast(node.clone()) {
            collect_callable_body(ctx, t.statements(), callable_scope);
        }
    });
}

/// Lower the body of a callable (function or task) using pre-extracted
/// typed statement iterators. Dispatches `VarDecl` children through
/// `collect_declarators` with the inherited local lifetime, and all
/// other statements through the statement walker.
fn collect_callable_body(
    ctx: &mut DefContext<'_>,
    stmts: impl Iterator<Item = lyra_ast::StmtNode>,
    callable_scope: ScopeId,
) {
    let local_lt = ctx.lifetime_env.local_default;
    for stmt in stmts {
        if stmt.syntax().kind() == SyntaxKind::VarDecl {
            collect_declarators(
                ctx,
                stmt.syntax(),
                SymbolKind::Variable,
                callable_scope,
                local_lt,
            );
        } else {
            crate::builder_stmt::collect_statement(ctx, stmt.syntax(), callable_scope);
        }
    }
}

fn collect_tf_ports(ctx: &mut DefContext<'_>, port_decls: &[TfPortDecl], scope: ScopeId) {
    for port_decl in port_decls {
        // Collect type spec refs for resolution
        if let Some(ts) = port_decl.type_spec() {
            collect_type_spec_refs(ctx, &ts, scope);
        }
        let Some(port_decl_site) = ctx.ast_id_map.erased_ast_id(port_decl.syntax()) else {
            ctx.emit_internal_error_unanchored(&format!(
                "erased_ast_id returned None for {:?} in collect_tf_ports",
                port_decl.syntax().kind()
            ));
            continue;
        };
        let port_type_site = port_decl
            .type_spec()
            .and_then(|ts| ctx.ast_id_map.erased_ast_id(ts.syntax()));
        for decl in port_decl.declarators() {
            if let Some(name_tok) = decl.name() {
                let Some(decl_name_site) = ctx.ast_id_map.erased_ast_id(decl.syntax()) else {
                    ctx.emit_internal_error_unanchored(&format!(
                        "erased_ast_id returned None for {:?} in collect_tf_ports declarator",
                        decl.syntax().kind()
                    ));
                    continue;
                };
                let port_sym = ctx.push_symbol(Symbol {
                    name: SmolStr::new(name_tok.text()),
                    kind: SymbolKind::PortTf,
                    constness: Constness::Mutable,
                    lifetime: Lifetime::Static,
                    decl_site: port_decl_site,
                    name_site: decl_name_site,
                    type_site: port_type_site,
                    name_span: NameSpan::new(name_tok.text_range()),
                    scope,
                    origin: SymbolOrigin::TypeSpec,
                });
                ctx.register_binding(port_sym);
            }
        }
    }
}

pub(crate) fn collect_modport_decl(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    let Some(decl) = ModportDecl::cast(node.clone()) else {
        return;
    };
    let Some(owner) = ctx.current_iface_def_id else {
        return;
    };

    for item in decl.items() {
        // Assign ordinal for every ModportItem node, even malformed ones
        // missing a name, so that later valid items get stable ordinals
        // regardless of whether earlier items have syntax errors.
        let ordinal = ctx.modport_ordinal;
        ctx.modport_ordinal += 1;

        let Some(name_tok) = item.name() else {
            continue;
        };
        let name = SmolStr::new(name_tok.text());

        // Placeholder ModportDefId with a dummy owner; finalization replaces it.
        let placeholder_id = ModportDefId {
            owner: InterfaceDefId::placeholder(),
            ordinal,
        };

        let collected = collect_modport_entries(ctx, &item);

        // Register symbol for navigation/diagnostics
        let Some(modport_decl_site) = ctx.ast_id_map.erased_ast_id(item.syntax()) else {
            ctx.emit_internal_error_unanchored(&format!(
                "erased_ast_id returned None for {:?} in collect_modport_decl",
                item.syntax().kind()
            ));
            continue;
        };
        ctx.push_symbol(Symbol {
            name: name.clone(),
            kind: SymbolKind::Modport,
            constness: Constness::Mutable,
            lifetime: Lifetime::Static,
            decl_site: modport_decl_site,
            name_site: modport_decl_site,
            type_site: None,
            name_span: NameSpan::new(name_tok.text_range()),
            scope,
            origin: SymbolOrigin::TypeSpec,
        });

        ctx.raw_modport_defs.push(RawModportEntry {
            owner,
            ordinal,
            name,
            def: ModportDef {
                id: placeholder_id,
                name: SmolStr::new(name_tok.text()),
                entries: collected.signal_entries.into_boxed_slice(),
                tf_entries: collected.tf_entries.into_boxed_slice(),
            },
        });
    }
}

struct CollectedModportEntries {
    signal_entries: Vec<ModportEntry>,
    tf_entries: Vec<ModportTfEntry>,
}

fn collect_modport_entries(
    ctx: &DefContext<'_>,
    item: &lyra_ast::ModportItem,
) -> CollectedModportEntries {
    let mut signal_entries = Vec::new();
    let mut tf_entries = Vec::new();
    let mut current_dir: Option<PortDirection> = None;
    for port_kind in item.port_items() {
        match port_kind {
            ModportPortKind::Bare(port) => {
                if let Some(dir_tok) = port.direction_token() {
                    current_dir = parse_direction(dir_tok.kind(), current_dir);
                }
                if let Some(dir) = current_dir
                    && let Some(port_name_tok) = port.name()
                    && let Some(port_id) = ctx.ast_id_map.erased_ast_id(port.syntax())
                {
                    let name = SmolStr::new(port_name_tok.text());
                    signal_entries.push(ModportEntry {
                        port_name: name.clone(),
                        direction: dir,
                        target: ModportTarget::ImplicitMember { member_name: name },
                        port_id,
                        name_span: NameSpan::new(port_name_tok.text_range()),
                    });
                }
            }
            ModportPortKind::Expr(port) => {
                if let Some(dir_tok) = port.direction_token() {
                    current_dir = parse_direction(dir_tok.kind(), current_dir);
                }
                if let Some(dir) = current_dir
                    && let Some(port_name_tok) = port.port_name()
                    && let Some(port_id) = ctx.ast_id_map.erased_ast_id(port.syntax())
                {
                    let target = if let Some(expr_node) = port.target_expr() {
                        match ctx.ast_id_map.erased_ast_id(expr_node.syntax()) {
                            Some(expr_id) => ModportTarget::Expr(expr_id),
                            None => ModportTarget::Empty,
                        }
                    } else {
                        ModportTarget::Empty
                    };
                    signal_entries.push(ModportEntry {
                        port_name: SmolStr::new(port_name_tok.text()),
                        direction: dir,
                        target,
                        port_id,
                        name_span: NameSpan::new(port_name_tok.text_range()),
                    });
                }
            }
            ModportPortKind::TfGroup(group) => {
                collect_tf_group_entries(ctx, &group, &mut tf_entries);
            }
        }
    }
    CollectedModportEntries {
        signal_entries,
        tf_entries,
    }
}

fn collect_tf_group_entries(
    ctx: &DefContext<'_>,
    group: &ModportTfPortsGroup,
    tf_entries: &mut Vec<ModportTfEntry>,
) {
    let tf_kind = match group.import_export_token().map(|t| t.kind()) {
        Some(SyntaxKind::ExportKw) => TfPortKind::Export,
        _ => TfPortKind::Import,
    };
    for entry in group.entries() {
        let Some(name_tok) = entry.name() else {
            continue;
        };
        let Some(port_site) = ctx.ast_id_map.erased_ast_id(entry.syntax()) else {
            continue;
        };
        let form = if entry.has_prototype() {
            ModportTfForm::Prototype
        } else {
            ModportTfForm::BareName
        };
        tf_entries.push(ModportTfEntry {
            kind: tf_kind,
            name: SmolStr::new(name_tok.text()),
            form,
            port_site,
            name_span: NameSpan::new(name_tok.text_range()),
        });
    }
}

fn parse_direction(kind: SyntaxKind, current: Option<PortDirection>) -> Option<PortDirection> {
    match kind {
        SyntaxKind::InputKw => Some(PortDirection::Input),
        SyntaxKind::OutputKw => Some(PortDirection::Output),
        SyntaxKind::InoutKw => Some(PortDirection::Inout),
        SyntaxKind::RefKw => Some(PortDirection::Ref),
        _ => current,
    }
}

pub(crate) fn collect_import_decl(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        if child.kind() == SyntaxKind::ImportItem {
            collect_import_item(ctx, &child, scope);
        }
    }
}

fn collect_import_item(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    let Some(ast_id) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error_unanchored(&format!(
            "erased_ast_id returned None for {:?} in collect_import_item",
            node.kind()
        ));
        return;
    };

    // Determine if wildcard or explicit
    let has_star = node
        .children_with_tokens()
        .filter_map(lyra_parser::SyntaxElement::into_token)
        .any(|tok| tok.kind() == SyntaxKind::Star);

    let ordinal = ctx.import_ordinals.entry(scope).or_insert(0);
    let ord = *ordinal;
    *ordinal += 1;
    let id = ImportDeclId {
        scope,
        ordinal: ord,
    };

    if has_star {
        // Wildcard: use typed accessor for package name
        if let Some(pkg_tok) = ImportItem::cast(node.clone()).and_then(|i| i.package_name()) {
            ctx.imports.push(Import {
                id,
                package: SmolStr::new(pkg_tok.text()),
                name: ImportName::Wildcard,
                scope,
                import_stmt_site: ast_id,
                order_key: 0,
            });
        }
    } else if let Some(qn) = node
        .children()
        .find(|c| c.kind() == SyntaxKind::QualifiedName)
    {
        // Non-wildcard: QualifiedName child with [pkg, sym]
        let idents: Vec<_> = qn
            .children_with_tokens()
            .filter_map(lyra_parser::SyntaxElement::into_token)
            .filter(|tok| tok.kind() == SyntaxKind::Ident)
            .collect();
        if idents.len() >= 2 {
            ctx.imports.push(Import {
                id,
                package: SmolStr::new(idents[0].text()),
                name: ImportName::Explicit(SmolStr::new(idents[1].text())),
                scope,
                import_stmt_site: ast_id,
                order_key: 0,
            });
        }
    }
}

pub(crate) fn collect_export_decl(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    let Some(decl) = ExportDecl::cast(node.clone()) else {
        return;
    };
    for item in decl.items() {
        collect_export_item(ctx, &item, scope);
    }
}

fn collect_export_item(ctx: &mut DefContext<'_>, item: &ExportItem, scope: ScopeId) {
    let key = if item.is_all_wildcard() {
        ExportKey::AllWildcard
    } else if let Some(pkg_tok) = item.package_name() {
        let package = SmolStr::new(pkg_tok.text());
        if item.is_wildcard() {
            ExportKey::PackageWildcard { package }
        } else if let Some(member_tok) = item.member_name() {
            ExportKey::Explicit {
                package,
                name: SmolStr::new(member_tok.text()),
            }
        } else {
            return;
        }
    } else {
        return;
    };
    let Some(export_site) = ctx.ast_id_map.erased_ast_id(item.syntax()) else {
        ctx.emit_internal_error_unanchored(&format!(
            "erased_ast_id returned None for {:?} in collect_export_item",
            item.syntax().kind()
        ));
        return;
    };
    let ordinal = ctx.export_ordinals.entry(scope).or_insert(0);
    let ord = *ordinal;
    *ordinal += 1;
    ctx.export_decls.push(crate::def_index::ExportDecl {
        id: ExportDeclId {
            scope,
            ordinal: ord,
        },
        key,
        export_stmt_site: export_site,
    });
}

pub(crate) fn collect_param_decl(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error_unanchored(&format!(
            "erased_ast_id returned None for {:?} in collect_param_decl",
            node.kind()
        ));
        return;
    };
    let param_decl = lyra_ast::ParamDecl::cast(node.clone());
    let is_type_param = param_decl.as_ref().is_some_and(|pd| pd.is_type_param());

    if is_type_param {
        collect_type_param_decl(ctx, node, scope, decl_site);
        return;
    }

    let param_type_site = param_decl
        .and_then(|pd| pd.type_spec())
        .and_then(|ts| ctx.ast_id_map.erased_ast_id(ts.syntax()));
    for child in node.children() {
        if let Some(ts) = TypeSpec::cast(child.clone()) {
            collect_type_spec_refs(ctx, &ts, scope);
        } else if child.kind() == SyntaxKind::Declarator {
            if let Some(name_tok) = Declarator::cast(child.clone()).and_then(|d| d.name()) {
                let Some(decl_name_site) = ctx.ast_id_map.erased_ast_id(&child) else {
                    ctx.emit_internal_error_unanchored(&format!(
                        "erased_ast_id returned None for {:?} in collect_param_decl declarator",
                        child.kind()
                    ));
                    continue;
                };
                let sym_id = ctx.push_symbol(Symbol {
                    name: SmolStr::new(name_tok.text()),
                    kind: SymbolKind::Parameter,
                    constness: Constness::Mutable,
                    lifetime: Lifetime::Static,
                    decl_site,
                    name_site: decl_name_site,
                    type_site: param_type_site,
                    name_span: NameSpan::new(name_tok.text_range()),
                    scope,
                    origin: SymbolOrigin::TypeSpec,
                });
                ctx.register_binding(sym_id);

                // Find init expression (first expression-like child node)
                let init_id = child
                    .children()
                    .find(|c| is_expression_kind(c.kind()))
                    .and_then(|expr| ctx.ast_id_map.erased_ast_id(&expr));
                ctx.name_site_to_init_expr.insert(decl_name_site, init_id);
            }
            // Collect name refs in default value expressions
            collect_name_refs(ctx, &child, scope);
        }
    }
}

fn collect_type_param_decl(
    ctx: &mut DefContext<'_>,
    node: &SyntaxNode,
    scope: ScopeId,
    decl_site: lyra_ast::ErasedAstId,
) {
    for child in node.children() {
        if child.kind() != SyntaxKind::Declarator {
            continue;
        }
        let Some(decl) = Declarator::cast(child.clone()) else {
            continue;
        };
        let Some(name_tok) = decl.name() else {
            continue;
        };
        let Some(decl_name_site) = ctx.ast_id_map.erased_ast_id(&child) else {
            ctx.emit_internal_error_unanchored(&format!(
                "erased_ast_id returned None for {:?} in collect_type_param_decl declarator",
                child.kind()
            ));
            continue;
        };
        // The type_site for a type param is its default TypeSpec (if any)
        let default_type_site = decl
            .default_type_spec()
            .and_then(|ts| ctx.ast_id_map.erased_ast_id(ts.syntax()));
        let sym_id = ctx.push_symbol(Symbol {
            name: SmolStr::new(name_tok.text()),
            kind: SymbolKind::TypeParam,
            constness: Constness::Mutable,
            lifetime: Lifetime::Static,
            decl_site,
            name_site: decl_name_site,
            type_site: default_type_site,
            name_span: NameSpan::new(name_tok.text_range()),
            scope,
            origin: SymbolOrigin::TypeSpec,
        });
        ctx.register_binding(sym_id);
        // Collect type refs in the default TypeSpec for resolution
        if let Some(ts) = decl.default_type_spec() {
            collect_type_spec_refs(ctx, &ts, scope);
        }
    }
}

pub(crate) fn collect_declarators(
    ctx: &mut DefContext<'_>,
    node: &SyntaxNode,
    kind: SymbolKind,
    scope: ScopeId,
    unqualified_lifetime: Lifetime,
) {
    let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error_unanchored(&format!(
            "erased_ast_id returned None for {:?} in collect_declarators",
            node.kind()
        ));
        return;
    };
    // Detect inline enum/struct in the TypeSpec child
    let origin = detect_aggregate_type(ctx, node, scope);
    // Detect const qualifier (LRM 6.20.6)
    let var_decl_ast = match node.kind() {
        SyntaxKind::VarDecl => lyra_ast::VarDecl::cast(node.clone()),
        _ => None,
    };
    let constness = if var_decl_ast
        .as_ref()
        .and_then(|vd| vd.const_token())
        .is_some()
    {
        Constness::Const
    } else {
        Constness::Mutable
    };
    // Detect explicit lifetime qualifier (LRM 6.21)
    let lifetime = match var_decl_ast.as_ref().and_then(|vd| vd.lifetime()) {
        Some(DeclLifetimeSyntax::Automatic(_)) => Lifetime::Automatic,
        Some(DeclLifetimeSyntax::Static(_)) => Lifetime::Static,
        None => unqualified_lifetime,
    };
    let decl_type_site = match node.kind() {
        SyntaxKind::VarDecl => lyra_ast::VarDecl::cast(node.clone())
            .and_then(|vd| vd.type_spec())
            .and_then(|ts| ctx.ast_id_map.erased_ast_id(ts.syntax())),
        SyntaxKind::NetDecl => lyra_ast::NetDecl::cast(node.clone())
            .and_then(|nd| nd.type_spec())
            .and_then(|ts| ctx.ast_id_map.erased_ast_id(ts.syntax())),
        other => {
            ctx.emit_internal_error_unanchored(&format!(
                "collect_declarators called with unexpected {other:?}"
            ));
            None
        }
    };
    for child in node.children() {
        if let Some(ts) = TypeSpec::cast(child.clone()) {
            collect_type_spec_refs(ctx, &ts, scope);
        } else if child.kind() == SyntaxKind::Declarator {
            if let Some(name_tok) = Declarator::cast(child.clone()).and_then(|d| d.name()) {
                let Some(decl_name_site) = ctx.ast_id_map.erased_ast_id(&child) else {
                    ctx.emit_internal_error_unanchored(&format!(
                        "erased_ast_id returned None for {:?} in collect_declarators declarator",
                        child.kind()
                    ));
                    continue;
                };
                let sym_id = ctx.push_symbol(Symbol {
                    name: SmolStr::new(name_tok.text()),
                    kind,
                    constness,
                    lifetime,
                    decl_site,
                    name_site: decl_name_site,
                    type_site: decl_type_site,
                    name_span: NameSpan::new(name_tok.text_range()),
                    scope,
                    origin,
                });
                ctx.register_binding(sym_id);
            }
            collect_name_refs(ctx, &child, scope);
        }
    }
}

pub(crate) fn collect_timeunit_decl(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error_unanchored(&format!(
            "erased_ast_id returned None for {:?} in collect_timeunit_decl",
            node.kind()
        ));
        return;
    };
    let Some(decl) = TimeunitDecl::cast(node.clone()) else {
        ctx.emit_internal_error("TimeunitDecl::cast failed on TimeunitDecl node", decl_site);
        return;
    };
    let Some(unit_tok) = decl.unit_literal_token() else {
        ctx.emit_internal_error("missing unit literal token in TimeunitDecl", decl_site);
        return;
    };
    let precision = decl.precision_literal_token().map(|tok| TimeLiteral {
        raw: SmolStr::new(tok.text()),
    });
    let entry = TimeUnitsDecl::Timeunit {
        decl_site,
        unit: TimeLiteral {
            raw: SmolStr::new(unit_tok.text()),
        },
        precision,
    };
    ctx.scope_time_units
        .entry(scope)
        .or_default()
        .decls
        .push(entry);
}

pub(crate) fn collect_timeprecision_decl(
    ctx: &mut DefContext<'_>,
    node: &SyntaxNode,
    scope: ScopeId,
) {
    let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error_unanchored(&format!(
            "erased_ast_id returned None for {:?} in collect_timeprecision_decl",
            node.kind()
        ));
        return;
    };
    let Some(decl) = TimeprecisionDecl::cast(node.clone()) else {
        ctx.emit_internal_error(
            "TimeprecisionDecl::cast failed on TimeprecisionDecl node",
            decl_site,
        );
        return;
    };
    let Some(prec_tok) = decl.precision_literal_token() else {
        ctx.emit_internal_error(
            "missing precision literal token in TimeprecisionDecl",
            decl_site,
        );
        return;
    };
    let entry = TimeUnitsDecl::Timeprecision {
        decl_site,
        precision: TimeLiteral {
            raw: SmolStr::new(prec_tok.text()),
        },
    };
    ctx.scope_time_units
        .entry(scope)
        .or_default()
        .decls
        .push(entry);
}

pub(crate) use lyra_ast::is_expression_kind;
