use lyra_ast::{
    AstNode, Declarator, ExportDecl, ExportItem, FunctionDecl, ImportItem, ModportDecl,
    ModportPortKind, ModuleInstantiation, TaskDecl, TfPortDecl, TypeSpec,
};
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;
use lyra_source::NameSpan;
use smol_str::SmolStr;

use crate::builder::DefContext;
use crate::builder_types::{collect_name_refs, collect_type_spec_refs, detect_aggregate_type};
use crate::def_index::{
    ExpectedNs, ExportDeclId, ExportKey, Import, ImportDeclId, ImportName, NamePath, UseSite,
};
use crate::instance_decl::{InstanceDecl, InstanceDeclIdx};
use crate::interface_id::InterfaceDefId;
use crate::modport_def::{ModportDef, ModportDefId, ModportEntry, ModportTarget, PortDirection};
use crate::record::SymbolOrigin;
use crate::scopes::{ScopeId, ScopeKind};
use crate::symbols::{Namespace, Symbol, SymbolKind};

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
        let type_name_range = name_tok.text_range();
        ctx.use_sites.push(UseSite {
            path: NamePath::Simple(SmolStr::new(name_tok.text())),
            expected_ns: ExpectedNs::Exact(Namespace::Definition),
            range: type_name_range,
            scope,
            name_ref_site: ast_id.erase(),
            order_key: 0,
        });
        // Register each instance name as an Instance symbol
        let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
            ctx.emit_internal_error(
                &format!(
                    "erased_ast_id returned None for {:?} in collect_module_instantiation",
                    node.kind()
                ),
                node.text_range(),
            );
            collect_name_refs(ctx, node, scope);
            return;
        };
        for hier_inst in inst.instances() {
            let Some(inst_name_tok) = hier_inst.name() else {
                continue;
            };
            let Some(inst_name_site) = ctx.ast_id_map.erased_ast_id(hier_inst.syntax()) else {
                ctx.emit_internal_error(
                    &format!(
                        "erased_ast_id returned None for {:?} in collect_module_instantiation hier_inst",
                        hier_inst.syntax().kind()
                    ),
                    hier_inst.syntax().text_range(),
                );
                continue;
            };
            let idx = InstanceDeclIdx(ctx.instance_decls.len() as u32);
            let sym_id = ctx.push_symbol(Symbol {
                name: SmolStr::new(inst_name_tok.text()),
                kind: SymbolKind::Instance,
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

pub(crate) fn collect_callable_decl(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    let is_function = node.kind() == SyntaxKind::FunctionDecl;
    let kind = if is_function {
        SymbolKind::Function
    } else {
        SymbolKind::Task
    };

    // Extract name: for functions, use FunctionDecl::name(); for tasks, use TaskDecl::name()
    let name_tok = if is_function {
        FunctionDecl::cast(node.clone()).and_then(|f| f.name())
    } else {
        TaskDecl::cast(node.clone()).and_then(|t| t.name())
    };

    let Some(name_tok) = name_tok else { return };
    let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error(
            &format!(
                "erased_ast_id returned None for {:?} in collect_callable_decl",
                node.kind()
            ),
            node.text_range(),
        );
        return;
    };
    let name = SmolStr::new(name_tok.text());
    let scope_kind = if is_function {
        ScopeKind::Function
    } else {
        ScopeKind::Task
    };
    let callable_scope = ctx.scopes.push(scope_kind, Some(scope));
    let callable_type_site = if is_function {
        FunctionDecl::cast(node.clone())
            .and_then(|f| f.type_spec())
            .and_then(|ts| ctx.ast_id_map.erased_ast_id(ts.syntax()))
    } else {
        None
    };
    let sym_id = ctx.push_symbol(Symbol {
        name: name.clone(),
        kind,
        decl_site,
        name_site: decl_site,
        type_site: callable_type_site,
        name_span: NameSpan::new(name_tok.text_range()),
        scope,
        origin: SymbolOrigin::TypeSpec,
    });

    ctx.register_binding(sym_id);

    // Collect return type spec refs (for typedef resolution in the enclosing scope)
    if is_function
        && let Some(func) = FunctionDecl::cast(node.clone())
        && let Some(ts) = func.type_spec()
    {
        collect_type_spec_refs(ctx, &ts, scope);
    }

    // Collect TF port declarations
    let tf_port_decls: Box<[TfPortDecl]> = if is_function {
        FunctionDecl::cast(node.clone())
            .map(|f| f.tf_port_decls().collect())
            .unwrap_or_default()
    } else {
        TaskDecl::cast(node.clone())
            .map(|t| t.tf_port_decls().collect())
            .unwrap_or_default()
    };
    collect_tf_ports(ctx, &tf_port_decls, callable_scope);
}

fn collect_tf_ports(ctx: &mut DefContext<'_>, port_decls: &[TfPortDecl], scope: ScopeId) {
    for port_decl in port_decls {
        // Collect type spec refs for resolution
        if let Some(ts) = port_decl.type_spec() {
            collect_type_spec_refs(ctx, &ts, scope);
        }
        let Some(port_decl_site) = ctx.ast_id_map.erased_ast_id(port_decl.syntax()) else {
            ctx.emit_internal_error(
                &format!(
                    "erased_ast_id returned None for {:?} in collect_tf_ports",
                    port_decl.syntax().kind()
                ),
                port_decl.syntax().text_range(),
            );
            continue;
        };
        let port_type_site = port_decl
            .type_spec()
            .and_then(|ts| ctx.ast_id_map.erased_ast_id(ts.syntax()));
        for decl in port_decl.declarators() {
            if let Some(name_tok) = decl.name() {
                let Some(decl_name_site) = ctx.ast_id_map.erased_ast_id(decl.syntax()) else {
                    ctx.emit_internal_error(
                        &format!(
                            "erased_ast_id returned None for {:?} in collect_tf_ports declarator",
                            decl.syntax().kind()
                        ),
                        decl.syntax().text_range(),
                    );
                    continue;
                };
                let port_sym = ctx.push_symbol(Symbol {
                    name: SmolStr::new(name_tok.text()),
                    kind: SymbolKind::PortTf,
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

        let entries = collect_modport_entries(ctx, &item);

        // Register symbol for navigation/diagnostics
        let Some(modport_decl_site) = ctx.ast_id_map.erased_ast_id(item.syntax()) else {
            ctx.emit_internal_error(
                &format!(
                    "erased_ast_id returned None for {:?} in collect_modport_decl",
                    item.syntax().kind()
                ),
                item.syntax().text_range(),
            );
            continue;
        };
        ctx.push_symbol(Symbol {
            name: name.clone(),
            kind: SymbolKind::Modport,
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
                entries: entries.into_boxed_slice(),
            },
        });
    }
}

fn collect_modport_entries(
    ctx: &DefContext<'_>,
    item: &lyra_ast::ModportItem,
) -> Vec<ModportEntry> {
    let mut entries = Vec::new();
    let mut current_dir: Option<PortDirection> = None;
    for port_kind in item.port_items() {
        match port_kind {
            ModportPortKind::Bare(port) => {
                if let Some(dir_tok) = port.direction() {
                    current_dir = parse_direction(dir_tok.kind(), current_dir);
                }
                if let Some(dir) = current_dir
                    && let Some(port_name_tok) = port.name()
                    && let Some(port_id) = ctx.ast_id_map.erased_ast_id(port.syntax())
                {
                    let name = SmolStr::new(port_name_tok.text());
                    entries.push(ModportEntry {
                        port_name: name.clone(),
                        direction: dir,
                        target: ModportTarget::ImplicitMember { member_name: name },
                        port_id,
                        name_span: NameSpan::new(port_name_tok.text_range()),
                    });
                }
            }
            ModportPortKind::Expr(port) => {
                if let Some(dir_tok) = port.direction() {
                    current_dir = parse_direction(dir_tok.kind(), current_dir);
                }
                if let Some(dir) = current_dir
                    && let Some(port_name_tok) = port.port_name()
                    && let Some(port_id) = ctx.ast_id_map.erased_ast_id(port.syntax())
                {
                    let target = if let Some(expr_node) = port.target_expr() {
                        match ctx.ast_id_map.erased_ast_id(&expr_node) {
                            Some(expr_id) => ModportTarget::Expr(expr_id),
                            None => ModportTarget::Empty,
                        }
                    } else {
                        ModportTarget::Empty
                    };
                    entries.push(ModportEntry {
                        port_name: SmolStr::new(port_name_tok.text()),
                        direction: dir,
                        target,
                        port_id,
                        name_span: NameSpan::new(port_name_tok.text_range()),
                    });
                }
            }
        }
    }
    entries
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
        ctx.emit_internal_error(
            &format!(
                "erased_ast_id returned None for {:?} in collect_import_item",
                node.kind()
            ),
            node.text_range(),
        );
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
        ctx.emit_internal_error(
            &format!(
                "erased_ast_id returned None for {:?} in collect_export_item",
                item.syntax().kind()
            ),
            item.syntax().text_range(),
        );
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
        ctx.emit_internal_error(
            &format!(
                "erased_ast_id returned None for {:?} in collect_param_decl",
                node.kind()
            ),
            node.text_range(),
        );
        return;
    };
    let param_type_site = lyra_ast::ParamDecl::cast(node.clone())
        .and_then(|pd| pd.type_spec())
        .and_then(|ts| ctx.ast_id_map.erased_ast_id(ts.syntax()));
    for child in node.children() {
        if let Some(ts) = TypeSpec::cast(child.clone()) {
            collect_type_spec_refs(ctx, &ts, scope);
        } else if child.kind() == SyntaxKind::Declarator {
            if let Some(name_tok) = Declarator::cast(child.clone()).and_then(|d| d.name()) {
                let Some(decl_name_site) = ctx.ast_id_map.erased_ast_id(&child) else {
                    ctx.emit_internal_error(
                        &format!(
                            "erased_ast_id returned None for {:?} in collect_param_decl declarator",
                            child.kind()
                        ),
                        child.text_range(),
                    );
                    continue;
                };
                let sym_id = ctx.push_symbol(Symbol {
                    name: SmolStr::new(name_tok.text()),
                    kind: SymbolKind::Parameter,
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

pub(crate) fn collect_declarators(
    ctx: &mut DefContext<'_>,
    node: &SyntaxNode,
    kind: SymbolKind,
    scope: ScopeId,
) {
    let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error(
            &format!(
                "erased_ast_id returned None for {:?} in collect_declarators",
                node.kind()
            ),
            node.text_range(),
        );
        return;
    };
    // Detect inline enum/struct in the TypeSpec child
    let origin = detect_aggregate_type(ctx, node, scope);
    let decl_type_site = match node.kind() {
        SyntaxKind::VarDecl => lyra_ast::VarDecl::cast(node.clone())
            .and_then(|vd| vd.type_spec())
            .and_then(|ts| ctx.ast_id_map.erased_ast_id(ts.syntax())),
        SyntaxKind::NetDecl => lyra_ast::NetDecl::cast(node.clone())
            .and_then(|nd| nd.type_spec())
            .and_then(|ts| ctx.ast_id_map.erased_ast_id(ts.syntax())),
        _ => None,
    };
    for child in node.children() {
        if let Some(ts) = TypeSpec::cast(child.clone()) {
            collect_type_spec_refs(ctx, &ts, scope);
        } else if child.kind() == SyntaxKind::Declarator {
            if let Some(name_tok) = Declarator::cast(child.clone()).and_then(|d| d.name()) {
                let Some(decl_name_site) = ctx.ast_id_map.erased_ast_id(&child) else {
                    ctx.emit_internal_error(
                        &format!(
                            "erased_ast_id returned None for {:?} in collect_declarators declarator",
                            child.kind()
                        ),
                        child.text_range(),
                    );
                    continue;
                };
                let sym_id = ctx.push_symbol(Symbol {
                    name: SmolStr::new(name_tok.text()),
                    kind,
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

pub(crate) use crate::expr_helpers::is_expression_kind;
