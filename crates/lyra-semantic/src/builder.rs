use std::collections::HashMap;

use lyra_ast::{AstIdMap, AstNode, ExportDecl, Port, TypeSpec};
use lyra_lexer::SyntaxKind;
use lyra_parser::{Parse, SyntaxNode};
use lyra_source::{FileId, TextRange};
use smol_str::SmolStr;

use crate::builder_order::{assign_order_keys, detect_duplicates};
use crate::builder_types::{collect_name_refs, collect_type_spec_refs, collect_typedef};

use crate::def_index::{DefIndex, Exports, Import, LocalDecl, LocalDeclId, UseSite};
use crate::diagnostic::SemanticDiag;
use crate::enum_def::EnumDef;
use crate::instance_decl::InstanceDecl;
use crate::interface_id::InterfaceDefId;
use crate::modport_def::{ModportDef, ModportDefId};
use crate::record::{RecordDef, SymbolOrigin};
use crate::scopes::{ScopeId, ScopeKind, ScopeTreeBuilder};
use crate::symbols::{Namespace, Symbol, SymbolId, SymbolKind, SymbolTableBuilder};

pub fn build_def_index(file: FileId, parse: &Parse, ast_id_map: &AstIdMap) -> DefIndex {
    let mut ctx = DefContext::new(file, ast_id_map);
    let root = parse.syntax();

    let file_scope = ctx.scopes.push(ScopeKind::File, None);
    collect_source_file(&mut ctx, &root, file_scope);
    assign_order_keys(&mut ctx, &root);

    let symbols = ctx.symbols.freeze();
    let scopes = ctx.scopes.freeze(&symbols);

    ctx.export_definitions
        .sort_by(|a, b| symbols.get(*a).name.cmp(&symbols.get(*b).name));
    ctx.export_packages
        .sort_by(|a, b| symbols.get(*a).name.cmp(&symbols.get(*b).name));

    let mut diagnostics = ctx.diagnostics;
    for scope_idx in 0..scopes.len() {
        let s = scopes.get(ScopeId(scope_idx as u32));
        detect_duplicates(&symbols, &s.value_ns, &mut diagnostics);
        detect_duplicates(&symbols, &s.type_ns, &mut diagnostics);
    }

    ctx.export_decls
        .sort_by(|a, b| (a.id.scope, a.id.ordinal).cmp(&(b.id.scope, b.id.ordinal)));
    debug_assert!(
        ctx.export_decls
            .windows(2)
            .all(|w| (w[0].id.scope, w[0].id.ordinal) < (w[1].id.scope, w[1].id.ordinal))
    );

    ctx.imports
        .sort_by(|a, b| (a.id.scope, a.id.ordinal).cmp(&(b.id.scope, b.id.ordinal)));
    ctx.local_decls
        .sort_by(|a, b| (a.id.scope, a.id.ordinal).cmp(&(b.id.scope, b.id.ordinal)));

    let mut enum_by_ast = HashMap::with_capacity(ctx.enum_defs.len());
    for (i, def) in ctx.enum_defs.iter().enumerate() {
        enum_by_ast.insert(def.ast_id, crate::enum_def::EnumDefIdx(i as u32));
    }
    let mut record_by_ast = HashMap::with_capacity(ctx.record_defs.len());
    for (i, def) in ctx.record_defs.iter().enumerate() {
        record_by_ast.insert(def.ast_id, crate::record::RecordDefIdx(i as u32));
    }

    // Finalize modport defs: convert raw GlobalDefId owners to InterfaceDefId
    let mut modport_defs = HashMap::new();
    let mut modport_name_map = HashMap::new();
    let def_index_partial = DefIndex {
        file,
        symbols,
        scopes,
        exports: Exports {
            definitions: ctx.export_definitions.into_boxed_slice(),
            packages: ctx.export_packages.into_boxed_slice(),
        },
        use_sites: ctx.use_sites.into_boxed_slice(),
        imports: ctx.imports.into_boxed_slice(),
        local_decls: ctx.local_decls.into_boxed_slice(),
        name_ast_to_symbol: ctx.name_ast_to_symbol,
        name_ast_to_init_expr: ctx.name_ast_to_init_expr,
        enum_defs: ctx.enum_defs.into_boxed_slice(),
        enum_by_ast,
        record_defs: ctx.record_defs.into_boxed_slice(),
        record_by_ast,
        instance_decls: ctx.instance_decls.into_boxed_slice(),
        modport_defs: HashMap::new(),
        modport_name_map: HashMap::new(),
        export_decls: ctx.export_decls.into_boxed_slice(),
        diagnostics: diagnostics.into_boxed_slice(),
        internal_errors: ctx.internal_errors.into_boxed_slice(),
    };
    for raw in ctx.raw_modport_defs {
        if let Some(iface_id) = InterfaceDefId::try_from_def_index(&def_index_partial, raw.owner) {
            let typed_id = ModportDefId {
                owner: iface_id,
                ordinal: raw.ordinal,
            };
            let mut def = raw.def;
            def.id = typed_id;
            modport_name_map
                .entry((iface_id, raw.name))
                .or_insert(typed_id);
            modport_defs.insert(typed_id, def);
        }
    }
    DefIndex {
        modport_defs,
        modport_name_map,
        ..def_index_partial
    }
}

/// Mutable context accumulated during a single `build_def_index` pass.
///
/// Fields are `pub(crate)` because `builder_stmt` needs direct access for
/// statement/expression collection. Both modules are internal to
/// `lyra-semantic` and maintain builder invariants together.
/// Intermediate modport data collected during building, before symbol table freeze.
/// Uses raw `GlobalDefId` for the owner; converted to `InterfaceDefId` during finalization.
pub(crate) struct RawModportEntry {
    pub(crate) owner: crate::symbols::GlobalDefId,
    pub(crate) ordinal: u32,
    pub(crate) name: SmolStr,
    pub(crate) def: ModportDef,
}

pub(crate) struct DefContext<'a> {
    pub(crate) file: FileId,
    pub(crate) ast_id_map: &'a AstIdMap,
    pub(crate) symbols: SymbolTableBuilder,
    pub(crate) scopes: ScopeTreeBuilder,
    pub(crate) export_definitions: Vec<SymbolId>,
    pub(crate) export_packages: Vec<SymbolId>,
    pub(crate) name_ast_to_symbol: HashMap<lyra_ast::ErasedAstId, SymbolId>,
    pub(crate) name_ast_to_init_expr: HashMap<lyra_ast::ErasedAstId, Option<lyra_ast::ErasedAstId>>,
    pub(crate) use_sites: Vec<UseSite>,
    pub(crate) imports: Vec<Import>,
    pub(crate) enum_defs: Vec<EnumDef>,
    pub(crate) record_defs: Vec<RecordDef>,
    pub(crate) instance_decls: Vec<InstanceDecl>,
    pub(crate) raw_modport_defs: Vec<RawModportEntry>,
    pub(crate) export_decls: Vec<crate::def_index::ExportDecl>,
    pub(crate) local_decls: Vec<LocalDecl>,
    pub(crate) local_decl_ordinals: HashMap<ScopeId, u32>,
    pub(crate) import_ordinals: HashMap<ScopeId, u32>,
    pub(crate) export_ordinals: HashMap<ScopeId, u32>,
    pub(crate) current_iface_def_id: Option<crate::symbols::GlobalDefId>,
    pub(crate) modport_ordinal: u32,
    pub(crate) diagnostics: Vec<SemanticDiag>,
    pub(crate) internal_errors: Vec<(TextRange, SmolStr)>,
}

impl<'a> DefContext<'a> {
    pub(crate) fn new(file: FileId, ast_id_map: &'a AstIdMap) -> Self {
        Self {
            file,
            ast_id_map,
            symbols: SymbolTableBuilder::new(),
            scopes: ScopeTreeBuilder::new(),
            export_definitions: Vec::new(),
            export_packages: Vec::new(),
            name_ast_to_symbol: HashMap::new(),
            name_ast_to_init_expr: HashMap::new(),
            use_sites: Vec::new(),
            imports: Vec::new(),
            enum_defs: Vec::new(),
            record_defs: Vec::new(),
            instance_decls: Vec::new(),
            raw_modport_defs: Vec::new(),
            export_decls: Vec::new(),
            local_decls: Vec::new(),
            local_decl_ordinals: HashMap::new(),
            import_ordinals: HashMap::new(),
            export_ordinals: HashMap::new(),
            current_iface_def_id: None,
            modport_ordinal: 0,
            diagnostics: Vec::new(),
            internal_errors: Vec::new(),
        }
    }

    pub(crate) fn emit_internal_error(&mut self, detail: &str, range: TextRange) {
        self.internal_errors.push((range, SmolStr::new(detail)));
    }

    pub(crate) fn register_binding(&mut self, sym_id: SymbolId) {
        let sym = self.symbols.get(sym_id);
        self.name_ast_to_symbol.insert(sym.name_ast, sym_id);
        let ns = sym.kind.namespace();
        if ns == Namespace::Definition {
            return;
        }
        let name = sym.name.clone();
        let scope = sym.scope;
        let ordinal = self.local_decl_ordinals.entry(scope).or_insert(0);
        let ord = *ordinal;
        *ordinal += 1;
        self.local_decls.push(LocalDecl {
            id: LocalDeclId {
                scope,
                ordinal: ord,
            },
            symbol_id: sym_id,
            name,
            namespace: ns,
            ast_id: sym.name_ast,
            order_key: 0,
        });
    }

    pub(crate) fn push_symbol(&mut self, sym: Symbol) -> SymbolId {
        debug_assert!(
            match sym.kind {
                SymbolKind::Module => sym.def_ast.kind() == SyntaxKind::ModuleDecl,
                SymbolKind::Package => sym.def_ast.kind() == SyntaxKind::PackageDecl,
                SymbolKind::Interface => sym.def_ast.kind() == SyntaxKind::InterfaceDecl,
                SymbolKind::Program => sym.def_ast.kind() == SyntaxKind::ProgramDecl,
                SymbolKind::Primitive => sym.def_ast.kind() == SyntaxKind::PrimitiveDecl,
                SymbolKind::Config => sym.def_ast.kind() == SyntaxKind::ConfigDecl,
                SymbolKind::Function => sym.def_ast.kind() == SyntaxKind::FunctionDecl,
                SymbolKind::Task => sym.def_ast.kind() == SyntaxKind::TaskDecl,
                SymbolKind::Variable => sym.def_ast.kind() == SyntaxKind::VarDecl,
                SymbolKind::Net => sym.def_ast.kind() == SyntaxKind::NetDecl,
                SymbolKind::Parameter => sym.def_ast.kind() == SyntaxKind::ParamDecl,
                SymbolKind::PortAnsi => sym.def_ast.kind() == SyntaxKind::Port,
                SymbolKind::PortTf => sym.def_ast.kind() == SyntaxKind::TfPortDecl,
                SymbolKind::Typedef => sym.def_ast.kind() == SyntaxKind::TypedefDecl,
                SymbolKind::EnumMember => sym.def_ast.kind() == SyntaxKind::EnumMember,
                SymbolKind::Modport => sym.def_ast.kind() == SyntaxKind::ModportItem,
                SymbolKind::Instance => sym.def_ast.kind() == SyntaxKind::ModuleInstantiation,
            },
            "def_ast kind {:?} does not match SymbolKind::{:?}",
            sym.def_ast.kind(),
            sym.kind,
        );
        self.check_name_ast_invariants(
            sym.kind,
            sym.name_ast,
            sym.def_ast,
            sym.type_ast,
            &sym.name,
            sym.def_range,
        );
        let scope = sym.scope;
        let kind = sym.kind;
        let id = self.symbols.push(sym);
        self.scopes.add_binding(scope, id, kind);
        id
    }

    fn check_name_ast_invariants(
        &mut self,
        kind: SymbolKind,
        name_ast: lyra_ast::ErasedAstId,
        def_ast: lyra_ast::ErasedAstId,
        type_ast: Option<lyra_ast::ErasedAstId>,
        name: &str,
        def_range: TextRange,
    ) {
        let expected_name_kind = match kind {
            SymbolKind::Variable | SymbolKind::Net | SymbolKind::Parameter | SymbolKind::PortTf => {
                SyntaxKind::Declarator
            }
            SymbolKind::PortAnsi => SyntaxKind::Port,
            SymbolKind::Instance => SyntaxKind::HierarchicalInstance,
            SymbolKind::Module => SyntaxKind::ModuleDecl,
            SymbolKind::Package => SyntaxKind::PackageDecl,
            SymbolKind::Interface => SyntaxKind::InterfaceDecl,
            SymbolKind::Program => SyntaxKind::ProgramDecl,
            SymbolKind::Primitive => SyntaxKind::PrimitiveDecl,
            SymbolKind::Config => SyntaxKind::ConfigDecl,
            SymbolKind::Function => SyntaxKind::FunctionDecl,
            SymbolKind::Task => SyntaxKind::TaskDecl,
            SymbolKind::Typedef => SyntaxKind::TypedefDecl,
            SymbolKind::EnumMember => SyntaxKind::EnumMember,
            SymbolKind::Modport => SyntaxKind::ModportItem,
        };
        if name_ast.kind() != expected_name_kind {
            self.emit_internal_error(
                &format!(
                    "name_ast kind {:?} (expected {:?}) for {:?} '{}'",
                    name_ast.kind(),
                    expected_name_kind,
                    kind,
                    name,
                ),
                def_range,
            );
        }
        if kind.namespace() == Namespace::Definition && name_ast != def_ast {
            self.emit_internal_error(
                &format!("name_ast != def_ast for definition-namespace {kind:?} '{name}'",),
                def_range,
            );
        }
        if let Some(ta) = type_ast
            && ta.kind() != SyntaxKind::TypeSpec
        {
            self.emit_internal_error(
                &format!(
                    "type_ast kind {:?} (expected TypeSpec) for {kind:?} '{name}'",
                    ta.kind(),
                ),
                def_range,
            );
        }
    }
}

fn collect_source_file(ctx: &mut DefContext<'_>, root: &SyntaxNode, file_scope: ScopeId) {
    for child in root.children() {
        match child.kind() {
            SyntaxKind::ModuleDecl => {
                collect_module(ctx, &child, file_scope);
            }
            SyntaxKind::PackageDecl => {
                collect_package(ctx, &child, file_scope);
            }
            SyntaxKind::InterfaceDecl => {
                collect_interface(ctx, &child);
            }
            SyntaxKind::ProgramDecl => {
                collect_program(ctx, &child);
            }
            SyntaxKind::PrimitiveDecl => {
                collect_primitive(ctx, &child);
            }
            SyntaxKind::ConfigDecl => {
                collect_config(ctx, &child);
            }
            _ => {}
        }
    }
}

fn collect_module(ctx: &mut DefContext<'_>, node: &SyntaxNode, _file_scope: ScopeId) {
    let module_name = first_ident_token(node);
    if let Some(name_tok) = &module_name {
        let Some(def_ast) = ctx.ast_id_map.erased_ast_id(node) else {
            ctx.emit_internal_error(
                &format!(
                    "erased_ast_id returned None for {:?} in collect_module",
                    node.kind()
                ),
                node.text_range(),
            );
            return;
        };
        let name = SmolStr::new(name_tok.text());
        let range = name_tok.text_range();
        let module_scope = ctx.scopes.push(ScopeKind::Module, None);
        let sym_id = ctx.push_symbol(Symbol {
            name: name.clone(),
            kind: SymbolKind::Module,
            def_ast,
            name_ast: def_ast,
            type_ast: None,
            def_range: range,
            scope: module_scope,
            origin: SymbolOrigin::TypeSpec,
        });
        ctx.export_definitions.push(sym_id);

        ctx.register_binding(sym_id);

        // Pass 1: header imports (scope facts collected first so ports see them)
        for child in node.children() {
            if child.kind() == SyntaxKind::ImportDecl {
                collect_import_decl(ctx, &child, module_scope);
            }
        }
        // Pass 2: ports and body
        for child in node.children() {
            match child.kind() {
                SyntaxKind::ParamPortList => {
                    collect_param_port_list(ctx, &child, module_scope);
                }
                SyntaxKind::PortList => {
                    collect_port_list(ctx, &child, module_scope);
                }
                SyntaxKind::ModuleBody => {
                    collect_module_body(ctx, &child, module_scope);
                }
                _ => {}
            }
        }
    }
}

fn collect_package(ctx: &mut DefContext<'_>, node: &SyntaxNode, _file_scope: ScopeId) {
    let package_name = first_ident_token(node);
    if let Some(name_tok) = &package_name {
        let Some(def_ast) = ctx.ast_id_map.erased_ast_id(node) else {
            ctx.emit_internal_error(
                &format!(
                    "erased_ast_id returned None for {:?} in collect_package",
                    node.kind()
                ),
                node.text_range(),
            );
            return;
        };
        let name = SmolStr::new(name_tok.text());
        let range = name_tok.text_range();
        let package_scope = ctx.scopes.push(ScopeKind::Package, None);
        let sym_id = ctx.push_symbol(Symbol {
            name: name.clone(),
            kind: SymbolKind::Package,
            def_ast,
            name_ast: def_ast,
            type_ast: None,
            def_range: range,
            scope: package_scope,
            origin: SymbolOrigin::TypeSpec,
        });
        ctx.export_packages.push(sym_id);

        ctx.register_binding(sym_id);

        for child in node.children() {
            if child.kind() == SyntaxKind::PackageBody {
                collect_package_body(ctx, &child, package_scope);
            }
        }
    }
}

fn collect_interface(ctx: &mut DefContext<'_>, node: &SyntaxNode) {
    let name_tok = first_ident_token(node);
    if let Some(name_tok) = &name_tok {
        let Some(def_ast) = ctx.ast_id_map.erased_ast_id(node) else {
            ctx.emit_internal_error(
                &format!(
                    "erased_ast_id returned None for {:?} in collect_interface",
                    node.kind()
                ),
                node.text_range(),
            );
            return;
        };
        let name = SmolStr::new(name_tok.text());
        let range = name_tok.text_range();
        let iface_scope = ctx.scopes.push(ScopeKind::Interface, None);
        let sym_id = ctx.push_symbol(Symbol {
            name: name.clone(),
            kind: SymbolKind::Interface,
            def_ast,
            name_ast: def_ast,
            type_ast: None,
            def_range: range,
            scope: iface_scope,
            origin: SymbolOrigin::TypeSpec,
        });
        ctx.export_definitions.push(sym_id);

        ctx.register_binding(sym_id);
        let iface_def_id = Some(crate::symbols::GlobalDefId::new(def_ast));

        let prev_iface = ctx.current_iface_def_id.take();
        ctx.current_iface_def_id = iface_def_id;
        ctx.modport_ordinal = 0;
        // Pass 1: header imports
        for child in node.children() {
            if child.kind() == SyntaxKind::ImportDecl {
                collect_import_decl(ctx, &child, iface_scope);
            }
        }
        // Pass 2: ports and body
        for child in node.children() {
            match child.kind() {
                SyntaxKind::ParamPortList => {
                    collect_param_port_list(ctx, &child, iface_scope);
                }
                SyntaxKind::PortList => {
                    collect_port_list(ctx, &child, iface_scope);
                }
                SyntaxKind::InterfaceBody => {
                    collect_module_body(ctx, &child, iface_scope);
                }
                _ => {}
            }
        }
        ctx.current_iface_def_id = prev_iface;
    }
}

fn collect_program(ctx: &mut DefContext<'_>, node: &SyntaxNode) {
    let name_tok = first_ident_token(node);
    if let Some(name_tok) = &name_tok {
        let Some(def_ast) = ctx.ast_id_map.erased_ast_id(node) else {
            ctx.emit_internal_error(
                &format!(
                    "erased_ast_id returned None for {:?} in collect_program",
                    node.kind()
                ),
                node.text_range(),
            );
            return;
        };
        let name = SmolStr::new(name_tok.text());
        let range = name_tok.text_range();
        let prog_scope = ctx.scopes.push(ScopeKind::Program, None);
        let sym_id = ctx.push_symbol(Symbol {
            name: name.clone(),
            kind: SymbolKind::Program,
            def_ast,
            name_ast: def_ast,
            type_ast: None,
            def_range: range,
            scope: prog_scope,
            origin: SymbolOrigin::TypeSpec,
        });
        ctx.export_definitions.push(sym_id);

        ctx.register_binding(sym_id);

        // Pass 1: header imports
        for child in node.children() {
            if child.kind() == SyntaxKind::ImportDecl {
                collect_import_decl(ctx, &child, prog_scope);
            }
        }
        // Pass 2: ports and body
        for child in node.children() {
            match child.kind() {
                SyntaxKind::ParamPortList => {
                    collect_param_port_list(ctx, &child, prog_scope);
                }
                SyntaxKind::PortList => {
                    collect_port_list(ctx, &child, prog_scope);
                }
                SyntaxKind::ProgramBody => {
                    collect_module_body(ctx, &child, prog_scope);
                }
                _ => {}
            }
        }
    }
}

fn collect_primitive(ctx: &mut DefContext<'_>, node: &SyntaxNode) {
    let name_tok = first_ident_token(node);
    if let Some(name_tok) = &name_tok {
        let Some(def_ast) = ctx.ast_id_map.erased_ast_id(node) else {
            ctx.emit_internal_error(
                &format!(
                    "erased_ast_id returned None for {:?} in collect_primitive",
                    node.kind()
                ),
                node.text_range(),
            );
            return;
        };
        let name = SmolStr::new(name_tok.text());
        let range = name_tok.text_range();
        let prim_scope = ctx.scopes.push(ScopeKind::Module, None);
        let sym_id = ctx.push_symbol(Symbol {
            name,
            kind: SymbolKind::Primitive,
            def_ast,
            name_ast: def_ast,
            type_ast: None,
            def_range: range,
            scope: prim_scope,
            origin: SymbolOrigin::TypeSpec,
        });
        ctx.export_definitions.push(sym_id);

        ctx.register_binding(sym_id);
    }
}

fn collect_config(ctx: &mut DefContext<'_>, node: &SyntaxNode) {
    let name_tok = first_ident_token(node);
    if let Some(name_tok) = &name_tok {
        let Some(def_ast) = ctx.ast_id_map.erased_ast_id(node) else {
            ctx.emit_internal_error(
                &format!(
                    "erased_ast_id returned None for {:?} in collect_config",
                    node.kind()
                ),
                node.text_range(),
            );
            return;
        };
        let name = SmolStr::new(name_tok.text());
        let range = name_tok.text_range();
        let cfg_scope = ctx.scopes.push(ScopeKind::Module, None);
        let sym_id = ctx.push_symbol(Symbol {
            name,
            kind: SymbolKind::Config,
            def_ast,
            name_ast: def_ast,
            type_ast: None,
            def_range: range,
            scope: cfg_scope,
            origin: SymbolOrigin::TypeSpec,
        });
        ctx.export_definitions.push(sym_id);

        ctx.register_binding(sym_id);
    }
}

fn collect_package_body(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        if ExportDecl::cast(child.clone()).is_some() {
            collect_export_decl(ctx, &child, scope);
        } else {
            collect_module_item(ctx, &child, scope);
        }
    }
}

fn collect_param_port_list(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        if child.kind() == SyntaxKind::ParamDecl {
            collect_param_decl(ctx, &child, scope);
        }
    }
}

fn collect_port_list(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        if child.kind() == SyntaxKind::Port
            && let Some(fallback_tok) = first_ident_token(&child)
        {
            let Some(def_ast) = ctx.ast_id_map.erased_ast_id(&child) else {
                ctx.emit_internal_error(
                    &format!(
                        "erased_ast_id returned None for {:?} in collect_port_list",
                        child.kind()
                    ),
                    child.text_range(),
                );
                continue;
            };
            // Prefer the port-name ident (last direct Ident child, after
            // any TypeSpec type name). Fall back to the first Ident.
            let name_tok = port_name_ident(&child).unwrap_or(fallback_tok);
            let port_type_ast = Port::cast(child.clone())
                .and_then(|p| p.type_spec())
                .and_then(|ts| ctx.ast_id_map.erased_ast_id(ts.syntax()));
            let sym_id = ctx.push_symbol(Symbol {
                name: SmolStr::new(name_tok.text()),
                kind: SymbolKind::PortAnsi,
                def_ast,
                name_ast: def_ast,
                type_ast: port_type_ast,
                def_range: name_tok.text_range(),
                scope,
                origin: SymbolOrigin::TypeSpec,
            });
            ctx.register_binding(sym_id);
            // Collect type-spec name refs for port typedef resolution
            for port_child in child.children() {
                if let Some(ts) = TypeSpec::cast(port_child) {
                    collect_type_spec_refs(ctx, &ts, scope);
                }
            }
        }
    }
}

fn collect_module_body(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        collect_module_item(ctx, &child, scope);
    }
}

fn collect_module_item(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    match node.kind() {
        SyntaxKind::NetDecl => {
            collect_declarators(ctx, node, SymbolKind::Net, scope);
        }
        SyntaxKind::VarDecl => {
            collect_declarators(ctx, node, SymbolKind::Variable, scope);
        }
        SyntaxKind::ParamDecl => {
            collect_param_decl(ctx, node, scope);
        }
        SyntaxKind::ContinuousAssign => {
            collect_name_refs(ctx, node, scope);
        }
        SyntaxKind::ImportDecl => {
            collect_import_decl(ctx, node, scope);
        }
        SyntaxKind::ModuleInstantiation => {
            collect_module_instantiation(ctx, node, scope);
        }
        SyntaxKind::TypedefDecl => {
            collect_typedef(ctx, node, scope);
        }
        SyntaxKind::AlwaysBlock | SyntaxKind::InitialBlock => {
            crate::builder_stmt::collect_procedural_block(ctx, node, scope);
        }
        SyntaxKind::FunctionDecl | SyntaxKind::TaskDecl => {
            collect_callable_decl(ctx, node, scope);
        }
        SyntaxKind::ModportDecl => {
            collect_modport_decl(ctx, node, scope);
        }
        SyntaxKind::IfStmt => {
            // Conditional generate: recurse into children (BlockStmt, etc.)
            for child in node.children() {
                collect_module_item(ctx, &child, scope);
            }
        }
        SyntaxKind::BlockStmt => {
            // Generate block: create a new scope and collect items inside
            let gen_scope = ctx.scopes.push(ScopeKind::Generate, Some(scope));
            for child in node.children() {
                collect_module_item(ctx, &child, gen_scope);
            }
        }
        _ => {}
    }
}

use crate::syntax_helpers::{first_ident_token, port_name_ident};

use crate::builder_items::{
    collect_callable_decl, collect_export_decl, collect_import_decl, collect_modport_decl,
    collect_module_instantiation, collect_param_decl,
};
pub(crate) use crate::builder_items::{collect_declarators, is_expression_kind};

#[cfg(test)]
#[path = "builder_tests.rs"]
mod tests;
