use std::collections::HashMap;

use lyra_ast::{
    AstIdMap, AstNode, ConfigDecl, ExportDecl, ExportItem, FunctionDecl, InterfaceDecl,
    ModportDecl, ModportPortKind, ModuleInstantiation, Port, PrimitiveDecl, ProgramDecl, TaskDecl,
    TfPortDecl, TypeSpec,
};
use lyra_lexer::SyntaxKind;
use lyra_parser::{Parse, SyntaxNode};
use lyra_source::{FileId, TextRange};
use smol_str::SmolStr;

use crate::builder_order::{assign_order_keys, detect_duplicates};
use crate::builder_types::{
    collect_name_refs, collect_type_spec_refs, collect_typedef, detect_aggregate_type,
};

use crate::def_index::{
    DefIndex, ExpectedNs, ExportDeclId, ExportKey, Exports, Import, ImportDeclId, ImportName,
    LocalDecl, LocalDeclId, NamePath, UseSite,
};
use crate::diagnostic::SemanticDiag;
use crate::enum_def::EnumDef;
use crate::instance_decl::{InstanceDecl, InstanceDeclIdx};
use crate::interface_id::InterfaceDefId;
use crate::modport_def::{ModportDef, ModportDefId, ModportEntry, ModportTarget, PortDirection};
use crate::record::{RecordDef, SymbolOrigin};
use crate::scopes::{ScopeId, ScopeKind, ScopeTreeBuilder};
use crate::symbols::{Namespace, Symbol, SymbolId, SymbolKind, SymbolTableBuilder};

pub fn build_def_index(file: FileId, parse: &Parse, ast_id_map: &AstIdMap) -> DefIndex {
    let mut ctx = DefContext::new(file, ast_id_map);
    let root = parse.syntax();

    // Root file scope
    let file_scope = ctx.scopes.push(ScopeKind::File, None);
    collect_source_file(&mut ctx, &root, file_scope);

    // Assign order keys via preorder walk matching by ErasedAstId.
    // IMPORTANT: root must be from the same parse result that produced ast_id_map.
    assign_order_keys(&mut ctx, &root);

    // Freeze symbols first, then scopes (which need symbol names for sorting)
    let symbols = ctx.symbols.freeze();
    let scopes = ctx.scopes.freeze(&symbols);

    // Sort export definition/package ids by symbol name
    ctx.export_definitions
        .sort_by(|a, b| symbols.get(*a).name.cmp(&symbols.get(*b).name));
    ctx.export_packages
        .sort_by(|a, b| symbols.get(*a).name.cmp(&symbols.get(*b).name));

    // Collect diagnostics from builder + detect duplicates
    let mut diagnostics = ctx.diagnostics;
    for scope_idx in 0..scopes.len() {
        let s = scopes.get(ScopeId(scope_idx as u32));
        detect_duplicates(&symbols, &s.value_ns, &mut diagnostics);
        detect_duplicates(&symbols, &s.type_ns, &mut diagnostics);
    }

    // Sort export_decls by (scope, ordinal) for binary-search range lookup
    ctx.export_decls
        .sort_by(|a, b| (a.id.scope, a.id.ordinal).cmp(&(b.id.scope, b.id.ordinal)));
    debug_assert!(
        ctx.export_decls
            .windows(2)
            .all(|w| (w[0].id.scope, w[0].id.ordinal) < (w[1].id.scope, w[1].id.ordinal))
    );

    // Sort imports by (id.scope, id.ordinal) for binary-search lookup
    ctx.imports
        .sort_by(|a, b| (a.id.scope, a.id.ordinal).cmp(&(b.id.scope, b.id.ordinal)));

    // Sort local_decls by (id.scope, id.ordinal) for binary-search lookup
    ctx.local_decls
        .sort_by(|a, b| (a.id.scope, a.id.ordinal).cmp(&(b.id.scope, b.id.ordinal)));

    // Build ast_id -> idx lookup maps for enum and record defs
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
        decl_to_symbol: ctx.decl_to_symbol,
        symbol_to_decl: ctx.symbol_to_decl.into_boxed_slice(),
        decl_to_init_expr: ctx.decl_to_init_expr,
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
    pub(crate) decl_to_symbol: HashMap<lyra_ast::ErasedAstId, SymbolId>,
    pub(crate) symbol_to_decl: Vec<Option<lyra_ast::ErasedAstId>>,
    pub(crate) decl_to_init_expr: HashMap<lyra_ast::ErasedAstId, Option<lyra_ast::ErasedAstId>>,
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
            decl_to_symbol: HashMap::new(),
            symbol_to_decl: Vec::new(),
            decl_to_init_expr: HashMap::new(),
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

    pub(crate) fn add_symbol(
        &mut self,
        def_ast: lyra_ast::ErasedAstId,
        name: SmolStr,
        kind: SymbolKind,
        def_range: TextRange,
        scope: ScopeId,
    ) -> SymbolId {
        self.add_symbol_with_origin(
            def_ast,
            name,
            kind,
            def_range,
            scope,
            SymbolOrigin::TypeSpec,
        )
    }

    pub(crate) fn register_binding(
        &mut self,
        sym_id: SymbolId,
        scope: ScopeId,
        ast_id: lyra_ast::ErasedAstId,
        range: TextRange,
    ) {
        self.decl_to_symbol.insert(ast_id, sym_id);
        self.symbol_to_decl[sym_id.index()] = Some(ast_id);

        let sym = self.symbols.get(sym_id);
        debug_assert_eq!(sym.scope, scope);
        let ns = sym.kind.namespace();
        if ns == Namespace::Definition {
            return;
        }
        let name = sym.name.clone();
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
            ast_id,
            order_key: 0,
            range,
        });
    }

    pub(crate) fn add_symbol_with_origin(
        &mut self,
        def_ast: lyra_ast::ErasedAstId,
        name: SmolStr,
        kind: SymbolKind,
        def_range: TextRange,
        scope: ScopeId,
        origin: SymbolOrigin,
    ) -> SymbolId {
        debug_assert!(
            match kind {
                SymbolKind::Module => def_ast.kind() == SyntaxKind::ModuleDecl,
                SymbolKind::Package => def_ast.kind() == SyntaxKind::PackageDecl,
                SymbolKind::Interface => def_ast.kind() == SyntaxKind::InterfaceDecl,
                SymbolKind::Program => def_ast.kind() == SyntaxKind::ProgramDecl,
                SymbolKind::Primitive => def_ast.kind() == SyntaxKind::PrimitiveDecl,
                SymbolKind::Config => def_ast.kind() == SyntaxKind::ConfigDecl,
                SymbolKind::Function => def_ast.kind() == SyntaxKind::FunctionDecl,
                SymbolKind::Task => def_ast.kind() == SyntaxKind::TaskDecl,
                SymbolKind::Variable => def_ast.kind() == SyntaxKind::VarDecl,
                SymbolKind::Net => def_ast.kind() == SyntaxKind::NetDecl,
                SymbolKind::Parameter => def_ast.kind() == SyntaxKind::ParamDecl,
                SymbolKind::Port =>
                    matches!(def_ast.kind(), SyntaxKind::Port | SyntaxKind::TfPortDecl),
                SymbolKind::Typedef => def_ast.kind() == SyntaxKind::TypedefDecl,
                SymbolKind::EnumMember => def_ast.kind() == SyntaxKind::EnumMember,
                SymbolKind::Modport => def_ast.kind() == SyntaxKind::ModportItem,
                SymbolKind::Instance => def_ast.kind() == SyntaxKind::ModuleInstantiation,
            },
            "def_ast kind {:?} does not match SymbolKind::{:?}",
            def_ast.kind(),
            kind,
        );
        let id = self.symbols.push(Symbol {
            name,
            kind,
            def_ast,
            def_range,
            scope,
            origin,
        });
        self.symbol_to_decl.push(None);
        self.scopes.add_binding(scope, id, kind);
        id
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
        let sym_id = ctx.symbols.push(Symbol {
            name: name.clone(),
            kind: SymbolKind::Module,
            def_ast,
            def_range: range,
            scope: module_scope,
            origin: SymbolOrigin::TypeSpec,
        });
        ctx.symbol_to_decl.push(None);
        ctx.export_definitions.push(sym_id);

        if let Some(module_decl) = lyra_ast::ModuleDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&module_decl)
        {
            ctx.register_binding(sym_id, module_scope, ast_id.erase(), range);
        }

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
        let sym_id = ctx.symbols.push(Symbol {
            name: name.clone(),
            kind: SymbolKind::Package,
            def_ast,
            def_range: range,
            scope: package_scope,
            origin: SymbolOrigin::TypeSpec,
        });
        ctx.symbol_to_decl.push(None);
        ctx.export_packages.push(sym_id);

        if let Some(pkg_decl) = lyra_ast::PackageDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&pkg_decl)
        {
            ctx.register_binding(sym_id, package_scope, ast_id.erase(), range);
        }

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
        let sym_id = ctx.symbols.push(Symbol {
            name: name.clone(),
            kind: SymbolKind::Interface,
            def_ast,
            def_range: range,
            scope: iface_scope,
            origin: SymbolOrigin::TypeSpec,
        });
        ctx.symbol_to_decl.push(None);
        ctx.export_definitions.push(sym_id);

        let mut iface_def_id = None;
        if let Some(iface_decl) = InterfaceDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&iface_decl)
        {
            let erased = ast_id.erase();
            ctx.register_binding(sym_id, iface_scope, erased, range);
            iface_def_id = Some(crate::symbols::GlobalDefId::new(erased));
        }

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
        let sym_id = ctx.symbols.push(Symbol {
            name: name.clone(),
            kind: SymbolKind::Program,
            def_ast,
            def_range: range,
            scope: prog_scope,
            origin: SymbolOrigin::TypeSpec,
        });
        ctx.symbol_to_decl.push(None);
        ctx.export_definitions.push(sym_id);

        if let Some(prog_decl) = ProgramDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&prog_decl)
        {
            ctx.register_binding(sym_id, prog_scope, ast_id.erase(), range);
        }

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
        let sym_id = ctx.symbols.push(Symbol {
            name,
            kind: SymbolKind::Primitive,
            def_ast,
            def_range: range,
            scope: prim_scope,
            origin: SymbolOrigin::TypeSpec,
        });
        ctx.symbol_to_decl.push(None);
        ctx.export_definitions.push(sym_id);

        if let Some(prim_decl) = PrimitiveDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&prim_decl)
        {
            ctx.register_binding(sym_id, prim_scope, ast_id.erase(), range);
        }
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
        let sym_id = ctx.symbols.push(Symbol {
            name,
            kind: SymbolKind::Config,
            def_ast,
            def_range: range,
            scope: cfg_scope,
            origin: SymbolOrigin::TypeSpec,
        });
        ctx.symbol_to_decl.push(None);
        ctx.export_definitions.push(sym_id);

        if let Some(cfg_decl) = ConfigDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&cfg_decl)
        {
            ctx.register_binding(sym_id, cfg_scope, ast_id.erase(), range);
        }
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
            let sym_id = ctx.add_symbol(
                def_ast,
                SmolStr::new(name_tok.text()),
                SymbolKind::Port,
                name_tok.text_range(),
                scope,
            );
            // Store Port node AstId for type extraction
            if let Some(port_node) = Port::cast(child.clone())
                && let Some(ast_id) = ctx.ast_id_map.ast_id(&port_node)
            {
                ctx.register_binding(sym_id, scope, ast_id.erase(), name_tok.text_range());
            }
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
            // Record the module type name as a Definition-namespace use-site
            if let Some(inst) = ModuleInstantiation::cast(node.clone())
                && let Some(name_tok) = inst.module_name()
                && let Some(ast_id) = ctx.ast_id_map.ast_id(&inst)
            {
                let def_ast = ctx.ast_id_map.erased_ast_id(node);
                let type_use_site_idx = ctx.use_sites.len() as u32;
                let type_name_range = name_tok.text_range();
                ctx.use_sites.push(UseSite {
                    path: NamePath::Simple(SmolStr::new(name_tok.text())),
                    expected_ns: ExpectedNs::Exact(Namespace::Definition),
                    range: type_name_range,
                    scope,
                    ast_id: ast_id.erase(),
                    order_key: 0,
                });
                // Register each instance name as an Instance symbol
                if let Some(def_ast) = def_ast {
                    for (inst_name_tok, _port_list) in inst.instances() {
                        let idx = InstanceDeclIdx(ctx.instance_decls.len() as u32);
                        let sym_id = ctx.add_symbol_with_origin(
                            def_ast,
                            SmolStr::new(inst_name_tok.text()),
                            SymbolKind::Instance,
                            inst_name_tok.text_range(),
                            scope,
                            SymbolOrigin::Instance(idx),
                        );
                        ctx.instance_decls.push(InstanceDecl {
                            type_use_site_idx,
                            sym_id,
                            name: SmolStr::new(inst_name_tok.text()),
                            name_range: inst_name_tok.text_range(),
                            type_name_range,
                            scope,
                        });
                    }
                }
            }
            // Still collect NameRefs in port connection expressions
            collect_name_refs(ctx, node, scope);
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

fn collect_callable_decl(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
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
    let Some(def_ast) = ctx.ast_id_map.erased_ast_id(node) else {
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
    let sym_id = ctx.add_symbol(def_ast, name.clone(), kind, name_tok.text_range(), scope);

    // Store decl-to-symbol mapping
    if is_function {
        if let Some(func) = FunctionDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&func)
        {
            ctx.register_binding(sym_id, scope, ast_id.erase(), name_tok.text_range());
        }
    } else if let Some(task) = TaskDecl::cast(node.clone())
        && let Some(ast_id) = ctx.ast_id_map.ast_id(&task)
    {
        ctx.register_binding(sym_id, scope, ast_id.erase(), name_tok.text_range());
    }

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

    for port_decl in &tf_port_decls {
        // Collect type spec refs for resolution
        if let Some(ts) = port_decl.type_spec() {
            collect_type_spec_refs(ctx, &ts, callable_scope);
        }
        let Some(port_def_ast) = ctx.ast_id_map.erased_ast_id(port_decl.syntax()) else {
            continue;
        };
        // Collect each declarator as a port symbol
        for decl in port_decl.declarators() {
            if let Some(name_tok) = decl.name() {
                let port_sym = ctx.add_symbol(
                    port_def_ast,
                    SmolStr::new(name_tok.text()),
                    SymbolKind::Port,
                    name_tok.text_range(),
                    callable_scope,
                );
                if let Some(ast_id) = ctx.ast_id_map.ast_id(&decl) {
                    ctx.register_binding(
                        port_sym,
                        callable_scope,
                        ast_id.erase(),
                        name_tok.text_range(),
                    );
                }
            }
        }
    }

    // Body is opaque for now: no local variables, statements, or name-refs
    // are collected. A follow-up PR must walk the body to:
    //   1. Collect local variable/parameter declarations
    //   2. Index name-refs for resolve_index (enables go-to-definition inside bodies)
    //   3. Walk statements for type-checking (assign width checks, etc.)
    // Until then, expressions inside callable bodies are invisible to the
    // semantic model. This is acceptable because call *sites* (outside the
    // body) are the primary target of this change.
}

fn collect_modport_decl(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
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

        // Collect port entries with sticky direction
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
                            span: lyra_source::Span {
                                file: ctx.file,
                                range: port_name_tok.text_range(),
                            },
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
                            span: lyra_source::Span {
                                file: ctx.file,
                                range: port_name_tok.text_range(),
                            },
                        });
                    }
                }
            }
        }

        // Register symbol for navigation/diagnostics
        let Some(modport_def_ast) = ctx.ast_id_map.erased_ast_id(item.syntax()) else {
            continue;
        };
        ctx.add_symbol(
            modport_def_ast,
            name.clone(),
            SymbolKind::Modport,
            name_tok.text_range(),
            scope,
        );

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

fn parse_direction(kind: SyntaxKind, current: Option<PortDirection>) -> Option<PortDirection> {
    match kind {
        SyntaxKind::InputKw => Some(PortDirection::Input),
        SyntaxKind::OutputKw => Some(PortDirection::Output),
        SyntaxKind::InoutKw => Some(PortDirection::Inout),
        SyntaxKind::RefKw => Some(PortDirection::Ref),
        _ => current,
    }
}

fn collect_import_decl(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        if child.kind() == SyntaxKind::ImportItem {
            collect_import_item(ctx, &child, scope);
        }
    }
}

fn collect_import_item(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    let Some(ast_id) = ctx.ast_id_map.erased_ast_id(node) else {
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
        // Wildcard: first Ident is the package name
        if let Some(pkg_tok) = first_ident_token(node) {
            ctx.imports.push(Import {
                id,
                package: SmolStr::new(pkg_tok.text()),
                name: ImportName::Wildcard,
                scope,
                range: node.text_range(),
                ast_id,
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
                range: node.text_range(),
                ast_id,
                order_key: 0,
            });
        }
    }
}

fn collect_export_decl(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
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
    let ordinal = ctx.export_ordinals.entry(scope).or_insert(0);
    let ord = *ordinal;
    *ordinal += 1;
    ctx.export_decls.push(crate::def_index::ExportDecl {
        id: ExportDeclId {
            scope,
            ordinal: ord,
        },
        key,
        range: item.syntax().text_range(),
    });
}

fn collect_param_decl(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    let Some(def_ast) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error(
            &format!(
                "erased_ast_id returned None for {:?} in collect_param_decl",
                node.kind()
            ),
            node.text_range(),
        );
        return;
    };
    for child in node.children() {
        if let Some(ts) = TypeSpec::cast(child.clone()) {
            collect_type_spec_refs(ctx, &ts, scope);
        } else if child.kind() == SyntaxKind::Declarator {
            if let Some(name_tok) = first_ident_token(&child) {
                let sym_id = ctx.add_symbol(
                    def_ast,
                    SmolStr::new(name_tok.text()),
                    SymbolKind::Parameter,
                    name_tok.text_range(),
                    scope,
                );
                if let Some(decl) = lyra_ast::Declarator::cast(child.clone())
                    && let Some(ast_id) = ctx.ast_id_map.ast_id(&decl)
                {
                    let erased = ast_id.erase();
                    ctx.register_binding(sym_id, scope, erased, name_tok.text_range());

                    // Find init expression (first expression-like child node)
                    let init_id = child
                        .children()
                        .find(|c| is_expression_kind(c.kind()))
                        .and_then(|expr| ctx.ast_id_map.erased_ast_id(&expr));
                    ctx.decl_to_init_expr.insert(erased, init_id);
                }
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
    let Some(def_ast) = ctx.ast_id_map.erased_ast_id(node) else {
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
    for child in node.children() {
        if let Some(ts) = TypeSpec::cast(child.clone()) {
            collect_type_spec_refs(ctx, &ts, scope);
        } else if child.kind() == SyntaxKind::Declarator {
            if let Some(name_tok) = first_ident_token(&child) {
                let sym_id = ctx.add_symbol_with_origin(
                    def_ast,
                    SmolStr::new(name_tok.text()),
                    kind,
                    name_tok.text_range(),
                    scope,
                    origin,
                );
                if let Some(decl) = lyra_ast::Declarator::cast(child.clone())
                    && let Some(ast_id) = ctx.ast_id_map.ast_id(&decl)
                {
                    ctx.register_binding(sym_id, scope, ast_id.erase(), name_tok.text_range());
                }
            }
            collect_name_refs(ctx, &child, scope);
        }
    }
}

pub(crate) use crate::expr_helpers::is_expression_kind;
use crate::syntax_helpers::{first_ident_token, port_name_ident};

#[cfg(test)]
#[path = "builder_tests.rs"]
mod tests;
