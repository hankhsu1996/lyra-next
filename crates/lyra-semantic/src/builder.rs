use std::collections::HashMap;

use lyra_ast::{
    AstIdMap, AstNode, ConfigDecl, EnumType, ExportDecl, ExportItem, FunctionDecl, InterfaceDecl,
    ModportDecl, ModuleInstantiation, NameRef, Port, PrimitiveDecl, ProgramDecl, QualifiedName,
    StructType, TaskDecl, TfPortDecl, TypedefDecl,
};
use lyra_lexer::SyntaxKind;
use lyra_parser::{Parse, SyntaxNode};
use lyra_source::{FileId, TextRange};
use smol_str::SmolStr;

use crate::def_index::{
    DefIndex, ExpectedNs, ExportEntry, Exports, Import, ImportName, NamePath, UseSite,
};
use crate::diagnostic::{SemanticDiag, SemanticDiagKind};
use crate::record::{
    EnumDef, EnumDefIdx, EnumVariant, ModportDef, ModportDefId, ModportEntry, Packing,
    PortDirection, RecordDef, RecordDefIdx, RecordField, RecordKind, TypeOrigin, TypeRef,
    extract_typeref_from_typespec,
};
use crate::scopes::{ScopeId, ScopeKind, ScopeTreeBuilder};
use crate::symbols::{Namespace, Symbol, SymbolId, SymbolKind, SymbolTableBuilder};
use crate::types::Ty;

pub fn build_def_index(file: FileId, parse: &Parse, ast_id_map: &AstIdMap) -> DefIndex {
    let mut ctx = DefContext::new(file, ast_id_map);
    let root = parse.syntax();

    // Root file scope
    let file_scope = ctx.scopes.push(ScopeKind::File, None);
    collect_source_file(&mut ctx, &root, file_scope);

    // Freeze symbols first, then scopes (which need symbol names for sorting)
    let symbols = ctx.symbols.freeze();
    let scopes = ctx.scopes.freeze(&symbols);

    // Sort export definition/package ids by symbol name
    ctx.export_definitions
        .sort_by(|a, b| symbols.get(*a).name.cmp(&symbols.get(*b).name));
    ctx.export_packages
        .sort_by(|a, b| symbols.get(*a).name.cmp(&symbols.get(*b).name));

    // Detect duplicate definitions within each scope, per namespace
    let mut diagnostics = Vec::new();
    for scope_idx in 0..scopes.len() {
        let s = scopes.get(ScopeId(scope_idx as u32));
        detect_duplicates(&symbols, &s.value_ns, &mut diagnostics);
        detect_duplicates(&symbols, &s.type_ns, &mut diagnostics);
    }

    DefIndex {
        file,
        symbols,
        scopes,
        exports: Exports {
            definitions: ctx.export_definitions.into_boxed_slice(),
            packages: ctx.export_packages.into_boxed_slice(),
        },
        use_sites: ctx.use_sites.into_boxed_slice(),
        imports: ctx.imports.into_boxed_slice(),
        decl_to_symbol: ctx.decl_to_symbol,
        symbol_to_decl: ctx.symbol_to_decl.into_boxed_slice(),
        decl_to_init_expr: ctx.decl_to_init_expr,
        enum_defs: ctx.enum_defs.into_boxed_slice(),
        record_defs: ctx.record_defs.into_boxed_slice(),
        modport_defs: ctx.modport_defs,
        modport_name_map: ctx.modport_name_map,
        export_decls: ctx.export_decls.into_boxed_slice(),
        diagnostics: diagnostics.into_boxed_slice(),
    }
}

fn detect_duplicates(
    symbols: &crate::symbols::SymbolTable,
    bindings: &[SymbolId],
    diagnostics: &mut Vec<SemanticDiag>,
) {
    for i in 1..bindings.len() {
        let prev = symbols.get(bindings[i - 1]);
        let curr = symbols.get(bindings[i]);
        if prev.name == curr.name {
            diagnostics.push(SemanticDiag {
                kind: SemanticDiagKind::DuplicateDefinition {
                    name: curr.name.clone(),
                    original: prev.def_range,
                },
                range: curr.def_range,
            });
        }
    }
}

/// Mutable context accumulated during a single `build_def_index` pass.
///
/// Fields are `pub(crate)` because `builder_stmt` needs direct access for
/// statement/expression collection. Both modules are internal to
/// `lyra-semantic` and maintain builder invariants together.
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
    pub(crate) modport_defs: HashMap<ModportDefId, ModportDef>,
    pub(crate) modport_name_map: HashMap<(crate::symbols::GlobalDefId, SmolStr), ModportDefId>,
    pub(crate) export_decls: Vec<ExportEntry>,
    pub(crate) current_owner: Option<SmolStr>,
    pub(crate) current_iface_def_id: Option<crate::symbols::GlobalDefId>,
    pub(crate) modport_ordinal: u32,
    pub(crate) enum_ordinals: HashMap<Option<SmolStr>, u32>,
    pub(crate) record_ordinals: HashMap<Option<SmolStr>, u32>,
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
            modport_defs: HashMap::new(),
            modport_name_map: HashMap::new(),
            export_decls: Vec::new(),
            current_owner: None,
            current_iface_def_id: None,
            modport_ordinal: 0,
            enum_ordinals: HashMap::new(),
            record_ordinals: HashMap::new(),
        }
    }

    pub(crate) fn add_symbol(
        &mut self,
        name: SmolStr,
        kind: SymbolKind,
        def_range: TextRange,
        scope: ScopeId,
    ) -> SymbolId {
        self.add_symbol_with_origin(name, kind, def_range, scope, TypeOrigin::TypeSpec)
    }

    pub(crate) fn add_symbol_with_origin(
        &mut self,
        name: SmolStr,
        kind: SymbolKind,
        def_range: TextRange,
        scope: ScopeId,
        type_origin: TypeOrigin,
    ) -> SymbolId {
        let id = self.symbols.push(Symbol {
            name,
            kind,
            def_range,
            scope,
            type_origin,
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
        let name = SmolStr::new(name_tok.text());
        let range = name_tok.text_range();
        let module_scope = ctx.scopes.push(ScopeKind::Module, None);
        let sym_id = ctx.symbols.push(Symbol {
            name: name.clone(),
            kind: SymbolKind::Module,
            def_range: range,
            scope: module_scope,
            type_origin: TypeOrigin::TypeSpec,
        });
        ctx.symbol_to_decl.push(None);
        ctx.export_definitions.push(sym_id);

        if let Some(module_decl) = lyra_ast::ModuleDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&module_decl)
        {
            let erased = ast_id.erase();
            ctx.decl_to_symbol.insert(erased, sym_id);
            ctx.symbol_to_decl[sym_id.index()] = Some(erased);
        }

        let prev_owner = ctx.current_owner.replace(name);
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
        ctx.current_owner = prev_owner;
    }
}

fn collect_package(ctx: &mut DefContext<'_>, node: &SyntaxNode, _file_scope: ScopeId) {
    let package_name = first_ident_token(node);
    if let Some(name_tok) = &package_name {
        let name = SmolStr::new(name_tok.text());
        let range = name_tok.text_range();
        let package_scope = ctx.scopes.push(ScopeKind::Package, None);
        let sym_id = ctx.symbols.push(Symbol {
            name: name.clone(),
            kind: SymbolKind::Package,
            def_range: range,
            scope: package_scope,
            type_origin: TypeOrigin::TypeSpec,
        });
        ctx.symbol_to_decl.push(None);
        ctx.export_packages.push(sym_id);

        if let Some(pkg_decl) = lyra_ast::PackageDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&pkg_decl)
        {
            let erased = ast_id.erase();
            ctx.decl_to_symbol.insert(erased, sym_id);
            ctx.symbol_to_decl[sym_id.index()] = Some(erased);
        }

        let prev_owner = ctx.current_owner.replace(name);
        for child in node.children() {
            if child.kind() == SyntaxKind::PackageBody {
                collect_package_body(ctx, &child, package_scope);
            }
        }
        ctx.current_owner = prev_owner;
    }
}

fn collect_interface(ctx: &mut DefContext<'_>, node: &SyntaxNode) {
    let name_tok = first_ident_token(node);
    if let Some(name_tok) = &name_tok {
        let name = SmolStr::new(name_tok.text());
        let range = name_tok.text_range();
        let iface_scope = ctx.scopes.push(ScopeKind::Interface, None);
        let sym_id = ctx.symbols.push(Symbol {
            name: name.clone(),
            kind: SymbolKind::Interface,
            def_range: range,
            scope: iface_scope,
            type_origin: TypeOrigin::TypeSpec,
        });
        ctx.symbol_to_decl.push(None);
        ctx.export_definitions.push(sym_id);

        let mut iface_def_id = None;
        if let Some(iface_decl) = InterfaceDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&iface_decl)
        {
            let erased = ast_id.erase();
            ctx.decl_to_symbol.insert(erased, sym_id);
            ctx.symbol_to_decl[sym_id.index()] = Some(erased);
            iface_def_id = Some(crate::symbols::GlobalDefId::new(erased));
        }

        let prev_owner = ctx.current_owner.replace(name);
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
        ctx.current_owner = prev_owner;
        ctx.current_iface_def_id = prev_iface;
    }
}

fn collect_program(ctx: &mut DefContext<'_>, node: &SyntaxNode) {
    let name_tok = first_ident_token(node);
    if let Some(name_tok) = &name_tok {
        let name = SmolStr::new(name_tok.text());
        let range = name_tok.text_range();
        let prog_scope = ctx.scopes.push(ScopeKind::Program, None);
        let sym_id = ctx.symbols.push(Symbol {
            name: name.clone(),
            kind: SymbolKind::Program,
            def_range: range,
            scope: prog_scope,
            type_origin: TypeOrigin::TypeSpec,
        });
        ctx.symbol_to_decl.push(None);
        ctx.export_definitions.push(sym_id);

        if let Some(prog_decl) = ProgramDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&prog_decl)
        {
            let erased = ast_id.erase();
            ctx.decl_to_symbol.insert(erased, sym_id);
            ctx.symbol_to_decl[sym_id.index()] = Some(erased);
        }

        let prev_owner = ctx.current_owner.replace(name);
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
        ctx.current_owner = prev_owner;
    }
}

fn collect_primitive(ctx: &mut DefContext<'_>, node: &SyntaxNode) {
    let name_tok = first_ident_token(node);
    if let Some(name_tok) = &name_tok {
        let name = SmolStr::new(name_tok.text());
        let range = name_tok.text_range();
        let prim_scope = ctx.scopes.push(ScopeKind::Module, None);
        let sym_id = ctx.symbols.push(Symbol {
            name,
            kind: SymbolKind::Primitive,
            def_range: range,
            scope: prim_scope,
            type_origin: TypeOrigin::TypeSpec,
        });
        ctx.symbol_to_decl.push(None);
        ctx.export_definitions.push(sym_id);

        if let Some(prim_decl) = PrimitiveDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&prim_decl)
        {
            let erased = ast_id.erase();
            ctx.decl_to_symbol.insert(erased, sym_id);
            ctx.symbol_to_decl[sym_id.index()] = Some(erased);
        }
    }
}

fn collect_config(ctx: &mut DefContext<'_>, node: &SyntaxNode) {
    let name_tok = first_ident_token(node);
    if let Some(name_tok) = &name_tok {
        let name = SmolStr::new(name_tok.text());
        let range = name_tok.text_range();
        let cfg_scope = ctx.scopes.push(ScopeKind::Module, None);
        let sym_id = ctx.symbols.push(Symbol {
            name,
            kind: SymbolKind::Config,
            def_range: range,
            scope: cfg_scope,
            type_origin: TypeOrigin::TypeSpec,
        });
        ctx.symbol_to_decl.push(None);
        ctx.export_definitions.push(sym_id);

        if let Some(cfg_decl) = ConfigDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&cfg_decl)
        {
            let erased = ast_id.erase();
            ctx.decl_to_symbol.insert(erased, sym_id);
            ctx.symbol_to_decl[sym_id.index()] = Some(erased);
        }
    }
}

fn collect_package_body(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        if ExportDecl::cast(child.clone()).is_some() {
            collect_export_decl(ctx, &child);
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
            // Prefer the port-name ident (last direct Ident child, after
            // any TypeSpec type name). Fall back to the first Ident.
            let name_tok = port_name_ident(&child).unwrap_or(fallback_tok);
            let sym_id = ctx.add_symbol(
                SmolStr::new(name_tok.text()),
                SymbolKind::Port,
                name_tok.text_range(),
                scope,
            );
            // Store Port node AstId for type extraction
            if let Some(port_node) = Port::cast(child.clone())
                && let Some(ast_id) = ctx.ast_id_map.ast_id(&port_node)
            {
                let erased = ast_id.erase();
                ctx.decl_to_symbol.insert(erased, sym_id);
                ctx.symbol_to_decl[sym_id.index()] = Some(erased);
            }
            // Collect type-spec name refs for port typedef resolution
            for port_child in child.children() {
                if port_child.kind() == SyntaxKind::TypeSpec {
                    collect_type_spec_refs(ctx, &port_child, scope);
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
                ctx.use_sites.push(UseSite {
                    path: NamePath::Simple(SmolStr::new(name_tok.text())),
                    expected_ns: ExpectedNs::Exact(Namespace::Definition),
                    range: name_tok.text_range(),
                    scope,
                    ast_id: ast_id.erase(),
                });
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
    let name = SmolStr::new(name_tok.text());
    let scope_kind = if is_function {
        ScopeKind::Function
    } else {
        ScopeKind::Task
    };
    let callable_scope = ctx.scopes.push(scope_kind, Some(scope));
    let sym_id = ctx.add_symbol(name.clone(), kind, name_tok.text_range(), scope);

    // Store decl-to-symbol mapping
    if is_function {
        if let Some(func) = FunctionDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&func)
        {
            let erased = ast_id.erase();
            ctx.decl_to_symbol.insert(erased, sym_id);
            ctx.symbol_to_decl[sym_id.index()] = Some(erased);
        }
    } else if let Some(task) = TaskDecl::cast(node.clone())
        && let Some(ast_id) = ctx.ast_id_map.ast_id(&task)
    {
        let erased = ast_id.erase();
        ctx.decl_to_symbol.insert(erased, sym_id);
        ctx.symbol_to_decl[sym_id.index()] = Some(erased);
    }

    // Collect return type spec refs (for typedef resolution in the enclosing scope)
    if is_function
        && let Some(func) = FunctionDecl::cast(node.clone())
        && let Some(ts) = func.type_spec()
    {
        collect_type_spec_refs(ctx, ts.syntax(), scope);
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
            collect_type_spec_refs(ctx, ts.syntax(), callable_scope);
        }
        // Collect each declarator as a port symbol
        for decl in port_decl.declarators() {
            if let Some(name_tok) = decl.name() {
                let port_sym = ctx.add_symbol(
                    SmolStr::new(name_tok.text()),
                    SymbolKind::Port,
                    name_tok.text_range(),
                    callable_scope,
                );
                if let Some(ast_id) = ctx.ast_id_map.ast_id(&decl) {
                    let erased = ast_id.erase();
                    ctx.decl_to_symbol.insert(erased, port_sym);
                    ctx.symbol_to_decl[port_sym.index()] = Some(erased);
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

        let modport_id = ModportDefId { owner, ordinal };

        // Collect port entries with sticky direction
        let mut entries = Vec::new();
        let mut current_dir: Option<PortDirection> = None;
        for port in item.ports() {
            if let Some(dir_tok) = port.direction() {
                current_dir = match dir_tok.kind() {
                    SyntaxKind::InputKw => Some(PortDirection::Input),
                    SyntaxKind::OutputKw => Some(PortDirection::Output),
                    SyntaxKind::InoutKw => Some(PortDirection::Inout),
                    SyntaxKind::RefKw => Some(PortDirection::Ref),
                    _ => current_dir,
                };
            }
            if let Some(dir) = current_dir
                && let Some(port_name_tok) = port.name()
            {
                entries.push(ModportEntry {
                    member_name: SmolStr::new(port_name_tok.text()),
                    direction: dir,
                    span: lyra_source::Span {
                        file: ctx.file,
                        range: port_name_tok.text_range(),
                    },
                });
            }
        }

        // Register symbol for navigation/diagnostics (skipped by add_binding)
        ctx.add_symbol(
            name.clone(),
            SymbolKind::Modport,
            name_tok.text_range(),
            scope,
        );

        // First-wins: later duplicates still get a def entry but won't
        // be reachable via name lookup.
        let map_key = (owner, name.clone());
        ctx.modport_name_map.entry(map_key).or_insert(modport_id);

        ctx.modport_defs.insert(
            modport_id,
            ModportDef {
                id: modport_id,
                name,
                entries: entries.into_boxed_slice(),
            },
        );
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
    // Determine if wildcard or explicit
    let has_star = node
        .children_with_tokens()
        .filter_map(lyra_parser::SyntaxElement::into_token)
        .any(|tok| tok.kind() == SyntaxKind::Star);

    if has_star {
        // Wildcard: first Ident is the package name
        if let Some(pkg_tok) = first_ident_token(node) {
            ctx.imports.push(Import {
                package: SmolStr::new(pkg_tok.text()),
                name: ImportName::Wildcard,
                scope,
                range: node.text_range(),
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
                package: SmolStr::new(idents[0].text()),
                name: ImportName::Explicit(SmolStr::new(idents[1].text())),
                scope,
                range: node.text_range(),
            });
        }
    }
}

fn collect_export_decl(ctx: &mut DefContext<'_>, node: &SyntaxNode) {
    let Some(decl) = ExportDecl::cast(node.clone()) else {
        return;
    };
    for item in decl.items() {
        collect_export_item(ctx, &item);
    }
}

fn collect_export_item(ctx: &mut DefContext<'_>, item: &ExportItem) {
    if item.is_all_wildcard() {
        ctx.export_decls.push(ExportEntry::AllWildcard);
    } else if let Some(pkg_tok) = item.package_name() {
        let package = SmolStr::new(pkg_tok.text());
        if item.is_wildcard() {
            ctx.export_decls
                .push(ExportEntry::PackageWildcard { package });
        } else if let Some(member_tok) = item.member_name() {
            ctx.export_decls.push(ExportEntry::Explicit {
                package,
                name: SmolStr::new(member_tok.text()),
            });
        }
    }
}

fn collect_param_decl(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        if child.kind() == SyntaxKind::TypeSpec {
            collect_type_spec_refs(ctx, &child, scope);
        } else if child.kind() == SyntaxKind::Declarator {
            if let Some(name_tok) = first_ident_token(&child) {
                let sym_id = ctx.add_symbol(
                    SmolStr::new(name_tok.text()),
                    SymbolKind::Parameter,
                    name_tok.text_range(),
                    scope,
                );
                // Record decl_to_symbol and symbol_to_decl
                if let Some(decl) = lyra_ast::Declarator::cast(child.clone())
                    && let Some(ast_id) = ctx.ast_id_map.ast_id(&decl)
                {
                    let erased = ast_id.erase();
                    ctx.decl_to_symbol.insert(erased, sym_id);
                    ctx.symbol_to_decl[sym_id.index()] = Some(erased);

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
    // Detect inline enum/struct in the TypeSpec child
    let type_origin = detect_aggregate_type(ctx, node, scope);
    for child in node.children() {
        if child.kind() == SyntaxKind::TypeSpec {
            collect_type_spec_refs(ctx, &child, scope);
        } else if child.kind() == SyntaxKind::Declarator {
            if let Some(name_tok) = first_ident_token(&child) {
                let sym_id = ctx.add_symbol_with_origin(
                    SmolStr::new(name_tok.text()),
                    kind,
                    name_tok.text_range(),
                    scope,
                    type_origin,
                );
                if let Some(decl) = lyra_ast::Declarator::cast(child.clone())
                    && let Some(ast_id) = ctx.ast_id_map.ast_id(&decl)
                {
                    let erased = ast_id.erase();
                    ctx.decl_to_symbol.insert(erased, sym_id);
                    ctx.symbol_to_decl[sym_id.index()] = Some(erased);
                }
            }
            collect_name_refs(ctx, &child, scope);
        }
    }
}

fn collect_type_spec_refs(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        match child.kind() {
            SyntaxKind::NameRef => {
                if let Some(name_ref) = NameRef::cast(child.clone())
                    && let Some(ident) = name_ref.ident()
                    && let Some(ast_id) = ctx.ast_id_map.ast_id(&name_ref)
                {
                    ctx.use_sites.push(UseSite {
                        path: NamePath::Simple(SmolStr::new(ident.text())),
                        expected_ns: ExpectedNs::TypeThenValue,
                        range: name_ref.text_range(),
                        scope,
                        ast_id: ast_id.erase(),
                    });
                }
            }
            SyntaxKind::QualifiedName => {
                if let Some(qn) = QualifiedName::cast(child.clone())
                    && let Some(ast_id) = ctx.ast_id_map.ast_id(&qn)
                {
                    let segments: Box<[SmolStr]> = qn
                        .segments()
                        .map(|ident| SmolStr::new(ident.text()))
                        .collect();
                    if !segments.is_empty() {
                        ctx.use_sites.push(UseSite {
                            path: NamePath::Qualified { segments },
                            expected_ns: ExpectedNs::TypeThenValue,
                            range: qn.text_range(),
                            scope,
                            ast_id: ast_id.erase(),
                        });
                    }
                }
            }
            SyntaxKind::PackedDimension | SyntaxKind::UnpackedDimension => {
                collect_name_refs(ctx, &child, scope);
            }
            _ => {}
        }
    }
}

fn collect_typedef(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    // Detect enum/struct in the TypeSpec child
    let type_origin = detect_aggregate_type(ctx, node, scope);
    for child in node.children() {
        if child.kind() == SyntaxKind::TypeSpec {
            collect_type_spec_refs(ctx, &child, scope);
        }
    }
    if let Some(td) = TypedefDecl::cast(node.clone())
        && let Some(name_tok) = td.name()
    {
        let typedef_name = SmolStr::new(name_tok.text());
        // Update the def name if we collected an aggregate
        match type_origin {
            TypeOrigin::Enum(idx) => {
                ctx.enum_defs[idx.0 as usize].name = Some(typedef_name.clone());
            }
            TypeOrigin::Record(idx) => {
                ctx.record_defs[idx.0 as usize].name = Some(typedef_name.clone());
            }
            TypeOrigin::TypeSpec => {}
        }
        let sym_id = ctx.add_symbol_with_origin(
            typedef_name,
            SymbolKind::Typedef,
            name_tok.text_range(),
            scope,
            type_origin,
        );
        if let Some(ast_id) = ctx.ast_id_map.ast_id(&td) {
            let erased = ast_id.erase();
            ctx.decl_to_symbol.insert(erased, sym_id);
            ctx.symbol_to_decl[sym_id.index()] = Some(erased);
        }
    }
}

// Detect enum/struct type in a declaration's TypeSpec child.
// If found, build the def table entry and return the appropriate TypeOrigin.
fn detect_aggregate_type(
    ctx: &mut DefContext<'_>,
    decl_node: &SyntaxNode,
    scope: ScopeId,
) -> TypeOrigin {
    for child in decl_node.children() {
        if child.kind() == SyntaxKind::TypeSpec {
            for ts_child in child.children() {
                if ts_child.kind() == SyntaxKind::EnumType
                    && let Some(et) = EnumType::cast(ts_child.clone())
                {
                    return TypeOrigin::Enum(collect_enum_def(ctx, &et, scope));
                } else if ts_child.kind() == SyntaxKind::StructType
                    && let Some(st) = StructType::cast(ts_child)
                {
                    return TypeOrigin::Record(collect_record_def(ctx, &st, scope));
                }
            }
        }
    }
    TypeOrigin::TypeSpec
}

fn collect_enum_def(ctx: &mut DefContext<'_>, enum_type: &EnumType, _scope: ScopeId) -> EnumDefIdx {
    let owner = ctx.current_owner.clone();
    let ordinal = ctx.enum_ordinals.entry(owner.clone()).or_insert(0);
    let ord = *ordinal;
    *ordinal += 1;
    let idx = EnumDefIdx(ctx.enum_defs.len() as u32);

    // Extract base type
    let base = if let Some(base_ts) = enum_type.base_type_spec() {
        extract_typeref_from_typespec(base_ts.syntax(), ctx.file, ctx.ast_id_map)
    } else {
        TypeRef::Resolved(Ty::int())
    };

    // Extract variants
    let variants: Box<[EnumVariant]> = enum_type
        .members()
        .filter_map(|member| {
            let name_tok = member.name()?;
            let init = member
                .syntax()
                .children()
                .find(|c| is_expression_kind(c.kind()))
                .and_then(|expr| ctx.ast_id_map.erased_ast_id(&expr));
            Some(EnumVariant {
                name: SmolStr::new(name_tok.text()),
                init,
            })
        })
        .collect();

    ctx.enum_defs.push(EnumDef {
        name: None,
        owner,
        ordinal: ord,
        base,
        variants,
    });
    idx
}

fn collect_record_def(
    ctx: &mut DefContext<'_>,
    struct_type: &StructType,
    scope: ScopeId,
) -> RecordDefIdx {
    let owner = ctx.current_owner.clone();
    let ordinal = ctx.record_ordinals.entry(owner.clone()).or_insert(0);
    let ord = *ordinal;
    *ordinal += 1;
    let idx = RecordDefIdx(ctx.record_defs.len() as u32);

    let kind = if struct_type.is_union() {
        RecordKind::Union
    } else {
        RecordKind::Struct
    };
    let packing = if struct_type.is_packed() {
        Packing::Packed
    } else {
        Packing::Unpacked
    };

    // Extract fields from StructMember children
    let mut fields = Vec::new();
    for member in struct_type.members() {
        let member_ts = member.type_spec();
        let ty = match member_ts {
            Some(ref ts) => {
                collect_type_spec_refs(ctx, ts.syntax(), scope);
                extract_typeref_from_typespec(ts.syntax(), ctx.file, ctx.ast_id_map)
            }
            None => TypeRef::Resolved(Ty::Error),
        };
        for decl in member.declarators() {
            if let Some(name_tok) = decl.name() {
                fields.push(RecordField {
                    name: SmolStr::new(name_tok.text()),
                    ty: ty.clone(),
                });
            }
        }
    }

    ctx.record_defs.push(RecordDef {
        name: None,
        owner,
        ordinal: ord,
        kind,
        packing,
        scope,
        fields: fields.into_boxed_slice(),
    });
    idx
}

pub(crate) fn collect_name_refs(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        if child.kind() == SyntaxKind::NameRef {
            if let Some(name_ref) = NameRef::cast(child.clone())
                && let Some(ident) = name_ref.ident()
                && let Some(ast_id) = ctx.ast_id_map.ast_id(&name_ref)
            {
                ctx.use_sites.push(UseSite {
                    path: NamePath::Simple(SmolStr::new(ident.text())),
                    expected_ns: ExpectedNs::Exact(Namespace::Value),
                    range: name_ref.text_range(),
                    scope,
                    ast_id: ast_id.erase(),
                });
            }
        } else if child.kind() == SyntaxKind::QualifiedName {
            if let Some(qn) = QualifiedName::cast(child.clone())
                && let Some(ast_id) = ctx.ast_id_map.ast_id(&qn)
            {
                let segments: Box<[SmolStr]> = qn
                    .segments()
                    .map(|ident| SmolStr::new(ident.text()))
                    .collect();
                if !segments.is_empty() {
                    ctx.use_sites.push(UseSite {
                        path: NamePath::Qualified { segments },
                        expected_ns: ExpectedNs::Exact(Namespace::Value),
                        range: qn.text_range(),
                        scope,
                        ast_id: ast_id.erase(),
                    });
                }
            }
        } else if child.kind() == SyntaxKind::TypeSpec {
            // Handled by collect_type_spec_refs with TypeThenValue.
        } else if child.kind() == SyntaxKind::Declarator {
            // Skip declarator names but collect refs in their initializers
            // (already handled by collect_declarators)
        } else if child.kind() == SyntaxKind::AssignmentPatternItem {
            let is_keyed = child
                .children_with_tokens()
                .any(|ct| ct.kind() == SyntaxKind::Colon);
            if is_keyed {
                let mut first = true;
                for item_child in child.children() {
                    if first {
                        first = false;
                        // Bare NameRef key is a struct field name, not a variable
                        if item_child.kind() == SyntaxKind::NameRef {
                            continue;
                        }
                    }
                    collect_name_refs(ctx, &item_child, scope);
                }
            } else {
                collect_name_refs(ctx, &child, scope);
            }
        } else {
            collect_name_refs(ctx, &child, scope);
        }
    }
}

pub(crate) use crate::expr_helpers::is_expression_kind;

fn first_ident_token(node: &SyntaxNode) -> Option<lyra_parser::SyntaxToken> {
    node.children_with_tokens()
        .filter_map(lyra_parser::SyntaxElement::into_token)
        .find(|tok| tok.kind() == SyntaxKind::Ident)
}

fn port_name_ident(port_node: &SyntaxNode) -> Option<lyra_parser::SyntaxToken> {
    // In an ANSI port declaration, the port name is the Ident token
    // that is a direct child of the Port node (not inside TypeSpec).
    // If there's a TypeSpec, the port name follows it.
    let mut last_ident = None;
    for el in port_node.children_with_tokens() {
        match el {
            lyra_parser::SyntaxElement::Token(tok) if tok.kind() == SyntaxKind::Ident => {
                last_ident = Some(tok);
            }
            _ => {}
        }
    }
    last_ident
}

#[cfg(test)]
#[path = "builder_tests.rs"]
mod tests;
