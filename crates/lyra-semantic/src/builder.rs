use std::collections::HashMap;

use lyra_ast::{
    AstIdMap, AstNode, ConfigDecl, EnumType, FunctionDecl, InterfaceDecl, ModuleInstantiation,
    NameRef, Port, PrimitiveDecl, ProgramDecl, QualifiedName, StructType, TaskDecl, TfPortDecl,
    TypedefDecl,
};
use lyra_lexer::SyntaxKind;
use lyra_parser::{Parse, SyntaxNode};
use lyra_source::{FileId, TextRange};
use smol_str::SmolStr;

use crate::aggregate::{
    EnumDef, EnumDefIdx, EnumVariant, StructDef, StructDefIdx, StructField, TypeOrigin, TypeRef,
    extract_typeref_from_typespec,
};
use crate::def_index::{DefIndex, ExpectedNs, Exports, Import, ImportName, NamePath, UseSite};
use crate::diagnostic::{SemanticDiag, SemanticDiagKind};
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
        struct_defs: ctx.struct_defs.into_boxed_slice(),
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
    pub(crate) struct_defs: Vec<StructDef>,
    pub(crate) current_owner: Option<SmolStr>,
    pub(crate) enum_ordinals: HashMap<Option<SmolStr>, u32>,
    pub(crate) struct_ordinals: HashMap<Option<SmolStr>, u32>,
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
            struct_defs: Vec::new(),
            current_owner: None,
            enum_ordinals: HashMap::new(),
            struct_ordinals: HashMap::new(),
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

        if let Some(iface_decl) = InterfaceDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&iface_decl)
        {
            let erased = ast_id.erase();
            ctx.decl_to_symbol.insert(erased, sym_id);
            ctx.symbol_to_decl[sym_id.index()] = Some(erased);
        }

        let prev_owner = ctx.current_owner.replace(name);
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
        collect_module_item(ctx, &child, scope);
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
            TypeOrigin::Struct(idx) => {
                ctx.struct_defs[idx.0 as usize].name = Some(typedef_name.clone());
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
                    return TypeOrigin::Struct(collect_struct_def(ctx, &st, scope));
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
        extract_typeref_from_typespec(base_ts.syntax(), ctx.file)
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

fn collect_struct_def(
    ctx: &mut DefContext<'_>,
    struct_type: &StructType,
    _scope: ScopeId,
) -> StructDefIdx {
    let owner = ctx.current_owner.clone();
    let ordinal = ctx.struct_ordinals.entry(owner.clone()).or_insert(0);
    let ord = *ordinal;
    *ordinal += 1;
    let idx = StructDefIdx(ctx.struct_defs.len() as u32);

    let packed = struct_type.is_packed();
    let is_union = struct_type.is_union();

    // Extract fields from StructMember children
    let mut fields = Vec::new();
    for member in struct_type.members() {
        let member_ts = member.type_spec();
        let ty = match member_ts {
            Some(ref ts) => extract_typeref_from_typespec(ts.syntax(), ctx.file),
            None => TypeRef::Resolved(Ty::Error),
        };
        for decl in member.declarators() {
            if let Some(name_tok) = decl.name() {
                fields.push(StructField {
                    name: SmolStr::new(name_tok.text()),
                    ty: ty.clone(),
                });
            }
        }
    }

    ctx.struct_defs.push(StructDef {
        name: None,
        owner,
        ordinal: ord,
        packed,
        is_union,
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
        } else {
            collect_name_refs(ctx, &child, scope);
        }
    }
}

pub(crate) fn is_expression_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Expression
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
            | SyntaxKind::SystemTfCall
            | SyntaxKind::ArgList
            | SyntaxKind::NameRef
            | SyntaxKind::Literal
            | SyntaxKind::QualifiedName
    )
}

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
mod tests {
    use lyra_source::FileId;

    use super::*;
    use crate::name_graph::NameGraph;

    fn parse_source(src: &str) -> (lyra_parser::Parse, lyra_ast::AstIdMap) {
        let tokens = lyra_lexer::lex(src);
        let pp = lyra_preprocess::preprocess_identity(FileId(0), &tokens, src);
        let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
        let map = lyra_ast::AstIdMap::from_root(FileId(0), &parse.syntax());
        (parse, map)
    }

    #[test]
    fn use_site_ordering_stable_across_whitespace() {
        let src_a = "module m; logic x; assign x = 0; endmodule";
        let src_b = "module  m;  logic  x;  assign  x  =  0;  endmodule";

        let (parse_a, map_a) = parse_source(src_a);
        let def_a = build_def_index(FileId(0), &parse_a, &map_a);
        let graph_a = NameGraph::from_def_index(&def_a);

        let (parse_b, map_b) = parse_source(src_b);
        let def_b = build_def_index(FileId(0), &parse_b, &map_b);
        let graph_b = NameGraph::from_def_index(&def_b);

        // NameGraph should be equal (offset-independent)
        assert_eq!(
            graph_a, graph_b,
            "NameGraph should be equal across whitespace edits"
        );

        // Ranges and ast_ids differ, confirming DefIndex is NOT equal
        assert_ne!(def_a, def_b, "DefIndex should differ (offsets changed)");
    }

    #[test]
    fn package_symbols_collected() {
        let src = "package pkg; logic x; parameter P = 1; endpackage";
        let (parse, map) = parse_source(src);
        let def = build_def_index(FileId(0), &parse, &map);

        assert_eq!(def.exports.packages.len(), 1);
        let pkg_sym = def.symbols.get(def.exports.packages[0]);
        assert_eq!(pkg_sym.name.as_str(), "pkg");
        assert_eq!(pkg_sym.kind, SymbolKind::Package);

        // Check that internal symbols were collected
        let has_x = def
            .symbols
            .iter()
            .any(|(_, s)| s.name.as_str() == "x" && s.kind == SymbolKind::Variable);
        assert!(has_x, "variable 'x' should be collected in package");
    }

    #[test]
    fn import_recorded_in_def_index() {
        let src = "module m; import pkg::x; import pkg2::*; endmodule";
        let (parse, map) = parse_source(src);
        let def = build_def_index(FileId(0), &parse, &map);

        assert_eq!(def.imports.len(), 2);
        assert_eq!(def.imports[0].package.as_str(), "pkg");
        assert_eq!(def.imports[0].name, ImportName::Explicit(SmolStr::new("x")));
        assert_eq!(def.imports[1].package.as_str(), "pkg2");
        assert_eq!(def.imports[1].name, ImportName::Wildcard);

        // Imports should NOT appear as use-sites
        let import_use_sites: Vec<_> = def
            .use_sites
            .iter()
            .filter(|u| {
                u.path
                    .as_simple()
                    .is_some_and(|s| s == "pkg" || s == "pkg2")
            })
            .collect();
        assert!(
            import_use_sites.is_empty(),
            "imports should not be recorded as use-sites"
        );
    }

    #[test]
    fn qualified_name_use_site() {
        let src = "module m; assign y = pkg::x; endmodule";
        let (parse, map) = parse_source(src);
        let def = build_def_index(FileId(0), &parse, &map);

        let qualified_sites: Vec<_> = def
            .use_sites
            .iter()
            .filter(|u| u.path.as_qualified().is_some())
            .collect();
        assert_eq!(qualified_sites.len(), 1);
        let segs = qualified_sites[0].path.as_qualified().unwrap();
        assert_eq!(segs.len(), 2);
        assert_eq!(segs[0].as_str(), "pkg");
        assert_eq!(segs[1].as_str(), "x");
    }

    #[test]
    fn name_graph_includes_imports() {
        let src = "module m; import pkg::x; endmodule";
        let (parse, map) = parse_source(src);
        let def = build_def_index(FileId(0), &parse, &map);
        let graph = NameGraph::from_def_index(&def);

        assert_eq!(graph.imports.len(), 1);
        assert_eq!(graph.imports[0].package.as_str(), "pkg");
    }

    #[test]
    fn typedef_collected_as_type_ns() {
        let src = "module m; typedef logic [7:0] byte_t; endmodule";
        let (parse, map) = parse_source(src);
        let def = build_def_index(FileId(0), &parse, &map);

        let typedef_sym = def
            .symbols
            .iter()
            .find(|(_, s)| s.name.as_str() == "byte_t")
            .map(|(_, s)| s);
        assert!(
            typedef_sym.is_some(),
            "typedef 'byte_t' should be collected"
        );
        assert_eq!(typedef_sym.expect("checked").kind, SymbolKind::Typedef);
    }

    #[test]
    fn typedef_use_site_type_then_value() {
        let src = "module m; typedef my_t other_t; endmodule";
        let (parse, map) = parse_source(src);
        let def = build_def_index(FileId(0), &parse, &map);

        let my_t_use = def
            .use_sites
            .iter()
            .find(|u| u.path.as_simple() == Some("my_t"));
        assert!(my_t_use.is_some(), "'my_t' should be a use-site");
        assert_eq!(
            my_t_use.expect("checked").expected_ns,
            ExpectedNs::TypeThenValue,
            "type-position NameRef should have TypeThenValue"
        );

        let other_sym = def
            .symbols
            .iter()
            .find(|(_, s)| s.name.as_str() == "other_t");
        assert!(other_sym.is_some(), "'other_t' should be a typedef symbol");
    }

    #[test]
    fn same_name_different_namespace_no_duplicate() {
        let src = "module m; logic x; typedef int x; endmodule";
        let (parse, map) = parse_source(src);
        let def = build_def_index(FileId(0), &parse, &map);

        assert!(
            def.diagnostics.is_empty(),
            "value 'x' and type 'x' should not conflict: {:?}",
            def.diagnostics
        );
    }

    #[test]
    fn same_namespace_typedef_duplicate() {
        let src = "module m; typedef int t; typedef logic t; endmodule";
        let (parse, map) = parse_source(src);
        let def = build_def_index(FileId(0), &parse, &map);

        let dup_diags: Vec<_> = def
            .diagnostics
            .iter()
            .filter(|d| matches!(d.kind, SemanticDiagKind::DuplicateDefinition { .. }))
            .collect();
        assert!(
            !dup_diags.is_empty(),
            "two typedefs with same name should produce duplicate diagnostic"
        );
    }
}
