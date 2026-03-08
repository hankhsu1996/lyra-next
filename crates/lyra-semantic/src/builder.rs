use std::collections::HashMap;

use lyra_ast::{AstIdMap, AstNode, ExportDecl, HasSyntax, Port, TypeSpec, semantic_spelling};
use lyra_lexer::SyntaxKind;
use lyra_parser::{Parse, SyntaxNode, SyntaxToken};
use lyra_source::{DeclSpan, FileId, TokenSpan};
use smol_str::SmolStr;

use crate::builder_order::{assign_order_keys, detect_duplicates};
use crate::builder_types::{
    collect_name_refs, collect_nettype_decl, collect_type_spec_refs, collect_typedef,
};

use crate::def_entry::{DefEntry, DefEntryBuilder, DefScope};
use crate::def_index::{
    DefIndex, ForeachVarDef, ImplicitNetSiteKind, Import, LocalDecl, LocalDeclId, UseSite,
};
use crate::design_element::DesignElement;
use crate::diagnostic::SemanticDiag;
use crate::enum_def::EnumDef;
use crate::global_index::DefinitionKind;
use crate::instance_decl::InstanceDecl;
use crate::interface_id::InterfaceDefId;
use crate::modport_def::{ModportDef, ModportDefId};
use crate::nettype_def::NettypeDef;
use crate::record::{RecordDef, SymbolOrigin};
use crate::scopes::{ScopeId, ScopeKind, ScopeTreeBuilder};
use crate::symbols::{GlobalDefId, Lifetime, Symbol, SymbolId, SymbolKind, SymbolTableBuilder};
use crate::time_scale::ScopeTimeUnits;

/// Cast a `SyntaxNode` to a typed AST wrapper at a matched-kind dispatch boundary.
///
/// This must only be called after the dispatcher has already matched
/// `node.kind()` to the expected AST kind. Cast failure after a kind
/// match is a builder invariant violation.
pub(crate) fn expect_typed<T: AstNode>(node: &SyntaxNode) -> T {
    match T::cast(node.clone()) {
        Some(it) => it,
        None => unreachable!(
            "builder dispatch invariant: {:?} did not cast to {}",
            node.kind(),
            std::any::type_name::<T>()
        ),
    }
}

pub fn build_def_index(file: FileId, parse: &Parse, ast_id_map: &AstIdMap) -> DefIndex {
    let mut ctx = DefContext::new(file, ast_id_map);
    let root = parse.syntax();

    let file_scope = ctx.scopes.push(ScopeKind::File, None);
    collect_source_file(&mut ctx, &root, file_scope);
    assign_order_keys(&mut ctx, &root);

    let symbols = ctx.symbols.freeze();
    let scopes = ctx.scopes.freeze(&symbols);

    let def_entries = ctx.def_entries.freeze();
    let decl_site_to_def = ctx.decl_site_to_def;
    let mut defs_by_name: Vec<GlobalDefId> = (0..def_entries.len())
        .map(|i| GlobalDefId::new(file, i as u32))
        .collect();
    defs_by_name.sort_by(|a, b| {
        let ea = def_entries
            .get(a.ordinal() as usize)
            .map(|e| (&e.name, e.kind as u8, a.ordinal()));
        let eb = def_entries
            .get(b.ordinal() as usize)
            .map(|e| (&e.name, e.kind as u8, b.ordinal()));
        ea.cmp(&eb)
    });

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

    let mut enum_by_site = HashMap::with_capacity(ctx.enum_defs.len());
    for (i, def) in ctx.enum_defs.iter().enumerate() {
        enum_by_site.insert(def.enum_type_site, crate::enum_def::EnumDefIdx(i as u32));
    }
    let mut record_by_site = HashMap::with_capacity(ctx.record_defs.len());
    for (i, def) in ctx.record_defs.iter().enumerate() {
        record_by_site.insert(def.record_type_site, crate::record::RecordDefIdx(i as u32));
    }

    let mut def_index = DefIndex {
        file,
        symbols,
        scopes,
        def_entries,
        decl_site_to_def,
        defs_by_name: defs_by_name.into_boxed_slice(),
        use_sites: ctx.use_sites.into_boxed_slice(),
        imports: ctx.imports.into_boxed_slice(),
        local_decls: ctx.local_decls.into_boxed_slice(),
        name_site_to_symbol: ctx.name_site_to_symbol,
        name_site_to_init_expr: ctx.name_site_to_init_expr,
        enum_defs: ctx.enum_defs.into_boxed_slice(),
        enum_by_site,
        record_defs: ctx.record_defs.into_boxed_slice(),
        record_by_site,
        instance_decls: ctx.instance_decls.into_boxed_slice(),
        nettype_defs: ctx.nettype_defs.into_boxed_slice(),
        modports: crate::def_index::ModportStorage::empty(),
        export_decls: ctx.export_decls.into_boxed_slice(),
        foreach_var_defs: ctx.foreach_var_defs,
        scope_time_units: ctx.scope_time_units,
        scope_owners: ctx.scope_owners,
        owner_to_scope: ctx.owner_to_scope,
        diagnostics: diagnostics.into_boxed_slice(),
        internal_errors: ctx.internal_errors.into_boxed_slice(),
        design_elements: ctx.design_elements.into_boxed_slice(),
    };
    finalize_modport_defs(&mut def_index, ctx.raw_modport_defs);
    def_index
}

/// Convert raw modport entries to typed `ModportDefId`s and build
/// declaration-order storage with a position index for keyed lookup.
fn finalize_modport_defs(def_index: &mut DefIndex, raw_entries: Vec<RawModportEntry>) {
    let mut defs_ordered = Vec::new();
    let mut index = HashMap::new();
    let mut name_map = HashMap::new();
    for raw in raw_entries {
        if let Some(iface_id) = InterfaceDefId::try_from_def_index(def_index, raw.owner) {
            let typed_id = ModportDefId {
                owner: iface_id,
                ordinal: raw.ordinal,
            };
            let mut def = raw.def;
            def.id = typed_id;
            name_map.entry((iface_id, raw.name)).or_insert(typed_id);
            let pos = defs_ordered.len() as u32;
            defs_ordered.push(def);
            index.insert(typed_id, pos);
        }
    }
    def_index.modports =
        crate::def_index::ModportStorage::new(defs_ordered.into_boxed_slice(), index, name_map);
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

/// Two-slot lifetime environment for the builder.
///
/// `callable_default` controls the lifetime assigned to unqualified
/// task/function declarations. Container-level `automatic`/`static` keywords
/// change this slot (LRM 6.21, 12.7.1).
///
/// `local_default` controls the lifetime assigned to unqualified local variable
/// declarations inside a procedural context. Entering a callable body sets
/// this to the callable's resolved lifetime.
#[derive(Debug, Clone, Copy)]
pub(crate) struct LifetimeEnv {
    pub(crate) callable_default: Lifetime,
    pub(crate) local_default: Lifetime,
}

impl Default for LifetimeEnv {
    fn default() -> Self {
        Self {
            callable_default: Lifetime::Static,
            local_default: Lifetime::Static,
        }
    }
}

pub(crate) struct DefContext<'a> {
    pub(crate) ast_id_map: &'a AstIdMap,
    pub(crate) lifetime_env: LifetimeEnv,
    pub(crate) symbols: SymbolTableBuilder,
    pub(crate) scopes: ScopeTreeBuilder,
    pub(crate) def_entries: DefEntryBuilder,
    pub(crate) decl_site_to_def: HashMap<crate::Site, GlobalDefId>,
    pub(crate) name_site_to_symbol: HashMap<crate::Site, SymbolId>,
    pub(crate) name_site_to_init_expr: HashMap<crate::Site, Option<crate::Site>>,
    pub(crate) use_sites: Vec<UseSite>,
    pub(crate) imports: Vec<Import>,
    pub(crate) enum_defs: Vec<EnumDef>,
    pub(crate) record_defs: Vec<RecordDef>,
    pub(crate) instance_decls: Vec<InstanceDecl>,
    pub(crate) nettype_defs: Vec<NettypeDef>,
    pub(crate) raw_modport_defs: Vec<RawModportEntry>,
    pub(crate) export_decls: Vec<crate::def_index::ExportDecl>,
    pub(crate) foreach_var_defs: HashMap<crate::symbols::SymbolId, ForeachVarDef>,
    pub(crate) scope_time_units: HashMap<ScopeId, ScopeTimeUnits>,
    pub(crate) local_decls: Vec<LocalDecl>,
    pub(crate) local_decl_ordinals: HashMap<ScopeId, u32>,
    pub(crate) import_ordinals: HashMap<ScopeId, u32>,
    pub(crate) export_ordinals: HashMap<ScopeId, u32>,
    pub(crate) current_iface_def_id: Option<GlobalDefId>,
    pub(crate) modport_ordinal: u32,
    pub(crate) scope_owners: HashMap<ScopeId, crate::Site>,
    pub(crate) owner_to_scope: HashMap<crate::Site, ScopeId>,
    pub(crate) diagnostics: Vec<SemanticDiag>,
    pub(crate) internal_errors: Vec<(Option<crate::Site>, SmolStr)>,
    pub(crate) design_elements: Vec<DesignElement>,
}

impl<'a> DefContext<'a> {
    pub(crate) fn new(file: FileId, ast_id_map: &'a AstIdMap) -> Self {
        Self {
            ast_id_map,
            lifetime_env: LifetimeEnv::default(),
            symbols: SymbolTableBuilder::new(),
            scopes: ScopeTreeBuilder::new(),
            def_entries: DefEntryBuilder::new(file),
            decl_site_to_def: HashMap::new(),
            name_site_to_symbol: HashMap::new(),
            name_site_to_init_expr: HashMap::new(),
            use_sites: Vec::new(),
            imports: Vec::new(),
            enum_defs: Vec::new(),
            record_defs: Vec::new(),
            instance_decls: Vec::new(),
            nettype_defs: Vec::new(),
            raw_modport_defs: Vec::new(),
            export_decls: Vec::new(),
            foreach_var_defs: HashMap::new(),
            scope_time_units: HashMap::new(),
            local_decls: Vec::new(),
            local_decl_ordinals: HashMap::new(),
            import_ordinals: HashMap::new(),
            export_ordinals: HashMap::new(),
            current_iface_def_id: None,
            modport_ordinal: 0,
            scope_owners: HashMap::new(),
            owner_to_scope: HashMap::new(),
            diagnostics: Vec::new(),
            internal_errors: Vec::new(),
            design_elements: Vec::new(),
        }
    }

    pub(crate) fn with_callable_default(&mut self, lt: Lifetime, f: impl FnOnce(&mut Self)) {
        let prev = self.lifetime_env;
        self.lifetime_env.callable_default = lt;
        f(self);
        self.lifetime_env = prev;
    }

    pub(crate) fn with_local_default(&mut self, lt: Lifetime, f: impl FnOnce(&mut Self)) {
        let prev = self.lifetime_env;
        self.lifetime_env.local_default = lt;
        f(self);
        self.lifetime_env = prev;
    }

    pub(crate) fn with_iface_def_id(&mut self, id: Option<GlobalDefId>, f: impl FnOnce(&mut Self)) {
        let prev = self.current_iface_def_id;
        self.current_iface_def_id = id;
        self.modport_ordinal = 0;
        f(self);
        self.current_iface_def_id = prev;
    }

    /// Register a bidirectional scope ownership relation.
    ///
    /// Inserts both `scope -> owner` and `owner -> scope` atomically.
    /// Emits an internal error if either side was already registered to
    /// a different counterpart.
    pub(crate) fn register_scope_owner(&mut self, scope: ScopeId, owner: crate::Site) {
        if let Some(&existing_owner) = self.scope_owners.get(&scope)
            && existing_owner != owner
        {
            self.internal_errors.push((
                Some(owner),
                SmolStr::new(format!(
                    "scope {scope:?} already owned by {existing_owner:?}, cannot re-register to {owner:?}"
                )),
            ));
            return;
        }
        if let Some(&existing_scope) = self.owner_to_scope.get(&owner)
            && existing_scope != scope
        {
            self.internal_errors.push((
                Some(owner),
                SmolStr::new(format!(
                    "owner {owner:?} already owns {existing_scope:?}, cannot re-register to {scope:?}"
                )),
            ));
            return;
        }
        self.scope_owners.insert(scope, owner);
        self.owner_to_scope.insert(owner, scope);
    }

    pub(crate) fn emit_internal_error(&mut self, detail: &str, site: crate::Site) {
        self.internal_errors
            .push((Some(site), SmolStr::new(detail)));
    }

    pub(crate) fn emit_internal_error_unanchored(&mut self, detail: &str) {
        self.internal_errors.push((None, SmolStr::new(detail)));
    }

    pub(crate) fn register_binding(&mut self, sym_id: SymbolId) {
        let sym = self.symbols.get(sym_id);
        self.name_site_to_symbol.insert(sym.name_site, sym_id);
        let ns = sym.kind.namespace();
        let name = sym.name.clone();
        let name_span = sym.name_span;
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
            decl_site: sym.name_site,
            name_span,
            order_key: 0,
        });
    }

    pub(crate) fn push_symbol(&mut self, sym: Symbol) -> SymbolId {
        debug_assert!(
            match sym.kind {
                SymbolKind::Function => sym.decl_site.kind() == SyntaxKind::FunctionDecl,
                SymbolKind::Task => sym.decl_site.kind() == SyntaxKind::TaskDecl,
                SymbolKind::Variable => matches!(
                    sym.decl_site.kind(),
                    SyntaxKind::VarDecl | SyntaxKind::ForeachStmt
                ),
                SymbolKind::Net => sym.decl_site.kind() == SyntaxKind::NetDecl,
                SymbolKind::Parameter | SymbolKind::TypeParam => {
                    sym.decl_site.kind() == SyntaxKind::ParamDecl
                }
                SymbolKind::PortAnsi => sym.decl_site.kind() == SyntaxKind::Port,
                SymbolKind::PortTf => sym.decl_site.kind() == SyntaxKind::TfPortDecl,
                SymbolKind::Typedef => sym.decl_site.kind() == SyntaxKind::TypedefDecl,
                SymbolKind::Nettype => sym.decl_site.kind() == SyntaxKind::NettypeDecl,
                SymbolKind::EnumMember => sym.decl_site.kind() == SyntaxKind::EnumMember,
                SymbolKind::Modport => sym.decl_site.kind() == SyntaxKind::ModportItem,
                SymbolKind::Instance => sym.decl_site.kind() == SyntaxKind::ModuleInstantiation,
                SymbolKind::Module
                | SymbolKind::Package
                | SymbolKind::Interface
                | SymbolKind::Program
                | SymbolKind::Primitive
                | SymbolKind::Config => false,
            },
            "decl_site kind {:?} does not match SymbolKind::{:?}",
            sym.decl_site.kind(),
            sym.kind,
        );
        self.check_name_site_invariants(
            sym.kind,
            sym.name_site,
            sym.decl_site,
            sym.type_site,
            &sym.name,
        );
        let scope = sym.scope;
        let kind = sym.kind;
        let id = self.symbols.push(sym);
        self.scopes.add_binding(scope, id, kind);
        id
    }

    /// Push a definition-namespace entry (module, package, interface, etc.).
    pub(crate) fn push_def_entry(
        &mut self,
        decl_site: crate::Site,
        kind: DefinitionKind,
        name: SmolStr,
        name_span: DeclSpan,
        scope: DefScope,
    ) -> GlobalDefId {
        let entry = DefEntry {
            kind,
            name,
            decl_site,
            name_site: decl_site,
            name_span,
            scope,
        };
        let def_id = self.def_entries.push(entry);
        self.decl_site_to_def.insert(decl_site, def_id);
        def_id
    }

    fn check_name_site_invariants(
        &mut self,
        kind: SymbolKind,
        name_site: crate::Site,
        _decl_site: crate::Site,
        type_site: Option<crate::Site>,
        name: &str,
    ) {
        let expected_name_kind = match kind {
            SymbolKind::Variable
            | SymbolKind::Net
            | SymbolKind::Parameter
            | SymbolKind::TypeParam
            | SymbolKind::PortTf => SyntaxKind::Declarator,
            SymbolKind::PortAnsi => SyntaxKind::Port,
            SymbolKind::Instance => SyntaxKind::HierarchicalInstance,
            SymbolKind::Function => SyntaxKind::FunctionDecl,
            SymbolKind::Task => SyntaxKind::TaskDecl,
            SymbolKind::Typedef => SyntaxKind::TypedefDecl,
            SymbolKind::Nettype => SyntaxKind::NettypeDecl,
            SymbolKind::EnumMember => SyntaxKind::EnumMember,
            SymbolKind::Modport => SyntaxKind::ModportItem,
            SymbolKind::Module
            | SymbolKind::Package
            | SymbolKind::Interface
            | SymbolKind::Program
            | SymbolKind::Primitive
            | SymbolKind::Config => {
                self.emit_internal_error(
                    &format!(
                        "def-namespace kind {kind:?} '{name}' routed to push_symbol instead of push_def_entry",
                    ),
                    name_site,
                );
                return;
            }
        };
        if name_site.kind() != expected_name_kind {
            self.emit_internal_error(
                &format!(
                    "name_site kind {:?} (expected {:?}) for {:?} '{}'",
                    name_site.kind(),
                    expected_name_kind,
                    kind,
                    name,
                ),
                name_site,
            );
        }
        if let Some(ta) = type_site
            && ta.kind() != SyntaxKind::TypeSpec
        {
            self.emit_internal_error(
                &format!(
                    "type_site kind {:?} (expected TypeSpec) for {kind:?} '{name}'",
                    ta.kind(),
                ),
                name_site,
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
            _ => collect_file_item(ctx, &child, file_scope),
        }
    }
}

fn collect_file_item(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    collect_declarative_item(ctx, node, scope);
}

fn collect_declarative_item(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    match node.kind() {
        SyntaxKind::VarDecl => {
            let decl = expect_typed::<lyra_ast::VarDecl>(node);
            collect_var_decl(ctx, &decl, scope, DeclaratorContext::ContainerItem);
        }
        SyntaxKind::NetDecl => {
            let decl = expect_typed::<lyra_ast::NetDecl>(node);
            collect_net_decl(ctx, &decl, scope);
        }
        SyntaxKind::ParamDecl => {
            let decl = expect_typed::<lyra_ast::ParamDecl>(node);
            collect_param_decl(ctx, &decl, scope);
        }
        SyntaxKind::ImportDecl => {
            let decl = expect_typed::<lyra_ast::ImportDecl>(node);
            collect_import_decl(ctx, &decl, scope);
        }
        SyntaxKind::TypedefDecl => {
            let decl = expect_typed::<lyra_ast::TypedefDecl>(node);
            collect_typedef(ctx, &decl, scope);
        }
        SyntaxKind::NettypeDecl => {
            let decl = expect_typed::<lyra_ast::NettypeDecl>(node);
            collect_nettype_decl(ctx, &decl, scope);
        }
        SyntaxKind::FunctionDecl => {
            let decl = expect_typed::<lyra_ast::FunctionDecl>(node);
            collect_function_decl(ctx, &decl, scope);
        }
        SyntaxKind::TaskDecl => {
            let decl = expect_typed::<lyra_ast::TaskDecl>(node);
            collect_task_decl(ctx, &decl, scope);
        }
        SyntaxKind::TimeunitDecl => {
            let decl = expect_typed::<lyra_ast::TimeunitDecl>(node);
            collect_timeunit_decl(ctx, &decl, scope);
        }
        SyntaxKind::TimeprecisionDecl => {
            let decl = expect_typed::<lyra_ast::TimeprecisionDecl>(node);
            collect_timeprecision_decl(ctx, &decl, scope);
        }
        _ => {}
    }
}

fn container_callable_default(lifetime_tok: Option<&SyntaxToken>) -> Lifetime {
    match lifetime_tok.map(|t| t.kind()) {
        Some(SyntaxKind::AutomaticKw) => Lifetime::Automatic,
        _ => Lifetime::Static,
    }
}

/// Compute the design-element extent trimmed of leading trivia.
///
/// Rowan attaches leading whitespace/comments to the first token of a node,
/// so `node.text_range().start()` can precede the actual keyword. This trims
/// to the first non-trivia token's start.
fn design_element_extent(node: &SyntaxNode) -> TokenSpan {
    let range = node.text_range();
    let mut tok = node.first_token();
    while let Some(t) = tok {
        if !t.kind().is_trivia() {
            let trimmed = lyra_source::TextRange::new(t.text_range().start(), range.end());
            return TokenSpan::new(trimmed);
        }
        tok = t.next_token();
    }
    TokenSpan::new(range)
}

fn collect_module(ctx: &mut DefContext<'_>, node: &SyntaxNode, _file_scope: ScopeId) {
    let module_ast = lyra_ast::ModuleDecl::cast(node.clone());
    let Some(name_tok) = module_ast.as_ref().and_then(|m| m.name()) else {
        return;
    };
    let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error_unanchored(&format!(
            "erased_ast_id returned None for {:?} in collect_module",
            node.kind()
        ));
        return;
    };
    let name = semantic_spelling(&name_tok);
    let name_span = DeclSpan::new(name_tok.text_range());
    let module_scope = ctx.scopes.push(ScopeKind::Module, None);
    ctx.register_scope_owner(module_scope, decl_site);
    ctx.push_def_entry(
        decl_site,
        DefinitionKind::Module,
        name,
        name_span,
        DefScope::Owned(module_scope),
    );
    ctx.design_elements.push(DesignElement {
        kind: DefinitionKind::Module,
        extent: design_element_extent(node),
    });

    let container_lifetime = container_callable_default(
        module_ast
            .as_ref()
            .and_then(|m| m.lifetime_token())
            .as_ref(),
    );
    ctx.with_callable_default(container_lifetime, |ctx| {
        // Pass 1: header imports (scope facts collected first so ports see them)
        for child in node.children() {
            if let Some(decl) = lyra_ast::ImportDecl::cast(child) {
                collect_import_decl(ctx, &decl, module_scope);
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
    });
}

fn collect_package(ctx: &mut DefContext<'_>, node: &SyntaxNode, _file_scope: ScopeId) {
    let pkg_ast = lyra_ast::PackageDecl::cast(node.clone());
    let Some(name_tok) = pkg_ast.as_ref().and_then(|p| p.name()) else {
        return;
    };
    let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error_unanchored(&format!(
            "erased_ast_id returned None for {:?} in collect_package",
            node.kind()
        ));
        return;
    };
    let name = semantic_spelling(&name_tok);
    let name_span = DeclSpan::new(name_tok.text_range());
    let package_scope = ctx.scopes.push(ScopeKind::Package, None);
    ctx.register_scope_owner(package_scope, decl_site);
    ctx.push_def_entry(
        decl_site,
        DefinitionKind::Package,
        name,
        name_span,
        DefScope::Owned(package_scope),
    );
    ctx.design_elements.push(DesignElement {
        kind: DefinitionKind::Package,
        extent: design_element_extent(node),
    });

    let container_lifetime =
        container_callable_default(pkg_ast.as_ref().and_then(|p| p.lifetime_token()).as_ref());
    ctx.with_callable_default(container_lifetime, |ctx| {
        for child in node.children() {
            if child.kind() == SyntaxKind::PackageBody {
                collect_package_body(ctx, &child, package_scope);
            }
        }
    });
}

fn collect_interface(ctx: &mut DefContext<'_>, node: &SyntaxNode) {
    let iface_ast = lyra_ast::InterfaceDecl::cast(node.clone());
    let Some(name_tok) = iface_ast.as_ref().and_then(|i| i.name()) else {
        return;
    };
    let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error_unanchored(&format!(
            "erased_ast_id returned None for {:?} in collect_interface",
            node.kind()
        ));
        return;
    };
    let name = semantic_spelling(&name_tok);
    let name_span = DeclSpan::new(name_tok.text_range());
    let iface_scope = ctx.scopes.push(ScopeKind::Interface, None);
    ctx.register_scope_owner(iface_scope, decl_site);
    let def_id = ctx.push_def_entry(
        decl_site,
        DefinitionKind::Interface,
        name,
        name_span,
        DefScope::Owned(iface_scope),
    );
    ctx.design_elements.push(DesignElement {
        kind: DefinitionKind::Interface,
        extent: design_element_extent(node),
    });
    let container_lifetime =
        container_callable_default(iface_ast.as_ref().and_then(|i| i.lifetime_token()).as_ref());

    ctx.with_iface_def_id(Some(def_id), |ctx| {
        ctx.with_callable_default(container_lifetime, |ctx| {
            // Pass 1: header imports
            for child in node.children() {
                if let Some(decl) = lyra_ast::ImportDecl::cast(child) {
                    collect_import_decl(ctx, &decl, iface_scope);
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
        });
    });
}

fn collect_program(ctx: &mut DefContext<'_>, node: &SyntaxNode) {
    let prog_ast = lyra_ast::ProgramDecl::cast(node.clone());
    let Some(name_tok) = prog_ast.as_ref().and_then(|p| p.name()) else {
        return;
    };
    let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error_unanchored(&format!(
            "erased_ast_id returned None for {:?} in collect_program",
            node.kind()
        ));
        return;
    };
    let name = semantic_spelling(&name_tok);
    let name_span = DeclSpan::new(name_tok.text_range());
    let prog_scope = ctx.scopes.push(ScopeKind::Program, None);
    ctx.register_scope_owner(prog_scope, decl_site);
    ctx.push_def_entry(
        decl_site,
        DefinitionKind::Program,
        name,
        name_span,
        DefScope::Owned(prog_scope),
    );
    ctx.design_elements.push(DesignElement {
        kind: DefinitionKind::Program,
        extent: design_element_extent(node),
    });

    let container_lifetime =
        container_callable_default(prog_ast.as_ref().and_then(|p| p.lifetime_token()).as_ref());
    ctx.with_callable_default(container_lifetime, |ctx| {
        // Pass 1: header imports
        for child in node.children() {
            if let Some(decl) = lyra_ast::ImportDecl::cast(child) {
                collect_import_decl(ctx, &decl, prog_scope);
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
    });
}

fn collect_primitive(ctx: &mut DefContext<'_>, node: &SyntaxNode) {
    let Some(name_tok) = lyra_ast::PrimitiveDecl::cast(node.clone()).and_then(|p| p.name()) else {
        return;
    };
    let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error_unanchored(&format!(
            "erased_ast_id returned None for {:?} in collect_primitive",
            node.kind()
        ));
        return;
    };
    let name = semantic_spelling(&name_tok);
    let name_span = DeclSpan::new(name_tok.text_range());
    let prim_scope = ctx.scopes.push(ScopeKind::Module, None);
    ctx.register_scope_owner(prim_scope, decl_site);
    ctx.push_def_entry(
        decl_site,
        DefinitionKind::Primitive,
        name,
        name_span,
        DefScope::Owned(prim_scope),
    );
    ctx.design_elements.push(DesignElement {
        kind: DefinitionKind::Primitive,
        extent: design_element_extent(node),
    });
}

fn collect_config(ctx: &mut DefContext<'_>, node: &SyntaxNode) {
    let Some(name_tok) = lyra_ast::ConfigDecl::cast(node.clone()).and_then(|c| c.name()) else {
        return;
    };
    let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error_unanchored(&format!(
            "erased_ast_id returned None for {:?} in collect_config",
            node.kind()
        ));
        return;
    };
    let name = semantic_spelling(&name_tok);
    let name_span = DeclSpan::new(name_tok.text_range());
    let cfg_scope = ctx.scopes.push(ScopeKind::Module, None);
    ctx.register_scope_owner(cfg_scope, decl_site);
    ctx.push_def_entry(
        decl_site,
        DefinitionKind::Config,
        name,
        name_span,
        DefScope::Owned(cfg_scope),
    );
    ctx.design_elements.push(DesignElement {
        kind: DefinitionKind::Config,
        extent: design_element_extent(node),
    });
}

fn collect_package_body(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        if let Some(decl) = ExportDecl::cast(child.clone()) {
            collect_export_decl(ctx, &decl, scope);
        } else {
            collect_declarative_item(ctx, &child, scope);
        }
    }
}

fn collect_param_port_list(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        if let Some(decl) = lyra_ast::ParamDecl::cast(child) {
            collect_param_decl(ctx, &decl, scope);
        }
    }
}

fn collect_port_list(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        if child.kind() == SyntaxKind::Port {
            let Some(port) = Port::cast(child.clone()) else {
                continue;
            };
            let Some(name_tok) = port.name() else {
                continue;
            };
            let Some(decl_site) = ctx.ast_id_map.erased_ast_id(&child) else {
                ctx.emit_internal_error_unanchored(&format!(
                    "erased_ast_id returned None for {:?} in collect_port_list",
                    child.kind()
                ));
                continue;
            };
            let name_span = DeclSpan::new(name_tok.text_range());
            let port_type_site = port
                .type_spec()
                .and_then(|ts| ctx.ast_id_map.erased_ast_id(ts.syntax()));
            let sym_id = ctx.push_symbol(Symbol {
                name: semantic_spelling(&name_tok),
                kind: SymbolKind::PortAnsi,
                constness: crate::symbols::Constness::Mutable,
                lifetime: Lifetime::Static,
                decl_site,
                name_site: decl_site,
                type_site: port_type_site,
                name_span,
                scope,
                origin: SymbolOrigin::TypeSpec,
            });
            ctx.register_binding(sym_id);
            // Collect type-spec name refs for port typedef resolution
            // and unpacked dim refs for typedef-as-assoc-key resolution
            for port_child in child.children() {
                if let Some(ts) = TypeSpec::cast(port_child.clone()) {
                    collect_type_spec_refs(ctx, &ts, scope);
                } else if port_child.kind() == SyntaxKind::UnpackedDimension {
                    crate::builder_types::collect_name_refs(ctx, &port_child, scope, None);
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
            let decl = expect_typed::<lyra_ast::NetDecl>(node);
            collect_net_decl(ctx, &decl, scope);
        }
        SyntaxKind::VarDecl => {
            let decl = expect_typed::<lyra_ast::VarDecl>(node);
            collect_var_decl(ctx, &decl, scope, DeclaratorContext::ContainerItem);
        }
        SyntaxKind::ParamDecl => {
            let decl = expect_typed::<lyra_ast::ParamDecl>(node);
            collect_param_decl(ctx, &decl, scope);
        }
        SyntaxKind::ContinuousAssign => {
            let ca = expect_typed::<lyra_ast::ContinuousAssign>(node);
            if let Some(lhs) = ca.lhs() {
                collect_name_refs(
                    ctx,
                    lhs.syntax(),
                    scope,
                    Some(ImplicitNetSiteKind::ContinuousAssignLhs),
                );
            }
            if let Some(rhs) = ca.rhs() {
                collect_name_refs(ctx, rhs.syntax(), scope, None);
            }
        }
        SyntaxKind::ImportDecl => {
            let decl = expect_typed::<lyra_ast::ImportDecl>(node);
            collect_import_decl(ctx, &decl, scope);
        }
        SyntaxKind::ModuleInstantiation => {
            let inst = expect_typed::<lyra_ast::ModuleInstantiation>(node);
            collect_module_instantiation(ctx, &inst, scope);
        }
        SyntaxKind::TypedefDecl => {
            let decl = expect_typed::<lyra_ast::TypedefDecl>(node);
            collect_typedef(ctx, &decl, scope);
        }
        SyntaxKind::NettypeDecl => {
            let decl = expect_typed::<lyra_ast::NettypeDecl>(node);
            collect_nettype_decl(ctx, &decl, scope);
        }
        SyntaxKind::AlwaysBlock | SyntaxKind::InitialBlock => {
            crate::builder_stmt::collect_procedural_block(ctx, node, scope);
        }
        SyntaxKind::FunctionDecl => {
            let decl = expect_typed::<lyra_ast::FunctionDecl>(node);
            collect_function_decl(ctx, &decl, scope);
        }
        SyntaxKind::TaskDecl => {
            let decl = expect_typed::<lyra_ast::TaskDecl>(node);
            collect_task_decl(ctx, &decl, scope);
        }
        SyntaxKind::ModportDecl => {
            let decl = expect_typed::<lyra_ast::ModportDecl>(node);
            collect_modport_decl(ctx, &decl, scope);
        }
        SyntaxKind::TimeunitDecl => {
            let decl = expect_typed::<lyra_ast::TimeunitDecl>(node);
            collect_timeunit_decl(ctx, &decl, scope);
        }
        SyntaxKind::TimeprecisionDecl => {
            let decl = expect_typed::<lyra_ast::TimeprecisionDecl>(node);
            collect_timeprecision_decl(ctx, &decl, scope);
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

pub(crate) use crate::builder_items::{DeclaratorContext, is_expression_kind};
use crate::builder_items::{
    collect_export_decl, collect_function_decl, collect_import_decl, collect_modport_decl,
    collect_module_instantiation, collect_net_decl, collect_param_decl, collect_task_decl,
    collect_timeprecision_decl, collect_timeunit_decl, collect_var_decl,
};

#[cfg(test)]
#[path = "builder_tests.rs"]
mod tests;
