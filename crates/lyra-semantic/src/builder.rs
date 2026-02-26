use std::collections::HashMap;

use lyra_ast::{AstIdMap, AstNode, ExportDecl, Port, TypeSpec};
use lyra_lexer::SyntaxKind;
use lyra_parser::{Parse, SyntaxNode};
use lyra_source::{FileId, NameSpan};
use smol_str::SmolStr;

use crate::builder_order::{assign_order_keys, detect_duplicates};
use crate::builder_types::{collect_name_refs, collect_type_spec_refs, collect_typedef};

use crate::def_entry::{DefEntry, DefEntryBuilder, DefScope};
use crate::def_index::{DefIndex, Import, LocalDecl, LocalDeclId, UseSite};
use crate::diagnostic::SemanticDiag;
use crate::enum_def::EnumDef;
use crate::global_index::DefinitionKind;
use crate::instance_decl::InstanceDecl;
use crate::interface_id::InterfaceDefId;
use crate::modport_def::{ModportDef, ModportDefId};
use crate::record::{RecordDef, SymbolOrigin};
use crate::scopes::{ScopeId, ScopeKind, ScopeTreeBuilder};
use crate::symbols::{GlobalDefId, Symbol, SymbolId, SymbolKind, SymbolTableBuilder};

pub fn build_def_index(file: FileId, parse: &Parse, ast_id_map: &AstIdMap) -> DefIndex {
    let mut ctx = DefContext::new(file, ast_id_map);
    let root = parse.syntax();

    let file_scope = ctx.scopes.push(ScopeKind::File, None);
    collect_source_file(&mut ctx, &root, file_scope);
    assign_order_keys(&mut ctx, &root);

    let symbols = ctx.symbols.freeze();
    let scopes = ctx.scopes.freeze(&symbols);

    let def_entries = ctx.def_entries.freeze();
    let name_site_to_def = ctx.name_site_to_def;
    let mut defs_by_name: Vec<GlobalDefId> = name_site_to_def.values().copied().collect();
    defs_by_name.sort_by(|a, b| {
        let ea = def_entries
            .iter()
            .find(|e| e.decl_site == a.ast_id())
            .map(|e| (&e.name, e.kind as u8, e.decl_site));
        let eb = def_entries
            .iter()
            .find(|e| e.decl_site == b.ast_id())
            .map(|e| (&e.name, e.kind as u8, e.decl_site));
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

    // Finalize modport defs: convert raw GlobalDefId owners to InterfaceDefId
    let mut modport_defs = HashMap::new();
    let mut modport_name_map = HashMap::new();
    let def_index_partial = DefIndex {
        file,
        symbols,
        scopes,
        def_entries,
        name_site_to_def,
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
    pub(crate) ast_id_map: &'a AstIdMap,
    pub(crate) symbols: SymbolTableBuilder,
    pub(crate) scopes: ScopeTreeBuilder,
    pub(crate) def_entries: DefEntryBuilder,
    pub(crate) name_site_to_def: HashMap<crate::Site, GlobalDefId>,
    pub(crate) name_site_to_symbol: HashMap<crate::Site, SymbolId>,
    pub(crate) name_site_to_init_expr: HashMap<crate::Site, Option<crate::Site>>,
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
    pub(crate) current_iface_def_id: Option<GlobalDefId>,
    pub(crate) modport_ordinal: u32,
    pub(crate) diagnostics: Vec<SemanticDiag>,
    pub(crate) internal_errors: Vec<(Option<crate::Site>, SmolStr)>,
}

impl<'a> DefContext<'a> {
    pub(crate) fn new(_file: FileId, ast_id_map: &'a AstIdMap) -> Self {
        Self {
            ast_id_map,
            symbols: SymbolTableBuilder::new(),
            scopes: ScopeTreeBuilder::new(),
            def_entries: DefEntryBuilder::new(),
            name_site_to_def: HashMap::new(),
            name_site_to_symbol: HashMap::new(),
            name_site_to_init_expr: HashMap::new(),
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
                SymbolKind::Variable => sym.decl_site.kind() == SyntaxKind::VarDecl,
                SymbolKind::Net => sym.decl_site.kind() == SyntaxKind::NetDecl,
                SymbolKind::Parameter => sym.decl_site.kind() == SyntaxKind::ParamDecl,
                SymbolKind::PortAnsi => sym.decl_site.kind() == SyntaxKind::Port,
                SymbolKind::PortTf => sym.decl_site.kind() == SyntaxKind::TfPortDecl,
                SymbolKind::Typedef => sym.decl_site.kind() == SyntaxKind::TypedefDecl,
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
        name_span: NameSpan,
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
        let def_id = self.def_entries.push(decl_site, entry);
        self.name_site_to_def.insert(decl_site, def_id);
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
            SymbolKind::Variable | SymbolKind::Net | SymbolKind::Parameter | SymbolKind::PortTf => {
                SyntaxKind::Declarator
            }
            SymbolKind::PortAnsi => SyntaxKind::Port,
            SymbolKind::Instance => SyntaxKind::HierarchicalInstance,
            SymbolKind::Function => SyntaxKind::FunctionDecl,
            SymbolKind::Task => SyntaxKind::TaskDecl,
            SymbolKind::Typedef => SyntaxKind::TypedefDecl,
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
            _ => {}
        }
    }
}

fn collect_module(ctx: &mut DefContext<'_>, node: &SyntaxNode, _file_scope: ScopeId) {
    let Some(name_tok) = lyra_ast::ModuleDecl::cast(node.clone()).and_then(|m| m.name()) else {
        return;
    };
    let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error_unanchored(&format!(
            "erased_ast_id returned None for {:?} in collect_module",
            node.kind()
        ));
        return;
    };
    let name = SmolStr::new(name_tok.text());
    let name_span = NameSpan::new(name_tok.text_range());
    let module_scope = ctx.scopes.push(ScopeKind::Module, None);
    ctx.push_def_entry(
        decl_site,
        DefinitionKind::Module,
        name,
        name_span,
        DefScope::Owned(module_scope),
    );

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

fn collect_package(ctx: &mut DefContext<'_>, node: &SyntaxNode, _file_scope: ScopeId) {
    let Some(name_tok) = lyra_ast::PackageDecl::cast(node.clone()).and_then(|p| p.name()) else {
        return;
    };
    let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error_unanchored(&format!(
            "erased_ast_id returned None for {:?} in collect_package",
            node.kind()
        ));
        return;
    };
    let name = SmolStr::new(name_tok.text());
    let name_span = NameSpan::new(name_tok.text_range());
    let package_scope = ctx.scopes.push(ScopeKind::Package, None);
    ctx.push_def_entry(
        decl_site,
        DefinitionKind::Package,
        name,
        name_span,
        DefScope::Owned(package_scope),
    );

    for child in node.children() {
        if child.kind() == SyntaxKind::PackageBody {
            collect_package_body(ctx, &child, package_scope);
        }
    }
}

fn collect_interface(ctx: &mut DefContext<'_>, node: &SyntaxNode) {
    let Some(name_tok) = lyra_ast::InterfaceDecl::cast(node.clone()).and_then(|i| i.name()) else {
        return;
    };
    let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error_unanchored(&format!(
            "erased_ast_id returned None for {:?} in collect_interface",
            node.kind()
        ));
        return;
    };
    let name = SmolStr::new(name_tok.text());
    let name_span = NameSpan::new(name_tok.text_range());
    let iface_scope = ctx.scopes.push(ScopeKind::Interface, None);
    let def_id = ctx.push_def_entry(
        decl_site,
        DefinitionKind::Interface,
        name,
        name_span,
        DefScope::Owned(iface_scope),
    );
    let iface_def_id = Some(def_id);

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

fn collect_program(ctx: &mut DefContext<'_>, node: &SyntaxNode) {
    let Some(name_tok) = lyra_ast::ProgramDecl::cast(node.clone()).and_then(|p| p.name()) else {
        return;
    };
    let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error_unanchored(&format!(
            "erased_ast_id returned None for {:?} in collect_program",
            node.kind()
        ));
        return;
    };
    let name = SmolStr::new(name_tok.text());
    let name_span = NameSpan::new(name_tok.text_range());
    let prog_scope = ctx.scopes.push(ScopeKind::Program, None);
    ctx.push_def_entry(
        decl_site,
        DefinitionKind::Program,
        name,
        name_span,
        DefScope::Owned(prog_scope),
    );

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
    let name = SmolStr::new(name_tok.text());
    let name_span = NameSpan::new(name_tok.text_range());
    let prim_scope = ctx.scopes.push(ScopeKind::Module, None);
    ctx.push_def_entry(
        decl_site,
        DefinitionKind::Primitive,
        name,
        name_span,
        DefScope::Owned(prim_scope),
    );
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
    let name = SmolStr::new(name_tok.text());
    let name_span = NameSpan::new(name_tok.text_range());
    let cfg_scope = ctx.scopes.push(ScopeKind::Module, None);
    ctx.push_def_entry(
        decl_site,
        DefinitionKind::Config,
        name,
        name_span,
        DefScope::Owned(cfg_scope),
    );
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
            let name_span = NameSpan::new(name_tok.text_range());
            let port_type_site = port
                .type_spec()
                .and_then(|ts| ctx.ast_id_map.erased_ast_id(ts.syntax()));
            let sym_id = ctx.push_symbol(Symbol {
                name: SmolStr::new(name_tok.text()),
                kind: SymbolKind::PortAnsi,
                decl_site,
                name_site: decl_site,
                type_site: port_type_site,
                name_span,
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

use crate::builder_items::{
    collect_callable_decl, collect_export_decl, collect_import_decl, collect_modport_decl,
    collect_module_instantiation, collect_param_decl,
};
pub(crate) use crate::builder_items::{collect_declarators, is_expression_kind};

#[cfg(test)]
#[path = "builder_tests.rs"]
mod tests;
