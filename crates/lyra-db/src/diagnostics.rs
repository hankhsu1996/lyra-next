use lyra_ast::{AstIdMap, AstNode, Expr, HasSyntax, SourceFile as AstSourceFile};
use lyra_semantic::coerce::IntegralCtx;
use lyra_semantic::type_check::TypeCheckCtx;
use lyra_semantic::type_infer::ExprType;
use lyra_semantic::types::SymbolType;

use crate::checks_index::{CheckKind, checks_index};
use crate::enum_queries::{EnumRef, enum_sem, enum_variant_index};
use crate::expr_queries::{ExprRef, IntegralCtxKey};
use crate::facts::modport::field_access_facts;
use crate::pipeline::ast_id_map as query_ast_id_map;
use crate::pipeline::{ast_id_map, parse_file, preprocess_file};
use crate::record_queries::{RecordRef, record_diagnostics};
use crate::semantic::{
    def_index_file, global_def_index, import_conflicts_file, resolve_core_file, resolve_index_file,
};
use crate::type_queries::{SymbolRef, type_of_symbol};
use crate::{CompilationUnit, SourceFile, source_file_by_id};

/// Convert parse, preprocess, and semantic errors into structured diagnostics (Salsa-cached).
#[salsa::tracked(return_ref)]
pub fn file_diagnostics(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> Vec<lyra_diag::Diagnostic> {
    let pp = preprocess_file(db, file);
    let parse = parse_file(db, file);
    let def = def_index_file(db, file);
    let resolve = resolve_index_file(db, file, unit);
    let conflicts = import_conflicts_file(db, file, unit);
    let core = resolve_core_file(db, file, unit);
    let mut diags =
        crate::lower_diag::lower_file_diagnostics(file.file_id(db), pp, parse, def, resolve);
    diags.extend(crate::lower_diag::lower_import_conflicts(
        file.file_id(db),
        pp,
        def,
        conflicts,
    ));
    diags.extend(crate::lower_diag::lower_wildcard_local_conflicts(
        file.file_id(db),
        pp,
        def,
        core,
    ));
    diags.extend(type_diagnostics(db, file, unit).iter().cloned());

    // Jump statement legality diagnostics (LRM 12.8)
    {
        let jump_index = crate::jump_check::jump_check_index(db, file);
        crate::lower_diag::lower_jump_check_items(
            file.file_id(db),
            &pp.source_map,
            &jump_index.items,
            &mut diags,
        );
    }

    // Foreach loop legality diagnostics (LRM 12.7.3)
    {
        let foreach_index = crate::foreach_check::foreach_check_index(db, file, unit);
        crate::lower_diag::lower_foreach_check_items(
            file.file_id(db),
            &pp.source_map,
            &foreach_index.items,
            &mut diags,
        );
    }

    // Enum base type diagnostics
    let file_id = file.file_id(db);
    for enum_def in &*def.enum_defs {
        let enum_id = lyra_semantic::enum_def::EnumId::new(enum_def.enum_type_site);
        let eref = EnumRef::new(db, unit, enum_id);
        let sem = enum_sem(db, eref);
        for diag in &*sem.diags {
            let chosen = crate::lower_diag::choose_best_diag_span(diag.primary, diag.label);
            let (primary_span, _) =
                crate::lower_diag::map_span_or_fallback(file_id, &pp.source_map, chosen);
            diags.push(crate::lower_diag::lower_semantic_diag(
                diag,
                primary_span,
                &pp.source_map,
            ));
        }
        if !sem.value_diags.is_empty() {
            let ast_map = query_ast_id_map(db, file);
            for vd in &*sem.value_diags {
                if let Some(d) = crate::lower_type_check::lower_enum_value_diag(
                    vd,
                    file_id,
                    ast_map,
                    &pp.source_map,
                ) {
                    diags.push(d);
                }
            }
        }
    }

    // Enum variant expansion diagnostics (range bounds, collisions)
    let ev_idx = enum_variant_index(db, file, unit);
    for diag in &*ev_idx.diagnostics {
        let chosen = crate::lower_diag::choose_best_diag_span(diag.primary, diag.label);
        let (primary_span, _) =
            crate::lower_diag::map_span_or_fallback(file_id, &pp.source_map, chosen);
        diags.push(crate::lower_diag::lower_semantic_diag(
            diag,
            primary_span,
            &pp.source_map,
        ));
    }

    // Record diagnostics (type resolution errors + packed union width)
    for record_def in &*def.record_defs {
        let record_id = lyra_semantic::record::RecordId::new(record_def.record_type_site);
        let rref = RecordRef::new(db, unit, record_id);
        diags.extend(record_diagnostics(db, rref).iter().cloned());
    }

    // Per-symbol diagnostics: internal errors and type-param-no-default.
    collect_symbol_diagnostics(db, file, unit, def, pp, &mut diags);

    diags
}

/// Collect per-symbol diagnostics: type-extraction internal errors and
/// type-parameter-no-default (demand-driven via the type system).
fn collect_symbol_diagnostics(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
    def: &lyra_semantic::def_index::DefIndex,
    pp: &lyra_preprocess::PreprocOutput,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let file_id = file.file_id(db);
    for (sym_id, sym) in def.symbols.iter() {
        let gsym = lyra_semantic::symbols::GlobalSymbolId {
            file: file_id,
            local: sym_id,
        };
        let sym_ref = SymbolRef::new(db, unit, gsym);
        let errors = crate::type_queries::type_of_symbol_internal_errors(db, sym_ref);
        for fact in errors {
            if let Some(span) = pp.source_map.map_span(fact.site.text_range()) {
                diags.push(crate::lower_diag::internal_error_diag(&fact.detail, span));
            }
        }
        // Type parameter without a default type: check via the type system
        // result rather than raw symbol inspection so the diagnostic is
        // demand-driven and will naturally go away when type parameter
        // overrides are implemented. Only check TypeParam symbols to avoid
        // emitting duplicates for variables that reference the type param.
        if sym.kind == lyra_semantic::symbols::SymbolKind::TypeParam
            && matches!(
                type_of_symbol(db, sym_ref),
                SymbolType::Error(lyra_semantic::types::SymbolTypeError::TypeParamNoDefault)
            )
        {
            let range = sym.name_span.text_range();
            if let Some(span) = pp.source_map.map_span(range) {
                diags.push(
                    lyra_diag::Diagnostic::new(
                        lyra_diag::Severity::Error,
                        lyra_diag::DiagnosticCode::TYPE_PARAM_NO_DEFAULT,
                        lyra_diag::Message::new(
                            lyra_diag::MessageId::TypeParamNoDefault,
                            vec![lyra_diag::Arg::Name(sym.name.clone())],
                        ),
                    )
                    .with_label(lyra_diag::Label {
                        kind: lyra_diag::LabelKind::Primary,
                        span,
                        message: lyra_diag::Message::new(
                            lyra_diag::MessageId::TypeParamNoDefault,
                            vec![lyra_diag::Arg::Name(sym.name.clone())],
                        ),
                    }),
                );
            }
        }
    }
}

/// Per-file type-check diagnostics (Salsa-cached).
///
/// Iterates the `ChecksIndex` for this file and dispatches each entry
/// to the appropriate pure `check_*` function in `lyra-semantic`.
#[salsa::tracked(return_ref)]
pub fn type_diagnostics(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> Vec<lyra_diag::Diagnostic> {
    let parse = parse_file(db, file);
    let map = ast_id_map(db, file);
    let pp = preprocess_file(db, file);
    let index = checks_index(db, file);
    let field_access = field_access_facts(db, file, unit);

    let ctx = DbTypeCheckCtx {
        db,
        unit,
        source_file: file,
        ast_id_map: map,
    };

    let root = parse.syntax();
    let mut items = Vec::new();
    for entry in &*index.entries {
        let Some(node) = map.get_node(&root, entry.site) else {
            continue;
        };
        dispatch_check_entry(entry, node, map, field_access, &ctx, &mut items);
    }

    check_drive_strength_in_file(&root, map, &mut items);

    let mut seen = std::collections::HashSet::new();
    let mut diags = Vec::new();
    for item in &items {
        crate::lower_type_check::lower_type_check_item(
            db,
            unit,
            item,
            &pp.source_map,
            &mut seen,
            &mut diags,
        );
    }
    diags
}

fn dispatch_check_entry(
    entry: &crate::checks_index::CheckEntry,
    node: lyra_parser::SyntaxNode,
    map: &lyra_ast::AstIdMap,
    field_access: &lyra_semantic::modport_facts::FieldAccessFacts,
    ctx: &DbTypeCheckCtx<'_>,
    items: &mut Vec<lyra_semantic::type_check::TypeCheckItem>,
) {
    let fallback = entry.site;
    match entry.kind {
        CheckKind::ContinuousAssign => {
            if let Some(ca) = lyra_ast::ContinuousAssign::cast(node) {
                lyra_semantic::type_check::check_continuous_assign(&ca, ctx, fallback, items);
                if let Some(ds) = ca.drive_strength() {
                    lyra_semantic::type_check::check_drive_strength_semantics(
                        &ds, map, fallback, items,
                    );
                }
            }
        }
        CheckKind::AssignStmt => {
            if let Some(assign) = lyra_ast::AssignStmt::cast(node) {
                lyra_semantic::type_check::check_assign_stmt(&assign, ctx, fallback, items);
            }
        }
        CheckKind::VarDecl => {
            if let Some(vd) = lyra_ast::VarDecl::cast(node) {
                lyra_semantic::type_check::check_var_decl(&vd, ctx, fallback, items);
            }
        }
        CheckKind::SystemTfCall => {
            if let Some(stf) = lyra_ast::SystemTfCall::cast(node) {
                lyra_semantic::type_check::check_system_call(&stf, ctx, fallback, items);
            }
        }
        CheckKind::FieldExpr => {
            if let Some(field) = lyra_ast::FieldExpr::cast(node) {
                lyra_semantic::type_check::check_field_direction(
                    &field,
                    ctx,
                    field_access,
                    entry.access,
                    items,
                );
                lyra_semantic::type_check::check_field_modport_restriction(&field, ctx, items);
            }
        }
        CheckKind::CastExpr => {
            if let Some(cast) = lyra_ast::CastExpr::cast(node) {
                lyra_semantic::type_check::check_cast_expr(&cast, ctx, fallback, items);
            }
        }
        CheckKind::StreamOperandItem => {
            if let Some(soi) = lyra_ast::StreamOperandItem::cast(node) {
                lyra_semantic::type_check::check_stream_operand(
                    &soi,
                    ctx,
                    fallback,
                    entry.access,
                    items,
                );
            }
        }
        CheckKind::StreamExpr => {
            if let Some(stream) = lyra_ast::StreamExpr::cast(node) {
                lyra_semantic::type_check::check_stream_slice_size_const(&stream, ctx, items);
            }
        }
        CheckKind::CallExpr => {
            if let Some(call) = lyra_ast::CallExpr::cast(node) {
                lyra_semantic::type_check::check_method_call(&call, ctx, items);
            }
        }
        CheckKind::NetDecl => {
            if let Some(nd) = lyra_ast::NetDecl::cast(node) {
                lyra_semantic::type_check::check_net_decl(&nd, ctx, fallback, items);
            }
        }
        CheckKind::TypedefDecl => {
            if let Some(td) = lyra_ast::TypedefDecl::cast(node) {
                lyra_semantic::type_check::check_typedef_decl(&td, ctx, fallback, items);
            }
        }
        CheckKind::PortDecl => {
            if let Some(port) = lyra_ast::Port::cast(node) {
                lyra_semantic::type_check::check_port_decl(&port, ctx, fallback, items);
            }
        }
    }
}

fn check_drive_strength_in_net_decls(
    net_decls: impl IntoIterator<Item = lyra_ast::NetDecl>,
    map: &lyra_ast::AstIdMap,
    items: &mut Vec<lyra_semantic::type_check::TypeCheckItem>,
) {
    for nd in net_decls {
        let Some(fallback) = map.id_of(&nd) else {
            continue;
        };
        if let Some(ts) = nd.type_spec()
            && let Some(ds) = ts.drive_strength()
        {
            lyra_semantic::type_check::check_drive_strength_semantics(&ds, map, fallback, items);
        }
    }
}

/// Drive strength legality on net declarations (LRM 6.3.2).
///
/// Walks modules, interfaces, programs, and packages present in this file.
fn check_drive_strength_in_file(
    root: &lyra_parser::SyntaxNode,
    map: &lyra_ast::AstIdMap,
    items: &mut Vec<lyra_semantic::type_check::TypeCheckItem>,
) {
    let Some(sf) = AstSourceFile::cast(root.clone()) else {
        return;
    };
    for module in sf.modules() {
        if let Some(body) = module.body() {
            check_drive_strength_in_net_decls(body.net_decls(), map, items);
        }
    }
    for iface in sf.interfaces() {
        if let Some(body) = iface.body() {
            check_drive_strength_in_net_decls(body.net_decls(), map, items);
        }
    }
    for prog in sf.programs() {
        if let Some(body) = prog.body() {
            check_drive_strength_in_net_decls(body.net_decls(), map, items);
        }
    }
    for pkg in sf.packages() {
        if let Some(body) = pkg.body() {
            check_drive_strength_in_net_decls(body.net_decls(), map, items);
        }
    }
}

struct DbTypeCheckCtx<'a> {
    db: &'a dyn salsa::Database,
    unit: CompilationUnit,
    source_file: SourceFile,
    ast_id_map: &'a lyra_ast::AstIdMap,
}

impl DbTypeCheckCtx<'_> {
    fn enum_bits_cb(&self) -> impl Fn(&lyra_semantic::enum_def::EnumId) -> Option<u32> + '_ {
        |enum_id| {
            let eref = EnumRef::new(self.db, self.unit, *enum_id);
            let sem = enum_sem(self.db, eref);
            sem.base_int.and_then(|bv| match bv.width {
                lyra_semantic::type_infer::BitWidth::Known(w) => Some(w),
                _ => None,
            })
        }
    }
}

impl TypeCheckCtx for DbTypeCheckCtx<'_> {
    fn file_id(&self) -> lyra_source::FileId {
        self.source_file.file_id(self.db)
    }

    fn ast_id_map(&self) -> &AstIdMap {
        self.ast_id_map
    }

    fn expr_type(&self, expr: &Expr) -> ExprType {
        let Some(ast_id) = self.ast_id_map.erased_ast_id(expr.syntax()) else {
            return ExprType::error(lyra_semantic::type_infer::ExprTypeErrorKind::Unresolved);
        };
        let expr_ref = ExprRef::new(self.db, self.unit, ast_id);
        crate::expr_queries::type_of_expr(self.db, expr_ref)
    }

    fn expr_type_in_ctx(&self, expr: &Expr, ctx: &IntegralCtx) -> ExprType {
        let Some(ast_id) = self.ast_id_map.erased_ast_id(expr.syntax()) else {
            return ExprType::error(lyra_semantic::type_infer::ExprTypeErrorKind::Unresolved);
        };
        let expr_ref = ExprRef::new(self.db, self.unit, ast_id);
        let ctx_key = IntegralCtxKey::new(self.db, ctx.width, ctx.signed, ctx.four_state);
        crate::expr_queries::type_of_expr_in_ctx(self.db, expr_ref, ctx_key)
    }

    fn expr_type_stmt(&self, expr: &Expr) -> ExprType {
        let Some(ast_id) = self.ast_id_map.erased_ast_id(expr.syntax()) else {
            return ExprType::error(lyra_semantic::type_infer::ExprTypeErrorKind::Unresolved);
        };
        let expr_ref = ExprRef::new(self.db, self.unit, ast_id);
        crate::expr_queries::type_of_expr_stmt(self.db, expr_ref)
    }

    fn expr_type_with_expected(
        &self,
        expr: &Expr,
        expected: &lyra_semantic::types::Ty,
    ) -> ExprType {
        crate::expr_queries::infer_expr_with_expected_inline(
            self.db,
            self.unit,
            self.source_file,
            self.ast_id_map,
            expr,
            expected,
        )
    }

    fn symbol_type_of_declarator(&self, declarator: &lyra_ast::Declarator) -> Option<SymbolType> {
        let ast_id = self.ast_id_map.id_of(declarator)?;
        let def = def_index_file(self.db, self.source_file);
        let sym_id = def.name_site_to_symbol.get(&ast_id).copied()?;
        let gsym = lyra_semantic::symbols::GlobalSymbolId {
            file: self.source_file.file_id(self.db),
            local: sym_id,
        };
        let sym_ref = SymbolRef::new(self.db, self.unit, gsym);
        Some(type_of_symbol(self.db, sym_ref))
    }

    fn resolve_type_arg(
        &self,
        utr: &lyra_semantic::UserTypeRef,
    ) -> Option<lyra_semantic::types::Ty> {
        crate::resolve_helpers::resolve_type_arg_impl(
            self.db,
            self.unit,
            self.source_file,
            self.ast_id_map,
            utr,
        )
    }

    fn const_eval_int(&self, expr: &Expr) -> Option<i64> {
        let ast_id = self.ast_id_map.erased_ast_id(expr.syntax())?;
        let expr_ref = crate::const_eval::ConstExprRef::new(self.db, self.unit, ast_id);
        match crate::const_eval::eval_const_int(self.db, expr_ref) {
            lyra_semantic::types::ConstInt::Known(v) => Some(v),
            _ => None,
        }
    }

    fn const_eval_int_by_site(&self, site: lyra_semantic::Site) -> Option<i64> {
        let expr_ref = crate::const_eval::ConstExprRef::new(self.db, self.unit, site);
        match crate::const_eval::eval_const_int(self.db, expr_ref) {
            lyra_semantic::types::ConstInt::Known(v) => Some(v),
            _ => None,
        }
    }

    fn const_eval_int_by_site_full(
        &self,
        site: lyra_semantic::Site,
    ) -> lyra_semantic::types::ConstInt {
        let expr_ref = crate::const_eval::ConstExprRef::new(self.db, self.unit, site);
        crate::const_eval::eval_const_int(self.db, expr_ref)
    }

    fn enum_known_value_set(
        &self,
        id: &lyra_semantic::enum_def::EnumId,
    ) -> Option<std::sync::Arc<[i64]>> {
        let eref = EnumRef::new(self.db, self.unit, *id);
        crate::enum_queries::enum_known_value_set(self.db, eref)
    }

    fn is_modport_target_lvalue(&self, expr_id: lyra_ast::ErasedAstId) -> bool {
        let file_id = expr_id.file();
        let Some(src) = source_file_by_id(self.db, self.unit, file_id) else {
            return false;
        };
        expr_is_assignable_ref(self.db, src, expr_id)
    }

    fn expr_type_by_id(&self, id: lyra_semantic::Site) -> ExprType {
        let expr_ref = ExprRef::new(self.db, self.unit, id);
        crate::expr_queries::type_of_expr(self.db, expr_ref)
    }

    fn fixed_stream_width_bits(&self, id: lyra_semantic::Site) -> Option<u32> {
        let et = self.expr_type_by_id(id);
        self.fixed_stream_width_bits_of_type(&et)
    }

    fn fixed_stream_width_bits_of_type(&self, et: &ExprType) -> Option<u32> {
        lyra_semantic::fixed_stream_width_bits_of_expr_type(et, &self.enum_bits_cb())
    }

    fn fixed_stream_width_bits_of_ty(&self, ty: &lyra_semantic::types::Ty) -> Option<u32> {
        let et = ExprType::from_ty(ty);
        self.fixed_stream_width_bits_of_type(&et)
    }

    fn readonly_target_kind(
        &self,
        expr_site: lyra_ast::ErasedAstId,
    ) -> Option<(lyra_semantic::type_check::ReadonlyKind, smol_str::SmolStr)> {
        let resolve = resolve_index_file(self.db, self.source_file, self.unit);
        let resolution = resolve.resolutions.get(&expr_site)?;
        let lyra_semantic::resolve_index::ResolvedTarget::Symbol(gsym) = &resolution.target else {
            return None;
        };
        let target_file = source_file_by_id(self.db, self.unit, gsym.file)?;
        let def = def_index_file(self.db, target_file);
        let sym = def.symbols.get(gsym.local);
        if sym.constness == lyra_semantic::symbols::Constness::Const {
            Some((
                lyra_semantic::type_check::ReadonlyKind::Const,
                sym.name.clone(),
            ))
        } else {
            None
        }
    }
}

/// Check whether a modport expression target is an assignable reference.
/// Modport ports require a concrete variable, not a concat or stream.
fn expr_is_assignable_ref(
    db: &dyn salsa::Database,
    file: SourceFile,
    expr_id: lyra_ast::ErasedAstId,
) -> bool {
    let parse = parse_file(db, file);
    let map = ast_id_map(db, file);
    let Some(node) = map.get_node(&parse.syntax(), expr_id) else {
        return false;
    };
    let Some(expr) = Expr::cast(node) else {
        return false;
    };
    lyra_semantic::lhs::is_assignable_ref(&expr)
}

/// Unit-level diagnostics: duplicate definitions in the definitions namespace.
///
/// Walks `GlobalDefIndex.definitions()`, finds adjacent entries with the
/// same name, and emits one diagnostic per duplicate. Catches module/module,
/// package/package, and module/package name collisions.
#[salsa::tracked(return_ref)]
pub fn unit_diagnostics(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
) -> Box<[lyra_diag::Diagnostic]> {
    let global = global_def_index(db, unit);
    let defs = global.definitions();
    let mut diags = Vec::new();

    let mut i = 0;
    while i < defs.len() {
        let (name, _, _) = &defs[i];
        let mut j = i + 1;
        while j < defs.len() && defs[j].0 == *name {
            j += 1;
        }
        if j - i > 1 {
            // Duplicate group: emit diagnostics for entries [i+1..j]
            for (_, dup_def_id, _) in &defs[(i + 1)..j] {
                let dup_file_id = dup_def_id.file();
                if let Some(dup_file) = source_file_by_id(db, unit, dup_file_id) {
                    let dup_def = def_index_file(db, dup_file);
                    if let Some(entry) = dup_def.def_entry(*dup_def_id) {
                        let pp = preprocess_file(db, dup_file);
                        let label_range = entry.name_span.text_range();
                        if let Some(span) = pp.source_map.map_span(label_range) {
                            diags.push(
                                lyra_diag::Diagnostic::new(
                                    lyra_diag::Severity::Error,
                                    lyra_diag::DiagnosticCode::DUPLICATE_DEFINITION,
                                    lyra_diag::Message::new(
                                        lyra_diag::MessageId::DuplicateDefinitionInUnit,
                                        vec![lyra_diag::Arg::Name(name.clone())],
                                    ),
                                )
                                .with_label(lyra_diag::Label {
                                    kind: lyra_diag::LabelKind::Primary,
                                    span,
                                    message: lyra_diag::Message::simple(
                                        lyra_diag::MessageId::RedefinedHere,
                                    ),
                                }),
                            );
                        }
                    }
                }
            }
        }
        i = j;
    }

    diags.into_boxed_slice()
}
