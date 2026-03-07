use lyra_ast::{
    AstIdMap, AstNode, EnumMember, EnumType, HasSyntax, NameRef, NettypeDecl, QualifiedName,
    StructType, TypeSpec, TypedefDecl,
};
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;
use lyra_source::NameSpan;
use smol_str::SmolStr;

use crate::builder::DefContext;
use crate::def_index::{ExpectedNs, NamePath, UseSite};
use crate::diagnostic::{DiagSpan, SemanticDiag, SemanticDiagKind};
use crate::enum_def::{EnumBase, EnumDef, EnumDefIdx, EnumMemberDef, EnumMemberRangeKind};
use crate::nettype_def::{NettypeDef, NettypeDefIdx, ResolveFnRef};
use crate::record::{
    Packing, RecordDef, RecordDefIdx, RecordField, RecordKind, SymbolOrigin, TypeRef,
    extract_typeref_from_typespec,
};
use crate::scopes::ScopeId;
use crate::symbols::{Constness, Lifetime, Namespace, Symbol, SymbolKind};
use crate::types::Ty;

pub(crate) fn collect_type_spec_refs(ctx: &mut DefContext<'_>, ts: &TypeSpec, scope: ScopeId) {
    // type(...) operator: names inside can be types or values; collect
    // inner expression refs as value-namespace and inner type refs recursively.
    if let Some(te) = ts.type_expr() {
        if let Some(inner_ts) = te.inner_type_spec() {
            // type(data_type_or_name): register inner NameRef with TypeOrValue
            if let Some(utr) = crate::type_extract::user_type_ref(&inner_ts) {
                register_type_expr_use_site(ctx, &utr, scope);
            }
            collect_typespec_dim_refs(ctx, &inner_ts, scope);
        } else {
            // type(expr): collect expression name refs
            for child in te.syntax().children() {
                if child.kind() != SyntaxKind::TypeSpec {
                    collect_name_refs(ctx, &child, scope);
                }
            }
        }
        return;
    }
    if let Some(utr) = crate::type_extract::user_type_ref(ts) {
        register_type_use_site(ctx, &utr, scope);
    }
    collect_typespec_dim_refs(ctx, ts, scope);
}

// Walk dimension children of a TypeSpec and collect name refs from expressions
// inside packed/unpacked dimensions.
fn collect_typespec_dim_refs(ctx: &mut DefContext<'_>, ts: &TypeSpec, scope: ScopeId) {
    for child in ts.syntax().children() {
        match child.kind() {
            SyntaxKind::PackedDimension | SyntaxKind::UnpackedDimension => {
                collect_name_refs(ctx, &child, scope);
            }
            _ => {}
        }
    }
}

/// Register a use site for a name inside `type(...)` (LRM 6.23).
///
/// Uses `ExpectedNs::TypeOrValue`: try Type first, fall back to Value
/// without the "not a type" diagnostic.
fn register_type_expr_use_site(
    ctx: &mut DefContext<'_>,
    utr: &crate::type_extract::UserTypeRef,
    scope: ScopeId,
) {
    match utr {
        crate::type_extract::UserTypeRef::Simple(nr)
        | crate::type_extract::UserTypeRef::DottedType { base: nr, .. } => {
            if let Some(ident) = nr.ident()
                && let Some(ast_id) = ctx.ast_id_map.ast_id(nr)
            {
                ctx.use_sites.push(UseSite {
                    path: NamePath::Simple(SmolStr::new(ident.text())),
                    expected_ns: ExpectedNs::TypeOrValue,
                    scope,
                    name_ref_site: ast_id.erase(),
                    order_key: 0,
                });
            }
        }
        crate::type_extract::UserTypeRef::Qualified(qn) => {
            if let Some(ast_id) = ctx.ast_id_map.ast_id(qn) {
                let segments: Box<[SmolStr]> = qn
                    .segments()
                    .map(|ident| SmolStr::new(ident.text()))
                    .collect();
                if !segments.is_empty() {
                    ctx.use_sites.push(UseSite {
                        path: NamePath::Qualified { segments },
                        expected_ns: ExpectedNs::TypeOrValue,
                        scope,
                        name_ref_site: ast_id.erase(),
                        order_key: 0,
                    });
                }
            }
        }
    }
}

fn register_type_use_site(
    ctx: &mut DefContext<'_>,
    utr: &crate::type_extract::UserTypeRef,
    scope: ScopeId,
) {
    match utr {
        crate::type_extract::UserTypeRef::Simple(nr) => {
            if let Some(ident) = nr.ident()
                && let Some(ast_id) = ctx.ast_id_map.ast_id(nr)
            {
                ctx.use_sites.push(UseSite {
                    path: NamePath::Simple(SmolStr::new(ident.text())),
                    expected_ns: ExpectedNs::TypeThenValue,
                    scope,
                    name_ref_site: ast_id.erase(),
                    order_key: 0,
                });
            }
        }
        crate::type_extract::UserTypeRef::DottedType { base: nr, .. } => {
            if let Some(ident) = nr.ident()
                && let Some(ast_id) = ctx.ast_id_map.ast_id(nr)
            {
                ctx.use_sites.push(UseSite {
                    path: NamePath::Simple(SmolStr::new(ident.text())),
                    expected_ns: ExpectedNs::TypeOrValue,
                    scope,
                    name_ref_site: ast_id.erase(),
                    order_key: 0,
                });
            }
        }
        crate::type_extract::UserTypeRef::Qualified(qn) => {
            if let Some(ast_id) = ctx.ast_id_map.ast_id(qn) {
                let segments: Box<[SmolStr]> = qn
                    .segments()
                    .map(|ident| SmolStr::new(ident.text()))
                    .collect();
                if !segments.is_empty() {
                    ctx.use_sites.push(UseSite {
                        path: NamePath::Qualified { segments },
                        expected_ns: ExpectedNs::TypeThenValue,
                        scope,
                        name_ref_site: ast_id.erase(),
                        order_key: 0,
                    });
                }
            }
        }
    }
}

pub(crate) fn collect_typedef(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error_unanchored(&format!(
            "erased_ast_id returned None for {:?} in collect_typedef",
            node.kind()
        ));
        return;
    };
    // Detect enum/struct in the TypeSpec child
    let origin = detect_aggregate_type(ctx, node, scope);
    for child in node.children() {
        if let Some(ts) = TypeSpec::cast(child.clone()) {
            collect_type_spec_refs(ctx, &ts, scope);
        } else if child.kind() == SyntaxKind::UnpackedDimension {
            collect_name_refs(ctx, &child, scope);
        }
    }
    if let Some(td) = TypedefDecl::cast(node.clone())
        && let Some(name_tok) = td.name()
    {
        let typedef_name = SmolStr::new(name_tok.text());
        // Update the def name if we collected an aggregate
        match origin {
            SymbolOrigin::Enum(idx) => {
                ctx.enum_defs[idx.0 as usize].name = Some(typedef_name.clone());
            }
            SymbolOrigin::Record(idx) => {
                ctx.record_defs[idx.0 as usize].name = Some(typedef_name.clone());
            }
            SymbolOrigin::TypeSpec
            | SymbolOrigin::Error
            | SymbolOrigin::EnumVariant(_)
            | SymbolOrigin::Instance(_)
            | SymbolOrigin::Nettype(_) => {}
        }
        let typedef_type_site = td
            .type_spec()
            .and_then(|ts| ctx.ast_id_map.erased_ast_id(ts.syntax()));
        let sym_id = ctx.push_symbol(Symbol {
            name: typedef_name,
            kind: SymbolKind::Typedef,
            constness: Constness::Mutable,
            lifetime: Lifetime::Static,
            decl_site,
            name_site: decl_site,
            type_site: typedef_type_site,
            name_span: NameSpan::new(name_tok.text_range()),
            scope,
            origin,
        });
        ctx.register_binding(sym_id);
    }
}

pub(crate) fn collect_nettype_decl(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    let Some(decl_site) = ctx.ast_id_map.erased_ast_id(node) else {
        ctx.emit_internal_error_unanchored(&format!(
            "erased_ast_id returned None for {:?} in collect_nettype_decl",
            node.kind()
        ));
        return;
    };
    let Some(nd) = NettypeDecl::cast(node.clone()) else {
        return;
    };
    if let Some(ts) = nd.type_spec() {
        collect_type_spec_refs(ctx, &ts, scope);
    }
    let Some(name_tok) = nd.name() else {
        return;
    };
    let nettype_name = SmolStr::new(name_tok.text());
    let Some(first_type_site) = nd
        .type_spec()
        .and_then(|ts| ctx.ast_id_map.erased_ast_id(ts.syntax()))
    else {
        ctx.emit_internal_error("missing TypeSpec anchor in NettypeDecl", decl_site);
        return;
    };
    let resolve_fn = nd.resolve_fn_token().map(|tok| ResolveFnRef {
        name: SmolStr::new(tok.text()),
        span: NameSpan::new(tok.text_range()),
    });
    let idx = NettypeDefIdx(ctx.nettype_defs.len() as u32);
    ctx.nettype_defs.push(NettypeDef {
        decl_site,
        name: nettype_name.clone(),
        first_type_site,
        resolve_fn,
    });
    let sym_id = ctx.push_symbol(Symbol {
        name: nettype_name,
        kind: SymbolKind::Nettype,
        constness: Constness::Mutable,
        lifetime: Lifetime::Static,
        decl_site,
        name_site: decl_site,
        type_site: Some(first_type_site),
        name_span: NameSpan::new(name_tok.text_range()),
        scope,
        origin: SymbolOrigin::Nettype(idx),
    });
    ctx.register_binding(sym_id);
}

// Detect enum/struct type in a declaration's TypeSpec child.
// If found, build the def table entry and return the appropriate SymbolOrigin.
pub(crate) fn detect_aggregate_type(
    ctx: &mut DefContext<'_>,
    decl_node: &SyntaxNode,
    scope: ScopeId,
) -> SymbolOrigin {
    for child in decl_node.children() {
        if child.kind() == SyntaxKind::TypeSpec {
            for ts_child in child.children() {
                if ts_child.kind() == SyntaxKind::EnumType
                    && let Some(et) = EnumType::cast(ts_child.clone())
                {
                    return match collect_enum_def(ctx, &et, scope) {
                        Some(idx) => SymbolOrigin::Enum(idx),
                        None => SymbolOrigin::Error,
                    };
                } else if ts_child.kind() == SyntaxKind::StructType
                    && let Some(st) = StructType::cast(ts_child)
                {
                    return match collect_record_def(ctx, &st, scope) {
                        Some(idx) => SymbolOrigin::Record(idx),
                        None => SymbolOrigin::Error,
                    };
                }
            }
        }
    }
    SymbolOrigin::TypeSpec
}

fn collect_enum_def(
    ctx: &mut DefContext<'_>,
    enum_type: &EnumType,
    scope: ScopeId,
) -> Option<EnumDefIdx> {
    debug_assert!(enum_type.syntax().kind() == SyntaxKind::EnumType);
    let ast_id = ctx.ast_id_map.erased_ast_id(enum_type.syntax())?;
    let idx = EnumDefIdx(ctx.enum_defs.len() as u32);

    // Extract base type with its stable anchor
    let base = if let Some(base_ts) = enum_type.base_type_spec() {
        let type_site = ctx
            .ast_id_map
            .erased_ast_id(base_ts.syntax())
            .unwrap_or(ast_id);
        let tref = extract_typeref_from_typespec(&base_ts, ctx.ast_id_map);
        EnumBase { tref, type_site }
    } else {
        // Default base (int): anchor to the enum type node itself
        EnumBase {
            tref: TypeRef::Resolved(Ty::int()),
            type_site: ast_id,
        }
    };

    // Extract members. Plain members get real symbols; range members are stored
    // as raw EnumMemberDef for later expansion via enum_variants query.
    let mut members = Vec::new();
    for member in enum_type.members() {
        let Some(name_tok) = member.name() else {
            continue;
        };
        let name = SmolStr::new(name_tok.text());

        // Capture ast_id for every member (used by value diagnostics)
        let member_ast_id = ctx.ast_id_map.ast_id(&member);
        debug_assert!(
            member_ast_id.is_some(),
            "EnumMember node must have an AstId"
        );
        let Some(member_ast_id) = member_ast_id else {
            continue;
        };
        let erased_ast_id = member_ast_id.erase();

        let init_expr = member.init_expr();
        let init = init_expr
            .as_ref()
            .and_then(|expr| ctx.ast_id_map.erased_ast_id(expr.syntax()));
        let init_literal_width = init_expr.as_ref().and_then(|expr| {
            let e = expr.peeled_expr()?;
            crate::literal::extract_sized_literal_width(&e)
        });

        let (range_kind, range_site) = extract_enum_range_spec(&member, ctx.ast_id_map);

        if member.range_spec().is_some() && range_site.is_none() {
            ctx.diagnostics.push(SemanticDiag {
                kind: SemanticDiagKind::InternalError {
                    detail: SmolStr::new("erased_ast_id returned None for enum range spec"),
                },
                primary: DiagSpan::Site(erased_ast_id),
                label: None,
            });
        }

        let has_range = range_kind.is_some();

        members.push(EnumMemberDef {
            name: name.clone(),
            name_site: erased_ast_id,
            name_span: NameSpan::new(name_tok.text_range()),
            range: range_kind,
            range_site,
            init,
            init_literal_width,
        });

        if has_range {
            // Range members: no symbol injection. These are resolved
            // exclusively via EnumVariantIndex after expansion.
            // We don't know how many variants there are until const-eval,
            // so we don't advance variant_ordinal here.
        } else {
            // Plain members: inject real symbol with decl binding.
            let sym_id = ctx.push_symbol(Symbol {
                name,
                kind: SymbolKind::EnumMember,
                constness: Constness::Mutable,
                lifetime: Lifetime::Static,
                decl_site: erased_ast_id,
                name_site: erased_ast_id,
                type_site: None,
                name_span: NameSpan::new(name_tok.text_range()),
                scope,
                origin: SymbolOrigin::EnumVariant(idx),
            });
            ctx.register_binding(sym_id);
        }
    }

    ctx.enum_defs.push(EnumDef {
        name: None,
        enum_type_site: ast_id,
        scope,
        base,
        members: members.into_boxed_slice(),
    });
    Some(idx)
}

fn extract_enum_range_spec(
    member: &EnumMember,
    ast_id_map: &AstIdMap,
) -> (Option<EnumMemberRangeKind>, Option<crate::Site>) {
    let Some(range_spec) = member.range_spec() else {
        return (None, None);
    };
    let site = ast_id_map.erased_ast_id(range_spec.syntax());
    let Some(first) = range_spec.first_expr() else {
        return (None, site);
    };
    let first_id = ast_id_map.erased_ast_id(first.syntax());
    if let Some(second) = range_spec.second_expr() {
        let second_id = ast_id_map.erased_ast_id(second.syntax());
        match (first_id, second_id) {
            (Some(f), Some(s)) => (Some(EnumMemberRangeKind::FromTo(f, s)), site),
            _ => (None, site),
        }
    } else {
        match first_id {
            Some(f) => (Some(EnumMemberRangeKind::Count(f)), site),
            None => (None, site),
        }
    }
}

fn collect_record_def(
    ctx: &mut DefContext<'_>,
    struct_type: &StructType,
    scope: ScopeId,
) -> Option<RecordDefIdx> {
    let struct_type_site = ctx.ast_id_map.erased_ast_id(struct_type.syntax());

    debug_assert!(struct_type.syntax().kind() == SyntaxKind::StructType);
    let ast_id = struct_type_site?;
    let idx = RecordDefIdx(ctx.record_defs.len() as u32);

    let kind = if struct_type.is_union() {
        if struct_type.is_tagged() {
            RecordKind::TaggedUnion
        } else {
            RecordKind::Union
        }
    } else {
        RecordKind::Struct
    };
    let packing = if struct_type.is_soft() {
        Packing::SoftPacked
    } else if struct_type.is_packed() {
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
                collect_type_spec_refs(ctx, ts, scope);
                extract_typeref_from_typespec(ts, ctx.ast_id_map)
            }
            None => TypeRef::Resolved(Ty::Error),
        };
        for decl in member.declarators() {
            if let Some(name_tok) = decl.name() {
                let Some(decl_site) = ctx.ast_id_map.erased_ast_id(decl.syntax()) else {
                    ctx.emit_internal_error_unanchored(&format!(
                        "erased_ast_id returned None for {:?} in collect_record_def declarator",
                        decl.syntax().kind()
                    ));
                    continue;
                };
                fields.push(RecordField {
                    name: SmolStr::new(name_tok.text()),
                    name_site: decl_site,
                    name_span: NameSpan::new(name_tok.text_range()),
                    ty: ty.clone(),
                });
            }
        }
    }

    ctx.record_defs.push(RecordDef {
        name: None,
        record_type_site: ast_id,
        kind,
        packing,
        scope,
        fields: fields.into_boxed_slice(),
    });
    Some(idx)
}

/// Collect name refs from an unpacked dimension, registering bare `NameRef`s
/// with `TypeOrValue` namespace so `[TypedefName]` can resolve as a type.
fn collect_unpacked_dim_refs(
    ctx: &mut DefContext<'_>,
    dim: &lyra_ast::UnpackedDimension,
    scope: ScopeId,
) {
    use lyra_ast::UnpackedDimKind;
    if let UnpackedDimKind::Size { ref expr } = dim.classify()
        && let Some(utr) = crate::type_extract::user_type_ref_from_expr(expr)
    {
        match &utr {
            crate::type_extract::UserTypeRef::Simple(nr) => {
                if let Some(ident) = nr.ident()
                    && let Some(ast_id) = ctx.ast_id_map.ast_id(nr)
                {
                    ctx.use_sites.push(UseSite {
                        path: NamePath::Simple(SmolStr::new(ident.text())),
                        expected_ns: ExpectedNs::TypeOrValue,
                        scope,
                        name_ref_site: ast_id.erase(),
                        order_key: 0,
                    });
                }
                return;
            }
            crate::type_extract::UserTypeRef::Qualified(qn) => {
                if let Some(ast_id) = ctx.ast_id_map.ast_id(qn) {
                    let segments: Box<[SmolStr]> = qn
                        .segments()
                        .map(|ident| SmolStr::new(ident.text()))
                        .collect();
                    if !segments.is_empty() {
                        ctx.use_sites.push(UseSite {
                            path: NamePath::Qualified { segments },
                            expected_ns: ExpectedNs::TypeOrValue,
                            scope,
                            name_ref_site: ast_id.erase(),
                            order_key: 0,
                        });
                    }
                }
                return;
            }
            crate::type_extract::UserTypeRef::DottedType { .. } => {}
        }
    }
    collect_name_refs_inner(ctx, dim.syntax(), scope);
}

pub(crate) fn collect_name_refs(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    // When called directly on an UnpackedDimension, apply TypeOrValue semantics.
    if let Some(dim) = lyra_ast::UnpackedDimension::cast(node.clone()) {
        collect_unpacked_dim_refs(ctx, &dim, scope);
        return;
    }
    collect_name_refs_inner(ctx, node, scope);
}

fn collect_name_refs_inner(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        if child.kind() == SyntaxKind::NameRef {
            if let Some(name_ref) = NameRef::cast(child.clone())
                && let Some(ident) = name_ref.ident()
                && let Some(ast_id) = ctx.ast_id_map.ast_id(&name_ref)
            {
                ctx.use_sites.push(UseSite {
                    path: NamePath::Simple(SmolStr::new(ident.text())),
                    expected_ns: ExpectedNs::Exact(Namespace::Value),
                    scope,
                    name_ref_site: ast_id.erase(),
                    order_key: 0,
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
                        scope,
                        name_ref_site: ast_id.erase(),
                        order_key: 0,
                    });
                }
            }
        } else if child.kind() == SyntaxKind::CastExpr {
            // CastExpr contains a TypeSpec (the cast target type) and an
            // inner expression. Process the TypeSpec as a type reference and
            // recurse into the rest of the expression.
            for cast_child in child.children() {
                if cast_child.kind() == SyntaxKind::TypeSpec {
                    if let Some(ts) = lyra_ast::TypeSpec::cast(cast_child) {
                        collect_type_spec_refs(ctx, &ts, scope);
                    }
                } else {
                    collect_name_refs(ctx, &cast_child, scope);
                }
            }
        } else if child.kind() == SyntaxKind::TypeExpr {
            // Standalone type(...) in expression context: inner names can be
            // types or values (TypeOrValue namespace).
            if let Some(te) = lyra_ast::TypeExpr::cast(child) {
                if let Some(inner_ts) = te.inner_type_spec() {
                    if let Some(utr) = crate::type_extract::user_type_ref(&inner_ts) {
                        register_type_expr_use_site(ctx, &utr, scope);
                    }
                    collect_typespec_dim_refs(ctx, &inner_ts, scope);
                } else if let Some(inner_expr) = te.inner_expr() {
                    collect_name_refs(ctx, inner_expr.syntax(), scope);
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
        } else if let Some(dim) = lyra_ast::UnpackedDimension::cast(child.clone()) {
            collect_unpacked_dim_refs(ctx, &dim, scope);
        } else {
            collect_name_refs(ctx, &child, scope);
        }
    }
}
