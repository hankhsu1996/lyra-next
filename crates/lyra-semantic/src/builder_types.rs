use lyra_ast::{
    AstIdMap, AstNode, EnumMember, EnumType, NameRef, QualifiedName, StructType, TypeSpec,
    TypedefDecl,
};
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;
use lyra_source::NameSpan;
use smol_str::SmolStr;

use lyra_source::TokenSpan;

use crate::builder::DefContext;
use crate::def_index::{ExpectedNs, NamePath, UseSite};
use crate::diagnostic::{DiagSpan, SemanticDiag, SemanticDiagKind};
use crate::enum_def::{EnumBase, EnumDef, EnumDefIdx, EnumMemberDef, EnumMemberRangeKind};
use crate::record::{
    Packing, RecordDef, RecordDefIdx, RecordField, RecordKind, SymbolOrigin, TypeRef,
    extract_typeref_from_typespec,
};
use crate::scopes::ScopeId;
use crate::symbols::{Namespace, Symbol, SymbolKind};
use crate::types::Ty;

pub(crate) fn collect_type_spec_refs(ctx: &mut DefContext<'_>, ts: &TypeSpec, scope: ScopeId) {
    let node = ts.syntax();
    if let Some(utr) = crate::type_extract::user_type_ref(node) {
        register_type_use_site(ctx, &utr, scope);
    }
    for child in node.children() {
        match child.kind() {
            SyntaxKind::PackedDimension | SyntaxKind::UnpackedDimension => {
                collect_name_refs(ctx, &child, scope);
            }
            _ => {}
        }
    }
}

fn register_type_use_site(
    ctx: &mut DefContext<'_>,
    utr: &crate::type_extract::UserTypeRef,
    scope: ScopeId,
) {
    match utr {
        crate::type_extract::UserTypeRef::Simple(nr)
        | crate::type_extract::UserTypeRef::InterfaceModport { iface: nr, .. } => {
            if let Some(ident) = nr.ident()
                && let Some(ast_id) = ctx.ast_id_map.ast_id(nr)
            {
                ctx.use_sites.push(UseSite {
                    path: NamePath::Simple(SmolStr::new(ident.text())),
                    expected_ns: ExpectedNs::TypeThenValue,
                    range: nr.text_range(),
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
                        range: qn.text_range(),
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
        ctx.emit_internal_error(
            &format!(
                "erased_ast_id returned None for {:?} in collect_typedef",
                node.kind()
            ),
            node.text_range(),
        );
        return;
    };
    // Detect enum/struct in the TypeSpec child
    let origin = detect_aggregate_type(ctx, node, scope);
    for child in node.children() {
        if let Some(ts) = TypeSpec::cast(child) {
            collect_type_spec_refs(ctx, &ts, scope);
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
            | SymbolOrigin::Instance(_) => {}
        }
        let typedef_type_site = td
            .type_spec()
            .and_then(|ts| ctx.ast_id_map.erased_ast_id(ts.syntax()));
        let sym_id = ctx.push_symbol(Symbol {
            name: typedef_name,
            kind: SymbolKind::Typedef,
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

    // Extract base type with its source range
    let base = if let Some(base_ts) = enum_type.base_type_spec() {
        let range = base_ts.text_range();
        let tref = extract_typeref_from_typespec(base_ts.syntax(), ctx.file, ctx.ast_id_map);
        EnumBase { tref, range }
    } else {
        // Default base: use the `enum` keyword token's range as anchor
        let Some(tok) = enum_type
            .syntax()
            .children_with_tokens()
            .filter_map(lyra_parser::SyntaxElement::into_token)
            .find(|tok| tok.kind() == SyntaxKind::EnumKw)
        else {
            ctx.diagnostics.push(SemanticDiag {
                kind: SemanticDiagKind::InternalError {
                    detail: SmolStr::new("missing enum keyword token"),
                },
                primary: DiagSpan::Site(ast_id),
                label: None,
            });
            return None;
        };
        let range = tok.text_range();
        EnumBase {
            tref: TypeRef::Resolved(Ty::int()),
            range,
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
        let init_literal_width = init_expr
            .as_ref()
            .and_then(|expr| crate::literal::extract_sized_literal_width(expr.syntax()));

        let (range_kind, range_text_range) = extract_enum_range_spec(&member, ctx.ast_id_map);

        let has_range = range_kind.is_some();

        members.push(EnumMemberDef {
            name: name.clone(),
            name_site: erased_ast_id,
            name_span: NameSpan::new(name_tok.text_range()),
            range: range_kind,
            range_text_range,
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
) -> (Option<EnumMemberRangeKind>, Option<lyra_source::TextRange>) {
    let Some(range_spec) = member.range_spec() else {
        return (None, None);
    };
    let rtr = range_spec.text_range();
    let Some(first) = range_spec.first_expr() else {
        return (None, Some(rtr));
    };
    let first_id = ast_id_map.erased_ast_id(first.syntax());
    if let Some(second) = range_spec.second_expr() {
        let second_id = ast_id_map.erased_ast_id(second.syntax());
        match (first_id, second_id) {
            (Some(f), Some(s)) => (Some(EnumMemberRangeKind::FromTo(f, s)), Some(rtr)),
            _ => (None, Some(rtr)),
        }
    } else {
        match first_id {
            Some(f) => (Some(EnumMemberRangeKind::Count(f)), Some(rtr)),
            None => (None, Some(rtr)),
        }
    }
}

fn collect_record_def(
    ctx: &mut DefContext<'_>,
    struct_type: &StructType,
    scope: ScopeId,
) -> Option<RecordDefIdx> {
    let struct_type_site = ctx.ast_id_map.erased_ast_id(struct_type.syntax());

    if struct_type.is_union() && struct_type.is_tagged() {
        if let Some(site) = struct_type_site {
            let primary = DiagSpan::Site(site);
            if let Some(tok) = struct_type
                .syntax()
                .children_with_tokens()
                .filter_map(lyra_parser::SyntaxElement::into_token)
                .find(|tok| tok.kind() == SyntaxKind::TaggedKw)
            {
                ctx.diagnostics.push(SemanticDiag {
                    kind: SemanticDiagKind::UnsupportedTaggedUnion,
                    primary,
                    label: Some(DiagSpan::Token(TokenSpan::new(tok.text_range()))),
                });
            } else {
                ctx.diagnostics.push(SemanticDiag {
                    kind: SemanticDiagKind::InternalError {
                        detail: SmolStr::new("missing tagged keyword token"),
                    },
                    primary,
                    label: None,
                });
                ctx.diagnostics.push(SemanticDiag {
                    kind: SemanticDiagKind::UnsupportedTaggedUnion,
                    primary,
                    label: None,
                });
            }
        } else {
            ctx.emit_internal_error(
                "erased_ast_id returned None for StructType in collect_record_def",
                struct_type.text_range(),
            );
        }
        return None;
    }

    debug_assert!(struct_type.syntax().kind() == SyntaxKind::StructType);
    let ast_id = struct_type_site?;
    let idx = RecordDefIdx(ctx.record_defs.len() as u32);

    let kind = if struct_type.is_union() {
        RecordKind::Union
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
                extract_typeref_from_typespec(ts.syntax(), ctx.file, ctx.ast_id_map)
            }
            None => TypeRef::Resolved(Ty::Error),
        };
        for decl in member.declarators() {
            if let Some(name_tok) = decl.name() {
                let Some(decl_site) = ctx.ast_id_map.erased_ast_id(decl.syntax()) else {
                    ctx.emit_internal_error(
                        &format!(
                            "erased_ast_id returned None for {:?} in collect_record_def declarator",
                            decl.syntax().kind()
                        ),
                        decl.syntax().text_range(),
                    );
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
                        range: qn.text_range(),
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
