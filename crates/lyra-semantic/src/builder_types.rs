use lyra_ast::{AstNode, EnumType, NameRef, QualifiedName, StructType, TypeSpec, TypedefDecl};
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;
use smol_str::SmolStr;

use crate::builder::DefContext;
use crate::def_index::{ExpectedNs, NamePath, UseSite};
use crate::diagnostic::{SemanticDiag, SemanticDiagKind};
use crate::enum_def::{EnumBase, EnumDef, EnumDefIdx, EnumVariant};
use crate::record::{
    Packing, RecordDef, RecordDefIdx, RecordField, RecordKind, SymbolOrigin, TypeRef,
    extract_typeref_from_typespec,
};
use crate::scopes::ScopeId;
use crate::symbols::{Namespace, SymbolKind};
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
                    ast_id: ast_id.erase(),
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
                        ast_id: ast_id.erase(),
                        order_key: 0,
                    });
                }
            }
        }
    }
}

pub(crate) fn collect_typedef(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
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
            | SymbolOrigin::EnumVariant { .. }
            | SymbolOrigin::Instance(_) => {}
        }
        let sym_id = ctx.add_symbol_with_origin(
            typedef_name,
            SymbolKind::Typedef,
            name_tok.text_range(),
            scope,
            origin,
        );
        if let Some(ast_id) = ctx.ast_id_map.ast_id(&td) {
            ctx.register_binding(sym_id, scope, ast_id.erase(), name_tok.text_range());
        }
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
                    return SymbolOrigin::Enum(collect_enum_def(ctx, &et, scope));
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

fn collect_enum_def(ctx: &mut DefContext<'_>, enum_type: &EnumType, scope: ScopeId) -> EnumDefIdx {
    let owner = ctx.current_owner.clone();
    let ordinal = ctx.enum_ordinals.entry(owner.clone()).or_insert(0);
    let ord = *ordinal;
    *ordinal += 1;
    let idx = EnumDefIdx(ctx.enum_defs.len() as u32);

    // Extract base type with its source range
    let base = if let Some(base_ts) = enum_type.base_type_spec() {
        let range = base_ts.text_range();
        let tref = extract_typeref_from_typespec(base_ts.syntax(), ctx.file, ctx.ast_id_map);
        EnumBase { tref, range }
    } else {
        // Default base: use the `enum` keyword token's range as anchor
        let range = enum_type
            .syntax()
            .children_with_tokens()
            .filter_map(lyra_parser::SyntaxElement::into_token)
            .find(|tok| tok.kind() == SyntaxKind::EnumKw)
            .map_or_else(|| enum_type.text_range(), |tok| tok.text_range());
        EnumBase {
            tref: TypeRef::Resolved(Ty::int()),
            range,
        }
    };

    // Extract variants and inject each as a value-namespace symbol
    let mut variants = Vec::new();
    let mut variant_ordinal: u32 = 0;
    for member in enum_type.members() {
        let Some(name_tok) = member.name() else {
            continue;
        };
        let name = SmolStr::new(name_tok.text());
        let init = member
            .init_expr()
            .and_then(|expr| ctx.ast_id_map.erased_ast_id(expr.syntax()));
        variants.push(EnumVariant {
            name: name.clone(),
            init,
        });

        ctx.add_symbol_with_origin(
            name,
            SymbolKind::EnumMember,
            name_tok.text_range(),
            scope,
            SymbolOrigin::EnumVariant {
                enum_idx: idx,
                variant_ordinal,
            },
        );
        variant_ordinal += 1;
    }

    ctx.enum_defs.push(EnumDef {
        name: None,
        owner,
        ordinal: ord,
        scope,
        base,
        variants: variants.into_boxed_slice(),
    });
    idx
}

fn collect_record_def(
    ctx: &mut DefContext<'_>,
    struct_type: &StructType,
    scope: ScopeId,
) -> Option<RecordDefIdx> {
    if struct_type.is_union() && struct_type.is_tagged() {
        let range = struct_type
            .syntax()
            .children_with_tokens()
            .filter_map(lyra_parser::SyntaxElement::into_token)
            .find(|tok| tok.kind() == SyntaxKind::TaggedKw)
            .map_or_else(|| struct_type.text_range(), |tok| tok.text_range());
        ctx.diagnostics.push(SemanticDiag {
            kind: SemanticDiagKind::UnsupportedTaggedUnion,
            range,
        });
        return None;
    }

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
                    ast_id: ast_id.erase(),
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
                        ast_id: ast_id.erase(),
                        order_key: 0,
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
