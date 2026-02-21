use lyra_ast::{AstIdMap, AstNode, DottedName, ErasedAstId, NameRef, QualifiedName};
use lyra_lexer::SyntaxKind;
use lyra_parser::{SyntaxElement, SyntaxNode};
use lyra_source::TextRange;
use smol_str::SmolStr;

use crate::expr_helpers::is_expression_kind;
use crate::types::{
    AssocIndex, AssocTypeRef, ConstEvalError, ConstInt, Integral, IntegralKw, NetKind, NetType,
    PackedDim, PackedDims, RealKw, SymbolType, SymbolTypeError, Ty, UnpackedDim, wrap_unpacked,
};

/// Extract a `SymbolType` from a declaration container node.
///
/// `container` must be one of: `VarDecl`, `NetDecl`, `ParamDecl`, `Port`, `TypedefDecl`.
/// `declarator` is the Declarator child node (for unpacked dims). Pass `None`
/// for `Port` and `TypedefDecl` nodes where the declarator is not separate.
///
/// This function never calls `parent()` or `ancestors()` on any node.
pub fn extract_type_from_container(
    container: &SyntaxNode,
    declarator: Option<&SyntaxNode>,
    ast_id_map: &AstIdMap,
) -> SymbolType {
    match container.kind() {
        SyntaxKind::VarDecl => extract_var_decl(container, declarator, ast_id_map),
        SyntaxKind::NetDecl => extract_net_decl(container, declarator, ast_id_map),
        SyntaxKind::ParamDecl => extract_param_decl(container, ast_id_map),
        SyntaxKind::Port => extract_port(container, ast_id_map),
        SyntaxKind::TypedefDecl => extract_typedef_decl(container, ast_id_map),
        _ => SymbolType::Error(SymbolTypeError::UnsupportedSymbolKind),
    }
}

/// Structured representation of a user-defined type reference inside a `TypeSpec`.
#[derive(Debug, Clone)]
pub enum UserTypeRef {
    Simple(NameRef),
    Qualified(QualifiedName),
    InterfaceModport {
        iface: NameRef,
        modport_name: SmolStr,
        modport_range: TextRange,
    },
}

impl UserTypeRef {
    /// The AST node whose `ErasedAstId` has a resolution in the resolve index.
    pub fn resolve_node(&self) -> &SyntaxNode {
        match self {
            Self::Simple(nr) | Self::InterfaceModport { iface: nr, .. } => nr.syntax(),
            Self::Qualified(qn) => qn.syntax(),
        }
    }
}

/// Extract the user-defined type reference from a `TypeSpec`, if any.
pub fn user_type_ref(typespec: &SyntaxNode) -> Option<UserTypeRef> {
    for child in typespec.children() {
        match child.kind() {
            SyntaxKind::NameRef => {
                return NameRef::cast(child).map(UserTypeRef::Simple);
            }
            SyntaxKind::QualifiedName => {
                return QualifiedName::cast(child).map(UserTypeRef::Qualified);
            }
            SyntaxKind::DottedName => {
                let dn = DottedName::cast(child)?;
                let nr = dn.interface_ref()?;
                // The modport ident is a direct Ident token child of DottedName.
                // The interface ident is nested inside the NameRef child node,
                // so only a direct token child can be the modport name.
                let mp_token = dn
                    .syntax()
                    .children_with_tokens()
                    .filter_map(|el| el.into_token())
                    .find(|t| t.kind() == SyntaxKind::Ident)?;
                return Some(UserTypeRef::InterfaceModport {
                    iface: nr,
                    modport_name: SmolStr::new(mp_token.text()),
                    modport_range: mp_token.text_range(),
                });
            }
            _ => {}
        }
    }
    None
}

// TODO: delete -- callers should migrate to user_type_ref
pub(crate) fn typespec_name_ref(typespec: &SyntaxNode) -> Option<SyntaxNode> {
    user_type_ref(typespec).map(|r| r.resolve_node().clone())
}

/// Normalize all `ConstInt` values inside a `SymbolType`.
///
/// Transforms `Unevaluated` dims to `Known`/`Error` by calling `eval`.
/// Never changes keyword, signedness, structure, or `NetKind`.
pub fn normalize_symbol_type(
    st: &SymbolType,
    eval: &dyn Fn(lyra_ast::ErasedAstId) -> ConstInt,
) -> SymbolType {
    match st {
        SymbolType::Value(ty) => SymbolType::Value(normalize_ty(ty, eval)),
        SymbolType::TypeAlias(ty) => SymbolType::TypeAlias(normalize_ty(ty, eval)),
        SymbolType::Net(net) => SymbolType::Net(NetType {
            kind: net.kind,
            data: normalize_ty(&net.data, eval),
        }),
        SymbolType::Error(e) => SymbolType::Error(*e),
    }
}

/// Normalize all `ConstInt::Unevaluated` values inside a `Ty`.
///
/// Transforms unevaluated dims to `Known`/`Error` by calling `eval`.
pub fn normalize_ty(ty: &Ty, eval: &dyn Fn(lyra_ast::ErasedAstId) -> ConstInt) -> Ty {
    match ty {
        Ty::Integral(i) => Ty::Integral(Integral {
            keyword: i.keyword,
            signed: i.signed,
            packed: i
                .packed
                .iter()
                .map(|d| normalize_packed_dim(d, eval))
                .collect(),
        }),
        Ty::Array { elem, dim } => Ty::Array {
            elem: Box::new(normalize_ty(elem, eval)),
            dim: normalize_unpacked_dim(dim, eval),
        },
        other => other.clone(),
    }
}

fn normalize_packed_dim(
    dim: &PackedDim,
    eval: &dyn Fn(lyra_ast::ErasedAstId) -> ConstInt,
) -> PackedDim {
    PackedDim {
        msb: normalize_const_int(&dim.msb, eval),
        lsb: normalize_const_int(&dim.lsb, eval),
    }
}

fn normalize_unpacked_dim(
    dim: &UnpackedDim,
    eval: &dyn Fn(lyra_ast::ErasedAstId) -> ConstInt,
) -> UnpackedDim {
    match dim {
        UnpackedDim::Range { msb, lsb } => UnpackedDim::Range {
            msb: normalize_const_int(msb, eval),
            lsb: normalize_const_int(lsb, eval),
        },
        UnpackedDim::Size(c) => UnpackedDim::Size(normalize_const_int(c, eval)),
        UnpackedDim::Unsized => UnpackedDim::Unsized,
        UnpackedDim::Queue { bound } => UnpackedDim::Queue {
            bound: bound.as_ref().map(|c| normalize_const_int(c, eval)),
        },
        UnpackedDim::Assoc(idx) => UnpackedDim::Assoc(idx.clone()),
    }
}

fn normalize_const_int(c: &ConstInt, eval: &dyn Fn(lyra_ast::ErasedAstId) -> ConstInt) -> ConstInt {
    match c {
        ConstInt::Unevaluated(id) => eval(*id),
        other => other.clone(),
    }
}

fn extract_var_decl(
    container: &SyntaxNode,
    declarator: Option<&SyntaxNode>,
    ast_id_map: &AstIdMap,
) -> SymbolType {
    let typespec = find_child(container, SyntaxKind::TypeSpec);
    let Some(typespec) = typespec else {
        return SymbolType::Value(Ty::Error);
    };
    if typespec_name_ref(&typespec).is_some() {
        // User-defined type -- db query handles typedef expansion
        return SymbolType::Value(Ty::Error);
    }
    let (base_ty, signed_override) = extract_typespec_base(&typespec);
    let packed = extract_packed_dims(&typespec, ast_id_map);
    let unpacked = declarator
        .map(|d| extract_unpacked_dims(d, ast_id_map))
        .unwrap_or_default();
    let ty = build_base_ty(base_ty, signed_override, packed);
    SymbolType::Value(wrap_unpacked(ty, &unpacked))
}

fn extract_net_decl(
    container: &SyntaxNode,
    declarator: Option<&SyntaxNode>,
    ast_id_map: &AstIdMap,
) -> SymbolType {
    let net_kind = net_keyword(container);
    let Some(net_kind) = net_kind else {
        return SymbolType::Error(SymbolTypeError::UnsupportedSymbolKind);
    };

    // Net declarations: implicit logic data type.
    // Check for optional signing and packed dims as direct children.
    let mut signed_override: Option<bool> = None;
    let mut packed = Vec::new();

    // Check for TypeSpec child (some net declarations have one)
    if let Some(typespec) = find_child(container, SyntaxKind::TypeSpec) {
        if typespec_name_ref(&typespec).is_some() {
            return SymbolType::Net(NetType {
                kind: net_kind,
                data: Ty::Error,
            });
        }
        let (base, so) = extract_typespec_base(&typespec);
        signed_override = so;
        packed = extract_packed_dims(&typespec, ast_id_map);
        let unpacked = declarator
            .map(|d| extract_unpacked_dims(d, ast_id_map))
            .unwrap_or_default();
        let ty = build_base_ty(base, signed_override, packed);
        return SymbolType::Net(NetType {
            kind: net_kind,
            data: wrap_unpacked(ty, &unpacked),
        });
    }

    // No TypeSpec -- check for signing/packed dims as direct children
    for el in container.children_with_tokens() {
        match el {
            SyntaxElement::Token(tok) => match tok.kind() {
                SyntaxKind::SignedKw => signed_override = Some(true),
                SyntaxKind::UnsignedKw => signed_override = Some(false),
                _ => {}
            },
            SyntaxElement::Node(node) => {
                if node.kind() == SyntaxKind::PackedDimension {
                    packed.push(extract_single_packed_dim(&node, ast_id_map));
                }
            }
        }
    }

    let unpacked = declarator
        .map(|d| extract_unpacked_dims(d, ast_id_map))
        .unwrap_or_default();

    let signed = signed_override.unwrap_or(IntegralKw::Logic.default_signed());
    let ty = Ty::Integral(Integral {
        keyword: IntegralKw::Logic,
        signed,
        packed: PackedDims::from(packed),
    });
    SymbolType::Net(NetType {
        kind: net_kind,
        data: wrap_unpacked(ty, &unpacked),
    })
}

fn extract_param_decl(container: &SyntaxNode, ast_id_map: &AstIdMap) -> SymbolType {
    // Check for `parameter type T` -- TypeKw token as direct child
    for el in container.children_with_tokens() {
        if let SyntaxElement::Token(tok) = &el
            && tok.kind() == SyntaxKind::TypeKw
        {
            return SymbolType::Error(SymbolTypeError::TypeParameterUnsupported);
        }
    }

    let typespec = find_child(container, SyntaxKind::TypeSpec);
    match typespec {
        Some(ts) => {
            if typespec_name_ref(&ts).is_some() {
                return SymbolType::Value(Ty::Error);
            }
            let (base_ty, signed_override) = extract_typespec_base(&ts);
            let packed = extract_packed_dims(&ts, ast_id_map);
            SymbolType::Value(build_base_ty(base_ty, signed_override, packed))
        }
        // No TypeSpec and this is a value parameter -> default to int (LRM 6.20.2)
        None => SymbolType::Value(Ty::int()),
    }
}

fn extract_port(container: &SyntaxNode, ast_id_map: &AstIdMap) -> SymbolType {
    let typespec = find_child(container, SyntaxKind::TypeSpec);
    match typespec {
        Some(ts) => {
            if typespec_name_ref(&ts).is_some() {
                return SymbolType::Value(Ty::Error);
            }
            let (base_ty, signed_override) = extract_typespec_base(&ts);
            if base_ty.is_none() {
                // TypeSpec present but no recognizable base type and no name ref
                return SymbolType::Error(SymbolTypeError::PortTypeMissing);
            }
            let packed = extract_packed_dims(&ts, ast_id_map);
            // Port unpacked dims are direct children of the Port node
            let unpacked = extract_unpacked_dims(container, ast_id_map);
            let ty = build_base_ty(base_ty, signed_override, packed);
            SymbolType::Value(wrap_unpacked(ty, &unpacked))
        }
        // M4 provisional: ports without explicit datatype default to logic
        None => SymbolType::Value(Ty::simple_logic()),
    }
}

fn extract_typedef_decl(container: &SyntaxNode, ast_id_map: &AstIdMap) -> SymbolType {
    let typespec = find_child(container, SyntaxKind::TypeSpec);
    let Some(ts) = typespec else {
        return SymbolType::TypeAlias(Ty::Error);
    };
    if typespec_name_ref(&ts).is_some() {
        return SymbolType::TypeAlias(Ty::Error);
    }
    let (base_ty, signed_override) = extract_typespec_base(&ts);
    let packed = extract_packed_dims(&ts, ast_id_map);
    // Typedef unpacked dims are direct children of TypedefDecl
    let unpacked = extract_unpacked_dims(container, ast_id_map);
    let ty = build_base_ty(base_ty, signed_override, packed);
    SymbolType::TypeAlias(wrap_unpacked(ty, &unpacked))
}

/// Extract the base `Ty` from a `TypeSpec` node, including signing and packed dims.
///
/// Used by callable signature extraction (in `lyra-db`) to type TF port parameters
/// without going through the full `extract_type_from_container` path.
pub fn extract_base_ty_from_typespec(typespec: &SyntaxNode, ast_id_map: &AstIdMap) -> Ty {
    let (base_ty, signed_override) = extract_typespec_base(typespec);
    let packed = extract_packed_dims(typespec, ast_id_map);
    build_base_ty(base_ty, signed_override, packed)
}

/// Extract the base type keyword and optional signed/unsigned override from a `TypeSpec`.
fn extract_typespec_base(typespec: &SyntaxNode) -> (Option<Ty>, Option<bool>) {
    let mut base_ty: Option<Ty> = None;
    let mut signed_override: Option<bool> = None;

    for el in typespec.children_with_tokens() {
        if let SyntaxElement::Token(tok) = el {
            let kind = tok.kind();
            if kind == SyntaxKind::SignedKw {
                signed_override = Some(true);
            } else if kind == SyntaxKind::UnsignedKw {
                signed_override = Some(false);
            } else if base_ty.is_none() {
                base_ty = keyword_to_ty(kind);
            }
        }
    }

    (base_ty, signed_override)
}

fn extract_packed_dims(typespec: &SyntaxNode, ast_id_map: &AstIdMap) -> Vec<PackedDim> {
    let mut dims = Vec::new();
    for child in typespec.children() {
        if child.kind() == SyntaxKind::PackedDimension {
            dims.push(extract_single_packed_dim(&child, ast_id_map));
        }
    }
    dims
}

fn extract_single_packed_dim(node: &SyntaxNode, ast_id_map: &AstIdMap) -> PackedDim {
    let exprs: Vec<_> = node
        .children()
        .filter(|c| is_expression_kind(c.kind()))
        .collect();
    if exprs.len() == 2 {
        let msb = expr_to_const_int(&exprs[0], ast_id_map);
        let lsb = expr_to_const_int(&exprs[1], ast_id_map);
        PackedDim { msb, lsb }
    } else {
        PackedDim {
            msb: ConstInt::Error(ConstEvalError::Unsupported),
            lsb: ConstInt::Error(ConstEvalError::Unsupported),
        }
    }
}

/// Extract unpacked dimensions from a parent node's `UnpackedDimension` children.
///
/// Handles all dimension forms: `[n]`, `[m:l]`, `[]`, `[$]`, `[$:n]`, `[*]`,
/// and `[type]` (associative with scalar type keyword).
pub fn extract_unpacked_dims(node: &SyntaxNode, ast_id_map: &AstIdMap) -> Vec<UnpackedDim> {
    let mut dims = Vec::new();
    for child in node.children() {
        if child.kind() == SyntaxKind::UnpackedDimension {
            dims.push(extract_single_unpacked_dim(&child, ast_id_map));
        }
    }
    dims
}

fn has_token_child(node: &SyntaxNode, kind: SyntaxKind) -> bool {
    node.children_with_tokens()
        .any(|el| el.as_token().is_some_and(|t| t.kind() == kind))
}

fn extract_single_unpacked_dim(node: &SyntaxNode, ast_id_map: &AstIdMap) -> UnpackedDim {
    // Star token -> Assoc(Wildcard)
    if has_token_child(node, SyntaxKind::Star) {
        return UnpackedDim::Assoc(AssocIndex::Wildcard);
    }

    // Dollar token -> Queue
    if has_token_child(node, SyntaxKind::Dollar) {
        let expr = node.children().find(|c| is_expression_kind(c.kind()));
        let bound = expr.map(|e| expr_to_const_int(&e, ast_id_map));
        return UnpackedDim::Queue { bound };
    }

    // TypeSpec child -> Assoc(Type) or Assoc(Unsupported)
    if let Some(ts) = node.children().find(|c| c.kind() == SyntaxKind::TypeSpec) {
        return extract_assoc_from_typespec(&ts, ast_id_map);
    }

    // Expression children -> Size, Range, or Unsized
    let exprs: Vec<_> = node
        .children()
        .filter(|c| is_expression_kind(c.kind()))
        .collect();
    match exprs.len() {
        0 => UnpackedDim::Unsized,
        1 => UnpackedDim::Size(expr_to_const_int(&exprs[0], ast_id_map)),
        2 => UnpackedDim::Range {
            msb: expr_to_const_int(&exprs[0], ast_id_map),
            lsb: expr_to_const_int(&exprs[1], ast_id_map),
        },
        _ => UnpackedDim::Size(ConstInt::Error(ConstEvalError::Unsupported)),
    }
}

fn extract_assoc_from_typespec(ts: &SyntaxNode, ast_id_map: &AstIdMap) -> UnpackedDim {
    // Find the first token in the TypeSpec (the keyword)
    let first_token = ts
        .children_with_tokens()
        .filter_map(|el| el.into_token())
        .find(|t| t.kind() != SyntaxKind::Whitespace);

    let Some(token) = first_token else {
        let ast_id = ast_id_map
            .erased_ast_id(ts)
            .unwrap_or_else(dummy_erased_ast_id);
        return UnpackedDim::Assoc(AssocIndex::Unsupported(ast_id));
    };

    // Check that the TypeSpec is simple (just a scalar keyword, no dims/signing)
    let has_extra_children = ts.children().any(|c| {
        matches!(
            c.kind(),
            SyntaxKind::PackedDimension | SyntaxKind::NameRef | SyntaxKind::QualifiedName
        )
    });

    if is_scalar_type_token(token.kind()) && !has_extra_children {
        let keyword = SmolStr::new(token.text());
        let ast_id = ast_id_map
            .erased_ast_id(ts)
            .unwrap_or_else(dummy_erased_ast_id);
        UnpackedDim::Assoc(AssocIndex::Type(AssocTypeRef { keyword, ast_id }))
    } else {
        let ast_id = ast_id_map
            .erased_ast_id(ts)
            .unwrap_or_else(dummy_erased_ast_id);
        UnpackedDim::Assoc(AssocIndex::Unsupported(ast_id))
    }
}

fn is_scalar_type_token(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::LogicKw
            | SyntaxKind::RegKw
            | SyntaxKind::BitKw
            | SyntaxKind::IntegerKw
            | SyntaxKind::IntKw
            | SyntaxKind::ShortintKw
            | SyntaxKind::LongintKw
            | SyntaxKind::ByteKw
            | SyntaxKind::TimeKw
            | SyntaxKind::RealtimeKw
            | SyntaxKind::RealKw
            | SyntaxKind::ShortRealKw
            | SyntaxKind::StringKw
            | SyntaxKind::ChandleKw
            | SyntaxKind::EventKw
            | SyntaxKind::VoidKw
    )
}

fn dummy_erased_ast_id() -> ErasedAstId {
    ErasedAstId::placeholder(lyra_source::FileId(u32::MAX))
}

fn expr_to_const_int(expr: &SyntaxNode, ast_id_map: &AstIdMap) -> ConstInt {
    match ast_id_map.erased_ast_id(expr) {
        Some(id) => ConstInt::Unevaluated(id),
        None => ConstInt::Error(ConstEvalError::Unsupported),
    }
}

/// Build the base type from a keyword + signing override + packed dims.
///
/// For integrals: applies signing and packed dims.
/// For non-integrals (real, string, etc.): returns as-is (signing/packed are inapplicable).
fn build_base_ty(base: Option<Ty>, signed_override: Option<bool>, packed: Vec<PackedDim>) -> Ty {
    match base {
        Some(Ty::Integral(mut i)) => {
            if let Some(s) = signed_override {
                i.signed = s;
            }
            i.packed = PackedDims::from(packed);
            Ty::Integral(i)
        }
        Some(other) => other,
        None => Ty::Error,
    }
}

fn keyword_to_ty(kind: SyntaxKind) -> Option<Ty> {
    let kw = keyword_to_integral_kw(kind);
    if let Some(kw) = kw {
        return Some(Ty::Integral(Integral {
            keyword: kw,
            signed: kw.default_signed(),
            packed: PackedDims::empty(),
        }));
    }
    match kind {
        SyntaxKind::RealKw => Some(Ty::Real(RealKw::Real)),
        SyntaxKind::ShortRealKw => Some(Ty::Real(RealKw::Short)),
        SyntaxKind::RealtimeKw => Some(Ty::Real(RealKw::Time)),
        SyntaxKind::StringKw => Some(Ty::String),
        SyntaxKind::ChandleKw => Some(Ty::Chandle),
        SyntaxKind::EventKw => Some(Ty::Event),
        SyntaxKind::VoidKw => Some(Ty::Void),
        _ => None,
    }
}

fn keyword_to_integral_kw(kind: SyntaxKind) -> Option<IntegralKw> {
    match kind {
        SyntaxKind::LogicKw => Some(IntegralKw::Logic),
        SyntaxKind::RegKw => Some(IntegralKw::Reg),
        SyntaxKind::BitKw => Some(IntegralKw::Bit),
        SyntaxKind::IntegerKw => Some(IntegralKw::Integer),
        SyntaxKind::IntKw => Some(IntegralKw::Int),
        SyntaxKind::ShortintKw => Some(IntegralKw::Shortint),
        SyntaxKind::LongintKw => Some(IntegralKw::Longint),
        SyntaxKind::ByteKw => Some(IntegralKw::Byte),
        SyntaxKind::TimeKw => Some(IntegralKw::Time),
        _ => None,
    }
}

fn net_keyword(node: &SyntaxNode) -> Option<NetKind> {
    for el in node.children_with_tokens() {
        if let SyntaxElement::Token(tok) = el {
            let nk = match tok.kind() {
                SyntaxKind::WireKw => Some(NetKind::Wire),
                SyntaxKind::TriKw => Some(NetKind::Tri),
                SyntaxKind::WandKw => Some(NetKind::Wand),
                SyntaxKind::WorKw => Some(NetKind::Wor),
                SyntaxKind::Tri0Kw => Some(NetKind::Tri0),
                SyntaxKind::Tri1Kw => Some(NetKind::Tri1),
                SyntaxKind::TriregKw => Some(NetKind::Trireg),
                SyntaxKind::Supply0Kw => Some(NetKind::Supply0),
                SyntaxKind::Supply1Kw => Some(NetKind::Supply1),
                SyntaxKind::UwireKw => Some(NetKind::Uwire),
                _ => None,
            };
            if nk.is_some() {
                return nk;
            }
        }
    }
    None
}

fn find_child(node: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxNode> {
    node.children().find(|c| c.kind() == kind)
}

#[cfg(test)]
mod tests {
    use lyra_source::FileId;

    use super::*;

    fn parse_source(src: &str) -> (lyra_parser::Parse, AstIdMap) {
        let tokens = lyra_lexer::lex(src);
        let pp = lyra_preprocess::preprocess_identity(FileId(0), &tokens, src);
        let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
        let map = AstIdMap::from_root(FileId(0), &parse.syntax());
        (parse, map)
    }

    // Find the first child node of a given kind within the first module body
    fn find_in_module(root: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxNode> {
        for child in root.children() {
            if child.kind() == SyntaxKind::ModuleDecl {
                for mc in child.children() {
                    if mc.kind() == SyntaxKind::ModuleBody {
                        for item in mc.children() {
                            if item.kind() == kind {
                                return Some(item);
                            }
                        }
                    }
                }
            }
        }
        None
    }

    fn first_declarator(container: &SyntaxNode) -> Option<SyntaxNode> {
        container
            .children()
            .find(|c| c.kind() == SyntaxKind::Declarator)
    }

    #[test]
    fn extract_logic_simple() {
        let (parse, map) = parse_source("module m; logic x; endmodule");
        let root = parse.syntax();
        let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
        let decl = first_declarator(&var_decl);
        let result = extract_type_from_container(&var_decl, decl.as_ref(), &map);
        assert_eq!(result, SymbolType::Value(Ty::simple_logic()));
    }

    #[test]
    fn extract_int() {
        let (parse, map) = parse_source("module m; int x; endmodule");
        let root = parse.syntax();
        let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
        let decl = first_declarator(&var_decl);
        let result = extract_type_from_container(&var_decl, decl.as_ref(), &map);
        assert_eq!(result, SymbolType::Value(Ty::int()));
    }

    #[test]
    fn extract_logic_packed_has_unevaluated() {
        let (parse, map) = parse_source("module m; logic [7:0] x; endmodule");
        let root = parse.syntax();
        let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
        let decl = first_declarator(&var_decl);
        let result = extract_type_from_container(&var_decl, decl.as_ref(), &map);
        if let SymbolType::Value(Ty::Integral(i)) = &result {
            assert_eq!(i.keyword, IntegralKw::Logic);
            assert!(!i.signed);
            assert_eq!(i.packed.len(), 1);
            assert!(matches!(i.packed[0].msb, ConstInt::Unevaluated(_)));
            assert!(matches!(i.packed[0].lsb, ConstInt::Unevaluated(_)));
        } else {
            panic!("expected Value(Integral), got {result:?}");
        }
    }

    #[test]
    fn extract_wire_simple() {
        let (parse, map) = parse_source("module m; wire w; endmodule");
        let root = parse.syntax();
        let net_decl = find_in_module(&root, SyntaxKind::NetDecl).expect("NetDecl");
        let decl = first_declarator(&net_decl);
        let result = extract_type_from_container(&net_decl, decl.as_ref(), &map);
        if let SymbolType::Net(nt) = &result {
            assert_eq!(nt.kind, NetKind::Wire);
            assert_eq!(nt.data, Ty::simple_logic());
        } else {
            panic!("expected Net, got {result:?}");
        }
    }

    #[test]
    fn extract_param_no_typespec_defaults_int() {
        let (parse, map) = parse_source("module m; parameter W = 8; endmodule");
        let root = parse.syntax();
        let param_decl = find_in_module(&root, SyntaxKind::ParamDecl).expect("ParamDecl");
        let result = extract_type_from_container(&param_decl, None, &map);
        assert_eq!(result, SymbolType::Value(Ty::int()));
    }

    #[test]
    fn extract_param_with_typespec() {
        let (parse, map) = parse_source("module m; parameter int W = 8; endmodule");
        let root = parse.syntax();
        let param_decl = find_in_module(&root, SyntaxKind::ParamDecl).expect("ParamDecl");
        let result = extract_type_from_container(&param_decl, None, &map);
        assert_eq!(result, SymbolType::Value(Ty::int()));
    }

    #[test]
    fn extract_port_with_typespec() {
        let (parse, map) = parse_source("module m(input logic [7:0] a); endmodule");
        let root = parse.syntax();
        // Find Port node inside PortList
        let module = root
            .children()
            .find(|c| c.kind() == SyntaxKind::ModuleDecl)
            .expect("module");
        let port_list = module
            .children()
            .find(|c| c.kind() == SyntaxKind::PortList)
            .expect("port list");
        let port = port_list
            .children()
            .find(|c| c.kind() == SyntaxKind::Port)
            .expect("port");
        let result = extract_type_from_container(&port, None, &map);
        if let SymbolType::Value(Ty::Integral(i)) = &result {
            assert_eq!(i.keyword, IntegralKw::Logic);
            assert_eq!(i.packed.len(), 1);
        } else {
            panic!("expected Value(Integral), got {result:?}");
        }
    }

    #[test]
    fn extract_port_no_typespec_defaults_logic() {
        let (parse, map) = parse_source("module m(input a); endmodule");
        let root = parse.syntax();
        let module = root
            .children()
            .find(|c| c.kind() == SyntaxKind::ModuleDecl)
            .expect("module");
        let port_list = module
            .children()
            .find(|c| c.kind() == SyntaxKind::PortList)
            .expect("port list");
        let port = port_list
            .children()
            .find(|c| c.kind() == SyntaxKind::Port)
            .expect("port");
        let result = extract_type_from_container(&port, None, &map);
        assert_eq!(result, SymbolType::Value(Ty::simple_logic()));
    }

    #[test]
    fn extract_typedef() {
        let (parse, map) = parse_source("module m; typedef logic [7:0] byte_t; endmodule");
        let root = parse.syntax();
        let td = find_in_module(&root, SyntaxKind::TypedefDecl).expect("TypedefDecl");
        let result = extract_type_from_container(&td, None, &map);
        if let SymbolType::TypeAlias(Ty::Integral(i)) = &result {
            assert_eq!(i.keyword, IntegralKw::Logic);
            assert_eq!(i.packed.len(), 1);
        } else {
            panic!("expected TypeAlias(Integral), got {result:?}");
        }
    }

    #[test]
    fn normalize_resolves_unevaluated() {
        let (parse, map) = parse_source("module m; logic [7:0] x; endmodule");
        let root = parse.syntax();
        let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
        let decl = first_declarator(&var_decl);
        let raw = extract_type_from_container(&var_decl, decl.as_ref(), &map);

        // Mock evaluator: use the expression AST offset to distinguish msb/lsb.
        // The first expression (msb=7) has a smaller offset than the second (lsb=0).
        let call_count = std::sync::atomic::AtomicU32::new(0);
        let eval = |_id: lyra_ast::ErasedAstId| -> ConstInt {
            let n = call_count.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            if n.is_multiple_of(2) {
                ConstInt::Known(7)
            } else {
                ConstInt::Known(0)
            }
        };
        let normalized = normalize_symbol_type(&raw, &eval);
        if let SymbolType::Value(Ty::Integral(i)) = &normalized {
            assert_eq!(i.packed[0].msb, ConstInt::Known(7));
            assert_eq!(i.packed[0].lsb, ConstInt::Known(0));
        } else {
            panic!("expected Value(Integral), got {normalized:?}");
        }
    }

    #[test]
    fn typespec_name_ref_detects_user_type() {
        let (parse, _map) = parse_source("module m; byte_t x; endmodule");
        let root = parse.syntax();
        let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
        let typespec = find_child(&var_decl, SyntaxKind::TypeSpec).expect("TypeSpec");
        assert!(typespec_name_ref(&typespec).is_some());
    }

    #[test]
    fn typespec_name_ref_none_for_builtin() {
        let (parse, _map) = parse_source("module m; logic x; endmodule");
        let root = parse.syntax();
        let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
        let typespec = find_child(&var_decl, SyntaxKind::TypeSpec).expect("TypeSpec");
        assert!(typespec_name_ref(&typespec).is_none());
    }

    #[test]
    fn user_type_ref_dotted_name_tree_shape() {
        let (parse, _map) = parse_source("module m; my_bus.master v; endmodule");
        let root = parse.syntax();
        let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
        let typespec = find_child(&var_decl, SyntaxKind::TypeSpec).expect("TypeSpec");

        // TypeSpec should contain a DottedName child
        let dotted = typespec
            .children()
            .find(|c| c.kind() == SyntaxKind::DottedName)
            .expect("DottedName child");

        // DottedName should contain a NameRef child (interface name)
        let name_ref = dotted
            .children()
            .find(|c| c.kind() == SyntaxKind::NameRef)
            .expect("NameRef child");
        let iface_ident = name_ref
            .children_with_tokens()
            .filter_map(|el| el.into_token())
            .find(|t| t.kind() == SyntaxKind::Ident)
            .expect("interface ident");
        assert_eq!(iface_ident.text(), "my_bus");

        // DottedName should have a direct Ident token child (modport name)
        let mp_ident = dotted
            .children_with_tokens()
            .filter_map(|el| el.into_token())
            .find(|t| t.kind() == SyntaxKind::Ident)
            .expect("modport ident");
        assert_eq!(mp_ident.text(), "master");

        // user_type_ref should return InterfaceModport
        let utr = user_type_ref(&typespec).expect("should extract UserTypeRef");
        match utr {
            UserTypeRef::InterfaceModport { modport_name, .. } => {
                assert_eq!(modport_name.as_str(), "master");
            }
            other => panic!("expected InterfaceModport, got {other:?}"),
        }
    }

    #[test]
    fn user_type_ref_simple_name() {
        let (parse, _map) = parse_source("module m; byte_t x; endmodule");
        let root = parse.syntax();
        let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
        let typespec = find_child(&var_decl, SyntaxKind::TypeSpec).expect("TypeSpec");
        let utr = user_type_ref(&typespec).expect("should extract UserTypeRef");
        assert!(matches!(utr, UserTypeRef::Simple(_)));
    }

    #[test]
    fn user_type_ref_none_for_keyword() {
        let (parse, _map) = parse_source("module m; logic x; endmodule");
        let root = parse.syntax();
        let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
        let typespec = find_child(&var_decl, SyntaxKind::TypeSpec).expect("TypeSpec");
        assert!(user_type_ref(&typespec).is_none());
    }
}
