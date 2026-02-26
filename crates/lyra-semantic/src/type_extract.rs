use lyra_ast::{
    AstIdMap, AstNode, Declarator, NameRef, NetDecl, PackedDimension, ParamDecl, Port,
    QualifiedName, TypeNameRef, TypeSpec, TypedefDecl, UnpackedDimKind, UnpackedDimension, VarDecl,
};
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;
use lyra_source::TextRange;
use smol_str::SmolStr;

use crate::types::{
    AssocIndex, ConstEvalError, ConstInt, Integral, IntegralKw, NetKind, NetType, PackedDim,
    PackedDims, RealKw, SymbolType, SymbolTypeError, Ty, UnpackedDim, wrap_unpacked,
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
    /// The AST node whose `Site` has a resolution in the resolve index.
    pub fn resolve_node(&self) -> &SyntaxNode {
        match self {
            Self::Simple(nr) | Self::InterfaceModport { iface: nr, .. } => nr.syntax(),
            Self::Qualified(qn) => qn.syntax(),
        }
    }
}

/// Extract the user-defined type reference from a `TypeSpec`, if any.
pub fn user_type_ref(typespec: &SyntaxNode) -> Option<UserTypeRef> {
    let ts = TypeSpec::cast(typespec.clone())?;
    match ts.type_name_ref()? {
        TypeNameRef::Simple(nr) => Some(UserTypeRef::Simple(nr)),
        TypeNameRef::Qualified(qn) => Some(UserTypeRef::Qualified(qn)),
        TypeNameRef::Dotted(dn) => {
            let nr = dn.interface_ref()?;
            let mp_token = dn.modport_ident()?;
            Some(UserTypeRef::InterfaceModport {
                iface: nr,
                modport_name: SmolStr::new(mp_token.text()),
                modport_range: mp_token.text_range(),
            })
        }
    }
}

/// Normalize all `ConstInt` values inside a `SymbolType`.
///
/// Transforms `Unevaluated` dims to `Known`/`Error` by calling `eval`.
/// Never changes keyword, signedness, structure, or `NetKind`.
pub fn normalize_symbol_type(
    st: &SymbolType,
    eval: &dyn Fn(crate::Site) -> ConstInt,
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
pub fn normalize_ty(ty: &Ty, eval: &dyn Fn(crate::Site) -> ConstInt) -> Ty {
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

fn normalize_packed_dim(dim: &PackedDim, eval: &dyn Fn(crate::Site) -> ConstInt) -> PackedDim {
    PackedDim {
        msb: normalize_const_int(&dim.msb, eval),
        lsb: normalize_const_int(&dim.lsb, eval),
    }
}

fn normalize_unpacked_dim(
    dim: &UnpackedDim,
    eval: &dyn Fn(crate::Site) -> ConstInt,
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

fn normalize_const_int(c: &ConstInt, eval: &dyn Fn(crate::Site) -> ConstInt) -> ConstInt {
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
    let var = VarDecl::cast(container.clone());
    let typespec = var.as_ref().and_then(|v| v.type_spec());
    let Some(typespec) = typespec else {
        return SymbolType::Value(Ty::Error);
    };
    if typespec.type_name_ref().is_some() {
        return SymbolType::Value(Ty::Error);
    }
    let (base_ty, signed_override) = extract_typespec_base(&typespec);
    let packed = extract_packed_dims(&typespec, ast_id_map);
    let unpacked = declarator
        .and_then(|d| Declarator::cast(d.clone()))
        .map(|d| extract_unpacked_dims_from_declarator(&d, ast_id_map))
        .unwrap_or_default();
    let ty = build_base_ty(base_ty, signed_override, packed);
    SymbolType::Value(wrap_unpacked(ty, &unpacked))
}

fn extract_net_decl(
    container: &SyntaxNode,
    declarator: Option<&SyntaxNode>,
    ast_id_map: &AstIdMap,
) -> SymbolType {
    let net = NetDecl::cast(container.clone());
    let net_kind = net.as_ref().and_then(|n| {
        let ts = n.type_spec()?;
        net_keyword_from_typespec(&ts)
    });
    let Some(net_kind) = net_kind else {
        return SymbolType::Error(SymbolTypeError::UnsupportedSymbolKind);
    };

    let typespec = net.as_ref().and_then(|n| n.type_spec());
    let Some(typespec) = typespec else {
        return SymbolType::Net(NetType {
            kind: net_kind,
            data: Ty::simple_logic(),
        });
    };
    if typespec.type_name_ref().is_some() {
        return SymbolType::Net(NetType {
            kind: net_kind,
            data: Ty::Error,
        });
    }
    let (_base, signed_override) = extract_typespec_base(&typespec);
    let packed = extract_packed_dims(&typespec, ast_id_map);
    let unpacked = declarator
        .and_then(|d| Declarator::cast(d.clone()))
        .map(|d| extract_unpacked_dims_from_declarator(&d, ast_id_map))
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
    let param = ParamDecl::cast(container.clone());
    if let Some(ref p) = param
        && p.type_keyword().is_some()
    {
        return SymbolType::Error(SymbolTypeError::TypeParameterUnsupported);
    }

    let typespec = param.as_ref().and_then(|p| p.type_spec());
    match typespec {
        Some(ts) => {
            if ts.type_name_ref().is_some() {
                return SymbolType::Value(Ty::Error);
            }
            let (base_ty, signed_override) = extract_typespec_base(&ts);
            let packed = extract_packed_dims(&ts, ast_id_map);
            SymbolType::Value(build_base_ty(base_ty, signed_override, packed))
        }
        None => SymbolType::Value(Ty::int()),
    }
}

fn extract_port(container: &SyntaxNode, ast_id_map: &AstIdMap) -> SymbolType {
    let port = Port::cast(container.clone());
    let typespec = port.as_ref().and_then(|p| p.type_spec());
    match typespec {
        Some(ts) => {
            if ts.type_name_ref().is_some() {
                return SymbolType::Value(Ty::Error);
            }
            let (base_ty, signed_override) = extract_typespec_base(&ts);
            if base_ty.is_none() {
                return SymbolType::Error(SymbolTypeError::PortTypeMissing);
            }
            let packed = extract_packed_dims(&ts, ast_id_map);
            let unpacked = port
                .as_ref()
                .map(|p| extract_unpacked_dims_from_port(p, ast_id_map))
                .unwrap_or_default();
            let ty = build_base_ty(base_ty, signed_override, packed);
            SymbolType::Value(wrap_unpacked(ty, &unpacked))
        }
        None => SymbolType::Value(Ty::simple_logic()),
    }
}

fn extract_typedef_decl(container: &SyntaxNode, ast_id_map: &AstIdMap) -> SymbolType {
    let td = TypedefDecl::cast(container.clone());
    let typespec = td.as_ref().and_then(|t| t.type_spec());
    let Some(ts) = typespec else {
        return SymbolType::TypeAlias(Ty::Error);
    };
    if ts.type_name_ref().is_some() {
        return SymbolType::TypeAlias(Ty::Error);
    }
    let (base_ty, signed_override) = extract_typespec_base(&ts);
    let packed = extract_packed_dims(&ts, ast_id_map);
    let unpacked = td
        .as_ref()
        .map(|t| extract_unpacked_dims_from_typedef(t, ast_id_map))
        .unwrap_or_default();
    let ty = build_base_ty(base_ty, signed_override, packed);
    SymbolType::TypeAlias(wrap_unpacked(ty, &unpacked))
}

/// Extract the base `Ty` from a `TypeSpec` node, including signing and packed dims.
///
/// Used by callable signature extraction (in `lyra-db`) to type TF port parameters
/// without going through the full `extract_type_from_container` path.
pub fn extract_base_ty_from_typespec(typespec: &SyntaxNode, ast_id_map: &AstIdMap) -> Ty {
    let Some(ts) = TypeSpec::cast(typespec.clone()) else {
        return Ty::Error;
    };
    let (base_ty, signed_override) = extract_typespec_base(&ts);
    let packed = extract_packed_dims(&ts, ast_id_map);
    build_base_ty(base_ty, signed_override, packed)
}

/// Extract the base type keyword and optional signed/unsigned override from a `TypeSpec`.
fn extract_typespec_base(typespec: &TypeSpec) -> (Option<Ty>, Option<bool>) {
    let base_ty = typespec.keyword().and_then(|tok| keyword_to_ty(tok.kind()));
    let signed_override = typespec
        .signed_token()
        .map(|tok| tok.kind() == SyntaxKind::SignedKw);
    (base_ty, signed_override)
}

fn extract_packed_dims(typespec: &TypeSpec, ast_id_map: &AstIdMap) -> Vec<PackedDim> {
    typespec
        .packed_dimensions()
        .map(|pd| extract_single_packed_dim(&pd, ast_id_map))
        .collect()
}

fn extract_single_packed_dim(dim: &PackedDimension, ast_id_map: &AstIdMap) -> PackedDim {
    match (dim.msb(), dim.lsb()) {
        (Some(msb), Some(lsb)) => PackedDim {
            msb: expr_to_const_int(msb.syntax(), ast_id_map),
            lsb: expr_to_const_int(lsb.syntax(), ast_id_map),
        },
        _ => PackedDim {
            msb: ConstInt::Error(ConstEvalError::Unsupported),
            lsb: ConstInt::Error(ConstEvalError::Unsupported),
        },
    }
}

/// Extract a single unpacked dimension using the typed classifier.
pub fn extract_unpacked_dim(dim: &UnpackedDimension, ast_id_map: &AstIdMap) -> UnpackedDim {
    match dim.classify() {
        UnpackedDimKind::Wildcard => UnpackedDim::Assoc(AssocIndex::Wildcard),
        UnpackedDimKind::Queue { bound } => UnpackedDim::Queue {
            bound: bound.map(|e| expr_to_const_int(e.syntax(), ast_id_map)),
        },
        UnpackedDimKind::Assoc => {
            let ts = dim.assoc_type_spec();
            match ts {
                Some(ts) => extract_assoc_from_typespec(&ts, ast_id_map),
                None => UnpackedDim::Assoc(AssocIndex::Typed(Box::new(Ty::Error))),
            }
        }
        UnpackedDimKind::Range { msb, lsb } => UnpackedDim::Range {
            msb: expr_to_const_int(msb.syntax(), ast_id_map),
            lsb: expr_to_const_int(lsb.syntax(), ast_id_map),
        },
        UnpackedDimKind::Size { expr } => {
            UnpackedDim::Size(expr_to_const_int(expr.syntax(), ast_id_map))
        }
        UnpackedDimKind::Unsized => UnpackedDim::Unsized,
    }
}

/// Extract unpacked dimensions from a Declarator node.
fn extract_unpacked_dims_from_declarator(
    decl: &Declarator,
    ast_id_map: &AstIdMap,
) -> Vec<UnpackedDim> {
    decl.unpacked_dimensions()
        .map(|d| extract_unpacked_dim(&d, ast_id_map))
        .collect()
}

/// Extract unpacked dimensions from a Port node.
fn extract_unpacked_dims_from_port(port: &Port, ast_id_map: &AstIdMap) -> Vec<UnpackedDim> {
    port.unpacked_dimensions()
        .map(|d| extract_unpacked_dim(&d, ast_id_map))
        .collect()
}

/// Extract unpacked dimensions from a `TypedefDecl` node.
fn extract_unpacked_dims_from_typedef(td: &TypedefDecl, ast_id_map: &AstIdMap) -> Vec<UnpackedDim> {
    td.unpacked_dimensions()
        .map(|d| extract_unpacked_dim(&d, ast_id_map))
        .collect()
}

fn extract_assoc_from_typespec(ts: &TypeSpec, _ast_id_map: &AstIdMap) -> UnpackedDim {
    let Some(token) = ts.keyword() else {
        return UnpackedDim::Assoc(AssocIndex::Typed(Box::new(Ty::Error)));
    };

    let has_extra = ts.type_name_ref().is_some() || ts.packed_dimensions().next().is_some();

    if is_scalar_type_token(token.kind()) && !has_extra {
        let key_ty = keyword_to_ty(token.kind()).unwrap_or(Ty::Error);
        UnpackedDim::Assoc(AssocIndex::Typed(Box::new(key_ty)))
    } else {
        UnpackedDim::Assoc(AssocIndex::Typed(Box::new(Ty::Error)))
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

fn net_keyword_from_typespec(typespec: &TypeSpec) -> Option<NetKind> {
    let tok = typespec.keyword()?;
    match tok.kind() {
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
    }
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

    fn find_in_module(root: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxNode> {
        use lyra_ast::SourceFile;
        let sf = SourceFile::cast(root.clone())?;
        let module = sf.modules().next()?;
        let body = module.body()?;
        let body_node: &SyntaxNode = body.syntax();
        body_node.children().find(|c| c.kind() == kind)
    }

    fn first_declarator(container: &SyntaxNode) -> Option<SyntaxNode> {
        lyra_ast::VarDecl::cast(container.clone())
            .and_then(|v| v.declarators().next())
            .or_else(|| {
                lyra_ast::NetDecl::cast(container.clone()).and_then(|n| n.declarators().next())
            })
            .map(|d| d.syntax().clone())
    }

    fn find_typespec(container: &SyntaxNode) -> Option<SyntaxNode> {
        match container.kind() {
            SyntaxKind::VarDecl => VarDecl::cast(container.clone())?
                .type_spec()
                .map(|ts| ts.syntax().clone()),
            SyntaxKind::NetDecl => NetDecl::cast(container.clone())?
                .type_spec()
                .map(|ts| ts.syntax().clone()),
            SyntaxKind::ParamDecl => ParamDecl::cast(container.clone())?
                .type_spec()
                .map(|ts| ts.syntax().clone()),
            _ => None,
        }
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

    fn find_first_port(root: &SyntaxNode) -> SyntaxNode {
        use lyra_ast::SourceFile;
        let sf = SourceFile::cast(root.clone()).expect("SourceFile");
        let module = sf.modules().next().expect("module");
        let port_list = module.port_list().expect("port list");
        let port = port_list.ports().next().expect("port");
        port.syntax().clone()
    }

    #[test]
    fn extract_port_with_typespec() {
        let (parse, map) = parse_source("module m(input logic [7:0] a); endmodule");
        let root = parse.syntax();
        let port = find_first_port(&root);
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
        let port = find_first_port(&root);
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

        let call_count = std::sync::atomic::AtomicU32::new(0);
        let eval = |_id: crate::Site| -> ConstInt {
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
    fn type_name_ref_detects_user_type() {
        let (parse, _map) = parse_source("module m; byte_t x; endmodule");
        let root = parse.syntax();
        let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
        let typespec = find_typespec(&var_decl).expect("TypeSpec");
        let ts = TypeSpec::cast(typespec).expect("cast");
        assert!(ts.type_name_ref().is_some());
    }

    #[test]
    fn type_name_ref_none_for_builtin() {
        let (parse, _map) = parse_source("module m; logic x; endmodule");
        let root = parse.syntax();
        let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
        let typespec = find_typespec(&var_decl).expect("TypeSpec");
        let ts = TypeSpec::cast(typespec).expect("cast");
        assert!(ts.type_name_ref().is_none());
    }

    #[test]
    fn user_type_ref_dotted_name() {
        let (parse, _map) = parse_source("module m; my_bus.master v; endmodule");
        let root = parse.syntax();
        let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
        let typespec = find_typespec(&var_decl).expect("TypeSpec");

        // Verify via typed accessor
        let ts = TypeSpec::cast(typespec.clone()).expect("cast");
        let tnr = ts.type_name_ref().expect("type_name_ref");
        let dn = match tnr {
            TypeNameRef::Dotted(d) => d,
            other => panic!("expected Dotted, got {other:?}"),
        };
        let iface = dn.interface_ref().expect("interface_ref");
        assert_eq!(iface.ident().expect("ident").text(), "my_bus");
        assert_eq!(dn.modport_ident().expect("modport").text(), "master");

        // Verify user_type_ref produces InterfaceModport
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
        let typespec = find_typespec(&var_decl).expect("TypeSpec");
        let utr = user_type_ref(&typespec).expect("should extract UserTypeRef");
        assert!(matches!(utr, UserTypeRef::Simple(_)));
    }

    #[test]
    fn user_type_ref_none_for_keyword() {
        let (parse, _map) = parse_source("module m; logic x; endmodule");
        let root = parse.syntax();
        let var_decl = find_in_module(&root, SyntaxKind::VarDecl).expect("VarDecl");
        let typespec = find_typespec(&var_decl).expect("TypeSpec");
        assert!(user_type_ref(&typespec).is_none());
    }
}
