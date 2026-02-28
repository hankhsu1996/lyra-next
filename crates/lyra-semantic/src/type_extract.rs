use lyra_ast::{
    AstIdMap, AstNode, Declarator, NameRef, NetDecl, PackedDimension, ParamDecl, Port,
    QualifiedName, Signing, TypeDeclSite, TypeNameRef, TypeSpec, TypeSpecKeyword, TypedefDecl,
    UnpackedDimKind, UnpackedDimension, VarDecl,
};
use lyra_parser::SyntaxNode;
use smol_str::SmolStr;

use crate::types::{
    AssocIndex, ConstEvalError, ConstInt, Integral, IntegralKw, NetKind, NetType, PackedDim,
    PackedDims, RealKw, SymbolType, SymbolTypeError, Ty, UnpackedDim, wrap_unpacked,
};

/// Extract a `SymbolType` from a typed declaration container.
///
/// `declarator` is the typed `Declarator` (for unpacked dims). Pass `None`
/// for `Port` and `TypedefDecl` nodes where the declarator is not separate.
///
/// This function never calls `parent()` or `ancestors()` on any node.
pub fn extract_type_from_container(
    container: &TypeDeclSite,
    declarator: Option<&Declarator>,
    ast_id_map: &AstIdMap,
) -> SymbolType {
    match container {
        TypeDeclSite::VarDecl(v) => extract_var_decl(v, declarator, ast_id_map),
        TypeDeclSite::NetDecl(n) => extract_net_decl(n, declarator, ast_id_map),
        TypeDeclSite::ParamDecl(p) => extract_param_decl(p, ast_id_map),
        TypeDeclSite::Port(p) => extract_port(p, ast_id_map),
        TypeDeclSite::TypedefDecl(td) => extract_typedef_decl(td, ast_id_map),
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
    var: &VarDecl,
    declarator: Option<&Declarator>,
    ast_id_map: &AstIdMap,
) -> SymbolType {
    let Some(typespec) = var.type_spec() else {
        return SymbolType::Value(Ty::Error);
    };
    if typespec.type_name_ref().is_some() {
        return SymbolType::Value(Ty::Error);
    }
    let (base_ty, signed_override) = extract_typespec_base(&typespec);
    let packed = extract_packed_dims(&typespec, ast_id_map);
    let unpacked = declarator
        .map(|d| extract_unpacked_dims_from_declarator(d, ast_id_map))
        .unwrap_or_default();
    let ty = build_base_ty(base_ty, signed_override, packed);
    SymbolType::Value(wrap_unpacked(ty, &unpacked))
}

fn extract_net_decl(
    net: &NetDecl,
    declarator: Option<&Declarator>,
    ast_id_map: &AstIdMap,
) -> SymbolType {
    let net_kind = net
        .type_spec()
        .and_then(|ts| net_keyword_from_typespec(&ts));
    let Some(net_kind) = net_kind else {
        return SymbolType::Error(SymbolTypeError::UnsupportedSymbolKind);
    };

    let Some(typespec) = net.type_spec() else {
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
    let (_base, signing) = extract_typespec_base(&typespec);
    let packed = extract_packed_dims(&typespec, ast_id_map);
    let unpacked = declarator
        .map(|d| extract_unpacked_dims_from_declarator(d, ast_id_map))
        .unwrap_or_default();
    let signed = signing.map_or(IntegralKw::Logic.default_signed(), |s| s.is_signed());
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

fn extract_param_decl(param: &ParamDecl, ast_id_map: &AstIdMap) -> SymbolType {
    if param.type_keyword().is_some() {
        return SymbolType::Error(SymbolTypeError::TypeParameterUnsupported);
    }

    match param.type_spec() {
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

fn extract_port(port: &Port, ast_id_map: &AstIdMap) -> SymbolType {
    match port.type_spec() {
        Some(ts) => {
            if ts.type_name_ref().is_some() {
                return SymbolType::Value(Ty::Error);
            }
            let (base_ty, signed_override) = extract_typespec_base(&ts);
            if base_ty.is_none() {
                return SymbolType::Error(SymbolTypeError::PortTypeMissing);
            }
            let packed = extract_packed_dims(&ts, ast_id_map);
            let unpacked = extract_unpacked_dims_from_port(port, ast_id_map);
            let ty = build_base_ty(base_ty, signed_override, packed);
            SymbolType::Value(wrap_unpacked(ty, &unpacked))
        }
        None => SymbolType::Value(Ty::simple_logic()),
    }
}

fn extract_typedef_decl(td: &TypedefDecl, ast_id_map: &AstIdMap) -> SymbolType {
    let Some(ts) = td.type_spec() else {
        return SymbolType::TypeAlias(Ty::Error);
    };
    if ts.type_name_ref().is_some() {
        return SymbolType::TypeAlias(Ty::Error);
    }
    let (base_ty, signed_override) = extract_typespec_base(&ts);
    let packed = extract_packed_dims(&ts, ast_id_map);
    let unpacked = extract_unpacked_dims_from_typedef(td, ast_id_map);
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

/// Extract the base type keyword and optional signing override from a `TypeSpec`.
fn extract_typespec_base(typespec: &TypeSpec) -> (Option<Ty>, Option<Signing>) {
    let base_ty = typespec.type_keyword().and_then(keyword_to_ty);
    let signing = typespec.signing();
    (base_ty, signing)
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
    let Some(kw) = ts.type_keyword() else {
        return UnpackedDim::Assoc(AssocIndex::Typed(Box::new(Ty::Error)));
    };

    if !kw.is_data_type() {
        return UnpackedDim::Assoc(AssocIndex::Typed(Box::new(Ty::Error)));
    }

    let has_extra = ts.type_name_ref().is_some() || ts.packed_dimensions().next().is_some();

    if has_extra {
        UnpackedDim::Assoc(AssocIndex::Typed(Box::new(Ty::Error)))
    } else {
        let key_ty = keyword_to_ty(kw).unwrap_or(Ty::Error);
        UnpackedDim::Assoc(AssocIndex::Typed(Box::new(key_ty)))
    }
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
fn build_base_ty(base: Option<Ty>, signing: Option<Signing>, packed: Vec<PackedDim>) -> Ty {
    match base {
        Some(Ty::Integral(mut i)) => {
            if let Some(s) = signing {
                i.signed = s.is_signed();
            }
            i.packed = PackedDims::from(packed);
            Ty::Integral(i)
        }
        Some(other) => other,
        None => Ty::Error,
    }
}

fn keyword_to_ty(kw: TypeSpecKeyword) -> Option<Ty> {
    if let Some(ikw) = keyword_to_integral_kw(kw) {
        return Some(Ty::Integral(Integral {
            keyword: ikw,
            signed: ikw.default_signed(),
            packed: PackedDims::empty(),
        }));
    }
    match kw {
        TypeSpecKeyword::Real => Some(Ty::Real(RealKw::Real)),
        TypeSpecKeyword::ShortReal => Some(Ty::Real(RealKw::Short)),
        TypeSpecKeyword::Realtime => Some(Ty::Real(RealKw::Time)),
        TypeSpecKeyword::String => Some(Ty::String),
        TypeSpecKeyword::Chandle => Some(Ty::Chandle),
        TypeSpecKeyword::Event => Some(Ty::Event),
        TypeSpecKeyword::Void => Some(Ty::Void),
        _ => None,
    }
}

fn keyword_to_integral_kw(kw: TypeSpecKeyword) -> Option<IntegralKw> {
    match kw {
        TypeSpecKeyword::Logic => Some(IntegralKw::Logic),
        TypeSpecKeyword::Reg => Some(IntegralKw::Reg),
        TypeSpecKeyword::Bit => Some(IntegralKw::Bit),
        TypeSpecKeyword::Integer => Some(IntegralKw::Integer),
        TypeSpecKeyword::Int => Some(IntegralKw::Int),
        TypeSpecKeyword::Shortint => Some(IntegralKw::Shortint),
        TypeSpecKeyword::Longint => Some(IntegralKw::Longint),
        TypeSpecKeyword::Byte => Some(IntegralKw::Byte),
        TypeSpecKeyword::Time => Some(IntegralKw::Time),
        _ => None,
    }
}

fn net_keyword_from_typespec(typespec: &TypeSpec) -> Option<NetKind> {
    match typespec.type_keyword()? {
        TypeSpecKeyword::Wire => Some(NetKind::Wire),
        TypeSpecKeyword::Tri => Some(NetKind::Tri),
        TypeSpecKeyword::Wand => Some(NetKind::Wand),
        TypeSpecKeyword::Wor => Some(NetKind::Wor),
        TypeSpecKeyword::Tri0 => Some(NetKind::Tri0),
        TypeSpecKeyword::Tri1 => Some(NetKind::Tri1),
        TypeSpecKeyword::Trireg => Some(NetKind::Trireg),
        TypeSpecKeyword::Supply0 => Some(NetKind::Supply0),
        TypeSpecKeyword::Supply1 => Some(NetKind::Supply1),
        TypeSpecKeyword::Uwire => Some(NetKind::Uwire),
        _ => None,
    }
}
