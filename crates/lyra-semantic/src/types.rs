use lyra_ast::ErasedAstId;
use smol_str::SmolStr;

use crate::aggregate::{EnumId, StructId};

/// A constant integer value, used for dimension bounds and widths.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstInt {
    Known(i64),
    Unevaluated(ErasedAstId),
    Error(ConstEvalError),
}

/// Reasons a constant expression evaluation can fail.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConstEvalError {
    NonConstant,
    DivideByZero,
    InvalidArgument,
    Overflow,
    Unresolved,
    Cycle,
    Unsupported,
}

/// A packed dimension with msb and lsb bounds, e.g. `[7:0]`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackedDim {
    pub msb: ConstInt,
    pub lsb: ConstInt,
}

impl PackedDim {
    pub fn try_width(&self) -> Option<u32> {
        match (&self.msb, &self.lsb) {
            (ConstInt::Known(m), ConstInt::Known(l)) => range_width(*m, *l),
            _ => None,
        }
    }
}

/// An unpacked dimension: either a range `[msb:lsb]` or a size `[n]`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnpackedDim {
    Range { msb: ConstInt, lsb: ConstInt },
    Size(ConstInt),
}

impl UnpackedDim {
    pub fn try_size(&self) -> Option<u32> {
        match self {
            UnpackedDim::Range { msb, lsb } => match (msb, lsb) {
                (ConstInt::Known(m), ConstInt::Known(l)) => range_width(*m, *l),
                _ => None,
            },
            UnpackedDim::Size(c) => match c {
                ConstInt::Known(v) => u32::try_from(*v).ok(),
                _ => None,
            },
        }
    }
}

/// The keyword that produced an integral type, preserving LRM identity.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntegralKw {
    Logic,
    Reg,
    Bit,
    Integer,
    Int,
    Shortint,
    Longint,
    Byte,
    Time,
}

impl IntegralKw {
    pub fn four_state(self) -> bool {
        matches!(self, Self::Logic | Self::Reg | Self::Integer | Self::Time)
    }

    pub fn default_signed(self) -> bool {
        matches!(
            self,
            Self::Integer | Self::Int | Self::Shortint | Self::Longint | Self::Byte
        )
    }

    pub fn base_width(self) -> u32 {
        match self {
            Self::Logic | Self::Reg | Self::Bit => 1,
            Self::Integer | Self::Int => 32,
            Self::Shortint => 16,
            Self::Longint | Self::Time => 64,
            Self::Byte => 8,
        }
    }

    fn keyword_str(self) -> &'static str {
        match self {
            Self::Logic => "logic",
            Self::Reg => "reg",
            Self::Bit => "bit",
            Self::Integer => "integer",
            Self::Int => "int",
            Self::Shortint => "shortint",
            Self::Longint => "longint",
            Self::Byte => "byte",
            Self::Time => "time",
        }
    }
}

/// An integral type: keyword identity, signedness, and packed dimensions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Integral {
    pub keyword: IntegralKw,
    pub signed: bool,
    pub packed: Box<[PackedDim]>,
}

impl Integral {
    pub fn try_packed_width(&self) -> Option<u32> {
        let mut width = self.keyword.base_width();
        for dim in &self.packed {
            width = width.checked_mul(dim.try_width()?)?;
        }
        Some(width)
    }

    fn pretty(&self) -> SmolStr {
        let mut s = String::from(self.keyword.keyword_str());
        if self.signed != self.keyword.default_signed() {
            if self.signed {
                s.push_str(" signed");
            } else {
                s.push_str(" unsigned");
            }
        }
        for dim in &self.packed {
            s.push(' ');
            fmt_packed_dim(&mut s, dim);
        }
        SmolStr::new(s)
    }
}

/// A real (floating-point) type keyword.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RealKw {
    Real,
    Short,
    Time,
}

impl RealKw {
    fn keyword_str(self) -> &'static str {
        match self {
            Self::Real => "real",
            Self::Short => "shortreal",
            Self::Time => "realtime",
        }
    }
}

/// The semantic type representation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Integral(Integral),
    Real(RealKw),
    Enum(EnumId),
    Struct(StructId),
    Array { elem: Box<Ty>, dim: UnpackedDim },
    String,
    Chandle,
    Event,
    Void,
    Error,
}

impl Ty {
    pub fn logic(packed: Box<[PackedDim]>, signed: bool) -> Self {
        Self::Integral(Integral {
            keyword: IntegralKw::Logic,
            signed,
            packed,
        })
    }

    pub fn reg(packed: Box<[PackedDim]>, signed: bool) -> Self {
        Self::Integral(Integral {
            keyword: IntegralKw::Reg,
            signed,
            packed,
        })
    }

    pub fn bit(packed: Box<[PackedDim]>, signed: bool) -> Self {
        Self::Integral(Integral {
            keyword: IntegralKw::Bit,
            signed,
            packed,
        })
    }

    pub fn simple_logic() -> Self {
        Self::logic(Box::new([]), false)
    }

    pub fn int() -> Self {
        Self::Integral(Integral {
            keyword: IntegralKw::Int,
            signed: IntegralKw::Int.default_signed(),
            packed: Box::new([]),
        })
    }

    pub fn integer() -> Self {
        Self::Integral(Integral {
            keyword: IntegralKw::Integer,
            signed: IntegralKw::Integer.default_signed(),
            packed: Box::new([]),
        })
    }

    pub fn byte() -> Self {
        Self::Integral(Integral {
            keyword: IntegralKw::Byte,
            signed: IntegralKw::Byte.default_signed(),
            packed: Box::new([]),
        })
    }

    pub fn shortint() -> Self {
        Self::Integral(Integral {
            keyword: IntegralKw::Shortint,
            signed: IntegralKw::Shortint.default_signed(),
            packed: Box::new([]),
        })
    }

    pub fn longint() -> Self {
        Self::Integral(Integral {
            keyword: IntegralKw::Longint,
            signed: IntegralKw::Longint.default_signed(),
            packed: Box::new([]),
        })
    }

    pub fn time() -> Self {
        Self::Integral(Integral {
            keyword: IntegralKw::Time,
            signed: IntegralKw::Time.default_signed(),
            packed: Box::new([]),
        })
    }

    /// Remove the outermost unpacked dimension and return the element type.
    pub fn peel_unpacked_dim(&self) -> Option<Ty> {
        match self {
            Self::Array { elem, .. } => Some(elem.as_ref().clone()),
            _ => None,
        }
    }

    pub fn pretty(&self) -> SmolStr {
        match self {
            Self::Array { .. } => {
                let (base, dims) = collect_array_dims(self);
                let mut s = String::from(base.pretty().as_str());
                for dim in &dims {
                    s.push(' ');
                    fmt_unpacked_dim(&mut s, dim);
                }
                SmolStr::new(s)
            }
            Self::Integral(i) => i.pretty(),
            Self::Real(r) => SmolStr::new_static(r.keyword_str()),
            Self::Enum(_) => SmolStr::new_static("enum"),
            Self::Struct(_) => SmolStr::new_static("struct"),
            Self::String => SmolStr::new_static("string"),
            Self::Chandle => SmolStr::new_static("chandle"),
            Self::Event => SmolStr::new_static("event"),
            Self::Void => SmolStr::new_static("void"),
            Self::Error => SmolStr::new_static("<error>"),
        }
    }
}

/// The type of a symbol, as extracted from its declaration.
///
/// Distinguishes value-bearing declarations (variables, ports, parameters),
/// type aliases (typedefs), and nets (which preserve `NetKind`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolType {
    /// Variables, ports, parameters -- anything with a data type value.
    Value(Ty),
    /// Typedefs -- the aliased underlying type.
    TypeAlias(Ty),
    /// Nets (wire, tri, etc.) -- preserves `NetKind` for tool queries.
    Net(NetType),
    /// Could not extract a type.
    Error(SymbolTypeError),
}

impl SymbolType {
    /// Human-readable representation.
    pub fn pretty(&self) -> SmolStr {
        match self {
            SymbolType::Value(ty) => ty.pretty(),
            SymbolType::Net(net) => {
                SmolStr::new(format!("{} {}", net.kind.keyword_str(), net.data.pretty()))
            }
            SymbolType::TypeAlias(ty) => SmolStr::new(format!("type = {}", ty.pretty())),
            SymbolType::Error(_) => SmolStr::new_static("<error>"),
        }
    }
}

/// Reasons a symbol's type could not be extracted.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolTypeError {
    /// `symbol_to_decl` returned None (index bug or unsupported node).
    MissingDecl,
    /// Module, package, interface, etc. -- no meaningful type.
    UnsupportedSymbolKind,
    /// Salsa cycle recovery for typedef chains.
    TypedefCycle,
    /// `parameter type T` -- not yet implemented.
    TypeParameterUnsupported,
    /// `NameRef` in `TypeSpec` could not be resolved or is not a typedef.
    UserTypeUnresolved,
    /// Typedef resolved but underlying type is not value-usable (net, error, etc.).
    TypedefUnderlyingUnsupported,
    /// Port AST node is structurally broken (cannot read type info at all).
    PortTypeMissing,
}

/// IEEE 1800-2023 net type keywords (LRM 6.7).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NetKind {
    Wire,
    Tri,
    Wand,
    Wor,
    Tri0,
    Tri1,
    Trireg,
    Supply0,
    Supply1,
    Uwire,
}

impl NetKind {
    pub fn keyword_str(self) -> &'static str {
        match self {
            Self::Wire => "wire",
            Self::Tri => "tri",
            Self::Wand => "wand",
            Self::Wor => "wor",
            Self::Tri0 => "tri0",
            Self::Tri1 => "tri1",
            Self::Trireg => "trireg",
            Self::Supply0 => "supply0",
            Self::Supply1 => "supply1",
            Self::Uwire => "uwire",
        }
    }
}

/// A net type: kind keyword plus underlying data type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NetType {
    pub kind: NetKind,
    pub data: Ty,
}

/// Compute `|msb - lsb| + 1` without overflow.
/// Widens to i128 so that extreme i64 bounds never panic.
fn range_width(msb: i64, lsb: i64) -> Option<u32> {
    let diff = (i128::from(msb) - i128::from(lsb)).unsigned_abs() + 1;
    u32::try_from(diff).ok()
}

/// Collect all `Array` dims outermost-first, returning the innermost non-Array base.
pub fn collect_array_dims(ty: &Ty) -> (&Ty, Vec<&UnpackedDim>) {
    let mut current = ty;
    let mut dims = Vec::new();
    while let Ty::Array { elem, dim } = current {
        dims.push(dim);
        current = elem;
    }
    (current, dims)
}

fn fmt_const_int(s: &mut String, c: &ConstInt) {
    match c {
        ConstInt::Known(v) => s.push_str(&v.to_string()),
        ConstInt::Unevaluated(_) => s.push('?'),
        ConstInt::Error(_) => s.push('!'),
    }
}

fn fmt_packed_dim(s: &mut String, dim: &PackedDim) {
    s.push('[');
    fmt_const_int(s, &dim.msb);
    s.push(':');
    fmt_const_int(s, &dim.lsb);
    s.push(']');
}

fn fmt_unpacked_dim(s: &mut String, dim: &UnpackedDim) {
    match dim {
        UnpackedDim::Range { msb, lsb } => {
            s.push('[');
            fmt_const_int(s, msb);
            s.push(':');
            fmt_const_int(s, lsb);
            s.push(']');
        }
        UnpackedDim::Size(c) => {
            s.push('[');
            fmt_const_int(s, c);
            s.push(']');
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dummy_ast_id() -> ErasedAstId {
        let src = "module m; endmodule";
        let tokens = lyra_lexer::lex(src);
        let pp = lyra_preprocess::preprocess_identity(lyra_source::FileId(0), &tokens, src);
        let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
        let map = lyra_ast::AstIdMap::from_root(lyra_source::FileId(0), &parse.syntax());
        let first_child = parse.syntax().first_child().expect("parsed module");
        let ast_node: lyra_ast::ModuleDecl =
            lyra_ast::AstNode::cast(first_child).expect("module decl");
        map.ast_id(&ast_node).expect("ast_id").erase()
    }

    #[test]
    fn integral_kw_logic() {
        assert!(IntegralKw::Logic.four_state());
        assert!(!IntegralKw::Logic.default_signed());
        assert_eq!(IntegralKw::Logic.base_width(), 1);
    }

    #[test]
    fn integral_kw_reg() {
        assert!(IntegralKw::Reg.four_state());
        assert!(!IntegralKw::Reg.default_signed());
        assert_eq!(IntegralKw::Reg.base_width(), 1);
    }

    #[test]
    fn integral_kw_bit() {
        assert!(!IntegralKw::Bit.four_state());
        assert!(!IntegralKw::Bit.default_signed());
        assert_eq!(IntegralKw::Bit.base_width(), 1);
    }

    #[test]
    fn integral_kw_integer() {
        assert!(IntegralKw::Integer.four_state());
        assert!(IntegralKw::Integer.default_signed());
        assert_eq!(IntegralKw::Integer.base_width(), 32);
    }

    #[test]
    fn integral_kw_int() {
        assert!(!IntegralKw::Int.four_state());
        assert!(IntegralKw::Int.default_signed());
        assert_eq!(IntegralKw::Int.base_width(), 32);
    }

    #[test]
    fn integral_kw_shortint() {
        assert!(!IntegralKw::Shortint.four_state());
        assert!(IntegralKw::Shortint.default_signed());
        assert_eq!(IntegralKw::Shortint.base_width(), 16);
    }

    #[test]
    fn integral_kw_longint() {
        assert!(!IntegralKw::Longint.four_state());
        assert!(IntegralKw::Longint.default_signed());
        assert_eq!(IntegralKw::Longint.base_width(), 64);
    }

    #[test]
    fn integral_kw_byte() {
        assert!(!IntegralKw::Byte.four_state());
        assert!(IntegralKw::Byte.default_signed());
        assert_eq!(IntegralKw::Byte.base_width(), 8);
    }

    #[test]
    fn integral_kw_time() {
        assert!(IntegralKw::Time.four_state());
        assert!(!IntegralKw::Time.default_signed());
        assert_eq!(IntegralKw::Time.base_width(), 64);
    }

    #[test]
    fn const_int_known() {
        let c = ConstInt::Known(7);
        assert_eq!(c, ConstInt::Known(7));
    }

    #[test]
    fn const_int_unevaluated() {
        let c = ConstInt::Unevaluated(dummy_ast_id());
        assert!(matches!(c, ConstInt::Unevaluated(_)));
    }

    #[test]
    fn const_int_error() {
        let c = ConstInt::Error(ConstEvalError::DivideByZero);
        assert!(matches!(c, ConstInt::Error(ConstEvalError::DivideByZero)));
    }

    #[test]
    fn const_int_known_ne_unevaluated() {
        assert_ne!(ConstInt::Known(7), ConstInt::Unevaluated(dummy_ast_id()));
    }

    #[test]
    fn const_eval_error_variants() {
        let _ = ConstEvalError::NonConstant;
        let _ = ConstEvalError::DivideByZero;
        let _ = ConstEvalError::InvalidArgument;
        let _ = ConstEvalError::Overflow;
        let _ = ConstEvalError::Unresolved;
        let _ = ConstEvalError::Cycle;
        let _ = ConstEvalError::Unsupported;
    }

    #[test]
    fn packed_dim_width_normal() {
        let dim = PackedDim {
            msb: ConstInt::Known(7),
            lsb: ConstInt::Known(0),
        };
        assert_eq!(dim.try_width(), Some(8));
    }

    #[test]
    fn packed_dim_width_reversed() {
        let dim = PackedDim {
            msb: ConstInt::Known(0),
            lsb: ConstInt::Known(7),
        };
        assert_eq!(dim.try_width(), Some(8));
    }

    #[test]
    fn packed_dim_width_unevaluated() {
        let dim = PackedDim {
            msb: ConstInt::Unevaluated(dummy_ast_id()),
            lsb: ConstInt::Known(0),
        };
        assert_eq!(dim.try_width(), None);
    }

    #[test]
    fn packed_dim_width_error() {
        let dim = PackedDim {
            msb: ConstInt::Error(ConstEvalError::Overflow),
            lsb: ConstInt::Known(0),
        };
        assert_eq!(dim.try_width(), None);
    }

    #[test]
    fn packed_dim_width_extreme_bounds() {
        let dim = PackedDim {
            msb: ConstInt::Known(i64::MIN),
            lsb: ConstInt::Known(i64::MAX),
        };
        // Difference exceeds u32::MAX, so returns None without panicking.
        assert_eq!(dim.try_width(), None);
    }

    #[test]
    fn unpacked_dim_size() {
        assert_eq!(
            UnpackedDim::Size(ConstInt::Known(256)).try_size(),
            Some(256)
        );
    }

    #[test]
    fn unpacked_dim_range() {
        let dim = UnpackedDim::Range {
            msb: ConstInt::Known(3),
            lsb: ConstInt::Known(0),
        };
        assert_eq!(dim.try_size(), Some(4));
    }

    #[test]
    fn unpacked_dim_unevaluated() {
        assert_eq!(
            UnpackedDim::Size(ConstInt::Unevaluated(dummy_ast_id())).try_size(),
            None
        );
    }

    #[test]
    fn packed_width_single_dim() {
        let ty = Ty::logic(
            Box::new([PackedDim {
                msb: ConstInt::Known(7),
                lsb: ConstInt::Known(0),
            }]),
            false,
        );
        if let Ty::Integral(i) = &ty {
            assert_eq!(i.try_packed_width(), Some(8));
        } else {
            panic!("expected Integral");
        }
    }

    #[test]
    fn packed_width_multi_dim() {
        let ty = Ty::logic(
            Box::new([
                PackedDim {
                    msb: ConstInt::Known(7),
                    lsb: ConstInt::Known(0),
                },
                PackedDim {
                    msb: ConstInt::Known(3),
                    lsb: ConstInt::Known(0),
                },
            ]),
            false,
        );
        if let Ty::Integral(i) = &ty {
            assert_eq!(i.try_packed_width(), Some(32));
        } else {
            panic!("expected Integral");
        }
    }

    #[test]
    fn packed_width_no_dims() {
        if let Ty::Integral(i) = &Ty::simple_logic() {
            assert_eq!(i.try_packed_width(), Some(1));
        } else {
            panic!("expected Integral");
        }
    }

    #[test]
    fn packed_width_unevaluated() {
        let ty = Ty::logic(
            Box::new([PackedDim {
                msb: ConstInt::Unevaluated(dummy_ast_id()),
                lsb: ConstInt::Known(0),
            }]),
            false,
        );
        if let Ty::Integral(i) = &ty {
            assert_eq!(i.try_packed_width(), None);
        } else {
            panic!("expected Integral");
        }
    }

    #[test]
    fn pretty_int() {
        assert_eq!(Ty::int().pretty(), "int");
    }

    #[test]
    fn pretty_simple_logic() {
        assert_eq!(Ty::simple_logic().pretty(), "logic");
    }

    #[test]
    fn pretty_logic_unsigned_with_dim() {
        let ty = Ty::logic(
            Box::new([PackedDim {
                msb: ConstInt::Known(7),
                lsb: ConstInt::Known(0),
            }]),
            false,
        );
        assert_eq!(ty.pretty(), "logic [7:0]");
    }

    #[test]
    fn pretty_logic_signed_with_dim() {
        let ty = Ty::logic(
            Box::new([PackedDim {
                msb: ConstInt::Known(7),
                lsb: ConstInt::Known(0),
            }]),
            true,
        );
        assert_eq!(ty.pretty(), "logic signed [7:0]");
    }

    #[test]
    fn pretty_int_unsigned() {
        let ty = Ty::Integral(Integral {
            keyword: IntegralKw::Int,
            signed: false,
            packed: Box::new([]),
        });
        assert_eq!(ty.pretty(), "int unsigned");
    }

    #[test]
    fn pretty_error() {
        assert_eq!(Ty::Error.pretty(), "<error>");
    }

    #[test]
    fn pretty_void() {
        assert_eq!(Ty::Void.pretty(), "void");
    }

    #[test]
    fn pretty_real() {
        assert_eq!(Ty::Real(RealKw::Real).pretty(), "real");
        assert_eq!(Ty::Real(RealKw::Short).pretty(), "shortreal");
        assert_eq!(Ty::Real(RealKw::Time).pretty(), "realtime");
    }

    #[test]
    fn pretty_string_chandle_event() {
        assert_eq!(Ty::String.pretty(), "string");
        assert_eq!(Ty::Chandle.pretty(), "chandle");
        assert_eq!(Ty::Event.pretty(), "event");
    }

    #[test]
    fn pretty_unevaluated_dim() {
        let ty = Ty::logic(
            Box::new([PackedDim {
                msb: ConstInt::Unevaluated(dummy_ast_id()),
                lsb: ConstInt::Known(0),
            }]),
            false,
        );
        assert_eq!(ty.pretty(), "logic [?:0]");
    }

    #[test]
    fn pretty_error_dim() {
        let ty = Ty::logic(
            Box::new([PackedDim {
                msb: ConstInt::Known(7),
                lsb: ConstInt::Error(ConstEvalError::Overflow),
            }]),
            false,
        );
        assert_eq!(ty.pretty(), "logic [7:!]");
    }

    #[test]
    fn ty_int_eq_int() {
        assert_eq!(Ty::int(), Ty::int());
    }

    #[test]
    fn ty_int_ne_integer() {
        assert_ne!(Ty::int(), Ty::integer());
    }

    #[test]
    fn net_type_construct() {
        let _net = NetType {
            kind: NetKind::Wire,
            data: Ty::simple_logic(),
        };
    }

    #[test]
    fn net_kind_ne() {
        assert_ne!(NetKind::Wire, NetKind::Tri);
    }
}
