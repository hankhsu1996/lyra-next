use std::sync::{Arc, LazyLock};

use crate::Site;
use smallvec::SmallVec;
use smol_str::SmolStr;

use crate::enum_def::EnumId;
use crate::interface_id::InterfaceDefId;
use crate::modport_def::{ModportDefId, PortDirection};
use crate::record::RecordId;
use crate::symbols::SymbolId;

/// A constant integer value, used for dimension bounds and widths.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstInt {
    Known(i64),
    Unevaluated(Site),
    Error(ConstEvalError),
}

/// Reasons a constant expression evaluation can fail.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConstEvalError {
    NonConstant,
    DivideByZero,
    InvalidArgument,
    Overflow,
    Unresolved,
    Cycle,
    Unsupported,
    AutoIncrementAfterUnknown,
    /// AST anchor lookup failed; carries the producing layer for diagnostics.
    MissingSite(MissingSiteOrigin),
}

/// Where a `MissingSite` error originated.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MissingSiteOrigin {
    /// `type_extract::expr_to_const_int` -- expression had no `AstId`.
    TypeExtract,
    /// `expr_queries::const_eval_impl` -- expression node had no `AstId`.
    ConstEval,
}

impl MissingSiteOrigin {
    /// Diagnostic detail string identifying the producing layer.
    pub fn detail(self) -> smol_str::SmolStr {
        match self {
            Self::TypeExtract => smol_str::SmolStr::new_static("missing AST anchor (type_extract)"),
            Self::ConstEval => smol_str::SmolStr::new_static("missing AST anchor (const_eval)"),
        }
    }
}

impl ConstInt {
    /// If this value is `Error(MissingSite(origin))`, return the origin.
    pub fn missing_site_origin(&self) -> Option<MissingSiteOrigin> {
        match self {
            Self::Error(ConstEvalError::MissingSite(origin)) => Some(*origin),
            _ => None,
        }
    }
}

/// A packed dimension with msb and lsb bounds, e.g. `[7:0]`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

/// The index type of an associative array dimension.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssocIndex {
    Wildcard,
    Typed(Box<Ty>),
}

/// An unpacked dimension on a variable declaration.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnpackedDim {
    Range { msb: ConstInt, lsb: ConstInt },
    Size(ConstInt),
    Unsized,
    Queue { bound: Option<ConstInt> },
    Assoc(AssocIndex),
}

impl UnpackedDim {
    /// Whether this dim has a known fixed length.
    /// Returns `Some(n)` for `Range` and `Size` dims, `None` for dynamic/queue/assoc/unsized.
    pub fn fixed_len(&self) -> Option<u32> {
        self.try_size()
    }

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
            UnpackedDim::Unsized | UnpackedDim::Queue { .. } | UnpackedDim::Assoc(_) => None,
        }
    }
}

/// Shared packed dimension list with O(1) peeling via start offset.
///
/// Backed by `Arc<[PackedDim]>` so cloning and peeling are cheap.
/// `PartialEq`/`Eq`/`Hash` compare the viewed slice (from `start`),
/// which is O(n) for remaining dims (tiny in practice, typically 1-2).
#[derive(Debug, Clone)]
pub struct PackedDims {
    dims: Arc<[PackedDim]>,
    start: u32,
}

static EMPTY_PACKED: LazyLock<Arc<[PackedDim]>> = LazyLock::new(|| Arc::from(Vec::new()));

impl PackedDims {
    pub fn empty() -> Self {
        Self {
            dims: Arc::clone(&EMPTY_PACKED),
            start: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.dims.len() - self.start as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn as_slice(&self) -> &[PackedDim] {
        &self.dims[self.start as usize..]
    }

    pub fn iter(&self) -> impl Iterator<Item = &PackedDim> {
        self.as_slice().iter()
    }

    /// Remove the outermost packed dimension, returning the rest.
    ///
    /// # Panics
    ///
    /// Panics if empty.
    #[must_use]
    pub fn peel_one(&self) -> Self {
        assert!(!self.is_empty(), "peel_one on empty PackedDims");
        Self {
            dims: Arc::clone(&self.dims),
            start: self.start + 1,
        }
    }
}

impl PartialEq for PackedDims {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl Eq for PackedDims {}

impl std::hash::Hash for PackedDims {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_slice().hash(state);
    }
}

impl std::ops::Index<usize> for PackedDims {
    type Output = PackedDim;
    fn index(&self, idx: usize) -> &PackedDim {
        &self.dims[self.start as usize + idx]
    }
}

impl From<Vec<PackedDim>> for PackedDims {
    fn from(v: Vec<PackedDim>) -> Self {
        if v.is_empty() {
            return Self::empty();
        }
        Self {
            dims: Arc::from(v),
            start: 0,
        }
    }
}

impl<'a> IntoIterator for &'a PackedDims {
    type Item = &'a PackedDim;
    type IntoIter = std::slice::Iter<'a, PackedDim>;
    fn into_iter(self) -> Self::IntoIter {
        self.as_slice().iter()
    }
}

impl FromIterator<PackedDim> for PackedDims {
    fn from_iter<I: IntoIterator<Item = PackedDim>>(iter: I) -> Self {
        let v: Vec<PackedDim> = iter.into_iter().collect();
        Self::from(v)
    }
}

/// The keyword that produced an integral type, preserving LRM identity.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Integral {
    pub keyword: IntegralKw,
    pub signed: bool,
    pub packed: PackedDims,
}

impl Integral {
    pub fn try_packed_width(&self) -> Option<u32> {
        let mut width = self.keyword.base_width();
        for dim in &self.packed {
            width = width.checked_mul(dim.try_width()?)?;
        }
        Some(width)
    }

    /// Construct the result type of a packed part-select of given width.
    ///
    /// LRM 11.8.1: result is unsigned. Preserves 2-state vs 4-state.
    pub fn part_select_result(&self, width: u32) -> Ty {
        debug_assert!(width > 0, "part_select_result called with width 0");
        let kw = if self.keyword.four_state() {
            IntegralKw::Logic
        } else {
            IntegralKw::Bit
        };
        let packed = if width > 1 {
            PackedDims::from(vec![PackedDim {
                msb: ConstInt::Known(i64::from(width) - 1),
                lsb: ConstInt::Known(0),
            }])
        } else {
            PackedDims::empty()
        };
        Ty::Integral(Integral {
            keyword: kw,
            signed: false,
            packed,
        })
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RealKw {
    Real,
    Short,
    Time,
}

impl RealKw {
    pub fn bit_width(self) -> u32 {
        match self {
            Self::Short => 32,
            Self::Real | Self::Time => 64,
        }
    }

    fn keyword_str(self) -> &'static str {
        match self {
            Self::Real => "real",
            Self::Short => "shortreal",
            Self::Time => "realtime",
        }
    }
}

/// Handle type for an interface instance value.
///
/// Not a primitive datatype -- represents a reference to an interface
/// instance. Compatibility is based on same `InterfaceDefId` (same
/// interface definition) and compatible modport constraints.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InterfaceType {
    pub iface: InterfaceDefId,
    pub modport: Option<ModportDefId>,
}

/// Derived fact: which ports a modport exposes, their directions, and targets.
///
/// Entries are sorted by `port_name` for deterministic binary search.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModportView {
    entries: Box<[ModportViewEntry]>,
}

/// A resolved modport port entry.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModportViewEntry {
    pub port_name: SmolStr,
    pub direction: PortDirection,
    pub port_id: Site,
    pub target: ModportViewTarget,
}

/// What the modport port resolves to.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModportViewTarget {
    /// Bare ident, resolved to interface member symbol.
    Member(SymbolId),
    /// Expression port: carries the expr AST ID.
    Expr(Site),
    /// `.P()`: no connection.
    Empty,
}

impl ModportView {
    pub fn new(mut entries: Vec<ModportViewEntry>) -> Self {
        entries.sort_by(|a, b| a.port_name.cmp(&b.port_name));
        Self {
            entries: entries.into_boxed_slice(),
        }
    }

    pub fn lookup(&self, port_name: &str) -> Option<&ModportViewEntry> {
        let idx = self
            .entries
            .binary_search_by(|e| e.port_name.as_str().cmp(port_name))
            .ok()?;
        Some(&self.entries[idx])
    }

    pub fn has_port(&self, port_name: &str) -> bool {
        self.lookup(port_name).is_some()
    }

    pub fn direction_of_port(&self, port_name: &str) -> Option<PortDirection> {
        self.lookup(port_name).map(|e| e.direction)
    }
}

/// The semantic type representation.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    Integral(Integral),
    Real(RealKw),
    Enum(EnumId),
    Record(RecordId),
    Interface(InterfaceType),
    Array { elem: Box<Ty>, dim: UnpackedDim },
    String,
    Chandle,
    Event,
    Void,
    Error,
}

impl Ty {
    pub fn logic(packed: PackedDims, signed: bool) -> Self {
        Self::Integral(Integral {
            keyword: IntegralKw::Logic,
            signed,
            packed,
        })
    }

    pub fn reg(packed: PackedDims, signed: bool) -> Self {
        Self::Integral(Integral {
            keyword: IntegralKw::Reg,
            signed,
            packed,
        })
    }

    pub fn bit(packed: PackedDims, signed: bool) -> Self {
        Self::Integral(Integral {
            keyword: IntegralKw::Bit,
            signed,
            packed,
        })
    }

    pub fn simple_logic() -> Self {
        Self::logic(PackedDims::empty(), false)
    }

    pub fn int() -> Self {
        Self::Integral(Integral {
            keyword: IntegralKw::Int,
            signed: IntegralKw::Int.default_signed(),
            packed: PackedDims::empty(),
        })
    }

    pub fn integer() -> Self {
        Self::Integral(Integral {
            keyword: IntegralKw::Integer,
            signed: IntegralKw::Integer.default_signed(),
            packed: PackedDims::empty(),
        })
    }

    pub fn byte() -> Self {
        Self::Integral(Integral {
            keyword: IntegralKw::Byte,
            signed: IntegralKw::Byte.default_signed(),
            packed: PackedDims::empty(),
        })
    }

    pub fn shortint() -> Self {
        Self::Integral(Integral {
            keyword: IntegralKw::Shortint,
            signed: IntegralKw::Shortint.default_signed(),
            packed: PackedDims::empty(),
        })
    }

    pub fn longint() -> Self {
        Self::Integral(Integral {
            keyword: IntegralKw::Longint,
            signed: IntegralKw::Longint.default_signed(),
            packed: PackedDims::empty(),
        })
    }

    pub fn time() -> Self {
        Self::Integral(Integral {
            keyword: IntegralKw::Time,
            signed: IntegralKw::Time.default_signed(),
            packed: PackedDims::empty(),
        })
    }

    /// Construct `bit[width-1:0]` (unsigned, 2-state) with a single packed dim.
    pub fn bit_n(width: u32) -> Self {
        debug_assert!(width > 0, "bit_n(0) is nonsensical");
        Self::bit(
            PackedDims::from(vec![PackedDim {
                msb: ConstInt::Known(i64::from(width) - 1),
                lsb: ConstInt::Known(0),
            }]),
            false,
        )
    }

    /// Remove the outermost unpacked dimension and return the element type.
    pub fn peel_unpacked_dim(&self) -> Option<Ty> {
        match self {
            Self::Array { elem, .. } => Some(elem.as_ref().clone()),
            _ => None,
        }
    }

    /// Peel one packed array dimension (multi-dim packed only: len >= 2).
    ///
    /// For `logic [3:0][7:0]`, returns `logic [7:0]`. Returns None when
    /// packed dims < 2 (use `bit_select_result` for 1D vectors).
    pub fn peel_packed_array_dim(&self) -> Option<Ty> {
        match self {
            Self::Integral(i) if i.packed.len() >= 2 => Some(Self::Integral(Integral {
                keyword: i.keyword,
                signed: i.signed,
                packed: i.packed.peel_one(),
            })),
            _ => None,
        }
    }

    /// Produce a 1-bit result for bit-select on an integral type.
    ///
    /// Preserves 4-state/2-state. Returns unsigned per LRM 11.8.1.
    pub fn bit_select_result(&self) -> Option<Ty> {
        match self {
            Self::Integral(i) => {
                let kw = if i.keyword.four_state() {
                    IntegralKw::Logic
                } else {
                    IntegralKw::Bit
                };
                Some(Self::Integral(Integral {
                    keyword: kw,
                    signed: false,
                    packed: PackedDims::empty(),
                }))
            }
            _ => None,
        }
    }

    /// Whether this is a real-valued type (real, shortreal, realtime).
    pub fn is_real(&self) -> bool {
        matches!(self, Ty::Real(_))
    }

    /// Whether this type is a data type (LRM 6.2.1).
    ///
    /// Returns false for void, event, interface, and error types.
    pub fn is_data_type(&self) -> bool {
        matches!(
            self,
            Ty::Integral(_)
                | Ty::Real(_)
                | Ty::Enum(_)
                | Ty::Record(_)
                | Ty::Array { .. }
                | Ty::String
                | Ty::Chandle
        )
    }

    /// Human-readable type representation (pure, no DB access).
    ///
    /// Always lossless: includes all packed dims, unpacked dims, and
    /// signedness overrides. Enum/record variants print as SV keywords
    /// without names (`Record` falls back to `"struct"`); use `TyFmt`
    /// in `lyra-db` for name-enriched output that distinguishes
    /// struct vs union.
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
            Self::Record(_) => SmolStr::new_static("struct"),
            Self::Interface(_) => SmolStr::new_static("interface"),
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
    /// Human-readable representation (delegates to `Ty::pretty()`).
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
    /// Declaration AST node not found (index bug or unsupported node).
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
    /// Modport name does not exist on the resolved interface.
    UnknownModport,
    /// Dotted name used on a non-interface type (e.g. `typedef int foo; foo.bar v;`).
    ModportOnNonInterface,
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

/// Wrap a `Ty` with `Ty::Array` layers for each unpacked dim.
///
/// Dims are applied right-to-left so the outermost (leftmost in source) dim
/// is the outermost `Array` wrapper. For example, given dims `[2], [3]`
/// (outermost-first), the result is `Array(Array(inner, Size(3)), Size(2))`.
pub fn wrap_unpacked(ty: Ty, unpacked: &[UnpackedDim]) -> Ty {
    unpacked.iter().rev().fold(ty, |inner, dim| Ty::Array {
        elem: Box::new(inner),
        dim: dim.clone(),
    })
}

/// Collect all `Array` dims outermost-first, returning the innermost non-Array base.
///
/// Outermost dim = first element in the returned slice. This matches source
/// order: `int x [2][3]` yields `[Size(2), Size(3)]`. The nesting invariant
/// guarantees that the outermost source dim is the outermost `Array` wrapper.
pub fn collect_array_dims(ty: &Ty) -> (&Ty, SmallVec<[&UnpackedDim; 2]>) {
    let mut current = ty;
    let mut dims = SmallVec::new();
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
        UnpackedDim::Unsized => s.push_str("[]"),
        UnpackedDim::Queue { bound: None } => s.push_str("[$]"),
        UnpackedDim::Queue { bound: Some(c) } => {
            s.push_str("[$:");
            fmt_const_int(s, c);
            s.push(']');
        }
        UnpackedDim::Assoc(AssocIndex::Wildcard) => s.push_str("[*]"),
        UnpackedDim::Assoc(AssocIndex::Typed(ty)) => {
            s.push('[');
            s.push_str(ty.pretty().as_str());
            s.push(']');
        }
    }
}

#[cfg(test)]
mod tests;
