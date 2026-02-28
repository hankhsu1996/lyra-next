use lyra_parser::SyntaxNode;
use smol_str::SmolStr;

use crate::enum_def::EnumId;
use crate::member::{MemberInfo, MemberLookupError, MethodInvalidReason};
use crate::symbols::{GlobalSymbolId, SymbolKind};
use crate::types::{
    ConstEvalError, ConstInt, Integral, IntegralKw, PackedDim, PackedDims, SymbolType, Ty,
};

/// Signedness for bit-vector expression typing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Signedness {
    Signed,
    Unsigned,
}

/// Bit width: known, unknown, context-dependent, or error.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BitWidth {
    Known(u32),
    Unknown,
    /// Width adapts to assignment context (LRM 11.6). No payload -- the
    /// width is unknown until context supplies it. The "default 1-bit in
    /// self-determined context" rule (LRM 5.7.1) is handled by
    /// `self_determined()`, not by storing a fallback here.
    ContextDependent,
    /// Width computation failed (distinct from Unknown, which means
    /// "not yet evaluated").
    Error,
}

impl BitWidth {
    /// Resolve to a concrete width in self-determined context.
    ///
    /// `ContextDependent` -> 1 bit per LRM 5.7.1.
    /// Call this only when the expression is genuinely in a self-determined
    /// position (concat/replication operands, shift RHS, logical operands).
    pub fn self_determined(&self) -> Option<u32> {
        match self {
            BitWidth::Known(w) => Some(*w),
            BitWidth::ContextDependent => Some(1),
            BitWidth::Unknown | BitWidth::Error => None,
        }
    }
}

/// A bit-vector value type (integral expressions).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BitVecType {
    pub width: BitWidth,
    pub signed: Signedness,
    /// True for 4-state types (logic/reg/integer/time or x/z-containing literals).
    pub four_state: bool,
}

impl BitVecType {
    /// Human-readable representation. Uses `logic` (4-state) or `bit`
    /// (2-state) depending on `four_state`.
    pub fn pretty(&self) -> SmolStr {
        let mut s = String::from(if self.four_state { "logic" } else { "bit" });
        if self.signed == Signedness::Signed {
            s.push_str(" signed");
        }
        match self.width {
            BitWidth::Known(1) | BitWidth::ContextDependent => {}
            BitWidth::Known(w) => {
                use core::fmt::Write;
                let _ = write!(s, " [{}:0]", w - 1);
            }
            BitWidth::Unknown | BitWidth::Error => {
                s.push_str(" [?:?]");
            }
        }
        SmolStr::new(s)
    }
}

/// Reasons an expression's type could not be determined.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprTypeErrorKind {
    Unresolved,
    NameRefIsTypeNotValue,
    UnsupportedExprKind,
    UnsupportedLiteralKind,
    UnsupportedBinaryOp,
    UnsupportedSystemCall,
    NonBitOperand,
    ConcatNonBitOperand,
    CondBranchTypeMismatch,
    InvalidReplicationCount,
    ReplicationConstEvalFailed(ConstEvalError),
    IndexNonIndexable,
    PartSelectNonIntegral,
    SliceWidthInvalid,
    SliceNonSliceableArray,
    NoMembersOnReceiver,
    UnknownMember,
    MemberNotInModport,
    MethodRequiresCall,
    MethodArityMismatch,
    MethodArgNotIntegral,
    UserCallUnsupported,
    UnsupportedCalleeForm(CalleeFormKind),
    UnresolvedCall,
    NotACallable(SymbolKind),
    TaskInExprContext,
    MethodNotValidOnReceiver(MethodInvalidReason),
    MethodArgTypeMismatch,
    MethodArgNotLvalue,
    MethodKeyTypeUnknown,
    VoidUsedAsExpr,
    CastTargetNotAType,
    InternalStreamOperandError,
    UnsupportedIncDec,
}

/// How an expression's type is viewed for operations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprView {
    /// Integral bit-vector view (logic/int/bit/etc).
    BitVec(BitVecType),
    /// Non-bit-vector type (real, string, struct, etc.).
    Plain,
    /// Type could not be determined.
    Error(ExprTypeErrorKind),
}

/// Result of typing an expression (self-determined).
///
/// Every typed expression always carries a `Ty`. `view` provides the
/// operational interpretation: bit-vector for integral types, plain for
/// non-integral types, or error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprType {
    pub ty: Ty,
    pub view: ExprView,
}

impl ExprType {
    /// Convert a `Ty` to `ExprType`. The only `Ty` -> `ExprType` conversion path.
    pub fn from_ty(ty: &Ty) -> ExprType {
        match ty {
            Ty::Integral(i) => {
                let width = match i.try_packed_width() {
                    Some(w) => BitWidth::Known(w),
                    None => BitWidth::Unknown,
                };
                let signed = if i.signed {
                    Signedness::Signed
                } else {
                    Signedness::Unsigned
                };
                let four_state = i.keyword.four_state();
                ExprType {
                    ty: ty.clone(),
                    view: ExprView::BitVec(BitVecType {
                        width,
                        signed,
                        four_state,
                    }),
                }
            }
            Ty::Real(_)
            | Ty::String
            | Ty::Chandle
            | Ty::Event
            | Ty::Void
            | Ty::Enum(_)
            | Ty::Record(_)
            | Ty::Interface(_)
            | Ty::Array { .. } => ExprType {
                ty: ty.clone(),
                view: ExprView::Plain,
            },
            Ty::Error => ExprType::error(ExprTypeErrorKind::Unresolved),
        }
    }

    /// Map `SymbolType` to `ExprType`.
    pub fn from_symbol_type(st: &SymbolType) -> ExprType {
        match st {
            SymbolType::Value(ty) => ExprType::from_ty(ty),
            SymbolType::Net(net) => ExprType::from_ty(&net.data),
            SymbolType::TypeAlias(_) => ExprType::error(ExprTypeErrorKind::NameRefIsTypeNotValue),
            SymbolType::Error(_) => ExprType::error(ExprTypeErrorKind::Unresolved),
        }
    }

    /// Construct an error `ExprType`.
    pub fn error(kind: ExprTypeErrorKind) -> ExprType {
        ExprType {
            ty: Ty::Error,
            view: ExprView::Error(kind),
        }
    }

    /// Construct a bit-vector `ExprType` from raw `BitVecType`.
    pub fn bitvec(bv: BitVecType) -> ExprType {
        ExprType {
            ty: Ty::Integral(Integral {
                keyword: if bv.four_state {
                    IntegralKw::Logic
                } else {
                    IntegralKw::Bit
                },
                signed: bv.signed == Signedness::Signed,
                packed: match bv.width {
                    BitWidth::Known(w) if w > 1 => PackedDims::from(vec![PackedDim {
                        msb: ConstInt::Known(i64::from(w) - 1),
                        lsb: ConstInt::Known(0),
                    }]),
                    _ => PackedDims::empty(),
                },
            }),
            view: ExprView::BitVec(bv),
        }
    }

    /// Unsized decimal literal default: `BitVec(32, Signed, 2-state)`.
    pub fn int_literal() -> ExprType {
        ExprType::bitvec(BitVecType {
            width: BitWidth::Known(32),
            signed: Signedness::Signed,
            four_state: false,
        })
    }

    /// Sized bit-vector (2-state by default).
    pub fn bits(width: u32, signed: Signedness) -> ExprType {
        ExprType::bitvec(BitVecType {
            width: BitWidth::Known(width),
            signed,
            four_state: false,
        })
    }

    /// Single-bit unsigned (reduction/logical results).
    pub fn one_bit() -> ExprType {
        ExprType::bits(1, Signedness::Unsigned)
    }

    /// Human-readable type representation.
    pub fn pretty(&self) -> SmolStr {
        match &self.view {
            ExprView::BitVec(bv) => bv.pretty(),
            ExprView::Plain => self.ty.pretty(),
            ExprView::Error(_) => SmolStr::new_static("<error>"),
        }
    }
}

/// Category of unsupported callee syntax (for future diagnostic messages).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CalleeFormKind {
    MethodCall,
    Other,
}

/// Error resolving a callable name.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResolveCallableError {
    NotFound,
    NotACallable(SymbolKind),
}

/// Signature of a callable, passed through inference by the DB layer.
///
/// This is the source of truth for call-site type checking. The DB layer
/// normalizes the raw `CallableSig` types (evaluating `ConstInt::Unevaluated`
/// dims) and builds this lightweight projection that `lyra-semantic` can
/// consume without depending on `lyra-db`.
pub struct CallableSigRef {
    pub kind: CallableKind,
    pub return_ty: Ty,
    pub ports: Box<[CallablePort]>,
}

/// Whether a callable is a function or task.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallableKind {
    Function,
    Task,
}

/// Port info needed for call checking. Contains only what inference uses.
#[derive(Debug, Clone)]
pub struct CallablePort {
    pub name: SmolStr,
    pub ty: Ty,
    pub has_default: bool,
}

/// Callbacks for the inference engine. No DB access -- pure.
pub trait InferCtx {
    /// The file being analyzed.
    fn file_id(&self) -> lyra_source::FileId;
    /// Resolve a `NameRef`/`QualifiedName` to its type.
    fn type_of_name(&self, name_node: &SyntaxNode) -> ExprType;
    /// Evaluate a constant expression (for replication count).
    fn const_eval(&self, expr_node: &SyntaxNode) -> ConstInt;
    /// Resolve a callee name node to a callable symbol.
    fn resolve_callable(
        &self,
        callee_node: &SyntaxNode,
    ) -> Result<GlobalSymbolId, ResolveCallableError>;
    /// Get the signature of a callable symbol.
    fn callable_sig(&self, sym: GlobalSymbolId) -> Option<CallableSigRef>;
    /// Look up a member (field) on a type.
    fn member_lookup(&self, ty: &Ty, member_name: &str) -> Result<MemberInfo, MemberLookupError>;
    /// Get the integral view of an enum's base type.
    fn enum_integral_view(&self, id: &EnumId) -> Option<BitVecType>;
    /// Resolve a `NameRef` node as a type (typedef/enum/struct name).
    /// Used by system functions like `$bits` that accept type arguments.
    fn resolve_type_arg(&self, name_node: &SyntaxNode) -> Option<Ty>;
}

/// Extract an integral view from an `ExprType`, auto-casting enums to
/// their base integral type when needed for arithmetic/bitwise operations.
pub(crate) fn try_integral_view(et: &ExprType, ctx: &dyn InferCtx) -> Option<BitVecType> {
    match &et.view {
        ExprView::BitVec(bv) => Some(*bv),
        ExprView::Plain => {
            if let Ty::Enum(id) = &et.ty {
                ctx.enum_integral_view(id)
            } else {
                None
            }
        }
        ExprView::Error(_) => None,
    }
}
