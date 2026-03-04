use crate::Site;

use crate::enum_def::EnumId;
use lyra_lexer::SyntaxKind;

use crate::member_name::MemberNameToken;
use crate::symbols::{GlobalSymbolId, SymbolId};
use crate::types::{AssocIndex, Ty, UnpackedDim};

/// Information about a resolved member access.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemberInfo {
    pub ty: Ty,
    pub kind: MemberKind,
    pub receiver: Option<ReceiverInfo>,
}

/// What kind of member was accessed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemberKind {
    Field {
        index: u32,
    },
    InterfaceMember {
        member: SymbolId,
    },
    /// A task/function declared in an interface (callable member).
    InterfaceCallable {
        global_sym: GlobalSymbolId,
    },
    Modport,
    ModportPort {
        port_id: Site,
        target: ModportPortTarget,
    },
    BuiltinMethod(BuiltinMethodKind),
}

/// What a modport port resolves to (for member lookup).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModportPortTarget {
    Member(SymbolId),
    Expr(Site),
    Empty,
}

/// Receiver info carried on `MemberInfo` for builtin method checking.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReceiverInfo {
    Array(ArrayReceiverInfo),
}

/// Reasons a member lookup can fail.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemberLookupError {
    NoMembersOnType,
    UnknownMember,
    NotInModport,
    MethodNotValidOnReceiver(MethodInvalidReason),
}

/// Why a method is not valid on its receiver.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MethodInvalidReason {
    WrongArrayKind,
    AssocKeyWildcard,
    AssocKeyUnknown,
}

/// A built-in method on a type. Structured by receiver category.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinMethodKind {
    Enum(EnumMethodKind),
    Array(ArrayMethodKind),
    String(StringMethodKind),
}

/// LRM 6.19.4 enum methods.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EnumMethodKind {
    First,
    Last,
    Next,
    Prev,
    Num,
    Name,
}

impl EnumMethodKind {
    /// Return type given the enum's identity.
    pub fn return_ty(self, enum_id: EnumId) -> Ty {
        match self {
            Self::First | Self::Last | Self::Next | Self::Prev => Ty::Enum(enum_id),
            Self::Num => Ty::int(),
            Self::Name => Ty::String,
        }
    }

    /// Returns `(min_arity, max_arity)`.
    pub fn arity(self) -> (usize, usize) {
        match self {
            Self::Next | Self::Prev => (0, 1),
            _ => (0, 0),
        }
    }

    /// Whether the optional argument must be integral.
    pub fn arg_requires_integral(self) -> bool {
        matches!(self, Self::Next | Self::Prev)
    }

    /// Resolve method name to kind.
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "first" => Some(Self::First),
            "last" => Some(Self::Last),
            "next" => Some(Self::Next),
            "prev" => Some(Self::Prev),
            "num" => Some(Self::Num),
            "name" => Some(Self::Name),
            _ => None,
        }
    }
}

/// LRM 7.5/7.9/7.10/7.12 array receiver classification.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArrayReceiverKind {
    Fixed,
    Dynamic,
    Queue,
    Assoc,
    Packed,
    NonArray,
}

/// Associative array key state.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssocKey {
    Wildcard,
    Known(Ty),
    Unknown,
}

/// Classified array receiver for method dispatch.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayReceiverInfo {
    pub kind: ArrayReceiverKind,
    pub elem_ty: Ty,
    pub assoc_key: Option<AssocKey>,
}

/// Classify any type as an array receiver. Total: always returns a result.
pub fn classify_array_receiver(ty: &Ty) -> ArrayReceiverInfo {
    match ty {
        Ty::Array { elem, dim } => {
            let (kind, assoc_key) = match dim {
                UnpackedDim::Range { .. } | UnpackedDim::Size(_) => {
                    (ArrayReceiverKind::Fixed, None)
                }
                UnpackedDim::Unsized => (ArrayReceiverKind::Dynamic, None),
                UnpackedDim::Queue { .. } => (ArrayReceiverKind::Queue, None),
                UnpackedDim::Assoc(AssocIndex::Wildcard) => {
                    (ArrayReceiverKind::Assoc, Some(AssocKey::Wildcard))
                }
                UnpackedDim::Assoc(AssocIndex::Typed(key_ty)) if matches!(**key_ty, Ty::Error) => {
                    (ArrayReceiverKind::Assoc, Some(AssocKey::Unknown))
                }
                UnpackedDim::Assoc(AssocIndex::Typed(key_ty)) => (
                    ArrayReceiverKind::Assoc,
                    Some(AssocKey::Known(key_ty.as_ref().clone())),
                ),
            };
            ArrayReceiverInfo {
                kind,
                elem_ty: elem.as_ref().clone(),
                assoc_key,
            }
        }
        Ty::Integral(_) => ArrayReceiverInfo {
            kind: ArrayReceiverKind::Packed,
            elem_ty: Ty::Error,
            assoc_key: None,
        },
        _ => ArrayReceiverInfo {
            kind: ArrayReceiverKind::NonArray,
            elem_ty: Ty::Error,
            assoc_key: None,
        },
    }
}

/// LRM 7.5/7.9/7.10/7.12 built-in array methods.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArrayMethodKind {
    // 7.5/7.9/7.10 basic methods
    Size,
    Delete,
    Num,
    Exists,
    First,
    Last,
    Next,
    Prev,
    Insert,
    PopFront,
    PopBack,
    PushFront,
    PushBack,
    // 7.12.1 locator methods
    Find,
    FindIndex,
    FindFirst,
    FindFirstIndex,
    FindLast,
    FindLastIndex,
    Min,
    Max,
    Unique,
    UniqueIndex,
    // 7.12.2 ordering methods
    Sort,
    Rsort,
    Reverse,
    Shuffle,
    // 7.12.3 reduction methods
    Sum,
    Product,
    And,
    Or,
    Xor,
    // 7.12.5 array mapping method (SV-2023)
    Map,
}

/// Return shape for 7.12 methods (before receiver-specific `Ty` construction).
enum ArrayMethodReturn {
    QueueOfElem,
    QueueOfInt,
    Elem,
    Int,
    Void,
}

impl ArrayMethodKind {
    /// Resolve from a `MemberNameToken`, using token kind for keyword methods.
    pub fn from_member_token(tok: &MemberNameToken) -> Option<Self> {
        match tok.kind {
            SyntaxKind::AndKw => Some(Self::And),
            SyntaxKind::OrKw => Some(Self::Or),
            SyntaxKind::XorKw => Some(Self::Xor),
            SyntaxKind::UniqueKw => Some(Self::Unique),
            _ => Self::from_ident_text(tok.text.as_str()),
        }
    }

    /// Resolve method name to kind (identifier-like tokens only).
    fn from_ident_text(name: &str) -> Option<Self> {
        match name {
            "size" => Some(Self::Size),
            "delete" => Some(Self::Delete),
            "num" => Some(Self::Num),
            "exists" => Some(Self::Exists),
            "first" => Some(Self::First),
            "last" => Some(Self::Last),
            "next" => Some(Self::Next),
            "prev" => Some(Self::Prev),
            "insert" => Some(Self::Insert),
            "pop_front" => Some(Self::PopFront),
            "pop_back" => Some(Self::PopBack),
            "push_front" => Some(Self::PushFront),
            "push_back" => Some(Self::PushBack),
            "find" => Some(Self::Find),
            "find_index" => Some(Self::FindIndex),
            "find_first" => Some(Self::FindFirst),
            "find_first_index" => Some(Self::FindFirstIndex),
            "find_last" => Some(Self::FindLast),
            "find_last_index" => Some(Self::FindLastIndex),
            "min" => Some(Self::Min),
            "max" => Some(Self::Max),
            "unique_index" => Some(Self::UniqueIndex),
            "sort" => Some(Self::Sort),
            "rsort" => Some(Self::Rsort),
            "reverse" => Some(Self::Reverse),
            "shuffle" => Some(Self::Shuffle),
            "sum" => Some(Self::Sum),
            "product" => Some(Self::Product),
            "map" => Some(Self::Map),
            _ => None,
        }
    }

    /// Whether this method accepts an optional `with (expr)` clause.
    ///
    /// Per LRM 7.12, all locator, ordering, and reduction methods accept
    /// a with clause except `reverse` and `shuffle`.
    pub fn accepts_with_clause(self) -> bool {
        self.is_manip_method() && !matches!(self, Self::Reverse | Self::Shuffle)
    }

    /// Whether this method requires a `with (expr)` clause.
    ///
    /// Per LRM 7.12, locator methods and `map` require a with clause.
    pub fn requires_with_clause(self) -> bool {
        matches!(
            self,
            Self::Find
                | Self::FindIndex
                | Self::FindFirst
                | Self::FindFirstIndex
                | Self::FindLast
                | Self::FindLastIndex
                | Self::Map
        )
    }

    /// Whether this is a 7.12 array manipulation method.
    fn is_manip_method(self) -> bool {
        matches!(
            self,
            Self::Find
                | Self::FindIndex
                | Self::FindFirst
                | Self::FindFirstIndex
                | Self::FindLast
                | Self::FindLastIndex
                | Self::Min
                | Self::Max
                | Self::Unique
                | Self::UniqueIndex
                | Self::Sort
                | Self::Rsort
                | Self::Reverse
                | Self::Shuffle
                | Self::Sum
                | Self::Product
                | Self::And
                | Self::Or
                | Self::Xor
                | Self::Map
        )
    }

    /// Whether this method requires a typed associative key.
    fn needs_typed_key(self) -> bool {
        matches!(
            self,
            Self::Exists | Self::First | Self::Last | Self::Next | Self::Prev
        )
    }

    /// Check whether this method is valid on the given receiver.
    pub fn allowed_on(self, recv: &ArrayReceiverInfo) -> Result<(), MethodInvalidReason> {
        match recv.kind {
            ArrayReceiverKind::Packed | ArrayReceiverKind::NonArray => {
                return Err(MethodInvalidReason::WrongArrayKind);
            }
            _ => {}
        }
        if self.is_manip_method() {
            return match recv.kind {
                ArrayReceiverKind::Fixed
                | ArrayReceiverKind::Dynamic
                | ArrayReceiverKind::Queue => Ok(()),
                ArrayReceiverKind::Assoc
                | ArrayReceiverKind::Packed
                | ArrayReceiverKind::NonArray => Err(MethodInvalidReason::WrongArrayKind),
            };
        }
        match recv.kind {
            ArrayReceiverKind::Fixed => match self {
                Self::Size => Ok(()),
                _ => Err(MethodInvalidReason::WrongArrayKind),
            },
            ArrayReceiverKind::Dynamic => match self {
                Self::Size | Self::Delete => Ok(()),
                _ => Err(MethodInvalidReason::WrongArrayKind),
            },
            ArrayReceiverKind::Queue => match self {
                Self::Size
                | Self::Delete
                | Self::Insert
                | Self::PopFront
                | Self::PopBack
                | Self::PushFront
                | Self::PushBack => Ok(()),
                _ => Err(MethodInvalidReason::WrongArrayKind),
            },
            ArrayReceiverKind::Assoc => {
                match self {
                    Self::Insert
                    | Self::PopFront
                    | Self::PopBack
                    | Self::PushFront
                    | Self::PushBack => {
                        return Err(MethodInvalidReason::WrongArrayKind);
                    }
                    _ => {}
                }
                if self.needs_typed_key() {
                    match &recv.assoc_key {
                        Some(AssocKey::Known(_)) => Ok(()),
                        Some(AssocKey::Wildcard) => Err(MethodInvalidReason::AssocKeyWildcard),
                        Some(AssocKey::Unknown) => Err(MethodInvalidReason::AssocKeyUnknown),
                        None => Err(MethodInvalidReason::WrongArrayKind),
                    }
                } else {
                    Ok(())
                }
            }
            ArrayReceiverKind::Packed | ArrayReceiverKind::NonArray => {
                Err(MethodInvalidReason::WrongArrayKind)
            }
        }
    }

    /// `(min_arity, max_arity)` of the method arguments (not counting receiver).
    ///
    /// Locator and map methods accept an optional iterator variable name
    /// as first argument (e.g. `find(x) with (x > 0)`) per LRM 7.12.
    pub fn arity(self) -> (usize, usize) {
        match self {
            Self::Size
            | Self::Num
            | Self::PopFront
            | Self::PopBack
            | Self::Min
            | Self::Max
            | Self::Unique
            | Self::UniqueIndex
            | Self::Sort
            | Self::Rsort
            | Self::Reverse
            | Self::Shuffle
            | Self::Sum
            | Self::Product
            | Self::And
            | Self::Or
            | Self::Xor => (0, 0),
            Self::Find
            | Self::FindIndex
            | Self::FindFirst
            | Self::FindFirstIndex
            | Self::FindLast
            | Self::FindLastIndex
            | Self::Map
            | Self::Delete => (0, 1),
            Self::Exists
            | Self::First
            | Self::Last
            | Self::Next
            | Self::Prev
            | Self::PushFront
            | Self::PushBack => (1, 1),
            Self::Insert => (2, 2),
        }
    }

    fn return_shape(self) -> ArrayMethodReturn {
        match self {
            Self::Find
            | Self::FindFirst
            | Self::FindLast
            | Self::Min
            | Self::Max
            | Self::Unique => ArrayMethodReturn::QueueOfElem,
            Self::FindIndex | Self::FindFirstIndex | Self::FindLastIndex | Self::UniqueIndex => {
                ArrayMethodReturn::QueueOfInt
            }
            Self::Sum
            | Self::Product
            | Self::And
            | Self::Or
            | Self::Xor
            | Self::PopFront
            | Self::PopBack
            | Self::Map => ArrayMethodReturn::Elem,
            Self::Size
            | Self::Num
            | Self::Exists
            | Self::First
            | Self::Last
            | Self::Next
            | Self::Prev => ArrayMethodReturn::Int,
            Self::Delete
            | Self::PushFront
            | Self::PushBack
            | Self::Insert
            | Self::Sort
            | Self::Rsort
            | Self::Reverse
            | Self::Shuffle => ArrayMethodReturn::Void,
        }
    }

    /// Return type for this method given the receiver info.
    pub fn return_ty(self, recv: &ArrayReceiverInfo) -> Ty {
        match self.return_shape() {
            ArrayMethodReturn::QueueOfElem => Ty::Array {
                elem: Box::new(recv.elem_ty.clone()),
                dim: UnpackedDim::Queue { bound: None },
            },
            ArrayMethodReturn::QueueOfInt => Ty::Array {
                elem: Box::new(Ty::int()),
                dim: UnpackedDim::Queue { bound: None },
            },
            ArrayMethodReturn::Elem => recv.elem_ty.clone(),
            ArrayMethodReturn::Int => Ty::int(),
            ArrayMethodReturn::Void => Ty::Void,
        }
    }

    /// Whether this method returns void (must be used as statement).
    pub fn returns_void(self) -> bool {
        matches!(self.return_shape(), ArrayMethodReturn::Void)
    }

    /// Whether the ref-arg (for assoc first/last/next/prev) must be an lvalue.
    pub fn requires_ref_arg(self) -> bool {
        matches!(self, Self::First | Self::Last | Self::Next | Self::Prev)
    }

    /// Whether this is a reduction method whose return type can be
    /// overridden by a `with (expr)` clause.
    pub fn is_reduction(self) -> bool {
        matches!(
            self,
            Self::Sum | Self::Product | Self::And | Self::Or | Self::Xor
        )
    }
}

// LRM 6.16 string built-in methods

/// Parameter type for builtin method signatures (maps to `Ty` at check time).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamTy {
    Int,
    Integer,
    Byte,
    SvString,
    Real,
}

impl ParamTy {
    /// Whether an actual argument type is acceptable for this parameter.
    ///
    /// Integral params accept any integral type (implicit conversion per LRM).
    /// String and real params require exact category match.
    pub fn accepts(self, ty: &Ty) -> bool {
        match self {
            Self::Int | Self::Integer | Self::Byte => matches!(ty, Ty::Integral(_)),
            Self::SvString => *ty == Ty::String,
            Self::Real => ty.is_real(),
        }
    }
}

/// Return type for builtin method signatures.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RetTy {
    Int,
    Integer,
    Byte,
    SvString,
    Real,
    Void,
}

impl RetTy {
    /// Convert to the corresponding `Ty`.
    pub fn to_ty(self) -> Ty {
        match self {
            Self::Int => Ty::int(),
            Self::Integer => Ty::integer(),
            Self::Byte => Ty::byte(),
            Self::SvString => Ty::String,
            Self::Real => Ty::Real(crate::types::RealKw::Real),
            Self::Void => Ty::Void,
        }
    }
}

/// Declarative signature for a builtin method.
pub struct BuiltinSig {
    pub params: &'static [ParamTy],
    pub ret: RetTy,
}

/// LRM 6.16 string built-in methods.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringMethodKind {
    Len,
    Putc,
    Getc,
    Toupper,
    Tolower,
    Compare,
    Icompare,
    Substr,
    Atoi,
    Atohex,
    Atooct,
    Atobin,
    Atoreal,
    Itoa,
    Hextoa,
    Octtoa,
    Bintoa,
    Realtoa,
}

impl StringMethodKind {
    /// Resolve method name to kind.
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "len" => Some(Self::Len),
            "putc" => Some(Self::Putc),
            "getc" => Some(Self::Getc),
            "toupper" => Some(Self::Toupper),
            "tolower" => Some(Self::Tolower),
            "compare" => Some(Self::Compare),
            "icompare" => Some(Self::Icompare),
            "substr" => Some(Self::Substr),
            "atoi" => Some(Self::Atoi),
            "atohex" => Some(Self::Atohex),
            "atooct" => Some(Self::Atooct),
            "atobin" => Some(Self::Atobin),
            "atoreal" => Some(Self::Atoreal),
            "itoa" => Some(Self::Itoa),
            "hextoa" => Some(Self::Hextoa),
            "octtoa" => Some(Self::Octtoa),
            "bintoa" => Some(Self::Bintoa),
            "realtoa" => Some(Self::Realtoa),
            _ => None,
        }
    }

    /// LRM-accurate signature. Single source of truth for param/return types.
    pub fn sig(self) -> BuiltinSig {
        match self {
            Self::Len => BuiltinSig {
                params: &[],
                ret: RetTy::Int,
            },
            Self::Putc => BuiltinSig {
                params: &[ParamTy::Int, ParamTy::Byte],
                ret: RetTy::Void,
            },
            Self::Getc => BuiltinSig {
                params: &[ParamTy::Int],
                ret: RetTy::Byte,
            },
            Self::Toupper | Self::Tolower => BuiltinSig {
                params: &[],
                ret: RetTy::SvString,
            },
            Self::Compare | Self::Icompare => BuiltinSig {
                params: &[ParamTy::SvString],
                ret: RetTy::Int,
            },
            Self::Substr => BuiltinSig {
                params: &[ParamTy::Int, ParamTy::Int],
                ret: RetTy::SvString,
            },
            Self::Atoi | Self::Atohex | Self::Atooct | Self::Atobin => BuiltinSig {
                params: &[],
                ret: RetTy::Integer,
            },
            Self::Atoreal => BuiltinSig {
                params: &[],
                ret: RetTy::Real,
            },
            Self::Itoa | Self::Hextoa | Self::Octtoa | Self::Bintoa => BuiltinSig {
                params: &[ParamTy::Integer],
                ret: RetTy::Void,
            },
            Self::Realtoa => BuiltinSig {
                params: &[ParamTy::Real],
                ret: RetTy::Void,
            },
        }
    }

    /// Fixed arity (all string methods have exactly one valid arity).
    pub fn arity(self) -> (usize, usize) {
        let n = self.sig().params.len();
        (n, n)
    }

    /// Whether this method returns void (must be used as statement).
    pub fn returns_void(self) -> bool {
        matches!(self.sig().ret, RetTy::Void)
    }
}
