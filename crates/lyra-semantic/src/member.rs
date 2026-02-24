use crate::Site;

use crate::enum_def::EnumId;
use crate::symbols::SymbolId;
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

/// LRM 7.5/7.9/7.10 array receiver classification.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArrayReceiverKind {
    Fixed,
    Dynamic,
    Queue,
    Assoc,
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

/// Classify a type as an array receiver.
pub fn classify_array_receiver(ty: &Ty) -> Option<ArrayReceiverInfo> {
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
            Some(ArrayReceiverInfo {
                kind,
                elem_ty: elem.as_ref().clone(),
                assoc_key,
            })
        }
        _ => None,
    }
}

/// LRM 7.5/7.9/7.10 built-in array methods.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArrayMethodKind {
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
}

impl ArrayMethodKind {
    /// Resolve method name to kind.
    pub fn from_name(name: &str) -> Option<Self> {
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
            _ => None,
        }
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
            ArrayReceiverKind::Fixed => Err(MethodInvalidReason::WrongArrayKind),
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
        }
    }

    /// `(min_arity, max_arity)` of the method arguments (not counting receiver).
    pub fn arity(self) -> (usize, usize) {
        match self {
            Self::Size | Self::Num | Self::PopFront | Self::PopBack => (0, 0),
            Self::Delete => (0, 1),
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

    /// Return type for this method given the receiver info.
    pub fn return_ty(self, recv: &ArrayReceiverInfo) -> Ty {
        match self {
            Self::Size
            | Self::Num
            | Self::Exists
            | Self::First
            | Self::Last
            | Self::Next
            | Self::Prev => Ty::int(),
            Self::PopFront | Self::PopBack => recv.elem_ty.clone(),
            Self::Delete | Self::PushFront | Self::PushBack | Self::Insert => Ty::Void,
        }
    }

    /// Whether this method returns void (must be used as statement).
    pub fn returns_void(self) -> bool {
        matches!(
            self,
            Self::Delete | Self::PushFront | Self::PushBack | Self::Insert
        )
    }

    /// Whether the ref-arg (for assoc first/last/next/prev) must be an lvalue.
    pub fn requires_ref_arg(self) -> bool {
        matches!(self, Self::First | Self::Last | Self::Next | Self::Prev)
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
