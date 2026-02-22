use crate::enum_def::EnumId;
use crate::symbols::SymbolId;
use crate::types::Ty;

/// Information about a resolved member access.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemberInfo {
    pub ty: Ty,
    pub kind: MemberKind,
}

/// What kind of member was accessed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemberKind {
    Field { index: u32 },
    InterfaceMember { member: SymbolId },
    BuiltinMethod(BuiltinMethodKind),
}

/// Reasons a member lookup can fail.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemberLookupError {
    NoMembersOnType,
    UnknownMember,
    NotInModport,
}

/// A built-in method on a type. Structured by receiver category.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinMethodKind {
    Enum(EnumMethodKind),
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
