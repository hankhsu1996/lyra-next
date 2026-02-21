use lyra_ast::ErasedAstId;
use lyra_source::{FileId, TextRange};
use smol_str::SmolStr;

use crate::diagnostic::SemanticDiag;
use crate::record::TypeRef;
use crate::scopes::ScopeId;
use crate::type_infer::BitVecType;
use crate::types::Ty;

// Index type for enum def table (file-local dense index)

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EnumDefIdx(pub(crate) u32);

// Global IDs (offset-independent, scope-owner-local ordinal)

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumId {
    pub file: FileId,
    pub owner: Option<SmolStr>,
    pub ordinal: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariantId {
    pub enum_id: EnumId,
    pub variant_ordinal: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: SmolStr,
    pub init: Option<ErasedAstId>,
}

/// Wrapper for enum base type that always carries the source range.
///
/// `TypeRef::Resolved` has no span, so this wrapper ensures diagnostics
/// always have a range to anchor to.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumBase {
    pub tref: TypeRef,
    pub range: TextRange,
}

/// Per-file enum definition with optional name, owner scope, and variants.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDef {
    pub name: Option<SmolStr>,
    pub owner: Option<SmolStr>,
    pub ordinal: u32,
    pub scope: ScopeId,
    pub base: EnumBase,
    pub variants: Box<[EnumVariant]>,
}

/// Resolved enum type information.
///
/// `base_ty` is the resolved `Ty` (for diagnostics/printing).
/// `base_int` is the canonical integral cast target -- `Some(BitVecType)`
/// when legal, `None` when illegal (non-integral base, unevaluated dims, error).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumSem {
    pub base_ty: Ty,
    pub base_int: Option<BitVecType>,
    pub diags: Box<[SemanticDiag]>,
}
