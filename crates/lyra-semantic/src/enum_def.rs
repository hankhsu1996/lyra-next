use std::collections::HashMap;
use std::sync::Arc;

use lyra_ast::ErasedAstId;
use lyra_source::{FileId, TextRange};
use smol_str::SmolStr;

use crate::diagnostic::SemanticDiag;
use crate::record::TypeRef;
use crate::scopes::ScopeId;
use crate::type_infer::BitVecType;
use crate::types::{ConstInt, Ty};

// Index type for enum def table (file-local dense index)

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EnumDefIdx(pub u32);

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

/// Range specification on an enum member: `[N]` (count) or `[N:M]` (from-to).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EnumMemberRangeKind {
    Count(ErasedAstId),
    FromTo(ErasedAstId, ErasedAstId),
}

/// A single enum member as declared in the source.
///
/// Plain members have `range: None`; range members (`name[N]`, `name[N:M]`)
/// have `range: Some(...)`. The `enum_variants` query only expands range
/// members; plain members are real symbols found via normal scope lookup.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumMemberDef {
    pub name: SmolStr,
    pub name_range: TextRange,
    pub ast_id: ErasedAstId,
    pub range: Option<EnumMemberRangeKind>,
    pub range_text_range: Option<TextRange>,
    pub init: Option<ErasedAstId>,
    /// Bit width of a sized literal initializer (e.g., `4'h5` -> `Some(4)`).
    /// `None` if no init, init is not a literal, or literal is unsized.
    pub init_literal_width: Option<u32>,
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

/// Per-file enum definition with optional name, owner scope, and members.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDef {
    pub name: Option<SmolStr>,
    pub owner: Option<SmolStr>,
    pub ordinal: u32,
    pub scope: ScopeId,
    pub base: EnumBase,
    pub members: Box<[EnumMemberDef]>,
}

/// Diagnostic for enum member value legality (duplicate, overflow, width mismatch).
///
/// Uses `ErasedAstId` anchors; conversion to `TextRange` happens in the
/// diagnostic lowering layer only.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EnumValueDiag {
    DuplicateValue {
        anchor: ErasedAstId,
        original: ErasedAstId,
        value: i128,
        member_name: SmolStr,
    },
    Overflow {
        anchor: ErasedAstId,
        value: i128,
        width: u32,
        signed: bool,
        member_name: SmolStr,
    },
    SizedLiteralWidth {
        anchor: ErasedAstId,
        literal_width: u32,
        base_width: u32,
    },
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
    pub member_values: Arc<[ConstInt]>,
    pub diags: Box<[SemanticDiag]>,
    pub value_diags: Box<[EnumValueDiag]>,
}

/// A range-generated enum variant resolved via `EnumVariantIndex`.
///
/// Uses stable `EnumId` so consumers can derive `Ty::Enum(enum_id)` directly.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariantTarget {
    pub enum_id: EnumId,
    pub variant_ordinal: u32,
    pub def_range: TextRange,
}

/// Per-file index of range-generated enum variant names, keyed by scope.
///
/// Plain enum members are NOT in this index -- they are resolved via
/// normal scope lookup. Only range-expanded names appear here.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariantIndex {
    pub by_scope: HashMap<ScopeId, HashMap<SmolStr, EnumVariantTarget>>,
    pub diagnostics: Box<[SemanticDiag]>,
}

/// Per-package enum variant index for wildcard imports.
/// Keyed by package name, then by variant name.
pub type PkgEnumVariantIndex = HashMap<SmolStr, HashMap<SmolStr, EnumVariantTarget>>;
