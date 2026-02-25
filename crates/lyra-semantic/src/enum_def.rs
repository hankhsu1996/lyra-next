use std::collections::HashMap;
use std::sync::Arc;

use crate::Site;
use lyra_source::{FileId, NameSpan};
use smol_str::SmolStr;

use crate::diagnostic::SemanticDiag;
use crate::record::TypeRef;
use crate::scopes::ScopeId;
use crate::type_infer::BitVecType;
use crate::types::{ConstInt, Ty};

// Index type for enum def table (file-local dense index)

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EnumDefIdx(pub u32);

/// Stable identity for an enum type definition.
///
/// Anchored to the `EnumType` CST node's `Site`. Invariants:
/// - Anchor is always a `SyntaxKind::EnumType` node.
/// - Builder collects exactly once per definition via `collect_enum_def`.
/// - File identity derived via `Site::file()`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct EnumId(Site);

impl EnumId {
    pub fn new(def: Site) -> Self {
        Self(def)
    }

    pub fn file(self) -> FileId {
        self.0.file()
    }

    pub fn as_erased(self) -> Site {
        self.0
    }
}

/// Range specification on an enum member: `[N]` (count) or `[N:M]` (from-to).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EnumMemberRangeKind {
    Count(Site),
    FromTo(Site, Site),
}

/// A single enum member as declared in the source.
///
/// Plain members have `range: None`; range members (`name[N]`, `name[N:M]`)
/// have `range: Some(...)`. The `enum_variants` query only expands range
/// members; plain members are real symbols found via normal scope lookup.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumMemberDef {
    pub name: SmolStr,
    pub name_site: Site,
    pub name_span: NameSpan,
    pub range: Option<EnumMemberRangeKind>,
    pub range_site: Option<Site>,
    pub init: Option<Site>,
    /// Bit width of a sized literal initializer (e.g., `4'h5` -> `Some(4)`).
    /// `None` if no init, init is not a literal, or literal is unsized.
    pub init_literal_width: Option<u32>,
}

/// Wrapper for enum base type that always carries a stable anchor.
///
/// `TypeRef::Resolved` has no site, so this wrapper ensures diagnostics
/// always have an anchor to point to.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumBase {
    pub tref: TypeRef,
    pub type_site: Site,
}

/// Per-file enum definition with optional name, owner scope, and members.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDef {
    pub name: Option<SmolStr>,
    pub enum_type_site: Site,
    pub scope: ScopeId,
    pub base: EnumBase,
    pub members: Box<[EnumMemberDef]>,
}

/// Diagnostic for enum member value legality (duplicate, overflow, width mismatch).
///
/// Uses `Site` anchors; conversion to `TextRange` happens in the
/// diagnostic lowering layer only.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EnumValueDiag {
    DuplicateValue {
        anchor: Site,
        original: Site,
        value: i128,
        member_name: SmolStr,
    },
    Overflow {
        anchor: Site,
        value: i128,
        width: u32,
        signed: bool,
        member_name: SmolStr,
    },
    SizedLiteralWidth {
        anchor: Site,
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
    /// Per-enum ordering index for value assignment; not a stable identity key.
    pub variant_ordinal: u32,
    pub name_site: Site,
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
