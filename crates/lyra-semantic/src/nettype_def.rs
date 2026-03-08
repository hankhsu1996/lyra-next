use crate::Site;
use lyra_source::TokenSpan;
use smol_str::SmolStr;

/// Dense file-local index into `DefIndex.nettype_defs`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NettypeDefIdx(pub(crate) u32);

/// Raw structural description of a nettype declaration (LRM 6.6.7).
///
/// Stores the declaration anchor, name, type-spec anchor, and optional
/// resolve function reference. Alias-vs-define classification is a
/// semantic query (resolution-based), not stored here.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NettypeDef {
    pub decl_site: Site,
    pub name: SmolStr,
    /// Anchor for the `TypeSpec` child (mandatory in well-formed declarations).
    pub first_type_site: Site,
    pub resolve_fn: Option<ResolveFnRef>,
}

/// Name and source span for a resolve function reference in a `with` clause.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolveFnRef {
    pub name: SmolStr,
    pub span: TokenSpan,
}
