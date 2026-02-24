/// Syntax-node anchor for semantic structs.
///
/// Transparent alias for `ErasedAstId`. Semantic structs use `Site`
/// to name their node anchors; `lyra-ast` and `lyra-db` continue
/// to use `ErasedAstId` directly.
pub type Site = lyra_ast::ErasedAstId;
