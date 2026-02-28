use lyra_parser::SyntaxNode;

use crate::ast_id::{AstIdMap, ErasedAstId};
use crate::node::AstNode;
use crate::nodes::{Declarator, Port, TypedefDecl, UnpackedDimension};
use crate::support::AstChildren;

/// Source of unpacked dimensions on a declaration.
#[derive(Debug, Clone)]
pub enum UnpackedDimSource {
    Declarator(Declarator),
    Port(Port),
    TypedefDecl(TypedefDecl),
}

impl UnpackedDimSource {
    /// Resolve a name-site AST id to its unpacked-dimension source.
    ///
    /// Looks up the node via `AstIdMap`, then classifies it as one of
    /// the three carriers. Consumers call this instead of obtaining a
    /// raw `SyntaxNode` and casting.
    pub fn from_name_site(root: &SyntaxNode, map: &AstIdMap, site: ErasedAstId) -> Option<Self> {
        let node = map.get_node(root, site)?;
        Self::cast(&node)
    }

    fn cast(node: &SyntaxNode) -> Option<Self> {
        Declarator::cast(node.clone())
            .map(Self::Declarator)
            .or_else(|| Port::cast(node.clone()).map(Self::Port))
            .or_else(|| TypedefDecl::cast(node.clone()).map(Self::TypedefDecl))
    }

    /// Iterate unpacked dimensions from the underlying node.
    pub fn unpacked_dimensions(&self) -> AstChildren<UnpackedDimension> {
        match self {
            Self::Declarator(x) => x.unpacked_dimensions(),
            Self::Port(x) => x.unpacked_dimensions(),
            Self::TypedefDecl(x) => x.unpacked_dimensions(),
        }
    }
}
