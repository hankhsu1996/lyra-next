use lyra_parser::SyntaxNode;

use crate::node::AstNode;
use crate::nodes::{NetDecl, ParamDecl, Port, TypeSpec, TypedefDecl, VarDecl};

/// A declaration node that carries a `TypeSpec`.
///
/// Replaces kind-switch dispatch on the five container kinds
/// (`VarDecl`, `NetDecl`, `ParamDecl`, `Port`, `TypedefDecl`)
/// with a single typed enum.
#[derive(Debug, Clone)]
pub enum TypeDeclSite {
    VarDecl(VarDecl),
    NetDecl(NetDecl),
    ParamDecl(ParamDecl),
    Port(Port),
    TypedefDecl(TypedefDecl),
}

impl TypeDeclSite {
    /// Try to cast a raw `SyntaxNode` into a `TypeDeclSite`.
    ///
    /// Clones the node only on the successful branch.
    pub fn cast(node: &SyntaxNode) -> Option<Self> {
        let kind = node.kind();
        if VarDecl::can_cast(kind) {
            Some(Self::VarDecl(VarDecl::cast(node.clone())?))
        } else if NetDecl::can_cast(kind) {
            Some(Self::NetDecl(NetDecl::cast(node.clone())?))
        } else if ParamDecl::can_cast(kind) {
            Some(Self::ParamDecl(ParamDecl::cast(node.clone())?))
        } else if Port::can_cast(kind) {
            Some(Self::Port(Port::cast(node.clone())?))
        } else if TypedefDecl::can_cast(kind) {
            Some(Self::TypedefDecl(TypedefDecl::cast(node.clone())?))
        } else {
            None
        }
    }

    /// Access the underlying `SyntaxNode`.
    pub fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::VarDecl(n) => n.syntax(),
            Self::NetDecl(n) => n.syntax(),
            Self::ParamDecl(n) => n.syntax(),
            Self::Port(n) => n.syntax(),
            Self::TypedefDecl(n) => n.syntax(),
        }
    }

    /// The `TypeSpec` child, if present.
    pub fn type_spec(&self) -> Option<TypeSpec> {
        match self {
            Self::VarDecl(n) => n.type_spec(),
            Self::NetDecl(n) => n.type_spec(),
            Self::ParamDecl(n) => n.type_spec(),
            Self::Port(n) => n.type_spec(),
            Self::TypedefDecl(n) => n.type_spec(),
        }
    }

    /// Walk ancestors of `node` and return the first `TypeDeclSite`.
    pub fn closest_ancestor(node: &SyntaxNode) -> Option<Self> {
        node.ancestors().find_map(|n| Self::cast(&n))
    }
}
