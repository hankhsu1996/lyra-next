use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;

use crate::node::{AstNode, HasSyntax};
use crate::nodes::{
    BlockStmt, CaseItem, CaseStmt, ForStmt, GenerateRegion, IfStmt, InterfaceBody, ModuleBody,
    ModuleInstantiation,
};

/// Typed sum for generate-level items inside module/interface bodies
/// and generate constructs.
#[derive(Debug, Clone)]
pub enum GenerateItem {
    ModuleInstantiation(ModuleInstantiation),
    IfStmt(IfStmt),
    ForStmt(ForStmt),
    CaseStmt(CaseStmt),
    GenerateRegion(GenerateRegion),
    BlockStmt(BlockStmt),
}

impl GenerateItem {
    /// Try to cast a raw `SyntaxNode` into a `GenerateItem`.
    ///
    /// Clones the node only on the successful branch.
    pub fn cast(node: &SyntaxNode) -> Option<Self> {
        let kind = node.kind();
        if ModuleInstantiation::can_cast(kind) {
            Some(Self::ModuleInstantiation(ModuleInstantiation::cast(
                node.clone(),
            )?))
        } else if IfStmt::can_cast(kind) {
            Some(Self::IfStmt(IfStmt::cast(node.clone())?))
        } else if ForStmt::can_cast(kind) {
            Some(Self::ForStmt(ForStmt::cast(node.clone())?))
        } else if CaseStmt::can_cast(kind) {
            Some(Self::CaseStmt(CaseStmt::cast(node.clone())?))
        } else if GenerateRegion::can_cast(kind) {
            Some(Self::GenerateRegion(GenerateRegion::cast(node.clone())?))
        } else if BlockStmt::can_cast(kind) {
            Some(Self::BlockStmt(BlockStmt::cast(node.clone())?))
        } else {
            None
        }
    }

    /// Access the underlying `SyntaxNode`.
    pub fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::ModuleInstantiation(n) => n.syntax(),
            Self::IfStmt(n) => n.syntax(),
            Self::ForStmt(n) => n.syntax(),
            Self::CaseStmt(n) => n.syntax(),
            Self::GenerateRegion(n) => n.syntax(),
            Self::BlockStmt(n) => n.syntax(),
        }
    }
}

impl ModuleBody {
    pub fn generate_items(&self) -> impl Iterator<Item = GenerateItem> + '_ {
        self.syntax()
            .children()
            .filter_map(|c| GenerateItem::cast(&c))
    }
}

impl InterfaceBody {
    pub fn generate_items(&self) -> impl Iterator<Item = GenerateItem> + '_ {
        self.syntax()
            .children()
            .filter_map(|c| GenerateItem::cast(&c))
    }
}

impl GenerateRegion {
    pub fn generate_items(&self) -> impl Iterator<Item = GenerateItem> + '_ {
        self.syntax()
            .children()
            .filter_map(|c| GenerateItem::cast(&c))
    }
}

impl BlockStmt {
    pub fn generate_items(&self) -> impl Iterator<Item = GenerateItem> + '_ {
        self.syntax()
            .children()
            .filter_map(|c| GenerateItem::cast(&c))
    }
}

/// Typed sum for containers that host `generate_items()`.
#[derive(Debug, Clone)]
pub enum GenerateScope {
    ModuleBody(ModuleBody),
    InterfaceBody(InterfaceBody),
    GenerateRegion(GenerateRegion),
    BlockStmt(BlockStmt),
}

impl GenerateScope {
    /// Try to cast a raw `SyntaxNode` into a `GenerateScope`.
    pub fn cast(node: &SyntaxNode) -> Option<Self> {
        let kind = node.kind();
        if ModuleBody::can_cast(kind) {
            Some(Self::ModuleBody(ModuleBody::cast(node.clone())?))
        } else if InterfaceBody::can_cast(kind) {
            Some(Self::InterfaceBody(InterfaceBody::cast(node.clone())?))
        } else if GenerateRegion::can_cast(kind) {
            Some(Self::GenerateRegion(GenerateRegion::cast(node.clone())?))
        } else if BlockStmt::can_cast(kind) {
            Some(Self::BlockStmt(BlockStmt::cast(node.clone())?))
        } else {
            None
        }
    }

    pub fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::ModuleBody(n) => n.syntax(),
            Self::InterfaceBody(n) => n.syntax(),
            Self::GenerateRegion(n) => n.syntax(),
            Self::BlockStmt(n) => n.syntax(),
        }
    }

    pub fn generate_items(&self) -> impl Iterator<Item = GenerateItem> + '_ {
        self.syntax()
            .children()
            .filter_map(|c| GenerateItem::cast(&c))
    }
}

/// Typed sum for generate construct bodies.
///
/// Block/Region bodies are treated as scopes (containers of items).
/// Cast tries `GenerateScope` first, so `BlockStmt` and `GenerateRegion`
/// as bodies are always scopes, never standalone items.
#[derive(Debug, Clone)]
pub enum GenerateBody {
    Scope(GenerateScope),
    Item(GenerateItem),
}

impl GenerateBody {
    /// Try to cast a raw `SyntaxNode` into a `GenerateBody`.
    ///
    /// Prefers `GenerateScope` over `GenerateItem` so that `BlockStmt`
    /// and `GenerateRegion` are always scopes.
    pub fn cast(node: &SyntaxNode) -> Option<Self> {
        if let Some(scope) = GenerateScope::cast(node) {
            return Some(Self::Scope(scope));
        }
        if let Some(item) = GenerateItem::cast(node) {
            return Some(Self::Item(item));
        }
        None
    }

    pub fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::Scope(s) => s.syntax(),
            Self::Item(i) => i.syntax(),
        }
    }

    /// The block label identifier if the body is a `BlockStmt` with `begin : name`.
    pub fn block_name(&self) -> Option<lyra_parser::SyntaxToken> {
        match self {
            Self::Scope(GenerateScope::BlockStmt(block)) => block.block_name(),
            _ => None,
        }
    }
}

/// Find the first node child after a token with the given kind.
fn first_node_after(parent: &SyntaxNode, anchor: SyntaxKind) -> Option<SyntaxNode> {
    let mut seen_anchor = false;
    for el in parent.children_with_tokens() {
        match el {
            rowan::NodeOrToken::Token(tok) => {
                if tok.kind() == anchor {
                    seen_anchor = true;
                }
            }
            rowan::NodeOrToken::Node(node) => {
                if seen_anchor {
                    return Some(node);
                }
            }
        }
    }
    None
}

impl IfStmt {
    /// The then-branch generate body (first node after `RParen`).
    pub fn then_generate_body(&self) -> Option<GenerateBody> {
        first_node_after(self.syntax(), SyntaxKind::RParen).and_then(|n| GenerateBody::cast(&n))
    }

    /// The else-branch generate body (first node after `ElseKw`).
    pub fn else_generate_body(&self) -> Option<GenerateBody> {
        first_node_after(self.syntax(), SyntaxKind::ElseKw).and_then(|n| GenerateBody::cast(&n))
    }
}

impl ForStmt {
    /// The generate body (first node after `RParen`).
    pub fn generate_body(&self) -> Option<GenerateBody> {
        first_node_after(self.syntax(), SyntaxKind::RParen).and_then(|n| GenerateBody::cast(&n))
    }
}

impl CaseItem {
    /// The generate body (first node after `Colon`).
    pub fn generate_body(&self) -> Option<GenerateBody> {
        first_node_after(self.syntax(), SyntaxKind::Colon).and_then(|n| GenerateBody::cast(&n))
    }
}
