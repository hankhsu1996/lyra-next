use lyra_parser::SyntaxNode;

use crate::node::AstNode;
use crate::nodes::{
    BlockStmt, CaseStmt, ForStmt, GenerateRegion, IfStmt, InterfaceBody, ModuleBody,
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
