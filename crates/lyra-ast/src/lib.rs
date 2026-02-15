use lyra_lexer::{NODE_START, SyntaxKind};
use lyra_source::{FileId, TextSize};

/// Stable identity for an AST node: file + (kind, offset).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstId {
    pub file: FileId,
    pub kind: SyntaxKind,
    pub offset: TextSize,
}

/// Per-file map from `SyntaxNode` to `AstId`.
///
/// Built once after parsing; used by semantic layer for stable references.
pub struct AstIdMap {
    entries: Vec<AstId>,
}

impl AstIdMap {
    pub fn from_root(file: FileId, root: &lyra_parser::SyntaxNode) -> Self {
        let mut entries = Vec::new();
        collect(file, root, &mut entries);
        Self { entries }
    }

    pub fn entries(&self) -> &[AstId] {
        &self.entries
    }
}

fn collect(file: FileId, node: &lyra_parser::SyntaxNode, out: &mut Vec<AstId>) {
    let kind: SyntaxKind = node.kind();
    if (kind as u16) >= NODE_START {
        out.push(AstId {
            file,
            kind,
            offset: node.text_range().start(),
        });
    }
    for child in node.children() {
        collect(file, &child, out);
    }
}
