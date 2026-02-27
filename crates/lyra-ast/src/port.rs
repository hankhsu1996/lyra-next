use lyra_lexer::SyntaxKind;

use crate::nodes::{Port, TfPortDecl};

/// Semantic port direction extracted from the source keyword.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PortDirection {
    Input,
    Output,
    Inout,
    Ref,
}

impl PortDirection {
    fn from_token_kind(kind: SyntaxKind) -> Option<Self> {
        match kind {
            SyntaxKind::InputKw => Some(Self::Input),
            SyntaxKind::OutputKw => Some(Self::Output),
            SyntaxKind::InoutKw => Some(Self::Inout),
            SyntaxKind::RefKw => Some(Self::Ref),
            _ => None,
        }
    }
}

impl Port {
    /// Parsed direction keyword in the source (if any).
    pub fn direction(&self) -> Option<PortDirection> {
        self.direction_token()
            .and_then(|t| PortDirection::from_token_kind(t.kind()))
    }
}

impl TfPortDecl {
    /// Parsed direction keyword in the source (if any).
    pub fn direction(&self) -> Option<PortDirection> {
        self.direction_token()
            .and_then(|t| PortDirection::from_token_kind(t.kind()))
    }
}
