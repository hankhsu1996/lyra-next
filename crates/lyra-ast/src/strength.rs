use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxToken;

use crate::nodes::{ChargeStrength, DriveStrength};

fn is_strength0_kw(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Supply0Kw
            | SyntaxKind::Strong0Kw
            | SyntaxKind::Pull0Kw
            | SyntaxKind::Weak0Kw
            | SyntaxKind::Highz0Kw
    )
}

fn is_strength1_kw(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Supply1Kw
            | SyntaxKind::Strong1Kw
            | SyntaxKind::Pull1Kw
            | SyntaxKind::Weak1Kw
            | SyntaxKind::Highz1Kw
    )
}

impl DriveStrength {
    /// The strength0 keyword token (`supply0`, `strong0`, `pull0`, `weak0`, `highz0`).
    pub fn strength0_kw(&self) -> Option<SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .find(|tok| is_strength0_kw(tok.kind()))
    }

    /// The strength1 keyword token (`supply1`, `strong1`, `pull1`, `weak1`, `highz1`).
    pub fn strength1_kw(&self) -> Option<SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .find(|tok| is_strength1_kw(tok.kind()))
    }
}

impl ChargeStrength {
    /// The charge strength keyword token (`small`, `medium`, or `large`).
    pub fn keyword(&self) -> Option<SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .find(|tok| {
                matches!(
                    tok.kind(),
                    SyntaxKind::SmallKw | SyntaxKind::MediumKw | SyntaxKind::LargeKw
                )
            })
    }
}
