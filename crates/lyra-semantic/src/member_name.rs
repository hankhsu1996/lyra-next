use lyra_lexer::SyntaxKind;
use smol_str::SmolStr;

/// A member name extracted from a syntax token.
///
/// Carries the token's `SyntaxKind` so keyword-named methods (`and`, `or`,
/// `xor`, `unique` from LRM 7.12) can be resolved structurally by the
/// token kind rather than string matching.
pub struct MemberNameToken {
    pub kind: SyntaxKind,
    pub text: SmolStr,
}
