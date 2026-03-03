// Shared predicates for net-type keywords.

use lyra_lexer::SyntaxKind;

/// Whether `kind` is a net-declaration head keyword (LRM 6.7, 6.6.8).
///
/// Includes both traditional net-type keywords (`wire`, `tri`, etc.)
/// and `interconnect`.
pub(crate) fn is_net_type_kw(kind: SyntaxKind) -> bool {
    kind.is_net_type_kw() || kind == SyntaxKind::InterconnectKw
}

/// Error message for strength on interconnect nets. Centralized so
/// the drive-strength and charge-strength branches stay consistent.
pub(crate) const INTERCONNECT_STRENGTH_MSG: &str =
    "strength specification is not allowed on interconnect nets";
