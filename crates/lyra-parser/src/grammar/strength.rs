use lyra_lexer::SyntaxKind;

use crate::parser::Parser;

pub(crate) fn is_drive_strength0_kw(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Supply0Kw
            | SyntaxKind::Strong0Kw
            | SyntaxKind::Pull0Kw
            | SyntaxKind::Weak0Kw
            | SyntaxKind::Highz0Kw
    )
}

pub(crate) fn is_drive_strength1_kw(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Supply1Kw
            | SyntaxKind::Strong1Kw
            | SyntaxKind::Pull1Kw
            | SyntaxKind::Weak1Kw
            | SyntaxKind::Highz1Kw
    )
}

pub(crate) fn is_drive_strength_kw(kind: SyntaxKind) -> bool {
    is_drive_strength0_kw(kind) || is_drive_strength1_kw(kind)
}

pub(crate) fn is_charge_strength_kw(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::SmallKw | SyntaxKind::MediumKw | SyntaxKind::LargeKw
    )
}

// `(` strength_kw `,` strength_kw `)` | `(` charge_kw `)`
fn parenthesized_strength(p: &mut Parser, kind: SyntaxKind, parse_inner: impl FnOnce(&mut Parser)) {
    let m = p.start();
    p.bump(); // LParen
    parse_inner(p);
    if !p.eat(SyntaxKind::RParen) {
        p.error("expected `)`");
        while !p.at(SyntaxKind::RParen) && !at_strength_recovery(p) && !p.at_end() {
            p.bump();
        }
        p.eat(SyntaxKind::RParen);
    }
    m.complete(p, kind);
}

// `( strength0_kw , strength1_kw )` or `( strength1_kw , strength0_kw )`
pub(crate) fn drive_strength(p: &mut Parser) {
    parenthesized_strength(p, SyntaxKind::DriveStrength, |p| {
        if is_drive_strength_kw(p.current()) {
            p.bump();
        } else {
            p.error("expected drive strength keyword");
            return;
        }
        if !p.eat(SyntaxKind::Comma) {
            p.error("expected `,`");
            return;
        }
        if is_drive_strength_kw(p.current()) {
            p.bump();
        } else {
            p.error("expected drive strength keyword");
        }
    });
}

// `( small | medium | large )`
pub(crate) fn charge_strength(p: &mut Parser) {
    parenthesized_strength(p, SyntaxKind::ChargeStrength, |p| {
        if is_charge_strength_kw(p.current()) {
            p.bump();
        } else {
            p.error("expected charge strength keyword");
        }
    });
}

fn at_strength_recovery(p: &Parser) -> bool {
    p.at(SyntaxKind::Semicolon)
        || p.at(SyntaxKind::EndmoduleKw)
        || p.at(SyntaxKind::EndKw)
        || p.at(SyntaxKind::EndpackageKw)
        || p.at(SyntaxKind::EndinterfaceKw)
        || p.at(SyntaxKind::EndprogramKw)
}
