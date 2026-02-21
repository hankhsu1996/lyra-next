use lyra_lexer::SyntaxKind;

use crate::parser::Parser;

use super::expressions;

// `parameter` or `localparam` declaration as a module item (with semicolon).
pub(crate) fn param_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // parameter | localparam
    if is_data_type_keyword(p.current())
        || p.at(SyntaxKind::SignedKw)
        || p.at(SyntaxKind::UnsignedKw)
    {
        type_spec(p);
    }
    param_declarator(p);
    while p.eat(SyntaxKind::Comma) {
        param_declarator(p);
    }
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::ParamDecl);
}

fn param_declarator(p: &mut Parser) {
    let m = p.start();
    p.expect(SyntaxKind::Ident);
    if p.eat(SyntaxKind::Assign) {
        expressions::expr(p);
    }
    m.complete(p, SyntaxKind::Declarator);
}

// Net declaration: `wire [7:0] a, b ;`
pub(crate) fn net_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // net type keyword (wire, tri, etc.)
    // Optional signing
    if p.at(SyntaxKind::SignedKw) || p.at(SyntaxKind::UnsignedKw) {
        p.bump();
    }
    // Optional packed dimensions
    while p.at(SyntaxKind::LBracket) {
        packed_dimension(p);
    }
    // Declarators
    net_declarator(p);
    while p.eat(SyntaxKind::Comma) {
        net_declarator(p);
    }
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::NetDecl);
}

fn net_declarator(p: &mut Parser) {
    let m = p.start();
    p.expect(SyntaxKind::Ident);
    while p.at(SyntaxKind::LBracket) {
        unpacked_dimension(p);
    }
    if p.eat(SyntaxKind::Assign) {
        expressions::expr(p);
    }
    m.complete(p, SyntaxKind::Declarator);
}

// Variable declaration: `logic [7:0] a = 0, b ;`
// Also handles declarations starting with direction keywords or user-defined types.
pub(crate) fn var_decl(p: &mut Parser) {
    let m = p.start();
    // Optional direction for port-like declarations in module body
    if matches!(
        p.current(),
        SyntaxKind::InputKw | SyntaxKind::OutputKw | SyntaxKind::InoutKw
    ) {
        p.bump();
    }
    // Optional var keyword
    if p.at(SyntaxKind::VarKw) {
        p.bump();
    }
    // Type
    type_spec(p);
    // Declarators
    var_declarator(p);
    while p.eat(SyntaxKind::Comma) {
        var_declarator(p);
    }
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::VarDecl);
}

fn var_declarator(p: &mut Parser) {
    let m = p.start();
    p.expect(SyntaxKind::Ident);
    while p.at(SyntaxKind::LBracket) {
        unpacked_dimension(p);
    }
    if p.eat(SyntaxKind::Assign) {
        expressions::expr(p);
    }
    m.complete(p, SyntaxKind::Declarator);
}

// Parse a type specifier: `logic`, `reg`, `bit`, `logic [7:0]`, `Ident`, etc.
pub(crate) fn type_spec(p: &mut Parser) {
    let m = p.start();
    if p.at(SyntaxKind::EnumKw) {
        enum_type(p);
    } else if p.at(SyntaxKind::StructKw) || p.at(SyntaxKind::UnionKw) {
        struct_type(p);
    } else if is_data_type_keyword(p.current()) {
        p.bump();
    } else if p.at(SyntaxKind::Ident) {
        if p.nth(1) == SyntaxKind::ColonColon && p.nth(2) == SyntaxKind::Ident {
            let qn = p.start();
            p.bump(); // pkg
            while p.at(SyntaxKind::ColonColon) && p.nth(1) == SyntaxKind::Ident {
                p.bump(); // ::
                p.bump(); // segment
            }
            qn.complete(p, SyntaxKind::QualifiedName);
        } else if p.nth(1) == SyntaxKind::Dot && p.nth(2) == SyntaxKind::Ident {
            let dn = p.start();
            let nr = p.start();
            p.expect(SyntaxKind::Ident); // interface name
            nr.complete(p, SyntaxKind::NameRef);
            p.expect(SyntaxKind::Dot);
            p.expect(SyntaxKind::Ident); // modport name
            dn.complete(p, SyntaxKind::DottedName);
        } else {
            let nr = p.start();
            p.bump();
            nr.complete(p, SyntaxKind::NameRef);
        }
    } else {
        p.error("expected type");
        m.abandon(p);
        return;
    }
    // Optional signing
    if p.at(SyntaxKind::SignedKw) || p.at(SyntaxKind::UnsignedKw) {
        p.bump();
    }
    // Packed dimensions
    while p.at(SyntaxKind::LBracket) {
        packed_dimension(p);
    }
    m.complete(p, SyntaxKind::TypeSpec);
}

pub(crate) fn packed_dimension(p: &mut Parser) {
    let m = p.start();
    p.bump(); // [
    if !p.at(SyntaxKind::RBracket) {
        expressions::expr(p);
        if p.eat(SyntaxKind::Colon) {
            expressions::expr(p);
        }
    }
    p.expect(SyntaxKind::RBracket);
    m.complete(p, SyntaxKind::PackedDimension);
}

pub(crate) fn unpacked_dimension(p: &mut Parser) {
    let m = p.start();
    p.bump(); // [
    if !p.at(SyntaxKind::RBracket) {
        expressions::expr(p);
        if p.eat(SyntaxKind::Colon) {
            expressions::expr(p);
        }
    }
    p.expect(SyntaxKind::RBracket);
    m.complete(p, SyntaxKind::UnpackedDimension);
}

// Parse a typedef declaration: `typedef <type> <name> ;`
pub(crate) fn typedef_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // typedef
    type_spec(p);
    p.expect(SyntaxKind::Ident);
    while p.at(SyntaxKind::LBracket) {
        unpacked_dimension(p);
    }
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::TypedefDecl);
}

// Parse enum type body: `enum [base_type] '{' member {',' member} '}'`
// Produces an EnumType child node within the enclosing TypeSpec.
fn enum_type(p: &mut Parser) {
    let m = p.start();
    p.bump(); // enum
    // Optional base type: keyword integral types or user-defined type names
    if is_base_type_keyword(p.current()) || p.at(SyntaxKind::Ident) {
        type_spec(p);
    }
    p.expect(SyntaxKind::LBrace);
    if !p.at(SyntaxKind::RBrace) {
        enum_member(p);
        while p.eat(SyntaxKind::Comma) {
            if p.at(SyntaxKind::RBrace) {
                break;
            }
            enum_member(p);
        }
    }
    p.expect(SyntaxKind::RBrace);
    m.complete(p, SyntaxKind::EnumType);
}

fn enum_member(p: &mut Parser) {
    let m = p.start();
    p.expect(SyntaxKind::Ident);
    if p.eat(SyntaxKind::Assign) {
        // Expression wrapper: canonical single-child wrapper for embedded
        // expressions. More sites (Declarator init, dimensions) will migrate later.
        let w = p.start();
        expressions::expr(p);
        w.complete(p, SyntaxKind::Expression);
    }
    m.complete(p, SyntaxKind::EnumMember);
}

// Parse struct/union type body:
// `(struct|union) [soft] [packed] [tagged] [signing] '{' member+ '}'`
// `[soft]` is union-only (LRM 7.3.1).
// Produces a StructType child node within the enclosing TypeSpec.
fn struct_type(p: &mut Parser) {
    let m = p.start();
    let is_union = p.at(SyntaxKind::UnionKw);
    p.bump(); // struct | union
    // soft is union-only (LRM 7.3.1)
    if is_union && p.at(SyntaxKind::SoftKw) {
        p.bump();
    }
    if p.at(SyntaxKind::PackedKw) {
        p.bump();
    }
    if p.at(SyntaxKind::TaggedKw) {
        p.bump();
    }
    // Optional signing after packed
    if p.at(SyntaxKind::SignedKw) || p.at(SyntaxKind::UnsignedKw) {
        p.bump();
    }
    p.expect(SyntaxKind::LBrace);
    while !p.at(SyntaxKind::RBrace) && !p.at(SyntaxKind::Eof) {
        let cp = p.checkpoint();
        struct_member(p);
        if !p.has_progressed(cp) {
            p.error_bump("expected struct member");
        }
    }
    p.expect(SyntaxKind::RBrace);
    m.complete(p, SyntaxKind::StructType);
}

fn struct_member(p: &mut Parser) {
    let m = p.start();
    type_spec(p);
    var_declarator(p);
    while p.eat(SyntaxKind::Comma) {
        var_declarator(p);
    }
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::StructMember);
}

// Keyword subset of valid enum base types (integral types only, per LRM 6.19).
// User-defined type names are also valid but handled via the Ident guard in enum_type().
fn is_base_type_keyword(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::LogicKw
            | SyntaxKind::RegKw
            | SyntaxKind::BitKw
            | SyntaxKind::IntegerKw
            | SyntaxKind::IntKw
            | SyntaxKind::ShortintKw
            | SyntaxKind::LongintKw
            | SyntaxKind::ByteKw
    )
}

// The 16 scalar SystemVerilog type keywords (LRM Table 6-1 data types minus
// aggregate constructors enum/struct/union). Shared predicate that other
// predicates compose from.
pub(crate) fn is_scalar_type_keyword(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::LogicKw
            | SyntaxKind::RegKw
            | SyntaxKind::BitKw
            | SyntaxKind::IntegerKw
            | SyntaxKind::IntKw
            | SyntaxKind::ShortintKw
            | SyntaxKind::LongintKw
            | SyntaxKind::ByteKw
            | SyntaxKind::TimeKw
            | SyntaxKind::RealtimeKw
            | SyntaxKind::RealKw
            | SyntaxKind::ShortRealKw
            | SyntaxKind::StringKw
            | SyntaxKind::ChandleKw
            | SyntaxKind::EventKw
            | SyntaxKind::VoidKw
    )
}

pub(crate) fn is_data_type_keyword(kind: SyntaxKind) -> bool {
    is_scalar_type_keyword(kind)
        || matches!(
            kind,
            SyntaxKind::EnumKw | SyntaxKind::StructKw | SyntaxKind::UnionKw
        )
}

/// Tokens that unambiguously start a data declaration.
/// Does NOT handle bare `Ident Ident` (ambiguous with module instantiation).
pub(crate) fn at_unambiguous_data_decl_start(p: &Parser) -> bool {
    let k = p.current();
    if is_data_type_keyword(k) || is_net_type(k) {
        return true;
    }
    matches!(
        k,
        SyntaxKind::SignedKw
            | SyntaxKind::UnsignedKw
            | SyntaxKind::VarKw
            | SyntaxKind::InputKw
            | SyntaxKind::OutputKw
            | SyntaxKind::InoutKw
    ) || (k == SyntaxKind::Ident && p.nth(1) == SyntaxKind::ColonColon)
        || (k == SyntaxKind::Ident
            && p.nth(1) == SyntaxKind::Dot
            && p.nth(2) == SyntaxKind::Ident
            && p.nth(3) == SyntaxKind::Ident)
}

fn is_net_type(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::WireKw
            | SyntaxKind::TriKw
            | SyntaxKind::WandKw
            | SyntaxKind::WorKw
            | SyntaxKind::Tri0Kw
            | SyntaxKind::Tri1Kw
            | SyntaxKind::TriregKw
            | SyntaxKind::Supply0Kw
            | SyntaxKind::Supply1Kw
            | SyntaxKind::UwireKw
    )
}
