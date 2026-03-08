use lyra_lexer::SyntaxKind;

use crate::parser::{CompletedMarker, Parser};

use super::ports::type_param_declarator;
use super::{expressions, net, strength};

// `parameter` or `localparam` declaration as a module item (with semicolon).
pub(crate) fn param_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // parameter | localparam
    // Type parameter: `parameter type T [= type_spec] ;`
    if p.at(SyntaxKind::TypeKw) {
        p.bump();
        type_param_declarator(p);
        while p.eat(SyntaxKind::Comma) {
            type_param_declarator(p);
        }
        p.expect(SyntaxKind::Semicolon);
        m.complete(p, SyntaxKind::ParamDecl);
        return;
    }
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
    let ts = p.start();
    let net_kw = p.current();
    p.bump(); // net type keyword (wire, tri, etc.)
    // Optional strength specification (LRM 6.3.2)
    // Interconnect nets do not allow strength (LRM 6.6.8).
    if p.at(SyntaxKind::LParen) {
        if net_kw == SyntaxKind::InterconnectKw {
            if strength::is_drive_strength_kw(p.nth(1)) {
                p.error(net::INTERCONNECT_STRENGTH_MSG);
                strength::drive_strength(p);
            } else if strength::is_charge_strength_kw(p.nth(1)) {
                p.error(net::INTERCONNECT_STRENGTH_MSG);
                strength::charge_strength(p);
            }
        } else if net_kw == SyntaxKind::TriregKw && strength::is_charge_strength_kw(p.nth(1)) {
            strength::charge_strength(p);
        } else if strength::is_drive_strength_kw(p.nth(1)) {
            strength::drive_strength(p);
        }
    }
    // Optional signing
    if p.at(SyntaxKind::SignedKw) || p.at(SyntaxKind::UnsignedKw) {
        p.bump();
    }
    // Optional packed dimensions
    while p.at(SyntaxKind::LBracket) {
        packed_dimension(p);
    }
    ts.complete(p, SyntaxKind::TypeSpec);
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
// Also handles declarations starting with direction keywords, `const`, or user-defined types.
// Non-ANSI port direction declarations (`input a;`) have implicit type (LRM 23.2.2.1).
pub(crate) fn var_decl(p: &mut Parser) {
    let m = p.start();
    // Optional lifetime qualifier (LRM 6.21)
    if matches!(p.current(), SyntaxKind::StaticKw | SyntaxKind::AutomaticKw) {
        p.bump();
    }
    // Optional const qualifier (LRM 6.20.6)
    if p.at(SyntaxKind::ConstKw) {
        p.bump();
    }
    // Optional direction for port-like declarations in module body
    let has_direction = matches!(
        p.current(),
        SyntaxKind::InputKw | SyntaxKind::OutputKw | SyntaxKind::InoutKw
    );
    if has_direction {
        p.bump();
    }
    // Optional var keyword
    if p.at(SyntaxKind::VarKw) {
        p.bump();
    }
    // Type: non-ANSI port direction declarations may have implicit type.
    // `input a, b;` has no type at all. `input [7:0] a;` and
    // `input signed [7:0] a;` have implicit type with range/signing only.
    if has_direction && is_bare_port_ident(p) {
        // Bare identifier list, no type at all (e.g. `input a, b;`).
    } else if has_direction && is_implicit_type_start(p) {
        // Implicit type: optional signing + packed dimensions (LRM 6.10).
        implicit_type_spec(p);
    } else {
        type_spec(p);
    }
    // Declarators
    var_declarator(p);
    while p.eat(SyntaxKind::Comma) {
        var_declarator(p);
    }
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::VarDecl);
}

// After a direction keyword, check whether the identifier list has no explicit
// type (non-ANSI port direction declaration like `input a, b;`). True when the
// current token is a plain identifier followed by `;` or `,`.
fn is_bare_port_ident(p: &Parser) -> bool {
    p.current() == SyntaxKind::Ident
        && matches!(p.nth(1), SyntaxKind::Semicolon | SyntaxKind::Comma)
}

// After a direction keyword, check whether the next tokens form an implicit
// type (signing and/or packed dimensions without a base type keyword).
// Matches `[7:0]`, `signed`, `signed [7:0]`, etc.
fn is_implicit_type_start(p: &Parser) -> bool {
    matches!(
        p.current(),
        SyntaxKind::SignedKw | SyntaxKind::UnsignedKw | SyntaxKind::LBracket
    )
}

// Parse an implicit data type: optional signing + packed dimensions.
// Produces a `TypeSpec` node with no base type keyword.
fn implicit_type_spec(p: &mut Parser) {
    let m = p.start();
    if p.at(SyntaxKind::SignedKw) || p.at(SyntaxKind::UnsignedKw) {
        p.bump();
    }
    while p.at(SyntaxKind::LBracket) {
        packed_dimension(p);
    }
    m.complete(p, SyntaxKind::TypeSpec);
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

// Parse `type(operand)` into a TypeExpr node (LRM 6.23).
//
// Operand disambiguation: attempt type_spec first under checkpoint if the
// next token plausibly starts a type; accept as type(data_type) only if it
// ends right before `)`. Otherwise rollback and parse as expr().
pub(crate) fn type_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.bump(); // TypeKw
    p.expect(SyntaxKind::LParen);

    if is_data_type_keyword(p.current())
        || p.at(SyntaxKind::Ident)
        || (p.at(SyntaxKind::TypeKw) && p.nth(1) == SyntaxKind::LParen)
    {
        let state = p.save_state();
        type_spec(p);
        if !p.at(SyntaxKind::RParen) {
            p.restore_state(state);
            expressions::expr(p);
        }
    } else {
        expressions::expr(p);
    }

    p.expect(SyntaxKind::RParen);
    m.complete(p, SyntaxKind::TypeExpr)
}

// Parse a type specifier: `logic`, `reg`, `bit`, `logic [7:0]`, `Ident`, etc.
pub(crate) fn type_spec(p: &mut Parser) {
    let m = p.start();
    // type(operand) as a complete type reference (LRM 6.23)
    if p.at(SyntaxKind::TypeKw) && p.nth(1) == SyntaxKind::LParen {
        type_expr(p);
        m.complete(p, SyntaxKind::TypeSpec);
        return;
    }
    if p.at(SyntaxKind::EnumKw) {
        enum_type(p);
    } else if p.at(SyntaxKind::StructKw) || p.at(SyntaxKind::UnionKw) {
        struct_type(p);
    } else if is_data_type_keyword(p.current()) {
        p.bump();
    } else if super::is_unit_scope_prefix(p)
        && p.nth(1) == SyntaxKind::ColonColon
        && p.nth(2) == SyntaxKind::Ident
    {
        super::parse_qualified_name(p);
    } else if p.at(SyntaxKind::Ident) {
        if p.nth(1) == SyntaxKind::ColonColon && p.nth(2) == SyntaxKind::Ident {
            super::parse_qualified_name(p);
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
    } else if p.at(SyntaxKind::InterfaceKw) {
        p.bump();
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
    if p.at(SyntaxKind::RBracket) {
        // [] -- unsized/dynamic
    } else if p.at(SyntaxKind::Star) && p.nth(1) == SyntaxKind::RBracket {
        // [*] -- associative wildcard
        p.bump(); // Star
    } else if p.at(SyntaxKind::Dollar) {
        // [$] or [$:expr] -- queue
        p.bump(); // Dollar
        if p.at(SyntaxKind::Colon) {
            p.bump(); // Colon
            expressions::expr(p); // bound expression
        } else if !p.at(SyntaxKind::RBracket) {
            p.error("expected ']' or ':' after '$' in queue dimension");
            while !p.at(SyntaxKind::RBracket) && !p.at(SyntaxKind::Eof) {
                p.bump();
            }
        }
    } else if is_scalar_type_keyword(p.current()) && p.nth(1) == SyntaxKind::RBracket {
        // [string], [int], etc. -- associative type-indexed (scalar keywords only)
        type_spec(p);
    } else {
        // [expr] or [expr:expr] -- size or range
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

// Parse a nettype declaration (LRM 6.6.7):
//   nettype <type_spec> <name> [with <resolve_fn>] ;
pub(crate) fn nettype_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // nettype
    type_spec(p);
    p.expect(SyntaxKind::Ident);
    if p.eat(SyntaxKind::WithKw) {
        p.expect(SyntaxKind::Ident);
    }
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::NettypeDecl);
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
    if p.at(SyntaxKind::LBracket) {
        let r = p.start();
        p.bump(); // [
        if p.at(SyntaxKind::RBracket) {
            p.error("expected expression in enum member range");
        } else {
            let w = p.start();
            expressions::expr(p);
            w.complete(p, SyntaxKind::Expression);
            if p.eat(SyntaxKind::Colon) {
                let w2 = p.start();
                expressions::expr(p);
                w2.complete(p, SyntaxKind::Expression);
            }
        }
        p.expect(SyntaxKind::RBracket);
        r.complete(p, SyntaxKind::EnumMemberRange);
    }
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

/// Whether `kind` can start a cast target type (`data_type` production).
/// Covers keyword types and user-defined type names (Ident).
pub(crate) fn at_cast_type(kind: SyntaxKind) -> bool {
    is_scalar_type_keyword(kind) || kind == SyntaxKind::Ident || kind == SyntaxKind::TypeKw
}

// `timeunit time_literal [ / time_literal ] ;`
pub(crate) fn timeunit_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // timeunit
    p.expect(SyntaxKind::TimeLiteral);
    if p.eat(SyntaxKind::Slash) {
        p.expect(SyntaxKind::TimeLiteral);
    }
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::TimeunitDecl);
}

// `timeprecision time_literal ;`
pub(crate) fn timeprecision_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // timeprecision
    p.expect(SyntaxKind::TimeLiteral);
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::TimeprecisionDecl);
}

/// Tokens that unambiguously start a data declaration.
/// Does NOT handle bare `Ident Ident` (ambiguous with module instantiation).
pub(crate) fn at_unambiguous_data_decl_start(p: &Parser) -> bool {
    let k = p.current();
    if is_data_type_keyword(k) || net::is_net_type_kw(k) {
        return true;
    }
    matches!(
        k,
        SyntaxKind::SignedKw
            | SyntaxKind::UnsignedKw
            | SyntaxKind::VarKw
            | SyntaxKind::ConstKw
            | SyntaxKind::InputKw
            | SyntaxKind::OutputKw
            | SyntaxKind::InoutKw
    ) || (matches!(k, SyntaxKind::StaticKw | SyntaxKind::AutomaticKw)
        && !matches!(p.nth(1), SyntaxKind::FunctionKw | SyntaxKind::TaskKw))
        || (k == SyntaxKind::Ident && p.nth(1) == SyntaxKind::ColonColon)
        || (super::is_unit_scope_prefix(p) && p.nth(1) == SyntaxKind::ColonColon)
        || (k == SyntaxKind::Ident
            && p.nth(1) == SyntaxKind::Dot
            && p.nth(2) == SyntaxKind::Ident
            && p.nth(3) == SyntaxKind::Ident)
}

/// Bare user-defined type name unambiguously starting a data declaration.
/// Two forms:
///   Ident # -- parameterized type (context decides if also instantiation)
///   Ident (Ident|EscapedIdent) <continuation> -- type followed by
///     declarator name and a token that continues a declaration
///
/// The continuation set covers declaration-specific patterns: unpacked
/// dimension `[`, initializer `=`, list `,`, terminator `;`, scope `::`.
/// `(` is deliberately excluded because `Ident Ident (` is ambiguous with
/// module instantiation; callers handle that case separately.
pub(crate) fn at_udt_data_decl_start(p: &Parser) -> bool {
    if p.current() != SyntaxKind::Ident {
        return false;
    }
    if p.nth(1) == SyntaxKind::Hash {
        return true;
    }
    matches!(p.nth(1), SyntaxKind::Ident | SyntaxKind::EscapedIdent)
        && matches!(
            p.nth(2),
            SyntaxKind::LBracket
                | SyntaxKind::Assign
                | SyntaxKind::Comma
                | SyntaxKind::Semicolon
                | SyntaxKind::ColonColon
        )
}
