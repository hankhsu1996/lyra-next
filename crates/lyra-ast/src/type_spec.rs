// Typed accessors for TypeSpec, PackedDimension, UnpackedDimension, DottedName,
// EnumType, StructType, and declaration-level dimension parents.
//
// Structural scan invariant: any token-based probing in this module inspects
// direct token children only, skips trivia, and never descends into subtrees.

use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxToken;

use crate::expr::Expr;
use crate::node::AstNode;
use crate::nodes::{
    DottedName, EnumType, NameRef, PackedDimension, Port, QualifiedName, StructType, TypeSpec,
    TypedefDecl, UnpackedDimension,
};
use crate::support::{self, AstChildren};

// TypeSpec accessors (moved from nodes.rs + new)

impl TypeSpec {
    pub fn keyword(&self) -> Option<SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .find(|tok| !tok.kind().is_trivia())
    }

    pub fn packed_dimensions(&self) -> AstChildren<PackedDimension> {
        support::children(&self.syntax)
    }

    pub fn enum_type(&self) -> Option<EnumType> {
        support::child(&self.syntax)
    }

    pub fn struct_type(&self) -> Option<StructType> {
        support::child(&self.syntax)
    }

    /// The user-defined type name reference, if any.
    pub fn type_name_ref(&self) -> Option<TypeNameRef> {
        for child in self.syntax.children() {
            match child.kind() {
                SyntaxKind::NameRef => {
                    return NameRef::cast(child).map(TypeNameRef::Simple);
                }
                SyntaxKind::QualifiedName => {
                    return QualifiedName::cast(child).map(TypeNameRef::Qualified);
                }
                SyntaxKind::DottedName => {
                    return DottedName::cast(child).map(TypeNameRef::Dotted);
                }
                _ => {}
            }
        }
        None
    }

    /// The `signed` or `unsigned` keyword token, if present.
    pub fn signed_token(&self) -> Option<SyntaxToken> {
        support::token_in(
            &self.syntax,
            &[SyntaxKind::SignedKw, SyntaxKind::UnsignedKw],
        )
    }
}

/// A user-defined type name reference inside a `TypeSpec`.
#[derive(Debug, Clone)]
pub enum TypeNameRef {
    Simple(NameRef),
    Qualified(QualifiedName),
    Dotted(DottedName),
}

// EnumType accessors (moved from nodes.rs)

impl EnumType {
    pub fn base_type_spec(&self) -> Option<TypeSpec> {
        support::child(&self.syntax)
    }
}

// StructType accessors (moved from nodes.rs)

impl StructType {
    pub fn is_packed(&self) -> bool {
        support::token(&self.syntax, SyntaxKind::PackedKw).is_some()
    }

    pub fn is_union(&self) -> bool {
        support::token(&self.syntax, SyntaxKind::UnionKw).is_some()
    }

    pub fn is_tagged(&self) -> bool {
        support::token(&self.syntax, SyntaxKind::TaggedKw).is_some()
    }

    pub fn is_soft(&self) -> bool {
        support::token(&self.syntax, SyntaxKind::SoftKw).is_some()
    }
}

// PackedDimension accessors

impl PackedDimension {
    /// The MSB expression (first expression child in `[msb:lsb]`).
    pub fn msb(&self) -> Option<Expr> {
        support::expr_child(&self.syntax, 0)
    }

    /// The LSB expression (second expression child in `[msb:lsb]`).
    pub fn lsb(&self) -> Option<Expr> {
        support::expr_child(&self.syntax, 1)
    }
}

// UnpackedDimension accessors

/// Syntactic classification of an unpacked dimension.
///
/// Returns only shape and lightweight leaves (tokens, `Expr`).
/// Never embeds nested typed wrapper payloads.
///
/// When both sides of a range are present, returns `Range`.
/// Recovery cases (Colon present but missing operand) fall through
/// to `Size` (one expr) or `Unsized` (no exprs).
#[derive(Debug)]
pub enum UnpackedDimKind {
    /// `[]`
    Unsized,
    /// `[*]`
    Wildcard,
    /// `[$]` or `[$:bound]`
    Queue { bound: Option<Expr> },
    /// `[type]` -- associative array with type key
    Assoc,
    /// `[expr]` -- fixed size
    Size { expr: Expr },
    /// `[msb:lsb]` -- explicit range (both sides present)
    Range { msb: Expr, lsb: Expr },
}

impl UnpackedDimension {
    /// Classify the syntactic form of this unpacked dimension.
    ///
    /// Detection precedence (token-driven, unambiguous):
    /// 1. `Star` -> `Wildcard`
    /// 2. `Dollar` -> `Queue` (bound is `expr_child(0)`)
    /// 3. `Colon` with both operands -> `Range`
    /// 4. `TypeSpec` child -> `Assoc`
    /// 5. One expr child -> `Size`
    /// 6. No expr children -> `Unsized`
    ///
    /// Recovery: Colon with missing operand(s) falls through to steps 5/6.
    pub fn classify(&self) -> UnpackedDimKind {
        // 1. Star -> Wildcard
        if support::token(&self.syntax, SyntaxKind::Star).is_some() {
            return UnpackedDimKind::Wildcard;
        }

        // 2. Dollar -> Queue
        if support::token(&self.syntax, SyntaxKind::Dollar).is_some() {
            let bound = support::expr_child(&self.syntax, 0);
            return UnpackedDimKind::Queue { bound };
        }

        // 3. Colon with both operands -> Range; partial falls through
        if support::token(&self.syntax, SyntaxKind::Colon).is_some() {
            let msb = support::expr_child(&self.syntax, 0);
            let lsb = support::expr_child(&self.syntax, 1);
            if let (Some(m), Some(l)) = (msb, lsb) {
                return UnpackedDimKind::Range { msb: m, lsb: l };
            }
            // Partial range (recovery): fall through to Size/Unsized
        }

        // 4. TypeSpec child -> Assoc
        if self
            .syntax
            .children()
            .any(|c| c.kind() == SyntaxKind::TypeSpec)
        {
            return UnpackedDimKind::Assoc;
        }

        // 5. One expr child -> Size
        if let Some(expr) = support::expr_child(&self.syntax, 0) {
            return UnpackedDimKind::Size { expr };
        }

        // 6. No expr children -> Unsized
        UnpackedDimKind::Unsized
    }

    /// The `TypeSpec` child for associative array dimensions (`[type]`).
    pub fn assoc_type_spec(&self) -> Option<TypeSpec> {
        support::child(&self.syntax)
    }
}

// DottedName accessor

impl DottedName {
    /// The modport identifier token after the dot.
    pub fn modport_ident(&self) -> Option<SyntaxToken> {
        support::token_after_any(&self.syntax, &[SyntaxKind::Dot], SyntaxKind::Ident)
    }
}

// Parent-specific unpacked dimension accessors

impl crate::nodes::Declarator {
    pub fn unpacked_dimensions(&self) -> AstChildren<UnpackedDimension> {
        support::children(&self.syntax)
    }
}

impl Port {
    pub fn unpacked_dimensions(&self) -> AstChildren<UnpackedDimension> {
        support::children(&self.syntax)
    }
}

impl TypedefDecl {
    pub fn unpacked_dimensions(&self) -> AstChildren<UnpackedDimension> {
        support::children(&self.syntax)
    }
}

impl crate::nodes::ParamDecl {
    /// The `type` keyword token after `parameter`/`localparam`, if present.
    pub fn type_keyword(&self) -> Option<SyntaxToken> {
        support::token_after_any(
            &self.syntax,
            &[SyntaxKind::ParameterKw, SyntaxKind::LocalparamKw],
            SyntaxKind::TypeKw,
        )
    }
}
