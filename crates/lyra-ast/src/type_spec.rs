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

    /// Typed classification of the base type keyword.
    pub fn type_keyword(&self) -> Option<TypeSpecKeyword> {
        self.keyword()
            .and_then(|tok| TypeSpecKeyword::from_token_kind(tok.kind()))
    }

    /// Signing modifier, if an explicit `signed` or `unsigned` keyword is present.
    pub fn signing(&self) -> Option<Signing> {
        self.signed_token().map(|tok| {
            if tok.kind() == SyntaxKind::SignedKw {
                Signing::Signed
            } else {
                Signing::Unsigned
            }
        })
    }
}

/// Explicit signing modifier on a `TypeSpec`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Signing {
    Signed,
    Unsigned,
}

impl Signing {
    /// Convert to the boolean convention used by `Integral::signed`
    /// (`true` = signed).
    pub fn is_signed(self) -> bool {
        self == Self::Signed
    }
}

/// Typed classification of a `TypeSpec` base type keyword.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeSpecKeyword {
    // Integral
    Logic,
    Reg,
    Bit,
    Integer,
    Int,
    Shortint,
    Longint,
    Byte,
    Time,
    // Real
    Realtime,
    Real,
    ShortReal,
    // Other
    String,
    Chandle,
    Event,
    Void,
    // Net
    Wire,
    Tri,
    Wand,
    Wor,
    Tri0,
    Tri1,
    Trireg,
    Supply0,
    Supply1,
    Uwire,
}

impl TypeSpecKeyword {
    /// Whether this keyword names a net type (`wire`, `tri`, etc.).
    pub fn is_net(self) -> bool {
        matches!(
            self,
            Self::Wire
                | Self::Tri
                | Self::Wand
                | Self::Wor
                | Self::Tri0
                | Self::Tri1
                | Self::Trireg
                | Self::Supply0
                | Self::Supply1
                | Self::Uwire
        )
    }

    /// Whether this keyword names an integral data type.
    pub fn is_integral(self) -> bool {
        matches!(
            self,
            Self::Logic
                | Self::Reg
                | Self::Bit
                | Self::Integer
                | Self::Int
                | Self::Shortint
                | Self::Longint
                | Self::Byte
                | Self::Time
        )
    }

    /// Whether this keyword names a real data type.
    pub fn is_real(self) -> bool {
        matches!(self, Self::Real | Self::ShortReal | Self::Realtime)
    }

    /// Whether this keyword names a data type (valid as an associative
    /// array key type, cast target, etc.). Excludes net types, `void`,
    /// and `event`.
    pub fn is_data_type(self) -> bool {
        self.is_integral() || self.is_real() || matches!(self, Self::String | Self::Chandle)
    }

    pub fn from_token_kind(kind: SyntaxKind) -> Option<Self> {
        match kind {
            SyntaxKind::LogicKw => Some(Self::Logic),
            SyntaxKind::RegKw => Some(Self::Reg),
            SyntaxKind::BitKw => Some(Self::Bit),
            SyntaxKind::IntegerKw => Some(Self::Integer),
            SyntaxKind::IntKw => Some(Self::Int),
            SyntaxKind::ShortintKw => Some(Self::Shortint),
            SyntaxKind::LongintKw => Some(Self::Longint),
            SyntaxKind::ByteKw => Some(Self::Byte),
            SyntaxKind::TimeKw => Some(Self::Time),
            SyntaxKind::RealtimeKw => Some(Self::Realtime),
            SyntaxKind::RealKw => Some(Self::Real),
            SyntaxKind::ShortRealKw => Some(Self::ShortReal),
            SyntaxKind::StringKw => Some(Self::String),
            SyntaxKind::ChandleKw => Some(Self::Chandle),
            SyntaxKind::EventKw => Some(Self::Event),
            SyntaxKind::VoidKw => Some(Self::Void),
            SyntaxKind::WireKw => Some(Self::Wire),
            SyntaxKind::TriKw => Some(Self::Tri),
            SyntaxKind::WandKw => Some(Self::Wand),
            SyntaxKind::WorKw => Some(Self::Wor),
            SyntaxKind::Tri0Kw => Some(Self::Tri0),
            SyntaxKind::Tri1Kw => Some(Self::Tri1),
            SyntaxKind::TriregKw => Some(Self::Trireg),
            SyntaxKind::Supply0Kw => Some(Self::Supply0),
            SyntaxKind::Supply1Kw => Some(Self::Supply1),
            SyntaxKind::UwireKw => Some(Self::Uwire),
            _ => None,
        }
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

    /// Whether this is a `parameter type` declaration.
    pub fn is_type_param(&self) -> bool {
        self.type_keyword().is_some()
    }
}
