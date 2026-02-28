use lyra_lexer::SyntaxKind;
use lyra_parser::{SyntaxNode, SyntaxToken};

use crate::node::{AstNode, HasSyntax, is_expression_kind};
use crate::nodes::{
    BinExpr, CallExpr, CastExpr, ConcatExpr, CondExpr, Expression, FieldExpr, IndexExpr, Literal,
    NameRef, ParenExpr, PrefixExpr, QualifiedName, RangeExpr, ReplicExpr, StreamExpr, SystemTfCall,
    TypeSpec,
};

/// Typed handle for any expression-kind syntax node.
///
/// `Expr` wraps a `SyntaxNode` whose kind passes `is_expression_kind`.
/// It is the typed boundary for all expression-position returns in the AST.
#[derive(Debug, Clone)]
pub struct Expr {
    syntax: SyntaxNode,
}

impl HasSyntax for Expr {
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}

impl Expr {
    pub fn can_cast(kind: SyntaxKind) -> bool {
        is_expression_kind(kind)
    }

    pub fn cast(node: SyntaxNode) -> Option<Expr> {
        if Self::can_cast(node.kind()) {
            Some(Expr { syntax: node })
        } else {
            None
        }
    }

    /// Wrap an `AstNode` that is known to be an expression kind.
    ///
    /// Returns `None` if the node's kind is not an expression kind.
    pub fn from_ast<T: AstNode>(node: &T) -> Option<Expr> {
        let syntax = node.syntax();
        if Self::can_cast(syntax.kind()) {
            Some(Expr {
                syntax: syntax.clone(),
            })
        } else {
            None
        }
    }

    pub fn kind(&self) -> SyntaxKind {
        self.syntax.kind()
    }

    /// Strip `Expression` and `ParenExpr` syntactic wrappers from a raw node.
    ///
    /// Prefer `peeled()` when you already have an `Expr`.
    pub(crate) fn peel(node: &SyntaxNode) -> Option<Expr> {
        let mut current = Expr::cast(node.clone())?;
        loop {
            let inner = match current.kind() {
                SyntaxKind::Expression => {
                    Expression::cast(current.syntax().clone()).and_then(|w| w.inner())
                }
                SyntaxKind::ParenExpr => {
                    ParenExpr::cast(current.syntax().clone()).and_then(|p| p.inner())
                }
                _ => return Some(current),
            };
            match inner {
                Some(expr) => current = expr,
                None => return Some(current),
            }
        }
    }

    /// Strip `Expression` and `ParenExpr` syntactic wrappers.
    ///
    /// If the current expression is an `Expression` or `ParenExpr`
    /// wrapper, recursively unwraps to the inner expression. Returns
    /// `None` only if the node is not an expression kind (should not
    /// happen on a valid `Expr`). If a wrapper has no inner child
    /// (malformed), returns the wrapper itself.
    pub fn peeled(&self) -> Option<Expr> {
        Self::peel(&self.syntax)
    }

    /// Peel wrappers and classify into a typed `ExprKind` variant.
    ///
    /// Returns `None` for malformed/error nodes or wrapper-only trees
    /// with no inner content.
    pub fn classify(&self) -> Option<ExprKind> {
        let peeled = self.peeled()?;
        let node = peeled.syntax().clone();
        match peeled.kind() {
            SyntaxKind::Literal => Literal::cast(node).map(ExprKind::Literal),
            SyntaxKind::BinExpr => BinExpr::cast(node).map(ExprKind::BinExpr),
            SyntaxKind::PrefixExpr => PrefixExpr::cast(node).map(ExprKind::PrefixExpr),
            SyntaxKind::CondExpr => CondExpr::cast(node).map(ExprKind::CondExpr),
            SyntaxKind::ConcatExpr => ConcatExpr::cast(node).map(ExprKind::ConcatExpr),
            SyntaxKind::ReplicExpr => ReplicExpr::cast(node).map(ExprKind::ReplicExpr),
            SyntaxKind::IndexExpr => IndexExpr::cast(node).map(ExprKind::IndexExpr),
            SyntaxKind::RangeExpr => RangeExpr::cast(node).map(ExprKind::RangeExpr),
            SyntaxKind::FieldExpr => FieldExpr::cast(node).map(ExprKind::FieldExpr),
            SyntaxKind::CallExpr => CallExpr::cast(node).map(ExprKind::CallExpr),
            SyntaxKind::SystemTfCall => SystemTfCall::cast(node).map(ExprKind::SystemTfCall),
            SyntaxKind::NameRef => NameRef::cast(node).map(ExprKind::NameRef),
            SyntaxKind::QualifiedName => QualifiedName::cast(node).map(ExprKind::QualifiedName),
            SyntaxKind::StreamExpr => StreamExpr::cast(node).map(ExprKind::StreamExpr),
            SyntaxKind::CastExpr => CastExpr::cast(node).map(ExprKind::CastExpr),
            _ => None,
        }
    }
}

/// Typed classification of a post-peel expression.
///
/// No `Expression` or `ParenExpr` variants -- those are stripped
/// by `Expr::classify()` before classification.
#[derive(Debug, Clone)]
pub enum ExprKind {
    Literal(Literal),
    BinExpr(BinExpr),
    PrefixExpr(PrefixExpr),
    CondExpr(CondExpr),
    ConcatExpr(ConcatExpr),
    ReplicExpr(ReplicExpr),
    IndexExpr(IndexExpr),
    RangeExpr(RangeExpr),
    FieldExpr(FieldExpr),
    CallExpr(CallExpr),
    SystemTfCall(SystemTfCall),
    NameRef(NameRef),
    QualifiedName(QualifiedName),
    StreamExpr(StreamExpr),
    CastExpr(CastExpr),
}

impl ExprKind {
    /// Whether this expression kind is a valid anchor for `type_at` queries.
    ///
    /// Includes kinds that carry a meaningful inferred type at their
    /// position: arithmetic, concatenation, indexing, calls, literals,
    /// and name references. Excludes range, stream, cast, field-access,
    /// and system-task expressions.
    pub fn is_type_at_anchor(self) -> bool {
        matches!(
            self,
            ExprKind::BinExpr(_)
                | ExprKind::PrefixExpr(_)
                | ExprKind::CondExpr(_)
                | ExprKind::ConcatExpr(_)
                | ExprKind::ReplicExpr(_)
                | ExprKind::IndexExpr(_)
                | ExprKind::CallExpr(_)
                | ExprKind::Literal(_)
                | ExprKind::NameRef(_)
                | ExprKind::QualifiedName(_)
        )
    }

    pub fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::Literal(n) => n.syntax(),
            Self::BinExpr(n) => n.syntax(),
            Self::PrefixExpr(n) => n.syntax(),
            Self::CondExpr(n) => n.syntax(),
            Self::ConcatExpr(n) => n.syntax(),
            Self::ReplicExpr(n) => n.syntax(),
            Self::IndexExpr(n) => n.syntax(),
            Self::RangeExpr(n) => n.syntax(),
            Self::FieldExpr(n) => n.syntax(),
            Self::CallExpr(n) => n.syntax(),
            Self::SystemTfCall(n) => n.syntax(),
            Self::NameRef(n) => n.syntax(),
            Self::QualifiedName(n) => n.syntax(),
            Self::StreamExpr(n) => n.syntax(),
            Self::CastExpr(n) => n.syntax(),
        }
    }
}

/// Thin newtype over `TypeSpec` for the `TfArg` boundary.
#[derive(Debug, Clone)]
pub struct TypeRef {
    inner: TypeSpec,
}

impl HasSyntax for TypeRef {
    fn syntax(&self) -> &SyntaxNode {
        self.inner.syntax()
    }
}

impl TypeRef {
    pub fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::TypeSpec
    }

    pub fn cast(node: SyntaxNode) -> Option<TypeRef> {
        TypeSpec::cast(node).map(|ts| TypeRef { inner: ts })
    }

    /// The inner `TypeSpec`. Always succeeds since `TypeRef` wraps a `TypeSpec`.
    pub fn type_spec(&self) -> TypeSpec {
        self.inner.clone()
    }

    pub fn into_type_spec(self) -> TypeSpec {
        self.inner
    }
}

/// Syntactic category for system task/function arguments.
///
/// The parser produces both expression-kind nodes and `TypeSpec` nodes inside
/// `SystemTfArgList` (e.g. `$bits(logic [7:0])` has a `TypeSpec` child).
/// This sum type models that reality.
pub enum TfArg {
    Expr(Expr),
    Type(TypeRef),
    Unknown(SyntaxNode),
}

impl HasSyntax for TfArg {
    fn syntax(&self) -> &SyntaxNode {
        match self {
            TfArg::Expr(e) => e.syntax(),
            TfArg::Type(tr) => tr.syntax(),
            TfArg::Unknown(n) => n,
        }
    }
}

/// Structured classification of a `Literal` node's token content.
#[derive(Debug, Clone)]
pub enum LiteralKind {
    Int {
        token: SyntaxToken,
    },
    Based {
        size_token: Option<SyntaxToken>,
        base_token: SyntaxToken,
    },
    UnbasedUnsized {
        token: SyntaxToken,
    },
    Real {
        token: SyntaxToken,
    },
    String {
        token: SyntaxToken,
    },
    Time {
        token: SyntaxToken,
    },
    Unknown {
        token: SyntaxToken,
    },
}
