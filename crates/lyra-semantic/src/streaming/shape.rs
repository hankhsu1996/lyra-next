use lyra_ast::{AstIdMap, ErasedAstId, ExprKind, StreamDir, StreamExpr};

use crate::lhs::{LhsClass, classify_lhs};
use crate::site::{self, Site};

/// Pure-data representation of a streaming unpack target.
pub(crate) struct StreamUnpackShape {
    pub(crate) _dir: StreamDir,
    pub(crate) items: Vec<StreamUnpackItem>,
}

/// One operand in a streaming unpack target.
pub(crate) struct StreamUnpackItem {
    /// Site of the `StreamOperandItem` wrapper node.
    pub(crate) item_site: Site,
    /// Site of the inner expression (more precise diagnostic anchor).
    /// `None` when the operand has no parseable expression.
    pub(crate) expr_site: Option<Site>,
    /// Stable identity of the inner `Expr` (for type queries).
    /// `None` when the operand has no parseable expression.
    pub(crate) expr_id: Option<ErasedAstId>,
    /// What kind of assignment target this operand is.
    pub(crate) target: StreamTargetKind,
    /// Presence of a `with` clause (extended with selection details in future).
    pub(crate) with_clause: Option<WithInfo>,
}

impl StreamUnpackItem {
    /// The most precise site for diagnostic anchoring.
    pub(crate) fn diag_site(&self) -> Site {
        self.expr_site.unwrap_or(self.item_site)
    }
}

/// Classification of a streaming unpack operand's lvalue shape.
///
/// Carries the structural kind so future rules (e.g., `with` base must be
/// array lvalue, dynamic resize slack absorber) do not need to re-derive
/// structure by re-walking or re-querying.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum StreamTargetKind {
    /// Simple name reference (`a`, `pkg::x`).
    Name,
    /// Field access (`s.field`).
    FieldAccess,
    /// Index selection (`a[i]`).
    IndexSelect,
    /// Range selection (`a[lo:hi]`, `a[base+:w]`, `a[base-:w]`).
    RangeSelect,
    /// Nested streaming expression (`{>> {...}}`).
    NestedStream,
    /// Not a valid assignment target (literal, call, etc.).
    NotAssignable,
}

impl StreamTargetKind {
    pub(crate) fn is_assignable(self) -> bool {
        !matches!(self, Self::NotAssignable)
    }
}

/// Presence marker for a `with` clause on a streaming operand.
pub(crate) struct WithInfo {
    pub(crate) with_site: Site,
}

/// Error from `build_unpack_shape` when the shape cannot be constructed.
pub(crate) struct BuildShapeError {
    fallback: Site,
    detail: &'static str,
}

impl BuildShapeError {
    pub(crate) fn fallback(&self) -> Site {
        self.fallback
    }

    pub(crate) fn detail(&self) -> &'static str {
        self.detail
    }
}

/// Build a pure-data unpack shape from a `StreamExpr`.
///
/// Returns `Err` if the direction token is missing (error-recovered tree),
/// carrying a fallback site and detail for deterministic internal error
/// reporting. Emits no diagnostics; all validation happens in `check`.
pub(crate) fn build_unpack_shape(
    stream: &StreamExpr,
    map: &AstIdMap,
    fallback: Site,
) -> Result<StreamUnpackShape, BuildShapeError> {
    let dir = stream.dir().ok_or(BuildShapeError {
        fallback,
        detail: "streaming expression missing direction token",
    })?;
    let mut items = Vec::new();

    for operand in stream.operand_items() {
        let item_site = site::opt_site_of(map, &operand).unwrap_or(fallback);

        let (expr_site, expr_id, target) = match operand.expr() {
            Some(expr) => {
                let es = site::opt_site_of(map, &expr);
                let eid = map.id_of(&expr);
                let kind = classify_target_kind(&expr);
                (es, eid, kind)
            }
            None => (None, None, StreamTargetKind::NotAssignable),
        };

        let with_clause = operand.with_clause().map(|wc| {
            let with_site = site::opt_site_of(map, &wc).unwrap_or(item_site);
            WithInfo { with_site }
        });

        items.push(StreamUnpackItem {
            item_site,
            expr_site,
            expr_id,
            target,
            with_clause,
        });
    }

    Ok(StreamUnpackShape { _dir: dir, items })
}

/// Map an expression to its structural lvalue kind for streaming targets.
fn classify_target_kind(expr: &lyra_ast::Expr) -> StreamTargetKind {
    match classify_lhs(expr) {
        LhsClass::Assignable(peeled) => match peeled.classify() {
            Some(ExprKind::FieldExpr(_)) => StreamTargetKind::FieldAccess,
            Some(ExprKind::IndexExpr(_)) => StreamTargetKind::IndexSelect,
            Some(ExprKind::RangeExpr(_)) => StreamTargetKind::RangeSelect,
            _ => StreamTargetKind::Name,
        },
        LhsClass::Stream(_) => StreamTargetKind::NestedStream,
        LhsClass::Unsupported | LhsClass::NotAssignable => StreamTargetKind::NotAssignable,
    }
}
