use crate::Site;
use crate::site;
use lyra_ast::{
    AssignStmt, AstIdMap, ContinuousAssign, Declarator, Expr, ExprKind, NewExpr, TfArg, VarDecl,
};
use lyra_source::{NameSpan, TextRange};

use crate::coerce::IntegralCtx;
use crate::enum_def::EnumId;
use crate::modport_def::PortDirection;
use crate::type_infer::{BitVecType, BitWidth, ExprType, ExprTypeErrorKind, ExprView};
use crate::types::{SymbolType, Ty, UnpackedDim};

use crate::type_check_dim::check_type_spec_member_dims;
pub use crate::type_check_dim::{check_net_decl, check_port_decl, check_typedef_decl};
pub use crate::type_check_expr::{
    check_cast_expr, check_field_direction, check_field_modport_restriction, check_method_call,
    check_stream_operand,
};
pub use crate::type_check_system_call::check_system_call;

/// Why an assignment target is readonly.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReadonlyKind {
    Const,
}

/// What direction a modport member was accessed in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AccessKind {
    Read,
    Write,
}

/// Access mode propagated from the index to individual check sites.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum AccessMode {
    Read = 0,
    Write = 1,
    ReadWrite = 2,
}

/// A type-check finding.
pub enum TypeCheckItem {
    AssignTruncation {
        assign_site: Site,
        lhs_site: Site,
        rhs_site: Site,
        lhs_width: u32,
        rhs_width: u32,
    },
    BitsNonDataType {
        call_site: Site,
        arg_site: Site,
    },
    EnumAssignFromNonEnum {
        assign_site: Site,
        lhs_site: Site,
        rhs_site: Site,
        lhs_enum: EnumId,
        rhs_ty: Ty,
    },
    EnumAssignWrongEnum {
        assign_site: Site,
        lhs_site: Site,
        rhs_site: Site,
        lhs_enum: EnumId,
        rhs_enum: EnumId,
    },
    ConversionArgCategory {
        call_site: Site,
        arg_site: Site,
        fn_name: smol_str::SmolStr,
        expected: &'static str,
    },
    ConversionWidthMismatch {
        call_site: Site,
        arg_site: Site,
        fn_name: smol_str::SmolStr,
        expected_width: u32,
        actual_width: u32,
    },
    ModportDirectionViolation {
        member_name_span: NameSpan,
        direction: PortDirection,
        access: AccessKind,
    },
    ModportRefUnsupported {
        member_name_span: NameSpan,
    },
    EnumCastOutOfRange {
        cast_site: Site,
        enum_id: EnumId,
        value: i64,
    },
    StreamWithNonArray {
        with_site: Site,
    },
    ModportEmptyPortAccess {
        member_name_span: NameSpan,
    },
    ModportExprNotAssignable {
        member_name_span: NameSpan,
    },
    MemberNotInModport {
        member_name_span: NameSpan,
        member_name: smol_str::SmolStr,
    },
    MethodCallError {
        call_name_span: NameSpan,
        method_name: smol_str::SmolStr,
        error_kind: crate::type_infer::ExprTypeErrorKind,
    },
    UnsupportedLhsForm {
        lhs_site: Site,
    },
    InvalidLhs {
        lhs_site: Site,
    },
    InternalError {
        detail: smol_str::SmolStr,
        site: Site,
    },
    NewExprNotDynArray {
        new_site: Site,
        lhs_site: Site,
    },
    NewExprTooManyInitArgs {
        new_site: Site,
    },
    NewExprSizeNotLongint {
        new_site: Site,
        size_site: Site,
    },
    NewExprSizeNegative {
        new_site: Site,
        size_site: Site,
    },
    NewExprInitIncompat {
        new_site: Site,
        init_site: Site,
        lhs_site: Site,
        lhs_ty: Ty,
        init_ty: Ty,
    },
    ArrayIncompatible {
        assign_site: Site,
        lhs_site: Site,
        rhs_site: Site,
        lhs_ty: Ty,
        rhs_ty: Ty,
    },
    StreamUnpackOperandInvalid {
        operand_site: Site,
    },
    StreamUnpackOperandUnsupported {
        operand_site: Site,
        operand_ty: Ty,
    },
    StreamUnpackGreedyRemainder {
        assign_site: Site,
        greedy_site: Site,
        rhs_site: Site,
        remaining: u32,
        elem_width: u32,
    },
    StreamUnpackWidthMismatch {
        assign_site: Site,
        op_range: Option<TextRange>,
        lhs_site: Site,
        rhs_site: Site,
        lhs_width: u32,
        rhs_width: u32,
    },
    AssignToReadonly {
        kind: ReadonlyKind,
        assign_site: Site,
        lhs_name_span: NameSpan,
        name: smol_str::SmolStr,
    },
    ConstMissingInit {
        decl_site: Site,
        name_span: NameSpan,
    },
    VoidObjectType {
        decl_site: Site,
        name_span: NameSpan,
    },
    VoidUsedAsValue {
        expr_site: Site,
    },
    ArrayQueryDynTypeForm {
        call_site: Site,
        arg_site: Site,
        fn_name: smol_str::SmolStr,
    },
    ArrayQueryVarSizedDimByNumber {
        call_site: Site,
        dim_arg_site: Site,
        fn_name: smol_str::SmolStr,
    },
    IllegalDriveStrengthBothHighz {
        strength_site: Site,
    },
    QueueBoundNotConst {
        bound_site: Site,
    },
    QueueBoundNotPositive {
        bound_site: Site,
        bound: i64,
    },
}

/// Callbacks for the type checker. No DB access -- pure.
pub trait TypeCheckCtx {
    /// The file being analyzed.
    fn file_id(&self) -> lyra_source::FileId;
    /// Per-file AST ID map, enabling `Site`/ID computation from typed nodes.
    fn ast_id_map(&self) -> &AstIdMap;
    /// Infer the type of an expression node (self-determined).
    fn expr_type(&self, expr: &Expr) -> ExprType;
    /// Infer the type of an expression node under an integral context.
    fn expr_type_in_ctx(&self, expr: &Expr, ctx: &IntegralCtx) -> ExprType;
    /// Infer the type of an expression in statement context.
    fn expr_type_stmt(&self, expr: &Expr) -> ExprType;
    /// Infer the type of an expression with an expected type for contextual typing.
    fn expr_type_with_expected(&self, expr: &Expr, expected: &Ty) -> ExprType;
    /// Get the declared type of a symbol via its `Declarator` node.
    fn symbol_type_of_declarator(&self, decl: &Declarator) -> Option<SymbolType>;
    /// Resolve a `UserTypeRef` as a type (typedef/enum/struct name).
    fn resolve_type_arg(&self, utr: &crate::type_extract::UserTypeRef) -> Option<crate::types::Ty>;
    /// Evaluate a constant integer expression. Returns None if non-const.
    fn const_eval_int(&self, expr: &Expr) -> Option<i64>;
    /// Evaluate a constant integer expression by its `Site` identity.
    /// Returns `None` if non-const or not evaluable.
    fn const_eval_int_by_site(&self, site: Site) -> Option<i64>;
    /// Get sorted set of known enum member values. None if any value unknown.
    fn enum_known_value_set(&self, id: &EnumId) -> Option<std::sync::Arc<[i64]>>;
    /// Check whether a modport expression target is an lvalue.
    fn is_modport_target_lvalue(&self, expr_id: Site) -> bool;
    /// Infer the type of an expression by its `Site`.
    fn expr_type_by_id(&self, id: Site) -> ExprType;
    /// Fixed streaming width in bits for an expression by its `Site`.
    ///
    /// Returns `Some(bits)` when the expression type has a statically known
    /// fixed streaming width (integral, enum, fixed-size array of streamable
    /// elements). Returns `None` for dynamic/unsupported types.
    fn fixed_stream_width_bits(&self, id: Site) -> Option<u32>;
    /// Fixed streaming width in bits for an already-inferred expression type.
    fn fixed_stream_width_bits_of_type(&self, et: &ExprType) -> Option<u32>;
    /// Fixed streaming width in bits for a `Ty` directly.
    ///
    /// Avoids the need to synthesize an `ExprType` from a raw `Ty`.
    fn fixed_stream_width_bits_of_ty(&self, ty: &Ty) -> Option<u32>;
    /// Evaluate a constant integer expression by `Site`, returning the full
    /// `ConstInt` result (including error discrimination).
    fn const_eval_int_by_site_full(&self, site: Site) -> crate::types::ConstInt;
    /// Check whether an assignment target resolves to a readonly symbol.
    fn readonly_target_kind(&self, expr_site: Site) -> Option<(ReadonlyKind, smol_str::SmolStr)>;
}

pub(crate) const MISSING_AST_ID: &str = "missing_ast_id in type checker";

/// Require a `Site` anchor, emitting `InternalError` on failure.
///
/// Type-check-specific: when anchor extraction yields `None` (error-recovered
/// tree), emits a diagnostic anchored at `fallback` and returns the fallback.
pub(crate) fn require_site(
    anchor: Option<Site>,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) -> Site {
    site::require_site(anchor, fallback, || {
        items.push(TypeCheckItem::InternalError {
            detail: smol_str::SmolStr::new_static(MISSING_AST_ID),
            site: fallback,
        });
    })
}

/// Check a `ContinuousAssign` node for type errors.
///
/// Performs only the local assignment check (truncation, enum compat, LHS
/// validity). Does not recurse into children -- the `ChecksIndex` ensures
/// nested expression sites are checked separately.
pub fn check_continuous_assign(
    ca: &ContinuousAssign,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    let map = ctx.ast_id_map();
    let self_site = require_site(site::opt_site_of(map, ca), fallback, items);
    let op_range = ca.eq_token().map(|t| t.text_range());
    let lhs = ca.lhs();
    let rhs = ca.rhs();
    if let (Some(l), Some(r)) = (&lhs, &rhs) {
        check_assignment_pair(self_site, op_range, l, r, ctx, items);
    }
}

/// Check a `DriveStrength` node for the illegal both-highz combination (LRM 6.3.2).
pub fn check_drive_strength_semantics(
    ds: &lyra_ast::DriveStrength,
    map: &AstIdMap,
    fallback_site: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    if ds.is_both_highz() {
        let strength_site = require_site(site::opt_site_of(map, ds), fallback_site, items);
        items.push(TypeCheckItem::IllegalDriveStrengthBothHighz { strength_site });
    }
}

/// Check an `AssignStmt` node for type errors.
///
/// Performs only the local assignment check. Does not recurse.
pub fn check_assign_stmt(
    assign: &AssignStmt,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    let map = ctx.ast_id_map();
    let self_site = require_site(site::opt_site_of(map, assign), fallback, items);
    let op_range = assign.op_token().map(|t| t.text_range());
    let lhs = assign.lhs();
    let rhs = assign.rhs();
    let has_op = assign.assign_op().is_some();

    if let (Some(l), Some(r)) = (&lhs, &rhs) {
        check_assignment_pair(self_site, op_range, l, r, ctx, items);
    } else if rhs.is_none()
        && !has_op
        && let Some(l) = &lhs
    {
        let _result = ctx.expr_type_stmt(l);
    }
}

/// Check a `VarDecl` node for initializer type compatibility.
pub fn check_var_decl(
    var_decl: &VarDecl,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    let map = ctx.ast_id_map();
    let decl_site = require_site(site::opt_site_of(map, var_decl), fallback, items);
    let is_const = var_decl.const_token().is_some();
    for decl in var_decl.declarators() {
        // Check unpacked dimension legality (queue bounds, etc.)
        for dim in decl.unpacked_dimensions() {
            crate::type_check_dim::check_unpacked_dim_legality(ctx, &dim, items);
        }

        // Const variable must have an initializer (LRM 6.20.6)
        if is_const && decl.init_expr().is_none() {
            let name_span = decl
                .ident_name_span()
                .unwrap_or(NameSpan::new(decl_site.text_range()));
            items.push(TypeCheckItem::ConstMissingInit {
                decl_site,
                name_span,
            });
        }

        let sym_type = ctx.symbol_type_of_declarator(&decl);

        // Void cannot be used as a variable type (LRM 6.13)
        if matches!(sym_type, Some(SymbolType::Value(Ty::Void))) {
            let name_span = decl
                .ident_name_span()
                .unwrap_or(NameSpan::new(decl_site.text_range()));
            items.push(TypeCheckItem::VoidObjectType {
                decl_site,
                name_span,
            });
            continue;
        }

        let Some(init_expr) = decl.init_expr() else {
            continue;
        };

        let Some(sym_type) = sym_type else {
            continue;
        };

        let lhs_site = require_site(site::opt_site_of(map, &decl), decl_site, items);
        let rhs_site = require_site(site::opt_site_of(map, &init_expr), decl_site, items);

        let lhs_type = ExprType::from_symbol_type(&sym_type);
        let rhs_type = type_rhs_for_assignment(ctx, &init_expr, &lhs_type.ty);

        if is_void_used_as_expr(&rhs_type) {
            items.push(TypeCheckItem::VoidUsedAsValue {
                expr_site: rhs_site,
            });
            continue;
        }

        if let Some(ne) = detect_new_expr(&init_expr) {
            check_new_expr(&ne, &lhs_type.ty, ctx, decl_site, lhs_site, rhs_site, items);
        }

        if !matches!(rhs_type.ty, Ty::Error) {
            check_assignment_compat(&lhs_type, &rhs_type, decl_site, lhs_site, rhs_site, items);
        }
    }

    if let Some(ts) = var_decl.type_spec() {
        check_type_spec_member_dims(ctx, &ts, items);
    }
}

fn check_assignment_pair(
    stmt_site: Site,
    op_range: Option<TextRange>,
    lhs: &Expr,
    rhs: &Expr,
    ctx: &dyn TypeCheckCtx,
    items: &mut Vec<TypeCheckItem>,
) {
    let map = ctx.ast_id_map();
    let lhs_site = require_site(site::opt_site_of(map, lhs), stmt_site, items);
    let rhs_site = require_site(site::opt_site_of(map, rhs), stmt_site, items);

    match crate::lhs::classify_lhs(lhs) {
        crate::lhs::LhsClass::Assignable(lhs_expr) => {
            // Check for assignment to readonly (const) target
            if let Some(peeled_site) = site::opt_site_of(map, &lhs_expr)
                && let Some((kind, name)) = ctx.readonly_target_kind(peeled_site)
            {
                let lhs_name_span = lhs_expr
                    .ident_name_span()
                    .unwrap_or(NameSpan::new(lhs_site.text_range()));
                items.push(TypeCheckItem::AssignToReadonly {
                    kind,
                    assign_site: stmt_site,
                    lhs_name_span,
                    name,
                });
            }

            let lhs_type = ctx.expr_type(&lhs_expr);
            let rhs_type = type_rhs_for_assignment(ctx, rhs, &lhs_type.ty);

            if is_void_used_as_expr(&rhs_type) {
                items.push(TypeCheckItem::VoidUsedAsValue {
                    expr_site: rhs_site,
                });
            }

            if let Some(ne) = detect_new_expr(rhs) {
                check_new_expr(&ne, &lhs_type.ty, ctx, stmt_site, lhs_site, rhs_site, items);
            }

            if !matches!(rhs_type.ty, Ty::Error) {
                check_assignment_compat(&lhs_type, &rhs_type, stmt_site, lhs_site, rhs_site, items);
            }
        }
        crate::lhs::LhsClass::Stream(stream_expr) => {
            let map = ctx.ast_id_map();
            match crate::streaming::shape::build_unpack_shape(&stream_expr, map, stmt_site) {
                Ok(shape) => {
                    let sites = crate::streaming::check::UnpackAssignSites {
                        assign_site: stmt_site,
                        op_range,
                        lhs_site,
                        rhs_site,
                    };
                    crate::streaming::check::check_streaming_unpack(
                        &shape, rhs, &sites, ctx, items,
                    );
                }
                Err(e) => {
                    items.push(TypeCheckItem::InternalError {
                        detail: smol_str::SmolStr::new_static(e.detail()),
                        site: e.fallback(),
                    });
                }
            }
        }
        crate::lhs::LhsClass::Unsupported => {
            items.push(TypeCheckItem::UnsupportedLhsForm { lhs_site });
        }
        crate::lhs::LhsClass::NotAssignable => {
            items.push(TypeCheckItem::InvalidLhs { lhs_site });
        }
    }
}

fn check_assignment_compat(
    lhs: &ExprType,
    rhs: &ExprType,
    assign_site: Site,
    lhs_site: Site,
    rhs_site: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    if matches!(lhs.ty, Ty::Error)
        || matches!(rhs.ty, Ty::Error)
        || matches!(lhs.view, ExprView::Error(_))
        || matches!(rhs.view, ExprView::Error(_))
    {
        return;
    }

    // Truncation check
    if let (Some(lhs_w), Some(rhs_w)) = (bitvec_known_width(lhs), bitvec_known_width(rhs))
        && !is_context_dependent(rhs)
        && rhs_w > lhs_w
    {
        items.push(TypeCheckItem::AssignTruncation {
            assign_site,
            lhs_site,
            rhs_site,
            lhs_width: lhs_w,
            rhs_width: rhs_w,
        });
    }

    // Enum assignment compatibility check (LRM 6.19.3)
    if let Ty::Enum(ref lhs_id) = lhs.ty {
        match &rhs.ty {
            Ty::Enum(rhs_id) if lhs_id == rhs_id => {}
            Ty::Enum(rhs_id) => {
                items.push(TypeCheckItem::EnumAssignWrongEnum {
                    assign_site,
                    lhs_site,
                    rhs_site,
                    lhs_enum: *lhs_id,
                    rhs_enum: *rhs_id,
                });
            }
            _ => {
                items.push(TypeCheckItem::EnumAssignFromNonEnum {
                    assign_site,
                    lhs_site,
                    rhs_site,
                    lhs_enum: *lhs_id,
                    rhs_ty: rhs.ty.clone(),
                });
            }
        }
    }

    // Array assignment compatibility check (both sides must be arrays)
    if matches!(lhs.ty, Ty::Array { .. })
        && matches!(rhs.ty, Ty::Array { .. })
        && matches!(
            array_assignment_compat(&lhs.ty, &rhs.ty),
            ArrayCompat::Incompatible
        )
    {
        items.push(TypeCheckItem::ArrayIncompatible {
            assign_site,
            lhs_site,
            rhs_site,
            lhs_ty: lhs.ty.clone(),
            rhs_ty: rhs.ty.clone(),
        });
    }
}

pub(crate) fn tf_arg_expr(arg: &TfArg) -> Option<&Expr> {
    match arg {
        TfArg::Expr(e) => Some(e),
        _ => None,
    }
}

fn type_rhs_for_assignment(ctx: &dyn TypeCheckCtx, rhs: &Expr, lhs_ty: &Ty) -> ExprType {
    if detect_new_expr(rhs).is_some() {
        ctx.expr_type_with_expected(rhs, lhs_ty)
    } else {
        ctx.expr_type(rhs)
    }
}

fn detect_new_expr(expr: &Expr) -> Option<NewExpr> {
    let peeled = expr.peeled()?;
    let ek = peeled.classify()?;
    match ek {
        ExprKind::NewExpr(ne) => Some(ne),
        _ => None,
    }
}

/// Check a `new[]` expression for type errors.
pub fn check_new_expr(
    ne: &NewExpr,
    lhs_ty: &Ty,
    ctx: &dyn TypeCheckCtx,
    _assign_site: Site,
    lhs_site: Site,
    new_site: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    let map = ctx.ast_id_map();

    // LHS must be a dynamic array (outermost dim = Unsized)
    let Ty::Array {
        elem: _,
        dim: UnpackedDim::Unsized,
    } = lhs_ty
    else {
        items.push(TypeCheckItem::NewExprNotDynArray { new_site, lhs_site });
        return;
    };

    // Too many init args (must be 0 or 1)
    if ne.init_arg_count() > 1 {
        items.push(TypeCheckItem::NewExprTooManyInitArgs { new_site });
    }

    // Size expression: must be coercible to longint
    if let Some(size_expr) = ne.size_expr() {
        let size_site = require_site(site::opt_site_of(map, &size_expr), new_site, items);
        let longint_ctx = IntegralCtx {
            width: Some(64),
            signed: crate::type_infer::Signedness::Signed,
            four_state: false,
        };
        let size_ty = ctx.expr_type_in_ctx(&size_expr, &longint_ctx);
        let is_integral =
            matches!(size_ty.view, ExprView::BitVec(_)) || matches!(size_ty.ty, Ty::Enum(_));
        if !matches!(size_ty.ty, Ty::Error) && !is_integral {
            items.push(TypeCheckItem::NewExprSizeNotLongint {
                new_site,
                size_site,
            });
        }
        // Negative size: only when constant-evaluable
        if let Some(val) = ctx.const_eval_int(&size_expr)
            && val < 0
        {
            items.push(TypeCheckItem::NewExprSizeNegative {
                new_site,
                size_site,
            });
        }
    }

    // Init arg: must be assignment-compatible with the target
    if let Some(init_expr) = ne.init_arg() {
        let init_site = require_site(site::opt_site_of(map, &init_expr), new_site, items);
        let init_ty = ctx.expr_type(&init_expr);
        if !matches!(init_ty.ty, Ty::Error) {
            let compat = array_assignment_compat(lhs_ty, &init_ty.ty);
            if matches!(compat, ArrayCompat::Incompatible) {
                items.push(TypeCheckItem::NewExprInitIncompat {
                    new_site,
                    init_site,
                    lhs_site,
                    lhs_ty: lhs_ty.clone(),
                    init_ty: init_ty.ty.clone(),
                });
            }
        }
    }
}

enum ArrayCompat {
    Compatible,
    Incompatible,
    Unknown,
}

fn array_assignment_compat(lhs: &Ty, rhs: &Ty) -> ArrayCompat {
    let (lhs_base, lhs_dims) = crate::types::collect_array_dims(lhs);
    let (rhs_base, rhs_dims) = crate::types::collect_array_dims(rhs);

    // Both sides must have unpacked dims
    if lhs_dims.is_empty() || rhs_dims.is_empty() {
        return ArrayCompat::Incompatible;
    }

    // Dim count must match
    if lhs_dims.len() != rhs_dims.len() {
        return ArrayCompat::Incompatible;
    }

    // Element types must be compatible
    if !array_elem_compatible(lhs_base, rhs_base) {
        return ArrayCompat::Incompatible;
    }

    let mut has_unknown = false;

    // Outermost dim
    match outermost_dim_compat(lhs_dims[0], rhs_dims[0]) {
        ArrayCompat::Incompatible => return ArrayCompat::Incompatible,
        ArrayCompat::Unknown => has_unknown = true,
        ArrayCompat::Compatible => {}
    }

    // Inner dims must be structurally equivalent
    for i in 1..lhs_dims.len() {
        if lhs_dims[i] != rhs_dims[i] {
            return ArrayCompat::Incompatible;
        }
    }

    if has_unknown {
        ArrayCompat::Unknown
    } else {
        ArrayCompat::Compatible
    }
}

/// Whether two array element types are assignment-compatible.
///
/// Integral types are compatible if they have the same packed width and
/// signedness, regardless of the underlying keyword (`int` vs `bit signed`).
fn array_elem_compatible(lhs: &Ty, rhs: &Ty) -> bool {
    if lhs == rhs {
        return true;
    }
    match (lhs, rhs) {
        (Ty::Integral(l), Ty::Integral(r)) => {
            l.signed == r.signed && l.try_packed_width() == r.try_packed_width()
        }
        _ => false,
    }
}

fn outermost_dim_compat(lhs: &UnpackedDim, rhs: &UnpackedDim) -> ArrayCompat {
    match (lhs, rhs) {
        (
            UnpackedDim::Unsized,
            UnpackedDim::Unsized
            | UnpackedDim::Size(_)
            | UnpackedDim::Range { .. }
            | UnpackedDim::Queue { .. },
        ) => ArrayCompat::Compatible,
        (UnpackedDim::Queue { .. }, UnpackedDim::Queue { .. } | UnpackedDim::Unsized) => {
            ArrayCompat::Compatible
        }
        (
            UnpackedDim::Size(_) | UnpackedDim::Range { .. },
            UnpackedDim::Size(_) | UnpackedDim::Range { .. },
        ) => match (lhs.try_size(), rhs.try_size()) {
            (Some(l), Some(r)) if l == r => ArrayCompat::Compatible,
            (Some(_), Some(_)) => ArrayCompat::Incompatible,
            _ => ArrayCompat::Unknown,
        },
        (UnpackedDim::Assoc(lk), UnpackedDim::Assoc(rk)) => {
            if lk == rk {
                ArrayCompat::Compatible
            } else {
                ArrayCompat::Incompatible
            }
        }
        _ => ArrayCompat::Incompatible,
    }
}

fn is_void_used_as_expr(ty: &ExprType) -> bool {
    matches!(ty.view, ExprView::Error(ExprTypeErrorKind::VoidUsedAsExpr))
}

fn is_context_dependent(ty: &ExprType) -> bool {
    matches!(
        ty.view,
        ExprView::BitVec(BitVecType {
            width: BitWidth::ContextDependent,
            ..
        })
    )
}

pub(crate) fn bitvec_known_width(ty: &ExprType) -> Option<u32> {
    match &ty.view {
        ExprView::BitVec(BitVecType {
            width: BitWidth::Known(w),
            ..
        }) => Some(*w),
        _ => None,
    }
}
