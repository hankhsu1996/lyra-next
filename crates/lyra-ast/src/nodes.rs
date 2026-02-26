use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxToken;

use crate::node::{AstNode, ast_nodes};
use crate::support::{self, AstChildren};

ast_nodes! {
    // Root
    SourceFile(SyntaxKind::SourceFile) {
        modules: [ModuleDecl],
    }

    // Declarations
    ModuleDecl(SyntaxKind::ModuleDecl) {
        name: token([Ident, EscapedIdent]),
        param_port_list: ParamPortList,
        port_list: PortList,
        body: ModuleBody,
    }

    ModuleBody(SyntaxKind::ModuleBody) {
        always_blocks: [AlwaysBlock],
        initial_blocks: [InitialBlock],
        continuous_assigns: [ContinuousAssign],
        var_decls: [VarDecl],
        net_decls: [NetDecl],
        param_decls: [ParamDecl],
        module_instantiations: [ModuleInstantiation],
    }

    PortList(SyntaxKind::PortList) {
        ports: [Port],
    }

    Port(SyntaxKind::Port) {
        name: token([Ident, EscapedIdent]),
        type_spec: TypeSpec,
        direction: token([InputKw, OutputKw, InoutKw, RefKw]),
    }

    ParamPortList(SyntaxKind::ParamPortList) {
        params: [ParamDecl],
    }

    ParamDecl(SyntaxKind::ParamDecl) {
        declarators: [Declarator],
        type_spec: TypeSpec,
    }

    AlwaysBlock(SyntaxKind::AlwaysBlock) { @custom }

    InitialBlock(SyntaxKind::InitialBlock) { @custom }

    VarDecl(SyntaxKind::VarDecl) {
        type_spec: TypeSpec,
        declarators: [Declarator],
    }

    NetDecl(SyntaxKind::NetDecl) {
        type_spec: TypeSpec,
        declarators: [Declarator],
    }

    ContinuousAssign(SyntaxKind::ContinuousAssign) { @custom }

    PackedDimension(SyntaxKind::PackedDimension) {}
    UnpackedDimension(SyntaxKind::UnpackedDimension) {}

    // Statements
    BlockStmt(SyntaxKind::BlockStmt) { @custom }
    IfStmt(SyntaxKind::IfStmt) { @custom }

    CaseStmt(SyntaxKind::CaseStmt) {
        items: [CaseItem],
    }

    CaseItem(SyntaxKind::CaseItem) { @custom }
    ForStmt(SyntaxKind::ForStmt) { @custom }
    WhileStmt(SyntaxKind::WhileStmt) { @custom }
    RepeatStmt(SyntaxKind::RepeatStmt) { @custom }
    ForeverStmt(SyntaxKind::ForeverStmt) { @custom }
    AssignStmt(SyntaxKind::AssignStmt) { @custom }
    TimingControl(SyntaxKind::TimingControl) { @custom }
    EventExpr(SyntaxKind::EventExpr) {}
    EventItem(SyntaxKind::EventItem) {}

    // Expressions
    Expression(SyntaxKind::Expression) { @custom }
    BinExpr(SyntaxKind::BinExpr) { @custom }
    PrefixExpr(SyntaxKind::PrefixExpr) { @custom }
    ParenExpr(SyntaxKind::ParenExpr) { @custom }
    CondExpr(SyntaxKind::CondExpr) { @custom }
    ConcatExpr(SyntaxKind::ConcatExpr) { @custom }
    ReplicExpr(SyntaxKind::ReplicExpr) { @custom }
    StreamExpr(SyntaxKind::StreamExpr) { @custom }
    StreamSliceSize(SyntaxKind::StreamSliceSize) {}
    StreamOperands(SyntaxKind::StreamOperands) { @custom }
    CastExpr(SyntaxKind::CastExpr) { @custom }
    IndexExpr(SyntaxKind::IndexExpr) { @custom }
    RangeExpr(SyntaxKind::RangeExpr) {}
    FieldExpr(SyntaxKind::FieldExpr) { @custom }

    CallExpr(SyntaxKind::CallExpr) { @custom }

    ArgList(SyntaxKind::ArgList) { @custom }

    SystemTfCall(SyntaxKind::SystemTfCall) { @custom }

    SystemTfArgList(SyntaxKind::SystemTfArgList) { @custom }

    NameRef(SyntaxKind::NameRef) {
        ident: token([Ident, EscapedIdent]),
    }

    Literal(SyntaxKind::Literal) { @custom }

    ErrorNode(SyntaxKind::ErrorNode) {}

    Declarator(SyntaxKind::Declarator) { @custom }

    ModuleInstantiation(SyntaxKind::ModuleInstantiation) {
        module_name: token([Ident, EscapedIdent]),
        instances: [HierarchicalInstance],
    }

    HierarchicalInstance(SyntaxKind::HierarchicalInstance) {
        name: token([Ident, EscapedIdent]),
        port_list: InstancePortList,
    }

    InstancePortList(SyntaxKind::InstancePortList) {
        ports: [InstancePort],
    }

    InstancePort(SyntaxKind::InstancePort) {}

    TypeSpec(SyntaxKind::TypeSpec) { @custom }

    EnumType(SyntaxKind::EnumType) {
        members: [EnumMember],
    }

    EnumMember(SyntaxKind::EnumMember) {
        name: token([Ident, EscapedIdent]),
    }

    EnumMemberRange(SyntaxKind::EnumMemberRange) {}

    StructType(SyntaxKind::StructType) {
        members: [StructMember],
    }

    StructMember(SyntaxKind::StructMember) {
        type_spec: TypeSpec,
        declarators: [Declarator],
    }

    // Package declarations
    PackageDecl(SyntaxKind::PackageDecl) { @custom }
    PackageBody(SyntaxKind::PackageBody) { @custom }
    ImportDecl(SyntaxKind::ImportDecl) { @custom }
    ImportItem(SyntaxKind::ImportItem) { @custom }
    ExportDecl(SyntaxKind::ExportDecl) {
        items: [ExportItem],
    }
    ExportItem(SyntaxKind::ExportItem) { @custom }
    QualifiedName(SyntaxKind::QualifiedName) { @custom }

    DottedName(SyntaxKind::DottedName) {
        interface_ref: NameRef,
    }

    TypedefDecl(SyntaxKind::TypedefDecl) {
        name: token([Ident, EscapedIdent]),
        type_spec: TypeSpec,
    }

    InterfaceDecl(SyntaxKind::InterfaceDecl) {
        name: token([Ident, EscapedIdent]),
        param_port_list: ParamPortList,
        port_list: PortList,
        body: InterfaceBody,
    }

    InterfaceBody(SyntaxKind::InterfaceBody) {}

    GenerateRegion(SyntaxKind::GenerateRegion) {}

    ModportDecl(SyntaxKind::ModportDecl) {
        items: [ModportItem],
    }

    ModportItem(SyntaxKind::ModportItem) { @custom }

    ModportPort(SyntaxKind::ModportPort) {
        direction: token([InputKw, OutputKw, InoutKw, RefKw]),
        name: token([Ident, EscapedIdent]),
    }

    ModportExprPort(SyntaxKind::ModportExprPort) {
        direction: token([InputKw, OutputKw, InoutKw, RefKw]),
        port_name: token([Ident, EscapedIdent]),
    }

    ProgramDecl(SyntaxKind::ProgramDecl) {
        name: token([Ident, EscapedIdent]),
        param_port_list: ParamPortList,
        port_list: PortList,
        body: ProgramBody,
    }

    ProgramBody(SyntaxKind::ProgramBody) {}

    PrimitiveDecl(SyntaxKind::PrimitiveDecl) {
        name: token([Ident, EscapedIdent]),
    }

    ConfigDecl(SyntaxKind::ConfigDecl) {
        name: token([Ident, EscapedIdent]),
    }

    FunctionDecl(SyntaxKind::FunctionDecl) {
        type_spec: TypeSpec,
        declarators: [Declarator],
    }

    TaskDecl(SyntaxKind::TaskDecl) {
        declarators: [Declarator],
    }

    TfPortDecl(SyntaxKind::TfPortDecl) {
        type_spec: TypeSpec,
        declarators: [Declarator],
        direction: token([InputKw, OutputKw, InoutKw, RefKw]),
    }

    // Streaming `with` clause (LRM 11.4.14.4)
    StreamOperandItem(SyntaxKind::StreamOperandItem) { @custom }
    StreamWithClause(SyntaxKind::StreamWithClause) { @custom }
    StreamRange(SyntaxKind::StreamRange) { @custom }
}

// Custom accessors

impl AlwaysBlock {
    pub fn keyword(&self) -> Option<SyntaxToken> {
        support::token_in(
            &self.syntax,
            &[
                SyntaxKind::AlwaysKw,
                SyntaxKind::AlwaysCombKw,
                SyntaxKind::AlwaysFfKw,
                SyntaxKind::AlwaysLatchKw,
            ],
        )
    }

    pub fn block_stmt(&self) -> Option<BlockStmt> {
        support::child(&self.syntax)
    }

    /// The body statement (first statement-kind child after keyword).
    pub fn body(&self) -> Option<crate::node::StmtNode> {
        support::child(&self.syntax)
    }
}

impl InitialBlock {
    pub fn block_stmt(&self) -> Option<BlockStmt> {
        support::child(&self.syntax)
    }

    /// The body statement.
    pub fn body(&self) -> Option<crate::node::StmtNode> {
        support::child(&self.syntax)
    }
}

impl BlockStmt {
    /// Iterate over all statement-kind children.
    pub fn statements(&self) -> AstChildren<crate::node::StmtNode> {
        support::children(&self.syntax)
    }
}

impl IfStmt {
    /// The condition expression.
    pub fn condition(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    /// The then-branch body (first statement-kind child).
    pub fn then_body(&self) -> Option<crate::node::StmtNode> {
        support::children::<crate::node::StmtNode>(&self.syntax).next()
    }

    /// The else-branch body (second statement-kind child), or `None`.
    pub fn else_body(&self) -> Option<crate::node::StmtNode> {
        support::children::<crate::node::StmtNode>(&self.syntax).nth(1)
    }

    /// Whether this if-statement has an `else` clause.
    pub fn has_else(&self) -> bool {
        support::token(&self.syntax, SyntaxKind::ElseKw).is_some()
    }
}

impl CaseItem {
    /// The body statement of this case item.
    pub fn body(&self) -> Option<crate::node::StmtNode> {
        support::child(&self.syntax)
    }

    /// Whether this is a `default` case item.
    pub fn is_default(&self) -> bool {
        support::token(&self.syntax, SyntaxKind::DefaultKw).is_some()
    }
}

impl ForStmt {
    /// The loop body (last statement-kind child).
    pub fn body(&self) -> Option<crate::node::StmtNode> {
        support::children::<crate::node::StmtNode>(&self.syntax).last()
    }
}

impl WhileStmt {
    /// The loop body.
    pub fn body(&self) -> Option<crate::node::StmtNode> {
        support::child(&self.syntax)
    }
}

impl RepeatStmt {
    /// The loop body.
    pub fn body(&self) -> Option<crate::node::StmtNode> {
        support::child(&self.syntax)
    }
}

impl ForeverStmt {
    /// The loop body.
    pub fn body(&self) -> Option<crate::node::StmtNode> {
        support::child(&self.syntax)
    }
}

impl TimingControl {
    /// The body statement following the timing control.
    pub fn body(&self) -> Option<crate::node::StmtNode> {
        support::child(&self.syntax)
    }
}

impl Expression {
    /// The single wrapped expression child.
    pub fn inner(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }
}

impl ParenExpr {
    /// The expression inside the parentheses.
    pub fn inner(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }
}

impl SourceFile {
    pub fn packages(&self) -> AstChildren<PackageDecl> {
        support::children(&self.syntax)
    }

    pub fn interfaces(&self) -> AstChildren<InterfaceDecl> {
        support::children(&self.syntax)
    }

    pub fn programs(&self) -> AstChildren<ProgramDecl> {
        support::children(&self.syntax)
    }

    pub fn primitives(&self) -> AstChildren<PrimitiveDecl> {
        support::children(&self.syntax)
    }

    pub fn configs(&self) -> AstChildren<ConfigDecl> {
        support::children(&self.syntax)
    }
}

impl BinExpr {
    pub fn op_token(&self) -> Option<SyntaxToken> {
        // The operator is the token between the two child nodes
        let mut saw_first_child = false;
        for el in self.syntax.children_with_tokens() {
            match el {
                rowan::NodeOrToken::Node(_) => {
                    if saw_first_child {
                        return None;
                    }
                    saw_first_child = true;
                }
                rowan::NodeOrToken::Token(tok) => {
                    if saw_first_child && tok.kind() != SyntaxKind::Whitespace {
                        return Some(tok);
                    }
                }
            }
        }
        None
    }

    pub fn lhs(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    pub fn rhs(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 1)
    }

    pub fn binary_op(&self) -> Option<SyntaxBinaryOp> {
        SyntaxBinaryOp::from_token(self.op_token()?.kind())
    }
}

/// Syntactic binary operator classification.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyntaxBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    BitXnor,
    Shl,
    Shr,
    Ashl,
    Ashr,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Eq,
    Neq,
    CaseEq,
    CaseNeq,
    WildEq,
    WildNeq,
    LogAnd,
    LogOr,
    Power,
}

impl SyntaxBinaryOp {
    pub fn from_token(kind: SyntaxKind) -> Option<SyntaxBinaryOp> {
        match kind {
            SyntaxKind::Plus => Some(SyntaxBinaryOp::Add),
            SyntaxKind::Minus => Some(SyntaxBinaryOp::Sub),
            SyntaxKind::Star => Some(SyntaxBinaryOp::Mul),
            SyntaxKind::Slash => Some(SyntaxBinaryOp::Div),
            SyntaxKind::Percent => Some(SyntaxBinaryOp::Mod),
            SyntaxKind::Amp => Some(SyntaxBinaryOp::BitAnd),
            SyntaxKind::Pipe => Some(SyntaxBinaryOp::BitOr),
            SyntaxKind::Caret => Some(SyntaxBinaryOp::BitXor),
            SyntaxKind::TildeCaret | SyntaxKind::CaretTilde => Some(SyntaxBinaryOp::BitXnor),
            SyntaxKind::LtLt => Some(SyntaxBinaryOp::Shl),
            SyntaxKind::GtGt => Some(SyntaxBinaryOp::Shr),
            SyntaxKind::LtLtLt => Some(SyntaxBinaryOp::Ashl),
            SyntaxKind::GtGtGt => Some(SyntaxBinaryOp::Ashr),
            SyntaxKind::Lt => Some(SyntaxBinaryOp::Lt),
            SyntaxKind::LtEq => Some(SyntaxBinaryOp::LtEq),
            SyntaxKind::Gt => Some(SyntaxBinaryOp::Gt),
            SyntaxKind::GtEq => Some(SyntaxBinaryOp::GtEq),
            SyntaxKind::EqEq => Some(SyntaxBinaryOp::Eq),
            SyntaxKind::BangEq => Some(SyntaxBinaryOp::Neq),
            SyntaxKind::EqEqEq => Some(SyntaxBinaryOp::CaseEq),
            SyntaxKind::BangEqEq => Some(SyntaxBinaryOp::CaseNeq),
            SyntaxKind::EqEqQuestion => Some(SyntaxBinaryOp::WildEq),
            SyntaxKind::BangEqQuestion => Some(SyntaxBinaryOp::WildNeq),
            SyntaxKind::AmpAmp => Some(SyntaxBinaryOp::LogAnd),
            SyntaxKind::PipePipe => Some(SyntaxBinaryOp::LogOr),
            SyntaxKind::StarStar => Some(SyntaxBinaryOp::Power),
            _ => None,
        }
    }
}

/// Syntactic assignment operator classification.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyntaxAssignOp {
    Blocking,
    NonBlocking,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    ShlAssign,
    ShrAssign,
    AshlAssign,
    AshrAssign,
}

impl SyntaxAssignOp {
    pub fn from_token(kind: SyntaxKind) -> Option<SyntaxAssignOp> {
        match kind {
            SyntaxKind::Assign => Some(SyntaxAssignOp::Blocking),
            SyntaxKind::LtEq => Some(SyntaxAssignOp::NonBlocking),
            SyntaxKind::PlusEq => Some(SyntaxAssignOp::AddAssign),
            SyntaxKind::MinusEq => Some(SyntaxAssignOp::SubAssign),
            SyntaxKind::StarEq => Some(SyntaxAssignOp::MulAssign),
            SyntaxKind::SlashEq => Some(SyntaxAssignOp::DivAssign),
            SyntaxKind::PercentEq => Some(SyntaxAssignOp::ModAssign),
            SyntaxKind::AmpEq => Some(SyntaxAssignOp::AndAssign),
            SyntaxKind::PipeEq => Some(SyntaxAssignOp::OrAssign),
            SyntaxKind::CaretEq => Some(SyntaxAssignOp::XorAssign),
            SyntaxKind::LtLtEq => Some(SyntaxAssignOp::ShlAssign),
            SyntaxKind::GtGtEq => Some(SyntaxAssignOp::ShrAssign),
            SyntaxKind::LtLtLtEq => Some(SyntaxAssignOp::AshlAssign),
            SyntaxKind::GtGtGtEq => Some(SyntaxAssignOp::AshrAssign),
            _ => None,
        }
    }

    pub fn is_compound(&self) -> bool {
        !matches!(self, SyntaxAssignOp::Blocking | SyntaxAssignOp::NonBlocking)
    }
}

/// Either a bare `ModportPort` or an expression `ModportExprPort`.
pub enum ModportPortKind {
    Bare(ModportPort),
    Expr(ModportExprPort),
}

impl ModportItem {
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token_in(&self.syntax, &[SyntaxKind::Ident, SyntaxKind::EscapedIdent])
    }

    pub fn ports(&self) -> AstChildren<ModportPort> {
        support::children(&self.syntax)
    }

    pub fn port_items(&self) -> impl Iterator<Item = ModportPortKind> + '_ {
        self.syntax.children().filter_map(|child| {
            if child.kind() == SyntaxKind::ModportPort {
                ModportPort::cast(child).map(ModportPortKind::Bare)
            } else if child.kind() == SyntaxKind::ModportExprPort {
                ModportExprPort::cast(child).map(ModportPortKind::Expr)
            } else {
                None
            }
        })
    }
}

impl ModportExprPort {
    pub fn target_expr(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }
}

impl PrefixExpr {
    pub fn op_token(&self) -> Option<SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .find(|tok| tok.kind() != SyntaxKind::Whitespace)
    }

    pub fn operand(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }
}

impl IndexExpr {
    /// The (base, index) expression pair. Returns None if not exactly 2
    /// expression children (error recovery).
    fn expr_pair(&self) -> Option<(crate::expr::Expr, crate::expr::Expr)> {
        let mut iter = support::expr_children(&self.syntax);
        let first = iter.next()?;
        let second = iter.next()?;
        if iter.next().is_some() {
            return None;
        }
        Some((first, second))
    }

    /// The base expression (left of the brackets).
    pub fn base_expr(&self) -> Option<crate::expr::Expr> {
        self.expr_pair().map(|(base, _)| base)
    }

    /// The index expression (inside the brackets).
    pub fn index_expr(&self) -> Option<crate::expr::Expr> {
        self.expr_pair().map(|(_, idx)| idx)
    }
}

impl FieldExpr {
    /// The base expression before the dot.
    pub fn base_expr(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    pub fn field_name(&self) -> Option<SyntaxToken> {
        // The field name is the last identifier token (simple or escaped)
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .filter(|tok| matches!(tok.kind(), SyntaxKind::Ident | SyntaxKind::EscapedIdent))
            .last()
    }
}

impl CondExpr {
    pub fn condition(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    pub fn then_expr(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 1)
    }

    pub fn else_expr(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 2)
    }
}

impl ConcatExpr {
    pub fn operands(&self) -> impl Iterator<Item = crate::expr::Expr> + '_ {
        support::expr_children(&self.syntax)
    }
}

impl ReplicExpr {
    pub fn count(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    pub fn body_exprs(&self) -> impl Iterator<Item = crate::expr::Expr> + '_ {
        support::expr_children(&self.syntax).skip(1)
    }
}

impl CallExpr {
    pub fn arg_list(&self) -> Option<ArgList> {
        support::child(&self.syntax)
    }

    pub fn callee(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }
}

impl StreamExpr {
    pub fn stream_operands(&self) -> Option<StreamOperands> {
        support::child(&self.syntax)
    }
}

impl StreamOperands {
    pub fn items(&self) -> AstChildren<StreamOperandItem> {
        support::children(&self.syntax)
    }
}

impl SystemTfCall {
    pub fn arg_list(&self) -> Option<SystemTfArgList> {
        support::child(&self.syntax)
    }

    pub fn system_name(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, SyntaxKind::SystemIdent)
    }
}

impl SystemTfArgList {
    pub fn args(&self) -> impl Iterator<Item = crate::expr::TfArg> + '_ {
        self.syntax.children().map(|child| {
            if let Some(t) = crate::expr::TypeRef::cast(child.clone()) {
                crate::expr::TfArg::Type(t)
            } else if let Some(e) = crate::expr::Expr::cast(child.clone()) {
                crate::expr::TfArg::Expr(e)
            } else {
                crate::expr::TfArg::Unknown(child)
            }
        })
    }
}

impl Literal {
    pub fn token(&self) -> Option<SyntaxToken> {
        support::token_in(
            &self.syntax,
            &[
                SyntaxKind::IntLiteral,
                SyntaxKind::RealLiteral,
                SyntaxKind::BasedLiteral,
                SyntaxKind::UnbasedUnsizedLiteral,
                SyntaxKind::StringLiteral,
            ],
        )
    }

    pub fn literal_kind(&self) -> Option<crate::expr::LiteralKind> {
        let tokens: Vec<SyntaxToken> = self
            .syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .filter(|t| {
                !matches!(
                    t.kind(),
                    SyntaxKind::Whitespace | SyntaxKind::LineComment | SyntaxKind::BlockComment
                )
            })
            .collect();

        if tokens.is_empty() {
            return None;
        }

        // Check for BasedLiteral anywhere
        let based_pos = tokens
            .iter()
            .position(|t| t.kind() == SyntaxKind::BasedLiteral);
        if let Some(bp) = based_pos {
            let size_token = if bp > 0 && tokens[bp - 1].kind() == SyntaxKind::IntLiteral {
                Some(tokens[bp - 1].clone())
            } else {
                None
            };
            return Some(crate::expr::LiteralKind::Based {
                size_token,
                base_token: tokens[bp].clone(),
            });
        }

        let first = &tokens[0];
        match first.kind() {
            SyntaxKind::IntLiteral => Some(crate::expr::LiteralKind::Int {
                token: first.clone(),
            }),
            SyntaxKind::UnbasedUnsizedLiteral => Some(crate::expr::LiteralKind::UnbasedUnsized {
                token: first.clone(),
            }),
            SyntaxKind::RealLiteral => Some(crate::expr::LiteralKind::Real {
                token: first.clone(),
            }),
            SyntaxKind::StringLiteral => Some(crate::expr::LiteralKind::String {
                token: first.clone(),
            }),
            SyntaxKind::TimeLiteral => Some(crate::expr::LiteralKind::Time {
                token: first.clone(),
            }),
            _ => Some(crate::expr::LiteralKind::Unknown {
                token: first.clone(),
            }),
        }
    }
}

impl AssignStmt {
    pub fn op_token(&self) -> Option<SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .find(|tok| SyntaxAssignOp::from_token(tok.kind()).is_some())
    }

    pub fn assign_op(&self) -> Option<SyntaxAssignOp> {
        SyntaxAssignOp::from_token(self.op_token()?.kind())
    }

    pub fn lhs(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    pub fn rhs(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 1)
    }

    /// Optional timing control (intra-assignment delay).
    pub fn timing_control(&self) -> Option<TimingControl> {
        support::child(&self.syntax)
    }
}

impl ContinuousAssign {
    pub fn lhs(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    pub fn rhs(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 1)
    }

    /// Optional timing control (delay) on the assign.
    pub fn timing_control(&self) -> Option<TimingControl> {
        support::child(&self.syntax)
    }
}

impl Declarator {
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token_in(&self.syntax, &[SyntaxKind::Ident, SyntaxKind::EscapedIdent])
    }

    /// The initializer expression after `=`, if present.
    pub fn init_expr(&self) -> Option<crate::expr::Expr> {
        let mut seen_assign = false;
        for child in self.syntax.children_with_tokens() {
            if child
                .as_token()
                .is_some_and(|t| t.kind() == SyntaxKind::Assign)
            {
                seen_assign = true;
                continue;
            }
            if seen_assign
                && let Some(node) = child.as_node()
                && crate::node::is_expression_kind(node.kind())
            {
                return crate::expr::Expr::cast(node.clone());
            }
        }
        None
    }
}

impl ArgList {
    /// Iterate over argument expression nodes.
    pub fn args(&self) -> impl Iterator<Item = crate::expr::Expr> + '_ {
        support::expr_children(&self.syntax)
    }
}

/// The kind of a range expression inside `[]`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RangeKind {
    /// `[hi:lo]` -- fixed part-select.
    Fixed,
    /// `[base+:width]` -- indexed part-select (ascending).
    IndexedPlus,
    /// `[base-:width]` -- indexed part-select (descending).
    IndexedMinus,
}

impl RangeExpr {
    /// Classify the range form by looking for direct `+` or `-` token children.
    ///
    /// Uses `support::token()` which only searches direct children of this
    /// node. A `+` inside a `BinExpr` child (e.g. `a[i+1:j]`) is NOT a
    /// direct child and will not trigger `IndexedPlus`.
    pub fn range_kind(&self) -> RangeKind {
        if support::token(&self.syntax, SyntaxKind::Plus).is_some() {
            RangeKind::IndexedPlus
        } else if support::token(&self.syntax, SyntaxKind::Minus).is_some() {
            RangeKind::IndexedMinus
        } else {
            RangeKind::Fixed
        }
    }

    /// The base object expression (before the brackets).
    /// For `a[hi:lo]` or `a[idx+:w]`, returns `a`.
    pub fn base_expr(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    /// The two operand expressions inside the brackets.
    /// Fixed `[hi:lo]`: returns (hi, lo).
    /// Indexed `[idx+:w]` / `[idx-:w]`: returns (idx, w).
    pub fn operand_exprs(&self) -> Option<(crate::expr::Expr, crate::expr::Expr)> {
        let first = support::expr_child(&self.syntax, 1)?;
        let second = support::expr_child(&self.syntax, 2)?;
        Some((first, second))
    }
}

impl EnumMember {
    pub fn range_spec(&self) -> Option<EnumMemberRange> {
        support::child(&self.syntax)
    }

    pub fn init_expr(&self) -> Option<Expression> {
        support::child(&self.syntax)
    }
}

impl EnumMemberRange {
    pub fn first_expr(&self) -> Option<Expression> {
        support::children::<Expression>(&self.syntax).next()
    }

    pub fn second_expr(&self) -> Option<Expression> {
        support::children::<Expression>(&self.syntax).nth(1)
    }
}

impl CastExpr {
    pub fn cast_type(&self) -> Option<TypeSpec> {
        support::child(&self.syntax)
    }

    /// The inner expression of the cast (the operand inside parentheses).
    /// Returns the first expression-kind direct child that is not a `TypeSpec`.
    pub fn inner_expr(&self) -> Option<crate::expr::Expr> {
        self.syntax
            .children()
            .find(|c| c.kind() != SyntaxKind::TypeSpec && crate::node::is_expression_kind(c.kind()))
            .and_then(crate::expr::Expr::cast)
    }
}

/// Classification of the range form inside a `StreamRange` node.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StreamRangeOp {
    /// `[expr]` -- single element.
    Single,
    /// `[lo : hi]` -- fixed range.
    Fixed,
    /// `[base +: width]` -- indexed ascending.
    IndexedPlus,
    /// `[base -: width]` -- indexed descending.
    IndexedMinus,
}

impl StreamOperandItem {
    /// The operand expression (first expression-kind child).
    pub fn expr(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    /// The optional `with [...]` clause.
    pub fn with_clause(&self) -> Option<StreamWithClause> {
        support::child(&self.syntax)
    }
}

impl StreamWithClause {
    /// The range inside `[...]`.
    pub fn range(&self) -> Option<StreamRange> {
        support::child(&self.syntax)
    }
}

impl StreamRange {
    /// Classify the range form by inspecting direct token children.
    ///
    /// A `+` inside a `BinExpr` child (e.g. `[a + b]`) is NOT a direct
    /// token of `StreamRange`, so it cannot trigger `IndexedPlus`.
    pub fn op(&self) -> Option<StreamRangeOp> {
        let expr_count = support::expr_children(&self.syntax).count();
        match expr_count {
            1 => Some(StreamRangeOp::Single),
            2 => {
                // Collect direct non-trivia tokens between expression children
                let direct_tokens: Vec<SyntaxKind> = self
                    .syntax
                    .children_with_tokens()
                    .filter_map(rowan::NodeOrToken::into_token)
                    .map(|t| t.kind())
                    .filter(|k| {
                        !matches!(
                            k,
                            SyntaxKind::Whitespace
                                | SyntaxKind::LineComment
                                | SyntaxKind::BlockComment
                        )
                    })
                    .collect();
                // Look for Plus+Colon, Minus+Colon, or lone Colon
                let has_plus_colon = direct_tokens
                    .windows(2)
                    .any(|w| w[0] == SyntaxKind::Plus && w[1] == SyntaxKind::Colon);
                let has_minus_colon = direct_tokens
                    .windows(2)
                    .any(|w| w[0] == SyntaxKind::Minus && w[1] == SyntaxKind::Colon);
                if has_plus_colon {
                    Some(StreamRangeOp::IndexedPlus)
                } else if has_minus_colon {
                    Some(StreamRangeOp::IndexedMinus)
                } else if direct_tokens.contains(&SyntaxKind::Colon) {
                    Some(StreamRangeOp::Fixed)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// First expression child (the index/base).
    pub fn lhs(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    /// Second expression child (upper bound/width), `None` for `Single`.
    pub fn rhs(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 1)
    }
}

impl InstancePort {
    /// The port name token for named connections (`.foo(expr)`).
    pub fn port_name(&self) -> Option<SyntaxToken> {
        // Named connection: `.` Ident `(` expr `)`
        // Wildcard `.*` is lexed as a single DotStar token, not Dot + Star
        let mut saw_dot = false;
        for el in self.syntax.children_with_tokens() {
            if let rowan::NodeOrToken::Token(tok) = el {
                if tok.kind() == SyntaxKind::Dot {
                    saw_dot = true;
                } else if saw_dot
                    && matches!(tok.kind(), SyntaxKind::Ident | SyntaxKind::EscapedIdent)
                {
                    return Some(tok);
                } else if tok.kind() == SyntaxKind::DotStar {
                    return None;
                }
            }
        }
        None
    }

    /// The actual expression node connected to this port.
    pub fn actual_expr(&self) -> Option<lyra_parser::SyntaxNode> {
        // For named: `.foo(expr)` -- the expr is a child node inside parens
        // For positional: the expr is a direct child node
        self.syntax.children().next()
    }

    /// Whether this is a named connection (`.foo(expr)`).
    pub fn is_named(&self) -> bool {
        self.port_name().is_some()
    }

    /// Whether this is a `.*` wildcard connection.
    pub fn is_wildcard(&self) -> bool {
        self.syntax.children_with_tokens().any(
            |el| matches!(el, rowan::NodeOrToken::Token(tok) if tok.kind() == SyntaxKind::DotStar),
        )
    }

    /// Whether the connection has explicit parentheses (`.foo(expr)`).
    pub fn has_parens(&self) -> bool {
        support::token(&self.syntax, SyntaxKind::LParen).is_some()
    }
}

impl ModuleInstantiation {
    /// The parameter override list `#(...)` if present.
    pub fn param_overrides(&self) -> Option<ParamPortList> {
        support::child(&self.syntax)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn find_range(node: &lyra_parser::SyntaxNode) -> Option<RangeExpr> {
        if node.kind() == SyntaxKind::RangeExpr {
            return RangeExpr::cast(node.clone());
        }
        for child in node.children() {
            if let Some(r) = find_range(&child) {
                return Some(r);
            }
        }
        None
    }

    fn parse_range_expr(src: &str) -> RangeExpr {
        let tokens = lyra_lexer::lex(src);
        let pp = lyra_preprocess::preprocess_identity(lyra_source::FileId(0), &tokens, src);
        let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
        find_range(&parse.syntax()).expect("should contain a RangeExpr")
    }

    #[test]
    fn range_kind_indexed_plus() {
        let re = parse_range_expr("module m; parameter P = a[i+:4]; endmodule");
        assert_eq!(re.range_kind(), RangeKind::IndexedPlus);
    }

    #[test]
    fn range_kind_indexed_minus() {
        let re = parse_range_expr("module m; parameter P = a[i-:4]; endmodule");
        assert_eq!(re.range_kind(), RangeKind::IndexedMinus);
    }

    #[test]
    fn range_kind_fixed_with_plus_in_expr() {
        // a[i+1:j] -- the + is inside BinExpr, not a direct child
        let re = parse_range_expr("module m; parameter P = a[i+1:j]; endmodule");
        assert_eq!(re.range_kind(), RangeKind::Fixed);
    }
}
