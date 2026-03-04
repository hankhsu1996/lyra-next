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
        function_decls: [FunctionDecl],
        task_decls: [TaskDecl],
        typedef_decls: [TypedefDecl],
    }

    PortList(SyntaxKind::PortList) {
        ports: [Port],
    }

    Port(SyntaxKind::Port) {
        name: token([Ident, EscapedIdent]),
        type_spec: TypeSpec,
        direction: token([InputKw, OutputKw, InoutKw, RefKw]) as direction_token,
    }

    ParamPortList(SyntaxKind::ParamPortList) { @custom }

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
    ForeachStmt(SyntaxKind::ForeachStmt) { @custom }
    ForeachVarList(SyntaxKind::ForeachVarList) {
        slots: [ForeachVarSlot],
    }
    ForeachVarSlot(SyntaxKind::ForeachVarSlot) {
        declarator: Declarator,
    }
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
    NewExpr(SyntaxKind::NewExpr) { @custom }
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
        base_ref: NameRef,
    }

    TypedefDecl(SyntaxKind::TypedefDecl) {
        name: token([Ident, EscapedIdent]),
        type_spec: TypeSpec,
    }

    NettypeDecl(SyntaxKind::NettypeDecl) {
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
        direction: token([InputKw, OutputKw, InoutKw, RefKw]) as direction_token,
        name: token([Ident, EscapedIdent]),
    }

    ModportExprPort(SyntaxKind::ModportExprPort) {
        direction: token([InputKw, OutputKw, InoutKw, RefKw]) as direction_token,
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
        direction: token([InputKw, OutputKw, InoutKw, RefKw]) as direction_token,
    }

    // Streaming `with` clause (LRM 11.4.14.4)
    StreamOperandItem(SyntaxKind::StreamOperandItem) { @custom }
    StreamWithClause(SyntaxKind::StreamWithClause) { @custom }
    StreamRange(SyntaxKind::StreamRange) { @custom }

    // Type operator (LRM 6.23)
    TypeExpr(SyntaxKind::TypeExpr) { @custom }

    // Array manipulation `with (expr)` clause (LRM 7.12)
    ArrayManipWithClause(SyntaxKind::ArrayManipWithClause) { @custom }

    // Strength specifications (LRM 6.3.2)
    DriveStrength(SyntaxKind::DriveStrength) { @custom }
    ChargeStrength(SyntaxKind::ChargeStrength) { @custom }
}

// Custom accessors

impl ParamPortList {
    pub fn params(&self) -> AstChildren<ParamDecl> {
        support::children(&self.syntax)
    }
}

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

impl Declarator {
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token_in(&self.syntax, &[SyntaxKind::Ident, SyntaxKind::EscapedIdent])
    }

    /// Token-level span of the declarator name identifier.
    ///
    /// Returns the span of the `Ident`/`EscapedIdent` token without
    /// leading trivia. Returns `None` if the name token is missing.
    pub fn ident_name_span(&self) -> Option<lyra_source::NameSpan> {
        self.name()
            .map(|t| lyra_source::NameSpan::new(t.text_range()))
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

    /// The default type spec after `=` in a type parameter declarator.
    ///
    /// For `parameter type T = int`, returns the `TypeSpec` for `int`.
    pub fn default_type_spec(&self) -> Option<TypeSpec> {
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
                && node.kind() == SyntaxKind::TypeSpec
            {
                return TypeSpec::cast(node.clone());
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

    /// Iterate `InstancePort` children in the parameter override list.
    ///
    /// In override context (`#(.W(8), .D(16))`), the `ParamPortList` node
    /// contains `InstancePort` children rather than `ParamDecl`. This
    /// accessor exposes that grammar-specific iteration from the parent
    /// that knows which production is in use.
    pub fn param_override_ports(&self) -> impl Iterator<Item = InstancePort> {
        self.param_overrides()
            .into_iter()
            .flat_map(|ppl| support::children::<InstancePort>(&ppl.syntax))
    }
}
