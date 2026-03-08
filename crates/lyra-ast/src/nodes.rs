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
        direction: token([InputKw, OutputKw, InoutKw]) as direction_token,
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

    CaseStmt(SyntaxKind::CaseStmt) { @custom }

    CaseItem(SyntaxKind::CaseItem) { @custom }
    CaseInsideItem(SyntaxKind::CaseInsideItem) { @custom }
    RangeList(SyntaxKind::RangeList) { @custom }
    ValueRange(SyntaxKind::ValueRange) { @custom }
    ForStmt(SyntaxKind::ForStmt) { @custom }
    WhileStmt(SyntaxKind::WhileStmt) { @custom }
    RepeatStmt(SyntaxKind::RepeatStmt) { @custom }
    ForeverStmt(SyntaxKind::ForeverStmt) { @custom }
    DoWhileStmt(SyntaxKind::DoWhileStmt) { @custom }
    BreakStmt(SyntaxKind::BreakStmt) {
        kw: token(BreakKw) as break_kw,
    }
    ContinueStmt(SyntaxKind::ContinueStmt) {
        kw: token(ContinueKw) as continue_kw,
    }
    ReturnStmt(SyntaxKind::ReturnStmt) { @custom }
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
    AssignmentPatternExpr(SyntaxKind::AssignmentPatternExpr) { @custom }
    ReplicExpr(SyntaxKind::ReplicExpr) { @custom }
    StreamExpr(SyntaxKind::StreamExpr) { @custom }
    StreamSliceSize(SyntaxKind::StreamSliceSize) { @custom }
    StreamOperands(SyntaxKind::StreamOperands) { @custom }
    CastExpr(SyntaxKind::CastExpr) { @custom }
    NewExpr(SyntaxKind::NewExpr) { @custom }
    TaggedExpr(SyntaxKind::TaggedExpr) { @custom }
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

    InterfaceBody(SyntaxKind::InterfaceBody) {
        var_decls: [VarDecl],
        function_decls: [FunctionDecl],
        task_decls: [TaskDecl],
    }

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

    // Modport TF port group (LRM 25.7): `import entry {, entry}` or `export entry {, entry}`
    ModportTfPortsGroup(SyntaxKind::ModportTfPortsGroup) {
        import_export: token([ImportKw, ExportKw]) as import_export_token,
        entries: [ModportTfPortEntry],
    }

    // Modport TF port entry (LRM 25.7): bare ident or task/function prototype
    ModportTfPortEntry(SyntaxKind::ModportTfPortEntry) { @custom }

    // Task prototype (LRM 25.7): `task name [ ( tf_port_list ) ]`
    TaskPrototype(SyntaxKind::TaskPrototype) {
        name: token([Ident, EscapedIdent]),
    }

    // Function prototype (LRM 25.7): `function data_type_or_void name [ ( tf_port_list ) ]`
    FunctionPrototype(SyntaxKind::FunctionPrototype) {
        name: token([Ident, EscapedIdent]),
        type_spec: TypeSpec,
    }

    ProgramDecl(SyntaxKind::ProgramDecl) {
        name: token([Ident, EscapedIdent]),
        param_port_list: ParamPortList,
        port_list: PortList,
        body: ProgramBody,
    }

    ProgramBody(SyntaxKind::ProgramBody) {
        var_decls: [VarDecl],
        function_decls: [FunctionDecl],
        task_decls: [TaskDecl],
    }

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

    // Time unit/precision declarations (LRM 3.14.2.2)
    TimeunitDecl(SyntaxKind::TimeunitDecl) { @custom }
    TimeprecisionDecl(SyntaxKind::TimeprecisionDecl) { @custom }
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

/// Variant kinds within a modport item's port list.
pub enum ModportPortKind {
    Bare(ModportPort),
    Expr(ModportExprPort),
    TfGroup(ModportTfPortsGroup),
}

impl ModportItem {
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token_in(&self.syntax, &[SyntaxKind::Ident, SyntaxKind::EscapedIdent])
    }

    pub fn ports(&self) -> AstChildren<ModportPort> {
        support::children(&self.syntax)
    }

    pub fn port_items(&self) -> impl Iterator<Item = ModportPortKind> + '_ {
        self.syntax
            .children()
            .filter_map(|child| match child.kind() {
                SyntaxKind::ModportPort => ModportPort::cast(child).map(ModportPortKind::Bare),
                SyntaxKind::ModportExprPort => {
                    ModportExprPort::cast(child).map(ModportPortKind::Expr)
                }
                SyntaxKind::ModportTfPortsGroup => {
                    ModportTfPortsGroup::cast(child).map(ModportPortKind::TfGroup)
                }
                _ => None,
            })
    }
}

impl TaskPrototype {
    pub fn tf_port_decls(&self) -> AstChildren<TfPortDecl> {
        support::children(&self.syntax)
    }
}

impl FunctionPrototype {
    pub fn tf_port_decls(&self) -> AstChildren<TfPortDecl> {
        support::children(&self.syntax)
    }
}

impl ModportTfPortEntry {
    /// The bare name token (for entries without a prototype, e.g. `import Read`).
    pub fn bare_name_token(&self) -> Option<SyntaxToken> {
        support::token_in(&self.syntax, &[SyntaxKind::Ident, SyntaxKind::EscapedIdent])
    }

    /// The `TaskPrototype` child, if this entry is `task name(...)`.
    pub fn task_prototype(&self) -> Option<TaskPrototype> {
        support::child(&self.syntax)
    }

    /// The `FunctionPrototype` child, if this entry is `function type name(...)`.
    pub fn function_prototype(&self) -> Option<FunctionPrototype> {
        support::child(&self.syntax)
    }

    /// The TF name token, resolved structurally from either bare form or prototype.
    pub fn name(&self) -> Option<SyntaxToken> {
        if let Some(proto) = self.task_prototype() {
            return proto.name();
        }
        if let Some(proto) = self.function_prototype() {
            return proto.name();
        }
        self.bare_name_token()
    }

    /// Whether this entry has a task/function prototype form.
    pub fn has_prototype(&self) -> bool {
        self.task_prototype().is_some() || self.function_prototype().is_some()
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
    pub fn ident_name_span(&self) -> Option<lyra_source::DeclSpan> {
        self.name()
            .map(|t| lyra_source::DeclSpan::new(t.text_range()))
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

impl VarDecl {
    /// Whether this `VarDecl` is a non-ANSI port direction declaration
    /// (e.g. `input a;`, `input [7:0] a;`, `input signed [7:0] a, b;`).
    ///
    /// These have a direction keyword but no explicit base type -- the
    /// `TypeSpec`, if present, contains only signing and/or packed dimensions
    /// (an implicit data type per LRM 6.7.1).
    pub fn is_non_ansi_port_decl(&self) -> bool {
        if self.direction_token().is_none() {
            return false;
        }
        let Some(ts) = self.type_spec() else {
            return true;
        };
        ts.type_keyword().is_none() && ts.type_name_ref().is_none()
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
