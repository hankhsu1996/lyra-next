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
        name: token(Ident),
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
        name: token(Ident),
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

    AlwaysBlock(SyntaxKind::AlwaysBlock) {
        keyword: token([AlwaysKw, AlwaysCombKw, AlwaysFfKw, AlwaysLatchKw]),
        block_stmt: BlockStmt,
    }

    InitialBlock(SyntaxKind::InitialBlock) {
        block_stmt: BlockStmt,
    }

    VarDecl(SyntaxKind::VarDecl) {
        type_spec: TypeSpec,
        declarators: [Declarator],
    }

    NetDecl(SyntaxKind::NetDecl) {
        net_type: token([
            WireKw, TriKw, WandKw, WorKw, Tri0Kw,
            Tri1Kw, TriregKw, Supply0Kw, Supply1Kw, UwireKw,
        ]),
        declarators: [Declarator],
    }

    ContinuousAssign(SyntaxKind::ContinuousAssign) {}

    PackedDimension(SyntaxKind::PackedDimension) {}
    UnpackedDimension(SyntaxKind::UnpackedDimension) {}

    // Statements
    BlockStmt(SyntaxKind::BlockStmt) {}
    IfStmt(SyntaxKind::IfStmt) {}

    CaseStmt(SyntaxKind::CaseStmt) {
        items: [CaseItem],
    }

    CaseItem(SyntaxKind::CaseItem) {}
    ForStmt(SyntaxKind::ForStmt) {}
    WhileStmt(SyntaxKind::WhileStmt) {}
    RepeatStmt(SyntaxKind::RepeatStmt) {}
    ForeverStmt(SyntaxKind::ForeverStmt) {}
    AssignStmt(SyntaxKind::AssignStmt) {}
    TimingControl(SyntaxKind::TimingControl) {}
    EventExpr(SyntaxKind::EventExpr) {}
    EventItem(SyntaxKind::EventItem) {}

    // Expressions
    Expression(SyntaxKind::Expression) {}
    BinExpr(SyntaxKind::BinExpr) { @custom }
    PrefixExpr(SyntaxKind::PrefixExpr) { @custom }
    ParenExpr(SyntaxKind::ParenExpr) {}
    CondExpr(SyntaxKind::CondExpr) {}
    ConcatExpr(SyntaxKind::ConcatExpr) {}
    ReplicExpr(SyntaxKind::ReplicExpr) {}
    IndexExpr(SyntaxKind::IndexExpr) {}
    RangeExpr(SyntaxKind::RangeExpr) {}
    FieldExpr(SyntaxKind::FieldExpr) { @custom }

    CallExpr(SyntaxKind::CallExpr) {
        arg_list: ArgList,
    }

    ArgList(SyntaxKind::ArgList) {}

    NameRef(SyntaxKind::NameRef) {
        ident: token(Ident),
    }

    Literal(SyntaxKind::Literal) {
        token: token([
            IntLiteral, RealLiteral, BasedLiteral,
            UnbasedUnsizedLiteral, StringLiteral,
        ]),
    }

    ErrorNode(SyntaxKind::ErrorNode) {}

    Declarator(SyntaxKind::Declarator) {
        name: token(Ident),
    }

    ModuleInstantiation(SyntaxKind::ModuleInstantiation) {
        module_name: token(Ident),
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
        name: token(Ident),
    }

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
    QualifiedName(SyntaxKind::QualifiedName) { @custom }

    TypedefDecl(SyntaxKind::TypedefDecl) {
        name: token(Ident),
        type_spec: TypeSpec,
    }

    InterfaceDecl(SyntaxKind::InterfaceDecl) {
        name: token(Ident),
        param_port_list: ParamPortList,
        port_list: PortList,
        body: InterfaceBody,
    }

    InterfaceBody(SyntaxKind::InterfaceBody) {}

    ProgramDecl(SyntaxKind::ProgramDecl) {
        name: token(Ident),
        param_port_list: ParamPortList,
        port_list: PortList,
        body: ProgramBody,
    }

    ProgramBody(SyntaxKind::ProgramBody) {}

    PrimitiveDecl(SyntaxKind::PrimitiveDecl) {
        name: token(Ident),
    }

    ConfigDecl(SyntaxKind::ConfigDecl) {
        name: token(Ident),
    }
}

// Custom accessors

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
}

impl PrefixExpr {
    pub fn op_token(&self) -> Option<SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .find(|tok| tok.kind() != SyntaxKind::Whitespace)
    }
}

impl FieldExpr {
    pub fn field_name(&self) -> Option<SyntaxToken> {
        // The field name is the last Ident token
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .filter(|tok| tok.kind() == SyntaxKind::Ident)
            .last()
    }
}

impl EnumType {
    pub fn base_type_spec(&self) -> Option<TypeSpec> {
        support::child(&self.syntax)
    }
}

impl StructType {
    pub fn is_packed(&self) -> bool {
        support::token(&self.syntax, SyntaxKind::PackedKw).is_some()
    }

    pub fn is_union(&self) -> bool {
        support::token(&self.syntax, SyntaxKind::UnionKw).is_some()
    }
}

impl TypeSpec {
    pub fn keyword(&self) -> Option<SyntaxToken> {
        // First token that is a keyword or ident
        self.syntax
            .children_with_tokens()
            .find_map(rowan::NodeOrToken::into_token)
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
}

impl PackageDecl {
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, SyntaxKind::Ident)
    }

    pub fn body(&self) -> Option<PackageBody> {
        support::child(&self.syntax)
    }
}

impl PackageBody {
    pub fn var_decls(&self) -> AstChildren<VarDecl> {
        support::children(&self.syntax)
    }

    pub fn net_decls(&self) -> AstChildren<NetDecl> {
        support::children(&self.syntax)
    }

    pub fn param_decls(&self) -> AstChildren<ParamDecl> {
        support::children(&self.syntax)
    }

    pub fn import_decls(&self) -> AstChildren<ImportDecl> {
        support::children(&self.syntax)
    }
}

impl ImportDecl {
    pub fn items(&self) -> AstChildren<ImportItem> {
        support::children(&self.syntax)
    }
}

impl ImportItem {
    pub fn qualified_name(&self) -> Option<QualifiedName> {
        support::child(&self.syntax)
    }

    pub fn package_name(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, SyntaxKind::Ident)
    }

    pub fn is_wildcard(&self) -> bool {
        support::token(&self.syntax, SyntaxKind::Star).is_some()
    }
}

impl QualifiedName {
    pub fn segments(&self) -> impl Iterator<Item = SyntaxToken> + '_ {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .filter(|tok| tok.kind() == SyntaxKind::Ident)
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
                } else if saw_dot && tok.kind() == SyntaxKind::Ident {
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
}

impl ModuleInstantiation {
    /// Iterate over all instance entries in this statement.
    ///
    /// A single `ModuleInstantiation` statement can declare multiple instances:
    /// `adder u1(.a(x)), u2(.a(y));`
    ///
    /// Each entry yields the instance name token and optional port list.
    pub fn instances(&self) -> impl Iterator<Item = (SyntaxToken, Option<InstancePortList>)> + '_ {
        InstanceIter {
            children: self.syntax.children_with_tokens(),
            past_module_name: false,
        }
    }
}

struct InstanceIter<I> {
    children: I,
    past_module_name: bool,
}

impl<I: Iterator<Item = rowan::NodeOrToken<lyra_parser::SyntaxNode, SyntaxToken>>> Iterator
    for InstanceIter<I>
{
    type Item = (SyntaxToken, Option<InstancePortList>);

    fn next(&mut self) -> Option<Self::Item> {
        let mut name_token: Option<SyntaxToken> = None;
        for el in self.children.by_ref() {
            match el {
                rowan::NodeOrToken::Token(tok) => {
                    if tok.kind() == SyntaxKind::Ident {
                        if self.past_module_name {
                            name_token = Some(tok);
                        } else {
                            // First ident is the module type name -- skip it
                            self.past_module_name = true;
                        }
                    }
                }
                rowan::NodeOrToken::Node(node) => {
                    if node.kind() == SyntaxKind::InstancePortList
                        && let Some(name) = name_token.take()
                    {
                        let port_list = InstancePortList::cast(node);
                        return Some((name, port_list));
                    }
                }
            }
        }
        // Instance without port list
        if let Some(name) = name_token {
            return Some((name, None));
        }
        None
    }
}
