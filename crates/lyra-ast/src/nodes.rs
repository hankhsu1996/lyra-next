use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxToken;

use crate::node::ast_nodes;
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
