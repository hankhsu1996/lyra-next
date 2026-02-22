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
    StreamExpr(SyntaxKind::StreamExpr) {}
    StreamSliceSize(SyntaxKind::StreamSliceSize) {}
    StreamOperands(SyntaxKind::StreamOperands) {}
    IndexExpr(SyntaxKind::IndexExpr) {}
    RangeExpr(SyntaxKind::RangeExpr) {}
    FieldExpr(SyntaxKind::FieldExpr) { @custom }

    CallExpr(SyntaxKind::CallExpr) {
        arg_list: ArgList,
    }

    ArgList(SyntaxKind::ArgList) { @custom }

    SystemTfCall(SyntaxKind::SystemTfCall) {
        arg_list: SystemTfArgList,
    }

    SystemTfArgList(SyntaxKind::SystemTfArgList) {}

    NameRef(SyntaxKind::NameRef) {
        ident: token([Ident, EscapedIdent]),
    }

    Literal(SyntaxKind::Literal) {
        token: token([
            IntLiteral, RealLiteral, BasedLiteral,
            UnbasedUnsizedLiteral, StringLiteral,
        ]),
    }

    ErrorNode(SyntaxKind::ErrorNode) {}

    Declarator(SyntaxKind::Declarator) {
        name: token([Ident, EscapedIdent]),
    }

    ModuleInstantiation(SyntaxKind::ModuleInstantiation) {
        module_name: token([Ident, EscapedIdent]),
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

    ModportDecl(SyntaxKind::ModportDecl) {
        items: [ModportItem],
    }

    ModportItem(SyntaxKind::ModportItem) {
        name: token([Ident, EscapedIdent]),
        ports: [ModportPort],
    }

    ModportPort(SyntaxKind::ModportPort) {
        direction: token([InputKw, OutputKw, InoutKw, RefKw]),
        name: token([Ident, EscapedIdent]),
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
    /// The base expression before the dot.
    pub fn base_expr(&self) -> Option<lyra_parser::SyntaxNode> {
        self.syntax
            .children()
            .find(|c| crate::node::is_expression_kind(c.kind()))
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

impl ArgList {
    /// Iterate over argument expression nodes.
    pub fn args(&self) -> impl Iterator<Item = lyra_parser::SyntaxNode> + '_ {
        self.syntax
            .children()
            .filter(|c| crate::node::is_expression_kind(c.kind()))
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

    pub fn is_tagged(&self) -> bool {
        support::token(&self.syntax, SyntaxKind::TaggedKw).is_some()
    }

    pub fn is_soft(&self) -> bool {
        support::token(&self.syntax, SyntaxKind::SoftKw).is_some()
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
        support::token_in(&self.syntax, &[SyntaxKind::Ident, SyntaxKind::EscapedIdent])
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
        support::token_in(&self.syntax, &[SyntaxKind::Ident, SyntaxKind::EscapedIdent])
    }

    pub fn is_wildcard(&self) -> bool {
        support::token(&self.syntax, SyntaxKind::Star).is_some()
    }
}

impl ExportItem {
    /// Returns the package name token (first Ident), or None for *::*
    pub fn package_name(&self) -> Option<SyntaxToken> {
        support::token_in(&self.syntax, &[SyntaxKind::Ident, SyntaxKind::EscapedIdent])
    }

    /// True if this item contains a wildcard (* after ::)
    pub fn is_wildcard(&self) -> bool {
        support::token(&self.syntax, SyntaxKind::Star).is_some()
    }

    /// True if this is *::* (re-export all)
    pub fn is_all_wildcard(&self) -> bool {
        let stars: Vec<_> = self
            .syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .filter(|tok| tok.kind() == SyntaxKind::Star)
            .collect();
        stars.len() >= 2
    }

    /// Returns the member name token (Ident after ::), or None for wildcards
    pub fn member_name(&self) -> Option<SyntaxToken> {
        let mut past_colon_colon = false;
        for el in self.syntax.children_with_tokens() {
            if let Some(tok) = el.into_token() {
                if tok.kind() == SyntaxKind::ColonColon {
                    past_colon_colon = true;
                } else if past_colon_colon
                    && matches!(tok.kind(), SyntaxKind::Ident | SyntaxKind::EscapedIdent)
                {
                    return Some(tok);
                }
            }
        }
        None
    }
}

impl QualifiedName {
    pub fn segments(&self) -> impl Iterator<Item = SyntaxToken> + '_ {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .filter(|tok| matches!(tok.kind(), SyntaxKind::Ident | SyntaxKind::EscapedIdent))
    }
}

impl FunctionDecl {
    pub fn name(&self) -> Option<SyntaxToken> {
        // The function name is the Ident that follows either:
        //   (a) the TypeSpec child (explicit return type), or
        //   (b) the `function` keyword + optional lifetime (implicit return type).
        // In case (b), no TypeSpec exists, so we find the first Ident after
        // skipping `function` and optional `automatic`/`static`.
        let has_type_spec = self
            .syntax
            .children()
            .any(|n| n.kind() == SyntaxKind::TypeSpec);

        if has_type_spec {
            let mut past_type_spec = false;
            for el in self.syntax.children_with_tokens() {
                match el {
                    rowan::NodeOrToken::Node(ref n) if n.kind() == SyntaxKind::TypeSpec => {
                        past_type_spec = true;
                    }
                    rowan::NodeOrToken::Token(tok) => {
                        if past_type_spec
                            && matches!(tok.kind(), SyntaxKind::Ident | SyntaxKind::EscapedIdent)
                        {
                            return Some(tok);
                        }
                    }
                    rowan::NodeOrToken::Node(_) => {}
                }
            }
            None
        } else {
            // No TypeSpec -- first Ident after FunctionKw and optional lifetime
            self.syntax
                .children_with_tokens()
                .filter_map(|el| el.into_token())
                .find(|tok| matches!(tok.kind(), SyntaxKind::Ident | SyntaxKind::EscapedIdent))
        }
    }

    pub fn tf_port_decls(&self) -> AstChildren<TfPortDecl> {
        support::children(&self.syntax)
    }
}

impl TaskDecl {
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token_in(&self.syntax, &[SyntaxKind::Ident, SyntaxKind::EscapedIdent])
    }

    pub fn tf_port_decls(&self) -> AstChildren<TfPortDecl> {
        support::children(&self.syntax)
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
}

impl ModuleInstantiation {
    /// The parameter override list `#(...)` if present.
    pub fn param_overrides(&self) -> Option<ParamPortList> {
        support::child(&self.syntax)
    }

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
                    if matches!(tok.kind(), SyntaxKind::Ident | SyntaxKind::EscapedIdent) {
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
