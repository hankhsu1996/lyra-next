use lyra_lexer::SyntaxKind;

use crate::node::ast_node;
use crate::support::{self, AstChildren};
use crate::tokens::{Ident, Keyword, LiteralToken};

// Root
ast_node!(SourceFile, SyntaxKind::SourceFile);
// Declarations
ast_node!(ModuleDecl, SyntaxKind::ModuleDecl);
ast_node!(PortList, SyntaxKind::PortList);
ast_node!(Port, SyntaxKind::Port);
ast_node!(ModuleBody, SyntaxKind::ModuleBody);
ast_node!(ContinuousAssign, SyntaxKind::ContinuousAssign);
ast_node!(Expression, SyntaxKind::Expression);
ast_node!(ParamDecl, SyntaxKind::ParamDecl);
ast_node!(ParamPortList, SyntaxKind::ParamPortList);
ast_node!(AlwaysBlock, SyntaxKind::AlwaysBlock);
ast_node!(InitialBlock, SyntaxKind::InitialBlock);
ast_node!(ModuleInstantiation, SyntaxKind::ModuleInstantiation);
ast_node!(InstancePortList, SyntaxKind::InstancePortList);
ast_node!(InstancePort, SyntaxKind::InstancePort);
ast_node!(NetDecl, SyntaxKind::NetDecl);
ast_node!(VarDecl, SyntaxKind::VarDecl);
ast_node!(TypeSpec, SyntaxKind::TypeSpec);
ast_node!(PackedDimension, SyntaxKind::PackedDimension);
ast_node!(UnpackedDimension, SyntaxKind::UnpackedDimension);
// Statements
ast_node!(BlockStmt, SyntaxKind::BlockStmt);
ast_node!(IfStmt, SyntaxKind::IfStmt);
ast_node!(CaseStmt, SyntaxKind::CaseStmt);
ast_node!(CaseItem, SyntaxKind::CaseItem);
ast_node!(ForStmt, SyntaxKind::ForStmt);
ast_node!(WhileStmt, SyntaxKind::WhileStmt);
ast_node!(RepeatStmt, SyntaxKind::RepeatStmt);
ast_node!(ForeverStmt, SyntaxKind::ForeverStmt);
ast_node!(AssignStmt, SyntaxKind::AssignStmt);
ast_node!(TimingControl, SyntaxKind::TimingControl);
ast_node!(EventExpr, SyntaxKind::EventExpr);
ast_node!(EventItem, SyntaxKind::EventItem);
// Expressions
ast_node!(BinExpr, SyntaxKind::BinExpr);
ast_node!(PrefixExpr, SyntaxKind::PrefixExpr);
ast_node!(ParenExpr, SyntaxKind::ParenExpr);
ast_node!(CondExpr, SyntaxKind::CondExpr);
ast_node!(ConcatExpr, SyntaxKind::ConcatExpr);
ast_node!(ReplicExpr, SyntaxKind::ReplicExpr);
ast_node!(IndexExpr, SyntaxKind::IndexExpr);
ast_node!(RangeExpr, SyntaxKind::RangeExpr);
ast_node!(FieldExpr, SyntaxKind::FieldExpr);
ast_node!(CallExpr, SyntaxKind::CallExpr);
ast_node!(ArgList, SyntaxKind::ArgList);
ast_node!(NameRef, SyntaxKind::NameRef);
ast_node!(Literal, SyntaxKind::Literal);
ast_node!(ErrorNode, SyntaxKind::ErrorNode);

// Accessors

impl SourceFile {
    pub fn modules(&self) -> AstChildren<ModuleDecl> {
        support::children(&self.syntax)
    }
}

impl ModuleDecl {
    pub fn name(&self) -> Option<Ident> {
        support::ident(&self.syntax)
    }

    pub fn param_port_list(&self) -> Option<ParamPortList> {
        support::child(&self.syntax)
    }

    pub fn port_list(&self) -> Option<PortList> {
        support::child(&self.syntax)
    }

    pub fn body(&self) -> Option<ModuleBody> {
        support::child(&self.syntax)
    }
}

impl ModuleBody {
    pub fn always_blocks(&self) -> AstChildren<AlwaysBlock> {
        support::children(&self.syntax)
    }

    pub fn initial_blocks(&self) -> AstChildren<InitialBlock> {
        support::children(&self.syntax)
    }

    pub fn continuous_assigns(&self) -> AstChildren<ContinuousAssign> {
        support::children(&self.syntax)
    }

    pub fn var_decls(&self) -> AstChildren<VarDecl> {
        support::children(&self.syntax)
    }

    pub fn net_decls(&self) -> AstChildren<NetDecl> {
        support::children(&self.syntax)
    }

    pub fn param_decls(&self) -> AstChildren<ParamDecl> {
        support::children(&self.syntax)
    }

    pub fn module_instantiations(&self) -> AstChildren<ModuleInstantiation> {
        support::children(&self.syntax)
    }
}

impl PortList {
    pub fn ports(&self) -> AstChildren<Port> {
        support::children(&self.syntax)
    }
}

impl Port {
    pub fn name(&self) -> Option<Ident> {
        support::ident(&self.syntax)
    }

    pub fn type_spec(&self) -> Option<TypeSpec> {
        support::child(&self.syntax)
    }

    pub fn direction(&self) -> Option<Keyword> {
        support::keyword(
            &self.syntax,
            &[
                SyntaxKind::InputKw,
                SyntaxKind::OutputKw,
                SyntaxKind::InoutKw,
                SyntaxKind::RefKw,
            ],
        )
    }
}

impl ParamPortList {
    pub fn params(&self) -> AstChildren<ParamDecl> {
        support::children(&self.syntax)
    }
}

impl ParamDecl {
    pub fn name(&self) -> Option<Ident> {
        support::ident(&self.syntax)
    }

    pub fn type_spec(&self) -> Option<TypeSpec> {
        support::child(&self.syntax)
    }
}

impl AlwaysBlock {
    pub fn keyword(&self) -> Option<Keyword> {
        support::keyword(
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
}

impl InitialBlock {
    pub fn block_stmt(&self) -> Option<BlockStmt> {
        support::child(&self.syntax)
    }
}

impl VarDecl {
    pub fn type_spec(&self) -> Option<TypeSpec> {
        support::child(&self.syntax)
    }
}

impl NetDecl {
    pub fn net_type(&self) -> Option<Keyword> {
        support::keyword(
            &self.syntax,
            &[
                SyntaxKind::WireKw,
                SyntaxKind::TriKw,
                SyntaxKind::WandKw,
                SyntaxKind::WorKw,
                SyntaxKind::Tri0Kw,
                SyntaxKind::Tri1Kw,
                SyntaxKind::TriregKw,
                SyntaxKind::Supply0Kw,
                SyntaxKind::Supply1Kw,
                SyntaxKind::UwireKw,
            ],
        )
    }
}

impl TypeSpec {
    pub fn keyword(&self) -> Option<Keyword> {
        // First token that is a keyword or ident
        self.syntax
            .children_with_tokens()
            .find_map(rowan::NodeOrToken::into_token)
            .map(|t| Keyword { token: t })
    }

    pub fn packed_dimensions(&self) -> AstChildren<PackedDimension> {
        support::children(&self.syntax)
    }
}

impl CaseStmt {
    pub fn items(&self) -> AstChildren<CaseItem> {
        support::children(&self.syntax)
    }
}

impl BinExpr {
    pub fn op_token(&self) -> Option<Keyword> {
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
                        return Some(Keyword { token: tok });
                    }
                }
            }
        }
        None
    }
}

impl PrefixExpr {
    pub fn op_token(&self) -> Option<Keyword> {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .find(|tok| tok.kind() != SyntaxKind::Whitespace)
            .map(|t| Keyword { token: t })
    }
}

impl NameRef {
    pub fn ident(&self) -> Option<Ident> {
        support::ident(&self.syntax)
    }
}

impl Literal {
    pub fn token(&self) -> Option<LiteralToken> {
        support::literal_token(&self.syntax)
    }
}

impl ModuleInstantiation {
    pub fn module_name(&self) -> Option<Ident> {
        support::ident(&self.syntax)
    }

    pub fn port_list(&self) -> Option<InstancePortList> {
        support::child(&self.syntax)
    }
}

impl InstancePortList {
    pub fn ports(&self) -> AstChildren<InstancePort> {
        support::children(&self.syntax)
    }
}

impl CallExpr {
    pub fn arg_list(&self) -> Option<ArgList> {
        support::child(&self.syntax)
    }
}

impl FieldExpr {
    pub fn field_name(&self) -> Option<Ident> {
        // The field name is the last Ident token
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .filter(|tok| tok.kind() == SyntaxKind::Ident)
            .last()
            .map(|t| Ident { token: t })
    }
}
