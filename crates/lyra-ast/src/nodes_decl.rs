// Custom accessors for package, import, export, function, and task declarations.
//
// Split from nodes.rs to stay under the 1200-line hard limit.

use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxToken;

use crate::node::StmtNode;
use crate::nodes::{
    ExportItem, FunctionDecl, ImportDecl, ImportItem, NetDecl, PackageBody, PackageDecl, ParamDecl,
    QualifiedName, TaskDecl, TfPortDecl, VarDecl,
};
use crate::support::{self, AstChildren};

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

    pub fn statements(&self) -> AstChildren<StmtNode> {
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

    pub fn statements(&self) -> AstChildren<StmtNode> {
        support::children(&self.syntax)
    }
}
