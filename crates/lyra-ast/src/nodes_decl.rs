// Custom accessors for package, import, export, function, and task declarations.
//
// Split from nodes.rs to stay under the 1200-line hard limit.

use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxToken;

use crate::node::StmtNode;
use crate::nodes::{
    ExportItem, FunctionDecl, ImportDecl, ImportItem, InterfaceBody, InterfaceDecl, ModuleDecl,
    NetDecl, NettypeDecl, PackageBody, PackageDecl, ParamDecl, ProgramBody, ProgramDecl,
    QualifiedName, TaskDecl, TfPortDecl, TimeprecisionDecl, TimeunitDecl, VarDecl,
};
use crate::support::{self, AstChildren};
use crate::type_spec::TypeSpecKeyword;

/// Find the `automatic` or `static` lifetime token immediately after
/// the declaration keyword `kw` in a container or callable header.
fn decl_lifetime_token(syntax: &lyra_parser::SyntaxNode, kw: SyntaxKind) -> Option<SyntaxToken> {
    let mut past_kw = false;
    for el in syntax.children_with_tokens() {
        if let Some(tok) = el.into_token() {
            let k = tok.kind();
            if k == kw {
                past_kw = true;
                continue;
            }
            if !past_kw || k.is_trivia() {
                continue;
            }
            if matches!(k, SyntaxKind::AutomaticKw | SyntaxKind::StaticKw) {
                return Some(tok);
            }
            return None;
        }
    }
    None
}

impl ModuleDecl {
    pub fn lifetime_token(&self) -> Option<SyntaxToken> {
        decl_lifetime_token(&self.syntax, SyntaxKind::ModuleKw)
    }
}

impl InterfaceDecl {
    pub fn lifetime_token(&self) -> Option<SyntaxToken> {
        decl_lifetime_token(&self.syntax, SyntaxKind::InterfaceKw)
    }
}

impl ProgramDecl {
    pub fn lifetime_token(&self) -> Option<SyntaxToken> {
        decl_lifetime_token(&self.syntax, SyntaxKind::ProgramKw)
    }
}

impl PackageDecl {
    pub fn lifetime_token(&self) -> Option<SyntaxToken> {
        decl_lifetime_token(&self.syntax, SyntaxKind::PackageKw)
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        support::token_in(&self.syntax, &[SyntaxKind::Ident, SyntaxKind::EscapedIdent])
    }

    pub fn body(&self) -> Option<PackageBody> {
        support::child(&self.syntax)
    }
}

impl PackageBody {
    pub fn net_decls(&self) -> AstChildren<NetDecl> {
        support::children(&self.syntax)
    }

    pub fn param_decls(&self) -> AstChildren<ParamDecl> {
        support::children(&self.syntax)
    }

    pub fn import_decls(&self) -> AstChildren<ImportDecl> {
        support::children(&self.syntax)
    }

    pub fn var_decls(&self) -> AstChildren<VarDecl> {
        support::children(&self.syntax)
    }

    pub fn function_decls(&self) -> AstChildren<FunctionDecl> {
        support::children(&self.syntax)
    }

    pub fn task_decls(&self) -> AstChildren<TaskDecl> {
        support::children(&self.syntax)
    }
}

impl InterfaceBody {
    pub fn net_decls(&self) -> AstChildren<NetDecl> {
        support::children(&self.syntax)
    }
}

impl ProgramBody {
    pub fn net_decls(&self) -> AstChildren<NetDecl> {
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
    pub fn lifetime_token(&self) -> Option<SyntaxToken> {
        decl_lifetime_token(&self.syntax, SyntaxKind::FunctionKw)
    }

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
    pub fn lifetime_token(&self) -> Option<SyntaxToken> {
        decl_lifetime_token(&self.syntax, SyntaxKind::TaskKw)
    }

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

/// Discriminator for the leading keyword of a net declaration.
///
/// Separates `interconnect` (LRM 6.6.8) from traditional net-type keywords
/// (`wire`, `tri`, etc.) so downstream consumers can apply kind-specific
/// rules without re-inspecting raw tokens.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NetDeclKind {
    /// `interconnect` declaration (LRM 6.6.8).
    Interconnect,
    /// Traditional net-type keyword (`wire`, `tri`, etc.).
    NetType(TypeSpecKeyword),
    /// CST is malformed -- no recognizable net keyword found.
    Unknown,
}

impl NetDecl {
    /// Classify the net declaration by its leading keyword token.
    ///
    /// The keyword lives inside the `TypeSpec` child (first child node).
    /// This walks one level deep to find it rather than searching all
    /// descendants.
    pub fn kind(&self) -> NetDeclKind {
        let first_child = self.syntax.first_child();
        let kw_kind = first_child
            .as_ref()
            .into_iter()
            .flat_map(|n| n.children_with_tokens())
            .filter_map(rowan::NodeOrToken::into_token)
            .find(|tok| !tok.kind().is_trivia())
            .map(|tok| tok.kind());
        match kw_kind {
            Some(SyntaxKind::InterconnectKw) => NetDeclKind::Interconnect,
            Some(k) if k.is_net_type_kw() => TypeSpecKeyword::from_token_kind(k)
                .map_or(NetDeclKind::Unknown, NetDeclKind::NetType),
            _ => NetDeclKind::Unknown,
        }
    }
}

impl NettypeDecl {
    /// The optional resolve function name token after the `with` keyword.
    pub fn resolve_fn_token(&self) -> Option<SyntaxToken> {
        let mut saw_with = false;
        for elem in self.syntax.children_with_tokens() {
            if let Some(tok) = elem.as_token() {
                if tok.kind() == SyntaxKind::WithKw {
                    saw_with = true;
                } else if saw_with
                    && matches!(tok.kind(), SyntaxKind::Ident | SyntaxKind::EscapedIdent)
                {
                    return Some(tok.clone());
                }
            }
        }
        None
    }
}

impl TimeunitDecl {
    /// The unit time literal token (always present if well-formed).
    pub fn unit_literal_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, SyntaxKind::TimeLiteral)
    }

    /// The precision time literal token after `/`, if present.
    pub fn precision_literal_token(&self) -> Option<SyntaxToken> {
        let mut saw_slash = false;
        for el in self.syntax.children_with_tokens() {
            if let Some(tok) = el.into_token() {
                if tok.kind() == SyntaxKind::Slash {
                    saw_slash = true;
                } else if saw_slash && tok.kind() == SyntaxKind::TimeLiteral {
                    return Some(tok);
                }
            }
        }
        None
    }
}

impl TimeprecisionDecl {
    /// The precision time literal token.
    pub fn precision_literal_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, SyntaxKind::TimeLiteral)
    }
}
