use std::hash::{Hash, Hasher};
use std::sync::Arc;

use lyra_lexer::Token;
use smol_str::SmolStr;

/// A single token stored in a macro body, pairing the original lexer
/// `Token` with its exact text spelling.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroTok {
    pub token: Token,
    pub text: SmolStr,
}

impl Hash for MacroTok {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.token.kind.hash(state);
        self.token.len.hash(state);
        self.text.hash(state);
    }
}

/// A deterministic sequence of tokens forming a macro body.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MacroTokenSeq {
    tokens: Vec<MacroTok>,
}

impl MacroTokenSeq {
    /// Wrap a pre-built vec of tokens (used by `handle_define`).
    pub fn from_vec(toks: Vec<MacroTok>) -> Self {
        Self { tokens: toks }
    }

    /// Iterate over tokens for expansion.
    pub fn tokens(&self) -> &[MacroTok] {
        &self.tokens
    }

    /// Concatenate all token texts. Debug/test use only -- allocates a
    /// new `String` on every call.
    pub fn text(&self) -> String {
        let mut s = String::new();
        for t in &self.tokens {
            s.push_str(&t.text);
        }
        s
    }

    /// Whether the token sequence is empty.
    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    /// Lex text and collect into a `MacroTokenSeq` (test-only convenience).
    #[cfg(test)]
    pub fn from_text(text: &str) -> Self {
        use lyra_lexer::SyntaxKind;
        let tokens = lyra_lexer::lex(text);
        let mut result = Vec::new();
        let mut cursor = 0usize;
        for t in &tokens {
            if t.kind == SyntaxKind::Eof {
                break;
            }
            let len: usize = t.len.into();
            result.push(MacroTok {
                token: *t,
                text: SmolStr::from(&text[cursor..cursor + len]),
            });
            cursor += len;
        }
        Self { tokens: result }
    }
}

/// Value of a defined macro.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MacroValue {
    /// Defined with no value (`` `define FOO ``).
    Flag,
    /// Object-like macro with a token body (`` `define FOO 42 ``).
    /// Wrapped in `Arc` so expansion clones cheaply.
    ObjectLike(Arc<MacroTokenSeq>),
}

/// A single macro definition (name + value).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MacroDef {
    pub name: SmolStr,
    pub value: MacroValue,
}

/// Immutable-ish macro environment, kept sorted by name for
/// deterministic `Hash`/`Eq` and Salsa compatibility.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MacroEnv {
    entries: Vec<MacroDef>,
}

impl MacroEnv {
    /// An empty environment with no definitions.
    pub fn empty() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    /// Whether `name` is currently defined.
    pub fn is_defined(&self, name: &str) -> bool {
        self.entries
            .binary_search_by(|d| d.name.as_str().cmp(name))
            .is_ok()
    }

    /// Look up a macro definition by name.
    pub fn get(&self, name: &str) -> Option<&MacroDef> {
        self.entries
            .binary_search_by(|d| d.name.as_str().cmp(name))
            .ok()
            .map(|idx| &self.entries[idx])
    }

    /// Define (or redefine) a macro. Maintains sorted order.
    pub fn define(&mut self, name: SmolStr, value: MacroValue) {
        match self
            .entries
            .binary_search_by(|d| d.name.as_str().cmp(name.as_str()))
        {
            Ok(idx) => {
                self.entries[idx].value = value;
            }
            Err(idx) => {
                self.entries.insert(idx, MacroDef { name, value });
            }
        }
    }

    /// Remove a macro definition. Silent if not defined.
    pub fn undef(&mut self, name: &str) {
        if let Ok(idx) = self.entries.binary_search_by(|d| d.name.as_str().cmp(name)) {
            self.entries.remove(idx);
        }
    }

    /// Number of defined macros.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Whether the environment is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_env() {
        let env = MacroEnv::empty();
        assert!(env.is_empty());
        assert!(!env.is_defined("FOO"));
        assert!(env.get("FOO").is_none());
    }

    #[test]
    fn define_flag() {
        let mut env = MacroEnv::empty();
        env.define(SmolStr::new("FOO"), MacroValue::Flag);
        assert!(env.is_defined("FOO"));
        assert_eq!(env.get("FOO").map(|d| &d.value), Some(&MacroValue::Flag));
    }

    #[test]
    fn define_object_like() {
        let mut env = MacroEnv::empty();
        env.define(
            SmolStr::new("WIDTH"),
            MacroValue::ObjectLike(Arc::new(MacroTokenSeq::from_text("8"))),
        );
        assert!(env.is_defined("WIDTH"));
        match env.get("WIDTH").map(|d| &d.value) {
            Some(MacroValue::ObjectLike(seq)) => assert_eq!(seq.text(), "8"),
            other => panic!("expected ObjectLike, got {other:?}"),
        }
    }

    #[test]
    fn redefine() {
        let mut env = MacroEnv::empty();
        env.define(SmolStr::new("X"), MacroValue::Flag);
        env.define(
            SmolStr::new("X"),
            MacroValue::ObjectLike(Arc::new(MacroTokenSeq::from_text("1"))),
        );
        assert_eq!(env.len(), 1);
        match env.get("X").map(|d| &d.value) {
            Some(MacroValue::ObjectLike(seq)) => assert_eq!(seq.text(), "1"),
            other => panic!("expected ObjectLike, got {other:?}"),
        }
    }

    #[test]
    fn undef_existing() {
        let mut env = MacroEnv::empty();
        env.define(SmolStr::new("A"), MacroValue::Flag);
        env.undef("A");
        assert!(!env.is_defined("A"));
        assert!(env.is_empty());
    }

    #[test]
    fn undef_nonexistent_is_silent() {
        let mut env = MacroEnv::empty();
        env.undef("NOPE");
        assert!(env.is_empty());
    }

    #[test]
    fn sorted_order() {
        let mut env = MacroEnv::empty();
        env.define(SmolStr::new("C"), MacroValue::Flag);
        env.define(SmolStr::new("A"), MacroValue::Flag);
        env.define(SmolStr::new("B"), MacroValue::Flag);
        assert_eq!(env.len(), 3);
        assert!(env.is_defined("A"));
        assert!(env.is_defined("B"));
        assert!(env.is_defined("C"));
    }
}
