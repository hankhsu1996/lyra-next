use smol_str::SmolStr;

/// Value of a defined macro.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MacroValue {
    /// Defined with no value (`` `define FOO ``).
    Flag,
    /// Object-like macro with a text body (`` `define FOO 42 ``).
    ObjectLike(SmolStr),
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
            MacroValue::ObjectLike(SmolStr::new("8")),
        );
        assert!(env.is_defined("WIDTH"));
        assert_eq!(
            env.get("WIDTH").map(|d| &d.value),
            Some(&MacroValue::ObjectLike(SmolStr::new("8")))
        );
    }

    #[test]
    fn redefine() {
        let mut env = MacroEnv::empty();
        env.define(SmolStr::new("X"), MacroValue::Flag);
        env.define(SmolStr::new("X"), MacroValue::ObjectLike(SmolStr::new("1")));
        assert_eq!(env.len(), 1);
        assert_eq!(
            env.get("X").map(|d| &d.value),
            Some(&MacroValue::ObjectLike(SmolStr::new("1")))
        );
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
