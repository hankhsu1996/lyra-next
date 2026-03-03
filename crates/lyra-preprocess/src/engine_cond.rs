//! Self-contained conditional compilation state.
//!
//! Owns the `ifdef`/`ifndef`/`elsif`/`else`/`endif` stack and
//! exposes narrow mutation methods. The `Preprocessor` drives
//! directive parsing and error reporting; `CondState` only
//! manipulates the stack.

/// Errors from `apply_elsif`.
pub(crate) enum ElsifError {
    /// No matching `ifdef`/`ifndef` on the stack.
    NoMatchingIfdef,
    /// `elsif` appeared after `else`.
    ElsifAfterElse,
}

/// Errors from `apply_else`.
pub(crate) enum ElseError {
    /// No matching `ifdef`/`ifndef` on the stack.
    NoMatchingIfdef,
    /// Duplicate `else` in the same conditional block.
    DuplicateElse,
}

struct CondFrame {
    allow_emit: bool,
    taken: bool,
    saw_else: bool,
}

pub(crate) struct CondState {
    stack: Vec<CondFrame>,
}

impl CondState {
    pub(crate) fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub(crate) fn currently_emitting(&self) -> bool {
        self.stack.last().is_none_or(|f| f.allow_emit)
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    /// Push a new `ifdef`/`ifndef` frame.
    pub(crate) fn push_ifdef(&mut self, predicate: bool) {
        let parent = self.currently_emitting();
        self.stack.push(CondFrame {
            allow_emit: parent && predicate,
            taken: predicate,
            saw_else: false,
        });
    }

    /// Apply an `elsif` directive.
    ///
    /// `predicate` is `Some(bool)` when a macro name was provided
    /// (the bool indicates whether it is defined), or `None` for a
    /// missing name (treated as false).
    pub(crate) fn apply_elsif(&mut self, predicate: Option<bool>) -> Result<(), ElsifError> {
        let Some((frame, parent_frames)) = self.stack.split_last_mut() else {
            return Err(ElsifError::NoMatchingIfdef);
        };
        if frame.saw_else {
            return Err(ElsifError::ElsifAfterElse);
        }
        let pred = predicate.unwrap_or(false);
        let parent_emit = parent_frames.last().is_none_or(|p| p.allow_emit);
        if frame.taken {
            frame.allow_emit = false;
        } else {
            frame.allow_emit = parent_emit && pred;
            if pred {
                frame.taken = true;
            }
        }
        Ok(())
    }

    /// Apply an `else` directive.
    pub(crate) fn apply_else(&mut self) -> Result<(), ElseError> {
        let Some((frame, parent_frames)) = self.stack.split_last_mut() else {
            return Err(ElseError::NoMatchingIfdef);
        };
        if frame.saw_else {
            return Err(ElseError::DuplicateElse);
        }
        let parent_emit = parent_frames.last().is_none_or(|p| p.allow_emit);
        frame.saw_else = true;
        frame.allow_emit = parent_emit && !frame.taken;
        Ok(())
    }

    /// Pop the top conditional frame for `endif`.
    /// Returns `false` if the stack was empty (no matching `ifdef`).
    pub(crate) fn pop_endif(&mut self) -> bool {
        self.stack.pop().is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_ifdef_true() {
        let mut cs = CondState::new();
        cs.push_ifdef(true);
        assert!(cs.currently_emitting());
        assert!(cs.pop_endif());
        assert!(cs.is_empty());
    }

    #[test]
    fn basic_ifdef_false() {
        let mut cs = CondState::new();
        cs.push_ifdef(false);
        assert!(!cs.currently_emitting());
        assert!(cs.pop_endif());
    }

    #[test]
    fn elsif_selects_first_true() {
        let mut cs = CondState::new();
        cs.push_ifdef(false);
        assert!(cs.apply_elsif(Some(true)).is_ok());
        assert!(cs.currently_emitting());
        assert!(cs.apply_elsif(Some(true)).is_ok());
        assert!(!cs.currently_emitting());
    }

    #[test]
    fn else_after_false() {
        let mut cs = CondState::new();
        cs.push_ifdef(false);
        assert!(cs.apply_else().is_ok());
        assert!(cs.currently_emitting());
    }

    #[test]
    fn duplicate_else_error() {
        let mut cs = CondState::new();
        cs.push_ifdef(false);
        assert!(cs.apply_else().is_ok());
        assert!(matches!(cs.apply_else(), Err(ElseError::DuplicateElse)));
    }

    #[test]
    fn elsif_after_else_error() {
        let mut cs = CondState::new();
        cs.push_ifdef(false);
        assert!(cs.apply_else().is_ok());
        assert!(matches!(
            cs.apply_elsif(Some(true)),
            Err(ElsifError::ElsifAfterElse)
        ));
    }

    #[test]
    fn pop_endif_empty_returns_false() {
        let mut cs = CondState::new();
        assert!(!cs.pop_endif());
    }

    #[test]
    fn nested_ifdef() {
        let mut cs = CondState::new();
        cs.push_ifdef(true);
        cs.push_ifdef(false);
        assert!(!cs.currently_emitting());
        assert!(cs.pop_endif());
        assert!(cs.currently_emitting());
    }

    #[test]
    fn parent_false_blocks_child() {
        let mut cs = CondState::new();
        cs.push_ifdef(false);
        cs.push_ifdef(true);
        assert!(!cs.currently_emitting());
    }
}
