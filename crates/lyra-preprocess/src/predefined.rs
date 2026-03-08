use lyra_lexer::{SyntaxKind, Token};

/// Built-in predefined text macros (LRM 22.13).
///
/// Always defined, cannot be redefined or undefined, and expand
/// based on the expansion site (not macro definition site).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PredefinedMacro {
    File,
    Line,
}

/// Classify a macro name (without backtick prefix) as a predefined
/// built-in macro. Returns `None` for user-defined macro names.
pub(crate) fn classify_predefined(name: &str) -> Option<PredefinedMacro> {
    match name {
        "__FILE__" => Some(PredefinedMacro::File),
        "__LINE__" => Some(PredefinedMacro::Line),
        _ => None,
    }
}

/// Produce the expansion token and text for a predefined macro, and
/// push them into the provided output buffers.
///
/// This is the single emission point for predefined macro expansion,
/// used by both top-level and nested (macro-body) expansion paths.
pub(crate) fn emit_predefined(
    kind: PredefinedMacro,
    file_path: &str,
    line: u32,
    out_tokens: &mut Vec<Token>,
    out_text: &mut String,
) {
    let (syn_kind, text) = expand_predefined(kind, file_path, line);
    out_tokens.push(Token {
        kind: syn_kind,
        len: (text.len() as u32).into(),
    });
    out_text.push_str(&text);
}

/// Produce the expansion text and token kind for a predefined macro.
///
/// `file_path` is the display path for the current file.
/// `line` is the 1-based line number at the expansion site.
fn expand_predefined(kind: PredefinedMacro, file_path: &str, line: u32) -> (SyntaxKind, String) {
    match kind {
        PredefinedMacro::File => (SyntaxKind::StringLiteral, sv_string_literal(file_path)),
        PredefinedMacro::Line => (SyntaxKind::IntLiteral, line.to_string()),
    }
}

/// Format a string as a valid `SystemVerilog` string literal,
/// wrapping in double quotes and escaping `\` and `"`.
pub(crate) fn sv_string_literal(content: &str) -> String {
    let mut s = String::with_capacity(content.len() + 2);
    s.push('"');
    for c in content.chars() {
        match c {
            '"' => s.push_str("\\\""),
            '\\' => s.push_str("\\\\"),
            _ => s.push(c),
        }
    }
    s.push('"');
    s
}

/// Precomputed source line map for O(log n) line number lookups.
///
/// Built once per preprocessing invocation from the source text.
pub(crate) struct LineMap {
    line_starts: Vec<usize>,
}

impl LineMap {
    /// Build a line map by scanning for newline positions.
    pub(crate) fn build(source: &str) -> Self {
        let mut starts = vec![0];
        for (i, b) in source.bytes().enumerate() {
            if b == b'\n' {
                starts.push(i + 1);
            }
        }
        Self {
            line_starts: starts,
        }
    }

    /// Return the 1-based line number for a byte offset.
    ///
    /// Offsets at or past EOF clamp to the last line.
    pub(crate) fn line_at(&self, byte_offset: usize) -> u32 {
        let clamped = byte_offset.min(self.last_valid_offset());
        match self.line_starts.binary_search(&clamped) {
            Ok(idx) => idx as u32 + 1,
            Err(idx) => idx as u32,
        }
    }

    /// The last valid byte offset: one past the last newline start,
    /// or 0 for an empty source.
    fn last_valid_offset(&self) -> usize {
        self.line_starts.last().copied().unwrap_or(0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn line_map_basic() {
        let map = LineMap::build("a\nb\nc\n");
        assert_eq!(map.line_at(0), 1); // 'a'
        assert_eq!(map.line_at(2), 2); // 'b'
        assert_eq!(map.line_at(4), 3); // 'c'
    }

    #[test]
    fn line_map_clamps_past_eof() {
        let map = LineMap::build("ab\n");
        assert_eq!(map.line_at(100), 2);
    }

    #[test]
    fn line_map_empty_source() {
        let map = LineMap::build("");
        assert_eq!(map.line_at(0), 1);
        assert_eq!(map.line_at(100), 1);
    }

    #[test]
    fn sv_string_literal_escaping() {
        assert_eq!(sv_string_literal("foo"), "\"foo\"");
        assert_eq!(sv_string_literal("a\\b"), "\"a\\\\b\"");
        assert_eq!(sv_string_literal("a\"b"), "\"a\\\"b\"");
    }

    #[test]
    fn classify_predefined_known() {
        assert_eq!(classify_predefined("__FILE__"), Some(PredefinedMacro::File));
        assert_eq!(classify_predefined("__LINE__"), Some(PredefinedMacro::Line));
        assert_eq!(classify_predefined("FOO"), None);
    }
}
