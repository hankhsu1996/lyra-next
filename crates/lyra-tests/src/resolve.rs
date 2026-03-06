// Anchor resolution operates on ASCII byte positions. All boundary checks
// index `line.as_bytes()` directly, which is valid because SystemVerilog
// source text is ASCII-only (per project policy and LRM convention).
// Non-ASCII input would produce incorrect boundary results.

use crate::annotation::Anchor;

/// SV-style identifier character class (ASCII-only).
pub(crate) fn is_word_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_' || c == '$'
}

/// Check if position `pos` in `line` is a word boundary (left side).
///
/// Assumes ASCII content (byte position == char position).
fn is_word_boundary_left(line: &str, pos: usize) -> bool {
    if pos == 0 {
        return true;
    }
    let prev = line.as_bytes()[pos - 1] as char;
    !is_word_char(prev)
}

/// Check if position `pos` in `line` is a word boundary (right side).
///
/// Assumes ASCII content (byte position == char position).
fn is_word_boundary_right(line: &str, pos: usize) -> bool {
    if pos >= line.len() {
        return true;
    }
    let next = line.as_bytes()[pos] as char;
    !is_word_char(next)
}

/// Find all whole-word matches of `token` in `line`.
fn find_word_matches(line: &str, token: &str) -> Vec<usize> {
    let mut matches = Vec::new();
    let mut search_start = 0;
    while let Some(pos) = line[search_start..].find(token) {
        let abs_pos = search_start + pos;
        let end_pos = abs_pos + token.len();
        if is_word_boundary_left(line, abs_pos) && is_word_boundary_right(line, end_pos) {
            matches.push(abs_pos);
        }
        search_start = abs_pos + 1;
    }
    matches
}

/// Find all literal substring matches of `text` in `line`.
fn find_literal_matches(line: &str, text: &str) -> Vec<usize> {
    let mut matches = Vec::new();
    let mut search_start = 0;
    while let Some(pos) = line[search_start..].find(text) {
        matches.push(search_start + pos);
        search_start = search_start + pos + 1;
    }
    matches
}

/// Resolve an anchor against a source line, returning (`start_col`, `span_len`).
pub fn resolve_anchor_on_line(line: &str, anchor: &Anchor) -> Result<(u32, u32), String> {
    match anchor {
        Anchor::Token { text, occurrence } => {
            let matches = find_word_matches(line, text);
            resolve_from_matches(&matches, text, *occurrence, "token", text.len())
        }
        Anchor::Literal { text, occurrence } => {
            let matches = find_literal_matches(line, text);
            resolve_from_matches(&matches, text, *occurrence, "literal", text.len())
        }
    }
}

fn resolve_from_matches(
    matches: &[usize],
    text: &str,
    occurrence: Option<u32>,
    kind: &str,
    span_len: usize,
) -> Result<(u32, u32), String> {
    if matches.is_empty() {
        return Err(format!("no {kind} match for '{text}' on source line"));
    }

    let idx = if let Some(n) = occurrence {
        let n_idx = (n - 1) as usize;
        if n_idx >= matches.len() {
            return Err(format!(
                "{kind} '{text}': occurrence {n} requested but only {} found",
                matches.len()
            ));
        }
        n_idx
    } else {
        if matches.len() > 1 {
            return Err(format!(
                "ambiguous {kind} '{text}': {} matches on source line; \
                 use @N:{anchor_syntax} to disambiguate",
                matches.len(),
                anchor_syntax = if kind == "literal" {
                    format!("[{text}]")
                } else {
                    text.to_owned()
                },
            ));
        }
        0
    };

    Ok((matches[idx] as u32, span_len as u32))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolve_unique_token() {
        let anchor = Anchor::Token {
            text: "foo".to_owned(),
            occurrence: None,
        };
        let (col, len) = resolve_anchor_on_line("  assign foo = bar;", &anchor).unwrap();
        assert_eq!(col, 9);
        assert_eq!(len, 3);
    }

    #[test]
    fn resolve_token_not_inside_word() {
        let anchor = Anchor::Token {
            text: "x".to_owned(),
            occurrence: None,
        };
        // "xx" has no word-boundary match for "x"
        let result = resolve_anchor_on_line("  xx = 1;", &anchor);
        assert!(result.is_err());
    }

    #[test]
    fn resolve_token_boundary_underscore() {
        let anchor = Anchor::Token {
            text: "x".to_owned(),
            occurrence: None,
        };
        // "foo_x" -- underscore is a word char, so "x" is not a word boundary match
        let result = resolve_anchor_on_line("  foo_x = 1;", &anchor);
        assert!(result.is_err());
    }

    #[test]
    fn resolve_token_boundary_dollar() {
        let anchor = Anchor::Token {
            text: "signed".to_owned(),
            occurrence: None,
        };
        // "$signed" -- $ is a word char, so "signed" is not a word match inside "$signed"
        let result = resolve_anchor_on_line("  assign s = $signed(vec);", &anchor);
        assert!(result.is_err());
    }

    #[test]
    fn resolve_ambiguous_token() {
        let anchor = Anchor::Token {
            text: "x".to_owned(),
            occurrence: None,
        };
        let result = resolve_anchor_on_line("  x = x + 1;", &anchor);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("ambiguous"));
    }

    #[test]
    fn resolve_occurrence_token() {
        let anchor = Anchor::Token {
            text: "x".to_owned(),
            occurrence: Some(2),
        };
        let (col, len) = resolve_anchor_on_line("  x = x + 1;", &anchor).unwrap();
        assert_eq!(col, 6);
        assert_eq!(len, 1);
    }

    #[test]
    fn resolve_unique_literal() {
        let anchor = Anchor::Literal {
            text: "8'd0".to_owned(),
            occurrence: None,
        };
        let (col, len) = resolve_anchor_on_line("  wire w = 8'd0;", &anchor).unwrap();
        assert_eq!(col, 11);
        assert_eq!(len, 4);
    }

    #[test]
    fn resolve_occurrence_literal() {
        let anchor = Anchor::Literal {
            text: "(".to_owned(),
            occurrence: Some(2),
        };
        let (col, len) = resolve_anchor_on_line("  f(a, g(b));", &anchor).unwrap();
        assert_eq!(col, 8);
        assert_eq!(len, 1);
    }

    #[test]
    fn resolve_literal_punctuation() {
        let anchor = Anchor::Literal {
            text: "(".to_owned(),
            occurrence: Some(1),
        };
        let (col, len) = resolve_anchor_on_line("  wire (highz0, highz1) w;", &anchor).unwrap();
        assert_eq!(col, 7);
        assert_eq!(len, 1);
    }

    #[test]
    fn resolve_missing_token() {
        let anchor = Anchor::Token {
            text: "missing".to_owned(),
            occurrence: None,
        };
        let result = resolve_anchor_on_line("  assign x = 1;", &anchor);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("no token match"));
    }

    #[test]
    fn resolve_occurrence_out_of_range() {
        let anchor = Anchor::Token {
            text: "x".to_owned(),
            occurrence: Some(5),
        };
        let result = resolve_anchor_on_line("  x = x + 1;", &anchor);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("occurrence 5"));
    }
}
