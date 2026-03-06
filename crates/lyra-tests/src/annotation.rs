use std::fmt::Write;

use lyra_diag::{Diagnostic, Severity};
use lyra_source::{LineCol, LineIndex};

use crate::TestWorkspace;
use crate::resolve::{is_word_char, resolve_anchor_on_line};

/// Expected severity parsed from an annotation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpectedSeverity {
    Error,
    Warning,
    Info,
}

impl ExpectedSeverity {
    fn matches(self, severity: Severity) -> bool {
        matches!(
            (self, severity),
            (Self::Error, Severity::Error)
                | (Self::Warning, Severity::Warning)
                | (Self::Info, Severity::Info)
        )
    }

    fn as_str(self) -> &'static str {
        match self {
            Self::Error => "error",
            Self::Warning => "warning",
            Self::Info => "info",
        }
    }
}

/// An authored anchor targeting a source fragment on the annotated line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Anchor {
    /// Whole-word token match (SV identifier character class boundaries).
    Token {
        text: String,
        occurrence: Option<u32>,
    },
    /// Literal substring match.
    Literal {
        text: String,
        occurrence: Option<u32>,
    },
}

/// A parsed file-level annotation before anchor resolution.
#[derive(Debug, Clone)]
pub struct AuthoredAnnotation {
    pub file_index: usize,
    pub target_line: u32,
    pub anchor: Anchor,
    pub severity: ExpectedSeverity,
    pub code: String,
    pub message: Option<String>,
}

/// A resolved file-level annotation with concrete byte positions.
#[derive(Debug, Clone)]
pub struct Annotation {
    pub file_index: usize,
    pub target_line: u32,
    pub anchor: Anchor,
    pub start_col: u32,
    pub span_len: u32,
    pub severity: ExpectedSeverity,
    pub code: String,
    pub message: Option<String>,
}

/// A unit-level annotation (no position).
#[derive(Debug, Clone)]
pub struct UnitAnnotation {
    pub severity: ExpectedSeverity,
    pub code: String,
    pub message: Option<String>,
}

/// Check whether any file in the workspace has the `ALLOW-EXTRA-DIAGS` directive.
pub fn has_allow_extra_diags(ws: &TestWorkspace) -> bool {
    for i in 0..ws.file_count() {
        for line in ws.file_text(i).lines() {
            let trimmed = line.trim();
            if trimmed == "// ALLOW-EXTRA-DIAGS" {
                return true;
            }
        }
    }
    false
}

/// Parse all authored annotations from workspace files without resolving anchors.
///
/// A file annotation (`// @anchor ...`) targets the immediately preceding
/// physical line (zero-based `line_num - 1`). Unit annotations, other file
/// annotations, and `ALLOW-EXTRA-DIAGS` directives are transparent -- they
/// do not count as "previous line" for subsequent file annotations. Only
/// non-annotation lines advance the target.
pub fn parse_authored_annotations(
    ws: &TestWorkspace,
) -> (Vec<AuthoredAnnotation>, Vec<UnitAnnotation>) {
    let mut authored = Vec::new();
    let mut unit_annots = Vec::new();
    let mut parse_errors = Vec::new();

    for file_idx in 0..ws.file_count() {
        let text = ws.file_text(file_idx);
        let lines: Vec<&str> = text.lines().collect();
        // Tracks the most recent non-annotation source line.
        let mut prev_source_line: Option<u32> = None;

        for (line_num, line) in lines.iter().enumerate() {
            let line_num_1 = line_num as u32 + 1;

            if let Some(ua) = try_parse_unit_annotation(line) {
                unit_annots.push(ua);
                continue;
            }

            if is_allow_extra_directive(line) {
                continue;
            }

            match try_parse_file_annotation(line) {
                Some(Ok(parsed)) => {
                    if let Some(target_line) = prev_source_line {
                        authored.push(AuthoredAnnotation {
                            file_index: file_idx,
                            target_line,
                            anchor: parsed.anchor,
                            severity: parsed.severity,
                            code: parsed.code,
                            message: parsed.message,
                        });
                    } else {
                        parse_errors.push(format!(
                            "{}:{}: file annotation with no preceding source line",
                            ws.file_path(file_idx),
                            line_num_1,
                        ));
                    }
                    continue;
                }
                Some(Err(msg)) => {
                    parse_errors.push(format!("{}:{}: {msg}", ws.file_path(file_idx), line_num_1,));
                    continue;
                }
                None => {}
            }

            prev_source_line = Some(line_num as u32);
        }
    }

    assert!(
        parse_errors.is_empty(),
        "annotation parse errors:\n{}",
        parse_errors.join("\n"),
    );

    (authored, unit_annots)
}

/// Check if a line is the `ALLOW-EXTRA-DIAGS` directive.
fn is_allow_extra_directive(line: &str) -> bool {
    line.trim() == "// ALLOW-EXTRA-DIAGS"
}

/// Resolve authored annotations against their target source lines.
pub fn resolve_annotations(authored: &[AuthoredAnnotation], ws: &TestWorkspace) -> Vec<Annotation> {
    // Build per-file line views once to avoid repeated splitting.
    let file_lines: Vec<Vec<&str>> = (0..ws.file_count())
        .map(|i| ws.file_text(i).lines().collect())
        .collect();

    let mut resolved = Vec::new();
    let mut resolve_errors = Vec::new();

    for auth in authored {
        let lines = &file_lines[auth.file_index];
        let target_idx = auth.target_line as usize;

        let Some(target_text) = lines.get(target_idx) else {
            resolve_errors.push(format!(
                "{}:{}: target line does not exist (file has {} lines)",
                ws.file_path(auth.file_index),
                auth.target_line + 1,
                lines.len(),
            ));
            continue;
        };

        match resolve_anchor_on_line(target_text, &auth.anchor) {
            Ok((start_col, span_len)) => {
                resolved.push(Annotation {
                    file_index: auth.file_index,
                    target_line: auth.target_line,
                    anchor: auth.anchor.clone(),
                    start_col,
                    span_len,
                    severity: auth.severity,
                    code: auth.code.clone(),
                    message: auth.message.clone(),
                });
            }
            Err(msg) => {
                resolve_errors.push(format!(
                    "{}:{}: {msg}",
                    ws.file_path(auth.file_index),
                    auth.target_line + 1,
                ));
            }
        }
    }

    assert!(
        resolve_errors.is_empty(),
        "anchor resolution errors:\n{}",
        resolve_errors.join("\n"),
    );

    resolved
}

/// Check annotations against actual diagnostics.
///
/// Returns `Ok(())` if all annotations match, or an error message listing mismatches.
pub fn check_annotations(
    ws: &TestWorkspace,
    file_diags: &[Diagnostic],
    unit_diags: &[Diagnostic],
    allow_extra: bool,
) -> Result<(), String> {
    let (authored, unit_annots) = parse_authored_annotations(ws);
    let file_annots = resolve_annotations(&authored, ws);

    let line_indices: Vec<LineIndex> = (0..ws.file_count())
        .map(|i| LineIndex::new(ws.file_text(i)))
        .collect();

    let mut file_diag_matched = vec![false; file_diags.len()];
    let mut unit_diag_matched = vec![false; unit_diags.len()];
    let mut errors = String::new();

    match_file_annotations(
        &file_annots,
        file_diags,
        ws,
        &line_indices,
        &mut file_diag_matched,
        &mut errors,
    );
    match_unit_annotations(
        &unit_annots,
        unit_diags,
        &mut unit_diag_matched,
        &mut errors,
    );

    if !allow_extra {
        report_unmatched_file_diags(
            file_diags,
            &file_diag_matched,
            ws,
            &line_indices,
            &mut errors,
        );
        report_unmatched_unit_diags(unit_diags, &unit_diag_matched, &mut errors);
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn match_file_annotations(
    annots: &[Annotation],
    diags: &[Diagnostic],
    ws: &TestWorkspace,
    line_indices: &[LineIndex],
    matched: &mut [bool],
    errors: &mut String,
) {
    for annot in annots {
        let li = &line_indices[annot.file_index];

        // Compute the byte-offset range the anchor covers, plus a 1-byte
        // tolerance on each side.  Many diagnostics emit zero-length spans
        // that land on the whitespace immediately before or after the real
        // token, so exact-column matching is too strict.
        let anchor_start = li.offset(LineCol {
            line: annot.target_line,
            col: annot.start_col,
        });
        let anchor_end = li.offset(LineCol {
            line: annot.target_line,
            col: annot.start_col + annot.span_len,
        });

        let found = diags.iter().enumerate().find(|(idx, d)| {
            if matched[*idx] {
                return false;
            }
            if !annot.severity.matches(d.severity) || d.code != annot.code {
                return false;
            }
            let Some(span) = d.primary_span() else {
                return false;
            };
            let Some(file_idx) = ws.file_index_for_id(span.file) else {
                return false;
            };
            if file_idx != annot.file_index {
                return false;
            }
            let (Some(a_start), Some(a_end)) = (anchor_start, anchor_end) else {
                return false;
            };
            let diag_start = span.range.start();
            let lo = a_start
                - lyra_source::TextSize::new(a_start.into()).min(lyra_source::TextSize::new(1));
            let hi = a_end + lyra_source::TextSize::new(1);
            if diag_start < lo || diag_start > hi {
                return false;
            }
            if let Some(ref msg) = annot.message
                && !d.render_message().contains(msg.as_str())
            {
                return false;
            }
            true
        });

        if let Some((idx, _)) = found {
            matched[idx] = true;
        } else {
            let _ = writeln!(
                errors,
                "  expected but not found in {} line {} (anchor {}): {}[{}]{}",
                ws.file_path(annot.file_index),
                annot.target_line + 1,
                format_anchor(&annot.anchor),
                annot.severity.as_str(),
                annot.code,
                annot
                    .message
                    .as_ref()
                    .map(|m| format!(": {m}"))
                    .unwrap_or_default(),
            );
        }
    }
}

fn match_unit_annotations(
    annots: &[UnitAnnotation],
    diags: &[Diagnostic],
    matched: &mut [bool],
    errors: &mut String,
) {
    for annot in annots {
        let found = diags.iter().enumerate().find(|(idx, d)| {
            if matched[*idx] {
                return false;
            }
            if !annot.severity.matches(d.severity) || d.code != annot.code {
                return false;
            }
            if let Some(ref msg) = annot.message
                && !d.render_message().contains(msg.as_str())
            {
                return false;
            }
            true
        });

        if let Some((idx, _)) = found {
            matched[idx] = true;
        } else {
            let _ = writeln!(
                errors,
                "  expected unit diagnostic not found: {}[{}]{}",
                annot.severity.as_str(),
                annot.code,
                annot
                    .message
                    .as_ref()
                    .map(|m| format!(": {m}"))
                    .unwrap_or_default(),
            );
        }
    }
}

fn report_unmatched_file_diags(
    diags: &[Diagnostic],
    matched: &[bool],
    ws: &TestWorkspace,
    line_indices: &[LineIndex],
    errors: &mut String,
) {
    for (idx, d) in diags.iter().enumerate() {
        if matched[idx] {
            continue;
        }
        let loc = d
            .primary_span()
            .and_then(|span| {
                let fi = ws.file_index_for_id(span.file)?;
                let li = &line_indices[fi];
                let lc = li.line_col(span.range.start());
                Some(format!(
                    "{} {}:{}",
                    ws.file_path(fi),
                    lc.line + 1,
                    lc.col + 1
                ))
            })
            .unwrap_or_else(|| "?".to_owned());
        let _ = writeln!(
            errors,
            "  unexpected diagnostic at {loc}: {sev}[{code}]: {msg}",
            sev = crate::severity_str(d.severity),
            code = d.code,
            msg = d.render_message(),
        );
    }
}

fn report_unmatched_unit_diags(diags: &[Diagnostic], matched: &[bool], errors: &mut String) {
    for (idx, d) in diags.iter().enumerate() {
        if matched[idx] {
            continue;
        }
        let _ = writeln!(
            errors,
            "  unexpected unit diagnostic: {sev}[{code}]: {msg}",
            sev = crate::severity_str(d.severity),
            code = d.code,
            msg = d.render_message(),
        );
    }
}

/// Format an anchor for display in error messages.
fn format_anchor(anchor: &Anchor) -> String {
    match anchor {
        Anchor::Token { text, occurrence } => match occurrence {
            Some(n) => format!("@{n}:{text}"),
            None => format!("@{text}"),
        },
        Anchor::Literal { text, occurrence } => match occurrence {
            Some(n) => format!("@{n}:[{text}]"),
            None => format!("@[{text}]"),
        },
    }
}

/// Parsed components of a file-level annotation line (before resolution).
struct ParsedAnnotation {
    anchor: Anchor,
    severity: ExpectedSeverity,
    code: String,
    message: Option<String>,
}

/// Try to parse a file-level annotation line.
///
/// Returns `None` if the line is not an annotation line.
/// Returns `Some(Ok(...))` for a valid annotation.
/// Returns `Some(Err(...))` for a malformed annotation.
fn try_parse_file_annotation(line: &str) -> Option<Result<ParsedAnnotation, String>> {
    let comment_start = line.find("//")?;
    let after_slashes = &line[(comment_start + 2)..];
    let trimmed = after_slashes.trim_start();

    if !trimmed.starts_with('@') {
        return None;
    }

    Some(parse_anchor_annotation(&trimmed[1..]))
}

/// Parse the part after `@` in an annotation line.
fn parse_anchor_annotation(s: &str) -> Result<ParsedAnnotation, String> {
    let (anchor, rest) = parse_anchor(s)?;

    let rest = rest.trim_start();
    let (severity, code, message) = parse_severity_code_message(rest)
        .ok_or_else(|| format!("expected severity[code] after anchor, got: {rest}"))?;

    Ok(ParsedAnnotation {
        anchor,
        severity,
        code,
        message,
    })
}

/// Parse an anchor from the start of a string, returning the anchor and remaining text.
fn parse_anchor(s: &str) -> Result<(Anchor, &str), String> {
    // Try to parse optional occurrence prefix: N:
    let (occurrence, rest) = parse_occurrence_prefix(s);

    if let Some(after_bracket) = rest.strip_prefix('[') {
        // Literal form: @[text] or @N:[text]
        let bracket_end = find_matching_square_bracket(after_bracket)
            .ok_or_else(|| "unclosed '[' in literal anchor".to_owned())?;
        let text = &after_bracket[..bracket_end];
        if text.is_empty() {
            return Err("empty literal anchor '[]'".to_owned());
        }
        let remaining = &after_bracket[bracket_end + 1..];
        Ok((
            Anchor::Literal {
                text: text.to_owned(),
                occurrence,
            },
            remaining,
        ))
    } else {
        // Token form: @token or @N:token
        // Bare token anchors cannot start with a digit
        if occurrence.is_none() {
            let first = rest
                .chars()
                .next()
                .ok_or_else(|| "empty anchor after '@'".to_owned())?;
            if first.is_ascii_digit() {
                return Err(format!(
                    "bare token anchor cannot start with digit '{first}'; \
                     use @[{rest_word}] for numeric anchors",
                    rest_word = take_word(rest)
                ));
            }
        }
        let word = take_word(rest);
        if word.is_empty() {
            return Err("empty token in anchor".to_owned());
        }
        let remaining = &rest[word.len()..];
        Ok((
            Anchor::Token {
                text: word.to_owned(),
                occurrence,
            },
            remaining,
        ))
    }
}

/// Parse an optional `N:` occurrence prefix.
///
/// Returns `(Some(n), rest)` if found, `(None, s)` otherwise.
/// Rejects leading zeros (e.g. `01:`) and zero (`0:`). Only canonical
/// positive decimals like `1:`, `2:`, `12:` are accepted.
fn parse_occurrence_prefix(s: &str) -> (Option<u32>, &str) {
    let digit_end = s.find(|c: char| !c.is_ascii_digit()).unwrap_or(s.len());
    if digit_end > 0 && s[digit_end..].starts_with(':') {
        let digits = &s[..digit_end];
        // Reject leading zeros: "01", "001", etc. Single "0" is also rejected (n > 0 below).
        if digits.len() > 1 && digits.starts_with('0') {
            return (None, s);
        }
        if let Ok(n) = digits.parse::<u32>()
            && n > 0
        {
            return (Some(n), &s[digit_end + 1..]);
        }
    }
    (None, s)
}

/// Take a word (SV-style identifier chars: [A-Za-z0-9_$]) from the start of a string.
fn take_word(s: &str) -> &str {
    let end = s.find(|c: char| !is_word_char(c)).unwrap_or(s.len());
    &s[..end]
}

/// Try to parse a unit-level annotation line.
fn try_parse_unit_annotation(line: &str) -> Option<UnitAnnotation> {
    let comment_start = line.find("//")?;
    let after_slashes = &line[(comment_start + 2)..];
    let trimmed = after_slashes.trim_start();

    if !trimmed.starts_with("unit ") {
        return None;
    }

    let rest = trimmed["unit ".len()..].trim_start();
    let (severity, code, message) = parse_severity_code_message(rest)?;

    Some(UnitAnnotation {
        severity,
        code,
        message,
    })
}

/// Parse `severity[code]: message` or `severity[code]` from a string.
fn parse_severity_code_message(s: &str) -> Option<(ExpectedSeverity, String, Option<String>)> {
    let (severity, rest) = if let Some(rest) = s.strip_prefix("error") {
        (ExpectedSeverity::Error, rest)
    } else if let Some(rest) = s.strip_prefix("warning") {
        (ExpectedSeverity::Warning, rest)
    } else if let Some(rest) = s.strip_prefix("info") {
        (ExpectedSeverity::Info, rest)
    } else {
        return None;
    };

    let rest = rest.strip_prefix('[')?;
    let bracket_end = find_matching_bracket(rest)?;
    let code = rest[..bracket_end].to_owned();
    let rest = &rest[(bracket_end + 1)..];

    let message = if let Some(after_colon) = rest.strip_prefix(':') {
        let msg = after_colon.trim();
        if msg.is_empty() {
            None
        } else {
            Some(msg.to_owned())
        }
    } else {
        None
    };

    Some((severity, code, message))
}

/// Find the position of the closing `]` that matches the implicit opening `[`.
fn find_matching_bracket(s: &str) -> Option<usize> {
    let mut depth: u32 = 1;
    for (i, c) in s.char_indices() {
        match c {
            '[' => depth += 1,
            ']' => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => {}
        }
    }
    None
}

/// Find the position of the closing `]` for a literal anchor.
///
/// Literal anchor text cannot contain `]` -- the first `]` ends the anchor.
/// This is intentional: the grammar `@[text]` uses `]` as an unescaped
/// delimiter, so literal anchors targeting `]` itself are not supported.
fn find_matching_square_bracket(s: &str) -> Option<usize> {
    s.find(']')
}

#[cfg(test)]
mod tests {
    use super::*;

    // Anchor parsing tests

    #[test]
    fn parse_token_anchor() {
        let line = "  // @foo error[lyra.semantic.x]: msg";
        let parsed = try_parse_file_annotation(line).unwrap().unwrap();
        assert_eq!(
            parsed.anchor,
            Anchor::Token {
                text: "foo".to_owned(),
                occurrence: None
            }
        );
        assert_eq!(parsed.severity, ExpectedSeverity::Error);
        assert_eq!(parsed.code, "lyra.semantic.x");
        assert_eq!(parsed.message.as_deref(), Some("msg"));
    }

    #[test]
    fn parse_occurrence_token_anchor() {
        let line = "// @2:foo warning[c]: w";
        let parsed = try_parse_file_annotation(line).unwrap().unwrap();
        assert_eq!(
            parsed.anchor,
            Anchor::Token {
                text: "foo".to_owned(),
                occurrence: Some(2)
            }
        );
        assert_eq!(parsed.severity, ExpectedSeverity::Warning);
    }

    #[test]
    fn parse_literal_anchor() {
        let line = "// @[8'd0] error[c]";
        let parsed = try_parse_file_annotation(line).unwrap().unwrap();
        assert_eq!(
            parsed.anchor,
            Anchor::Literal {
                text: "8'd0".to_owned(),
                occurrence: None
            }
        );
        assert!(parsed.message.is_none());
    }

    #[test]
    fn parse_occurrence_literal_anchor() {
        let line = "// @3:[(] error[c]";
        let parsed = try_parse_file_annotation(line).unwrap().unwrap();
        assert_eq!(
            parsed.anchor,
            Anchor::Literal {
                text: "(".to_owned(),
                occurrence: Some(3)
            }
        );
    }

    #[test]
    fn parse_no_message() {
        let line = "// @foo error[c]";
        let parsed = try_parse_file_annotation(line).unwrap().unwrap();
        assert!(parsed.message.is_none());
    }

    #[test]
    fn regular_comment_not_annotation() {
        assert!(try_parse_file_annotation("  // this is a regular comment").is_none());
        assert!(try_parse_file_annotation("  // no anchor here").is_none());
    }

    #[test]
    fn digit_start_rejected_for_bare_token() {
        let result = try_parse_file_annotation("// @0 error[c]");
        assert!(result.is_some());
        assert!(result.unwrap().is_err());
    }

    #[test]
    fn leading_zero_occurrence_rejected() {
        // @01:foo should parse as bare token "01" which fails (digit start)
        let result = try_parse_file_annotation("// @01:foo error[c]");
        assert!(result.is_some());
        assert!(result.unwrap().is_err());
    }

    #[test]
    fn parse_unit_annotation() {
        let line = "// unit error[lyra.semantic.duplicate_definition]: duplicate module";
        let ua = try_parse_unit_annotation(line).unwrap();
        assert_eq!(ua.severity, ExpectedSeverity::Error);
        assert_eq!(ua.code, "lyra.semantic.duplicate_definition");
        assert_eq!(ua.message.as_deref(), Some("duplicate module"));
    }

    #[test]
    fn unit_annotation_no_message() {
        let line = "// unit warning[lyra.semantic.ambiguous_import]";
        let ua = try_parse_unit_annotation(line).unwrap();
        assert_eq!(ua.severity, ExpectedSeverity::Warning);
        assert_eq!(ua.code, "lyra.semantic.ambiguous_import");
        assert!(ua.message.is_none());
    }
}
