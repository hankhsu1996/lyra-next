use std::fmt::Write;

use lyra_diag::{Diagnostic, Severity};
use lyra_source::{LineCol, LineIndex};

use crate::TestWorkspace;

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

/// A parsed file-level annotation from a source comment.
#[derive(Debug, Clone)]
pub struct Annotation {
    pub file_index: usize,
    pub target_line: u32,
    pub start_col: u32,
    pub span_len: Option<u32>,
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

/// Parse all annotations from workspace files.
pub fn parse_annotations(ws: &TestWorkspace) -> (Vec<Annotation>, Vec<UnitAnnotation>) {
    let mut file_annots = Vec::new();
    let mut unit_annots = Vec::new();

    for file_idx in 0..ws.file_count() {
        let text = ws.file_text(file_idx);
        let mut prev_non_annot_line: Option<u32> = None;

        for (line_num, line) in text.lines().enumerate() {
            let line_num = line_num as u32;

            if let Some(ua) = try_parse_unit_annotation(line) {
                unit_annots.push(ua);
                // Unit annotation lines are not annotation lines (they don't
                // target a previous line), so they count as non-annotation.
                prev_non_annot_line = Some(line_num);
                continue;
            }

            if let Some(parsed) = try_parse_file_annotation(line) {
                if let Some(target_line) = prev_non_annot_line {
                    file_annots.push(Annotation {
                        file_index: file_idx,
                        target_line,
                        start_col: parsed.caret_col,
                        span_len: parsed.span_len,
                        severity: parsed.severity,
                        code: parsed.code,
                        message: parsed.message,
                    });
                }
                // Annotation lines do NOT update prev_non_annot_line
                continue;
            }

            // Regular line (including empty lines and normal comments)
            prev_non_annot_line = Some(line_num);
        }
    }

    (file_annots, unit_annots)
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
    let (file_annots, unit_annots) = parse_annotations(ws);

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
        let expected_start = li.offset(LineCol {
            line: annot.target_line,
            col: annot.start_col,
        });

        let found = diags.iter().enumerate().find(|(idx, d)| {
            if matched[*idx] {
                return false;
            }
            if !annot.severity.matches(d.severity) || d.code.as_str() != annot.code {
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
            let Some(expected_start) = expected_start else {
                return false;
            };
            if span.range.start() != expected_start {
                return false;
            }
            if let Some(len) = annot.span_len {
                let expected_end = expected_start + lyra_source::TextSize::new(len);
                if span.range.end() != expected_end {
                    return false;
                }
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
                "  expected but not found in {} line {}: {}[{}]{}",
                ws.file_path(annot.file_index),
                annot.target_line,
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
            if !annot.severity.matches(d.severity) || d.code.as_str() != annot.code {
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
                Some(format!("{} {}:{}", ws.file_path(fi), lc.line, lc.col))
            })
            .unwrap_or_else(|| "?".to_owned());
        let _ = writeln!(
            errors,
            "  unexpected diagnostic at {loc}: {sev}[{code}]: {msg}",
            sev = crate::severity_str(d.severity),
            code = d.code.as_str(),
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
            code = d.code.as_str(),
            msg = d.render_message(),
        );
    }
}

/// Parsed components of a file-level annotation line.
struct ParsedAnnotation {
    caret_col: u32,
    span_len: Option<u32>,
    severity: ExpectedSeverity,
    code: String,
    message: Option<String>,
}

/// Try to parse a file-level annotation line.
fn try_parse_file_annotation(line: &str) -> Option<ParsedAnnotation> {
    let comment_start = line.find("//")?;
    let after_slashes = &line[(comment_start + 2)..];

    let trimmed = after_slashes.trim_start();
    let whitespace_after_slashes = after_slashes.len() - trimmed.len();

    if !trimmed.starts_with('^') {
        return None;
    }

    let marker_chars: usize = 1 + trimmed[1..].chars().take_while(|&c| c == '~').count();
    let span_len = if marker_chars > 1 {
        Some(marker_chars as u32)
    } else {
        None
    };

    // The caret column: position of ^ relative to line start
    let caret_col = (comment_start + 2 + whitespace_after_slashes) as u32;

    let rest = trimmed[marker_chars..].trim_start();
    let (severity, code, message) = parse_severity_code_message(rest)?;

    Some(ParsedAnnotation {
        caret_col,
        span_len,
        severity,
        code,
        message,
    })
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

    // Code may contain nested brackets (e.g. `lyra.semantic[1]`)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_file_annotation_basic() {
        let line = "  //         ^ error[lyra.semantic[1]]: unresolved name";
        let parsed = try_parse_file_annotation(line).unwrap();
        assert_eq!(parsed.caret_col, 13);
        assert!(parsed.span_len.is_none());
        assert_eq!(parsed.severity, ExpectedSeverity::Error);
        assert_eq!(parsed.code, "lyra.semantic[1]");
        assert_eq!(parsed.message.as_deref(), Some("unresolved name"));
    }

    #[test]
    fn parse_file_annotation_with_span() {
        let line = "  // ^~~~ error[lyra.parse[1]]: expected semicolon";
        let parsed = try_parse_file_annotation(line).unwrap();
        assert_eq!(parsed.caret_col, 5);
        assert_eq!(parsed.span_len, Some(4));
        assert_eq!(parsed.severity, ExpectedSeverity::Error);
        assert_eq!(parsed.code, "lyra.parse[1]");
        assert_eq!(parsed.message.as_deref(), Some("expected semicolon"));
    }

    #[test]
    fn parse_file_annotation_no_message() {
        let line = "  // ^ warning[lyra.semantic[3]]";
        let parsed = try_parse_file_annotation(line).unwrap();
        assert_eq!(parsed.severity, ExpectedSeverity::Warning);
        assert_eq!(parsed.code, "lyra.semantic[3]");
        assert!(parsed.message.is_none());
    }

    #[test]
    fn regular_comment_not_annotation() {
        assert!(try_parse_file_annotation("  // this is a regular comment").is_none());
        assert!(try_parse_file_annotation("  // no caret here").is_none());
    }

    #[test]
    fn parse_unit_annotation() {
        let line = "// unit error[lyra.semantic[2]]: duplicate module";
        let ua = try_parse_unit_annotation(line).unwrap();
        assert_eq!(ua.severity, ExpectedSeverity::Error);
        assert_eq!(ua.code, "lyra.semantic[2]");
        assert_eq!(ua.message.as_deref(), Some("duplicate module"));
    }

    #[test]
    fn unit_annotation_no_message() {
        let line = "// unit warning[lyra.semantic[5]]";
        let ua = try_parse_unit_annotation(line).unwrap();
        assert_eq!(ua.severity, ExpectedSeverity::Warning);
        assert_eq!(ua.code, "lyra.semantic[5]");
        assert!(ua.message.is_none());
    }
}
