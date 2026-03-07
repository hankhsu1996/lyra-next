use lyra_lexer::{SyntaxKind, Token};

use crate::env::{MacroTok, MacroTokenSeq};

/// Parsed argument list from a `MacroTok` slice.
pub(crate) struct ParsedArgs {
    pub args: Vec<MacroTokenSeq>,
    /// Token index past the closing `)`.
    pub end_idx: usize,
}

/// Argument parsing failure for `MacroTok` slice parsing.
pub(crate) enum ArgParseError {
    /// EOF reached before closing `)`.
    Unterminated,
}

/// Parsed argument list from a file-level token stream.
///
/// Arguments are represented as token-index ranges. The caller
/// materializes `MacroTokenSeq` from these ranges using source text,
/// keeping all cursor/text logic out of the parser.
pub(crate) struct TokenStreamParsedArgs {
    /// `(start_idx, end_idx)` token-index ranges for each argument.
    /// `start_idx` is the first token of the argument; `end_idx` is
    /// one past the last token (exclusive). An empty argument has
    /// `start == end`.
    pub arg_ranges: Vec<(usize, usize)>,
    /// Token index past the closing `)`.
    pub end_idx: usize,
}

/// Parse a parenthesized argument list from a `MacroTok` slice.
///
/// `start_lparen_idx` is the index of the opening `(` within `toks`.
/// Returns the parsed arguments and the index past the closing `)`.
///
/// The parser uses a "commas + 1" rule: it always starts with one
/// accumulator. Each comma at depth 0 pushes the current accumulator
/// and starts a new one. `)` at depth 0 pushes the final accumulator.
///
/// Nested parentheses adjust depth but do not split arguments.
/// Arguments may span multiple lines (newlines are valid per LRM 22.5.1).
///
/// Token-opacity invariant: argument splitting operates on
/// `SyntaxKind` only. Commas, parentheses, and other delimiter
/// characters embedded inside literal tokens (including triple-quoted
/// `StringLiteral` tokens) are opaque to the splitter and do not
/// affect argument boundaries.
pub(crate) fn parse_args_from_seq(
    toks: &[MacroTok],
    start_lparen_idx: usize,
) -> Result<ParsedArgs, ArgParseError> {
    let mut depth: u32 = 0;
    let mut current: Vec<MacroTok> = Vec::new();
    let mut args: Vec<MacroTokenSeq> = Vec::new();
    let mut i = start_lparen_idx + 1;

    while i < toks.len() {
        let tok = &toks[i];
        match tok.token.kind {
            SyntaxKind::LParen => {
                depth += 1;
                current.push(tok.clone());
            }
            SyntaxKind::RParen if depth == 0 => {
                args.push(MacroTokenSeq::from_vec(current));
                return Ok(ParsedArgs {
                    args,
                    end_idx: i + 1,
                });
            }
            SyntaxKind::RParen => {
                depth -= 1;
                current.push(tok.clone());
            }
            SyntaxKind::Comma if depth == 0 => {
                args.push(MacroTokenSeq::from_vec(current));
                current = Vec::new();
            }
            _ => {
                current.push(tok.clone());
            }
        }
        i += 1;
    }

    Err(ArgParseError::Unterminated)
}

/// Parse a parenthesized argument list from a file-level token stream,
/// returning token-index ranges for each argument.
///
/// Purely structural: operates on `SyntaxKind` only, does not touch
/// source text or track byte cursors. The caller materializes
/// `MacroTokenSeq` from the returned ranges.
///
/// Token-opacity invariant: delimiter characters inside literal
/// tokens (e.g. commas or parentheses in triple-quoted strings) are
/// invisible to the splitter because they share a single token with
/// `SyntaxKind::StringLiteral`, not individual punctuation kinds.
///
/// `lparen_idx` is the index of the opening `(` in `tokens`.
/// On success, returns the argument ranges and the index past the
/// closing `)`. On failure (unterminated), returns `Err` with the
/// end token index.
pub(crate) fn parse_args_from_tokens(
    tokens: &[Token],
    lparen_idx: usize,
) -> Result<TokenStreamParsedArgs, usize> {
    let mut j = lparen_idx + 1;
    let mut depth: u32 = 0;
    let mut arg_start = j;
    let mut arg_ranges: Vec<(usize, usize)> = Vec::new();

    while j < tokens.len() {
        if tokens[j].kind == SyntaxKind::Eof {
            break;
        }

        match tokens[j].kind {
            SyntaxKind::LParen => {
                depth += 1;
            }
            SyntaxKind::RParen if depth == 0 => {
                arg_ranges.push((arg_start, j));
                return Ok(TokenStreamParsedArgs {
                    arg_ranges,
                    end_idx: j + 1,
                });
            }
            SyntaxKind::RParen => {
                depth -= 1;
            }
            SyntaxKind::Comma if depth == 0 => {
                arg_ranges.push((arg_start, j));
                arg_start = j + 1;
            }
            _ => {}
        }

        j += 1;
    }

    Err(j)
}
