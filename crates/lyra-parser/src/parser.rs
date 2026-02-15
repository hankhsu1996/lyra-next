use std::cell::Cell;

use lyra_lexer::{SyntaxKind, Token};
use lyra_source::{TextRange, TextSize};

use crate::ParseError;
use crate::event::Event;

const PARSER_FUEL: u32 = 256;

pub(crate) struct Parser<'t> {
    tokens: &'t [Token],
    pos: usize,
    fuel: Cell<u32>,
    pub(crate) events: Vec<Event>,
    pub(crate) errors: Vec<ParseError>,
    // Precomputed byte offset for each token index.
    offsets: Vec<u32>,
}

pub(crate) struct Marker {
    pos: u32,
    completed: bool,
}

pub(crate) struct CompletedMarker {
    pub(crate) pos: u32,
}

impl<'t> Parser<'t> {
    pub(crate) fn new(tokens: &'t [Token]) -> Self {
        let mut offsets = Vec::with_capacity(tokens.len() + 1);
        let mut off = 0u32;
        for tok in tokens {
            offsets.push(off);
            off += u32::from(tok.len);
        }
        offsets.push(off);
        Self {
            tokens,
            pos: 0,
            fuel: Cell::new(PARSER_FUEL),
            events: Vec::new(),
            errors: Vec::new(),
            offsets,
        }
    }

    // Return the kind of the nth significant (non-trivia) token ahead.
    pub(crate) fn nth(&self, n: usize) -> SyntaxKind {
        let fuel = self.fuel.get();
        if fuel == 0 {
            return SyntaxKind::Eof;
        }
        self.fuel.set(fuel - 1);

        let mut pos = self.pos;
        let mut seen = 0usize;
        while pos < self.tokens.len() {
            let kind = self.tokens[pos].kind;
            if is_trivia(kind) {
                pos += 1;
                continue;
            }
            if seen == n {
                return kind;
            }
            seen += 1;
            pos += 1;
        }
        SyntaxKind::Eof
    }

    pub(crate) fn current(&self) -> SyntaxKind {
        self.nth(0)
    }

    pub(crate) fn at(&self, kind: SyntaxKind) -> bool {
        self.current() == kind
    }

    pub(crate) fn eat(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    // Expect a specific token kind; emit error if not found (does not skip).
    pub(crate) fn expect(&mut self, kind: SyntaxKind) {
        if !self.eat(kind) {
            self.error(&format!("expected {}", token_name(kind)));
        }
    }

    // Consume the next significant token, emitting leading trivia with it.
    pub(crate) fn bump(&mut self) {
        self.fuel.set(PARSER_FUEL);
        let mut n_raw = 0u16;
        while self.pos < self.tokens.len() && is_trivia(self.tokens[self.pos].kind) {
            self.pos += 1;
            n_raw = n_raw.saturating_add(1);
        }
        if self.pos < self.tokens.len() && self.tokens[self.pos].kind != SyntaxKind::Eof {
            self.pos += 1;
            n_raw = n_raw.saturating_add(1);
        }
        self.events.push(Event::Token {
            n_raw_tokens: n_raw,
        });
    }

    pub(crate) fn start(&mut self) -> Marker {
        #[allow(clippy::cast_possible_truncation)]
        let pos = self.events.len() as u32;
        self.events.push(Event::Start {
            kind: SyntaxKind::ErrorNode,
            forward_parent: None,
        });
        Marker {
            pos,
            completed: false,
        }
    }

    pub(crate) fn error(&mut self, message: &str) {
        let range = self.current_range();
        self.errors.push(ParseError {
            range,
            message: message.to_string(),
        });
    }

    // Emit error and skip one token wrapped in ErrorNode.
    pub(crate) fn error_bump(&mut self, message: &str) {
        self.error(message);
        if !self.at(SyntaxKind::Eof) {
            let m = self.start();
            self.bump();
            m.complete(self, SyntaxKind::ErrorNode);
        }
    }

    pub(crate) fn at_end(&self) -> bool {
        self.current() == SyntaxKind::Eof
    }

    // Consume all remaining trivia tokens individually. Call before closing
    // the root node so trailing whitespace/comments end up inside it.
    pub(crate) fn eat_remaining_trivia(&mut self) {
        while self.pos < self.tokens.len() && is_trivia(self.tokens[self.pos].kind) {
            self.events.push(Event::Token { n_raw_tokens: 1 });
            self.pos += 1;
        }
    }

    pub(crate) fn finish(self) -> (Vec<Event>, Vec<ParseError>) {
        (self.events, self.errors)
    }

    fn current_range(&self) -> TextRange {
        let mut p = self.pos;
        while p < self.tokens.len() && is_trivia(self.tokens[p].kind) {
            p += 1;
        }
        if p < self.tokens.len() && self.tokens[p].kind != SyntaxKind::Eof {
            let start = self.offsets[p];
            let end = self.offsets[p + 1];
            TextRange::new(TextSize::new(start), TextSize::new(end))
        } else {
            let end = match self.offsets.last() {
                Some(&v) => v,
                None => 0,
            };
            TextRange::new(TextSize::new(end), TextSize::new(end))
        }
    }
}

impl Marker {
    pub(crate) fn complete(mut self, p: &mut Parser, kind: SyntaxKind) -> CompletedMarker {
        self.completed = true;
        if let Event::Start { kind: k, .. } = &mut p.events[self.pos as usize] {
            *k = kind;
        }
        p.events.push(Event::Finish);
        CompletedMarker { pos: self.pos }
    }

    pub(crate) fn abandon(mut self, p: &mut Parser) {
        self.completed = true;
        if self.pos as usize == p.events.len() - 1
            && let Some(Event::Start { .. }) = p.events.last()
        {
            p.events.pop();
        }
    }
}

impl Drop for Marker {
    fn drop(&mut self) {
        debug_assert!(self.completed, "Marker must be completed or abandoned");
    }
}

impl CompletedMarker {
    pub(crate) fn precede(self, p: &mut Parser) -> Marker {
        #[allow(clippy::cast_possible_truncation)]
        let new_pos = p.events.len() as u32;
        p.events.push(Event::Start {
            kind: SyntaxKind::ErrorNode,
            forward_parent: None,
        });
        if let Event::Start { forward_parent, .. } = &mut p.events[self.pos as usize] {
            *forward_parent = Some(new_pos - self.pos);
        }
        Marker {
            pos: new_pos,
            completed: false,
        }
    }
}

pub(crate) fn is_trivia(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Whitespace | SyntaxKind::LineComment | SyntaxKind::BlockComment
    )
}

fn token_name(kind: SyntaxKind) -> &'static str {
    match kind {
        SyntaxKind::Semicolon => "`;`",
        SyntaxKind::Comma => "`,`",
        SyntaxKind::LParen => "`(`",
        SyntaxKind::RParen => "`)`",
        SyntaxKind::LBrace => "`{`",
        SyntaxKind::RBrace => "`}`",
        SyntaxKind::LBracket => "`[`",
        SyntaxKind::RBracket => "`]`",
        SyntaxKind::Colon => "`:`",
        SyntaxKind::Hash => "`#`",
        SyntaxKind::Assign => "`=`",
        SyntaxKind::Ident => "identifier",
        SyntaxKind::ModuleKw => "`module`",
        SyntaxKind::EndmoduleKw => "`endmodule`",
        SyntaxKind::BeginKw => "`begin`",
        SyntaxKind::EndKw => "`end`",
        SyntaxKind::EndcaseKw => "`endcase`",
        _ => "token",
    }
}
