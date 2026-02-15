use lyra_source::Span;

use crate::code::DiagnosticCode;
use crate::label::{Label, LabelKind, TextEdit};
use crate::message::{self, Message};

/// Severity level for a diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Info,
}

/// A structured diagnostic with code identity, labels, notes, and fixits.
///
/// The primary span is derived from the first `Primary` label -- there is
/// no redundant stored `span` field.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: DiagnosticCode,
    pub message: Message,
    pub labels: Vec<Label>,
    pub notes: Vec<Message>,
    pub fixits: Vec<TextEdit>,
}

impl Diagnostic {
    pub fn new(severity: Severity, code: DiagnosticCode, message: Message) -> Self {
        Self {
            severity,
            code,
            message,
            labels: Vec::new(),
            notes: Vec::new(),
            fixits: Vec::new(),
        }
    }

    #[must_use]
    pub fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    #[must_use]
    pub fn with_note(mut self, note: Message) -> Self {
        self.notes.push(note);
        self
    }

    #[must_use]
    pub fn with_fixit(mut self, edit: TextEdit) -> Self {
        self.fixits.push(edit);
        self
    }

    /// Primary span -- derived from the first `Primary` label.
    ///
    /// Returns `None` only if labels is empty (should not happen for
    /// well-formed diagnostics constructed via the builder).
    pub fn primary_span(&self) -> Option<Span> {
        self.labels
            .iter()
            .find(|l| l.kind == LabelKind::Primary)
            .map(|l| l.span)
    }

    /// Backward-compat convenience: returns primary span.
    pub fn span(&self) -> Option<Span> {
        self.primary_span()
    }

    /// Render headline message to string.
    pub fn render_message(&self) -> String {
        message::render_message(&self.message)
    }
}
