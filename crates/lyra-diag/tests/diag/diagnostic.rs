use lyra_diag::{Arg, Diagnostic, DiagnosticCode, Label, LabelKind, Message, MessageId, Severity};
use lyra_source::{FileId, Span, TextRange, TextSize};

fn dummy_span() -> Span {
    Span {
        file: FileId(0),
        range: TextRange::new(TextSize::new(0), TextSize::new(5)),
    }
}

#[test]
fn primary_span_returns_first_primary() {
    let d = Diagnostic::new(
        Severity::Error,
        DiagnosticCode::UNRESOLVED_NAME,
        Message::new(MessageId::UnresolvedName, vec![Arg::Name("x".into())]),
    )
    .with_label(Label {
        kind: LabelKind::Primary,
        span: dummy_span(),
        message: Message::simple(MessageId::NotFoundInScope),
    });

    let span = d.primary_span().expect("should have primary span");
    assert_eq!(span.file, FileId(0));
}

#[test]
fn primary_span_skips_secondary() {
    let secondary_span = Span {
        file: FileId(1),
        range: TextRange::new(TextSize::new(10), TextSize::new(15)),
    };
    let primary_span = dummy_span();

    let d = Diagnostic::new(
        Severity::Error,
        DiagnosticCode::DUPLICATE_DEFINITION,
        Message::new(MessageId::DuplicateDefinition, vec![Arg::Name("x".into())]),
    )
    .with_label(Label {
        kind: LabelKind::Secondary,
        span: secondary_span,
        message: Message::simple(MessageId::FirstDefinedHere),
    })
    .with_label(Label {
        kind: LabelKind::Primary,
        span: primary_span,
        message: Message::simple(MessageId::RedefinedHere),
    });

    let span = d.primary_span().expect("should have primary span");
    assert_eq!(span.file, FileId(0));
}

#[test]
fn empty_labels_returns_none() {
    let d = Diagnostic::new(
        Severity::Warning,
        DiagnosticCode::PARSE_ERROR,
        Message::simple(MessageId::ParseError),
    );
    assert!(d.primary_span().is_none());
}

#[test]
fn render_message_delegates() {
    let d = Diagnostic::new(
        Severity::Error,
        DiagnosticCode::UNRESOLVED_NAME,
        Message::new(MessageId::UnresolvedName, vec![Arg::Name("clk".into())]),
    );
    assert_eq!(d.render_message(), "unresolved name `clk`");
}
