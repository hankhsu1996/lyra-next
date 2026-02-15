use lyra_diag::{Arg, Message, MessageId, render_message};

#[test]
fn render_unresolved_name() {
    let msg = Message::new(MessageId::UnresolvedName, vec![Arg::Name("foo".into())]);
    assert_eq!(render_message(&msg), "unresolved name `foo`");
}

#[test]
fn render_duplicate_definition() {
    let msg = Message::new(
        MessageId::DuplicateDefinition,
        vec![Arg::Name("bar".into())],
    );
    assert_eq!(render_message(&msg), "duplicate definition of `bar`");
}

#[test]
fn render_label_messages() {
    assert_eq!(
        render_message(&Message::simple(MessageId::NotFoundInScope)),
        "not found in this scope"
    );
    assert_eq!(
        render_message(&Message::simple(MessageId::RedefinedHere)),
        "redefined here"
    );
    assert_eq!(
        render_message(&Message::simple(MessageId::FirstDefinedHere)),
        "first defined here"
    );
}

#[test]
fn render_parse_error_freeform() {
    let msg = Message::new(
        MessageId::ParseError,
        vec![Arg::Name("expected semicolon".into())],
    );
    assert_eq!(render_message(&msg), "expected semicolon");
}

#[test]
fn render_parse_error_no_args() {
    let msg = Message::simple(MessageId::ParseError);
    assert_eq!(render_message(&msg), "");
}
