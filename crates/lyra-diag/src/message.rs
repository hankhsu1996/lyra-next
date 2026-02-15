use smol_str::SmolStr;

/// Identifies the template for a diagnostic message.
///
/// Each variant corresponds to a fixed message template. Arguments
/// (in `Message::args`) fill placeholders at render time.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MessageId {
    ParseError,
    PreprocessError,
    UnresolvedName,
    DuplicateDefinition,
    DuplicateModuleDefinition,
    // Label messages
    NotFoundInScope,
    RedefinedHere,
    FirstDefinedHere,
}

/// A typed argument that fills a placeholder in a message template.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Arg {
    Name(SmolStr),
}

impl Arg {
    /// Extract the inner `&str` if this is a `Name` variant.
    pub fn as_name(&self) -> Option<&str> {
        match self {
            Arg::Name(s) => Some(s.as_str()),
        }
    }
}

/// A structured message: template id plus arguments.
///
/// No pre-rendered text -- call `render_message()` at the presentation
/// boundary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Message {
    pub id: MessageId,
    pub args: Box<[Arg]>,
}

impl Message {
    pub fn new(id: MessageId, args: impl Into<Box<[Arg]>>) -> Self {
        Self {
            id,
            args: args.into(),
        }
    }

    /// Convenience for messages with no arguments.
    pub fn simple(id: MessageId) -> Self {
        Self {
            id,
            args: Box::new([]),
        }
    }
}

/// Render a `Message` to a human-readable string.
pub fn render_message(msg: &Message) -> String {
    let name = || msg.args.first().and_then(Arg::as_name).unwrap_or("?");
    match msg.id {
        MessageId::UnresolvedName => format!("unresolved name `{}`", name()),
        MessageId::DuplicateDefinition => format!("duplicate definition of `{}`", name()),
        MessageId::DuplicateModuleDefinition => format!("duplicate module definition `{}`", name()),
        MessageId::NotFoundInScope => "not found in this scope".into(),
        MessageId::RedefinedHere => "redefined here".into(),
        MessageId::FirstDefinedHere => "first defined here".into(),
        MessageId::ParseError | MessageId::PreprocessError => msg
            .args
            .first()
            .and_then(Arg::as_name)
            .map(String::from)
            .unwrap_or_default(),
    }
}
