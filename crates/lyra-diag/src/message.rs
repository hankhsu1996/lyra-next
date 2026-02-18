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
    DuplicateDefinitionInUnit,
    PackageNotFound,
    MemberNotFound,
    AmbiguousWildcardImport,
    UnsupportedQualifiedPath,
    // Type check messages
    WidthMismatch,
    BitsWide,
    UndeclaredType,
    NotAType,
    // Elaboration messages
    UnresolvedModuleInst,
    NotAModule,
    UnknownPort,
    DuplicatePortConn,
    TooManyPositionalPorts,
    MissingPortConn,
    PortWidthMismatch,
    ElabRecursionLimit,
    // Label messages
    NotFoundInScope,
    NotFoundAsType,
    ValueNotType,
    RedefinedHere,
    FirstDefinedHere,
}

/// A typed argument that fills a placeholder in a message template.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Arg {
    Name(SmolStr),
    Width(u32),
    Count(usize),
}

impl Arg {
    /// Extract the inner `&str` if this is a `Name` variant.
    pub fn as_name(&self) -> Option<&str> {
        match self {
            Arg::Name(s) => Some(s.as_str()),
            _ => None,
        }
    }

    /// Extract the inner `u32` if this is a `Width` variant.
    pub fn as_width(&self) -> Option<u32> {
        match self {
            Arg::Width(w) => Some(*w),
            _ => None,
        }
    }

    /// Extract the inner `usize` if this is a `Count` variant.
    pub fn as_count(&self) -> Option<usize> {
        match self {
            Arg::Count(c) => Some(*c),
            _ => None,
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
        MessageId::DuplicateDefinitionInUnit => format!("duplicate definition `{}`", name()),
        MessageId::PackageNotFound => format!("package `{}` not found", name()),
        MessageId::MemberNotFound => {
            let pkg = name();
            let member = msg.args.get(1).and_then(Arg::as_name).unwrap_or("?");
            format!("member `{member}` not found in package `{pkg}`")
        }
        MessageId::AmbiguousWildcardImport => {
            let sym_name = name();
            let pkgs = msg.args.get(1).and_then(Arg::as_name).unwrap_or("?");
            format!("name `{sym_name}` is ambiguous: imported from packages {pkgs}")
        }
        MessageId::UnsupportedQualifiedPath => {
            format!("qualified path `{}` is not supported", name())
        }
        MessageId::WidthMismatch => {
            let lhs_w = msg.args.first().and_then(Arg::as_width).unwrap_or(0);
            let rhs_w = msg.args.get(1).and_then(Arg::as_width).unwrap_or(0);
            format!("width mismatch in assignment: LHS is {lhs_w} bits, RHS is {rhs_w} bits")
        }
        MessageId::BitsWide => {
            let w = msg.args.first().and_then(Arg::as_width).unwrap_or(0);
            format!("{w} bits")
        }
        MessageId::UndeclaredType => format!("undeclared type `{}`", name()),
        MessageId::NotAType => format!("`{}` is not a type", name()),
        MessageId::UnresolvedModuleInst => format!("module `{}` not found", name()),
        MessageId::NotAModule => format!("`{}` is not a module", name()),
        MessageId::UnknownPort => {
            let port = name();
            let module = msg.args.get(1).and_then(Arg::as_name).unwrap_or("?");
            format!("port `{port}` not found on module `{module}`")
        }
        MessageId::DuplicatePortConn => format!("port `{}` connected more than once", name()),
        MessageId::TooManyPositionalPorts => {
            let expected = msg.args.first().and_then(Arg::as_count).unwrap_or(0);
            let got = msg.args.get(1).and_then(Arg::as_count).unwrap_or(0);
            format!("too many positional ports: expected {expected}, got {got}")
        }
        MessageId::MissingPortConn => {
            let port = name();
            let module = msg.args.get(1).and_then(Arg::as_name).unwrap_or("?");
            format!("port `{port}` not connected on module `{module}`")
        }
        MessageId::PortWidthMismatch => {
            let port = name();
            let formal_w = msg.args.get(1).and_then(Arg::as_width).unwrap_or(0);
            let actual_w = msg.args.get(2).and_then(Arg::as_width).unwrap_or(0);
            format!("port `{port}`: formal is {formal_w} bits, actual is {actual_w} bits")
        }
        MessageId::ElabRecursionLimit => "elaboration recursion limit reached".into(),
        MessageId::NotFoundInScope => "not found in this scope".into(),
        MessageId::NotFoundAsType => "not found as a type in this scope".into(),
        MessageId::ValueNotType => "this is a value, not a type".into(),
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
