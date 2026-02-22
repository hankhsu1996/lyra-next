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
    ExplicitImportConflictsWithLocal,
    ExplicitConflictsWithWildcard,
    // Type check messages
    WidthMismatch,
    BitsWide,
    BitsNonDataType,
    NotADataType,
    UndeclaredType,
    NotAType,
    UnsupportedTaggedUnion,
    IllegalEnumBaseType,
    EnumBaseDimsNotConstant,
    EnumRangeBoundNotEvaluable,
    EnumRangeCountNegative,
    EnumRangeTooLarge,
    EnumAssignFromNonEnum,
    EnumAssignWrongEnum,
    EnumTypeHere,
    ConversionArgCategory,
    ConversionWidthMismatch,
    PackedUnionWidthMismatch,
    ExpectedMemberWidth,
    ModportDirectionViolation,
    ModportRefUnsupported,
    // Elaboration messages
    UnresolvedModuleInst,
    NotInstantiable,
    UnknownPort,
    DuplicatePortConn,
    TooManyPositionalPorts,
    MissingPortConn,
    PortWidthMismatch,
    ElabRecursionLimit,
    UnknownParam,
    DuplicateParamOverride,
    TooManyPositionalParams,
    ParamNotConst,
    GenCondNotConst,
    GenvarNotConst,
    DuplicateGenBlockName,
    GenerateIterationLimit,
    WildcardLocalConflict,
    // Label messages
    RealizedHere,
    WildcardImportHere,
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
        MessageId::ExplicitImportConflictsWithLocal => {
            let sym_name = name();
            let pkg = msg.args.get(1).and_then(Arg::as_name).unwrap_or("?");
            format!("import of `{sym_name}` from package `{pkg}` conflicts with local declaration")
        }
        MessageId::ExplicitConflictsWithWildcard => {
            let sym_name = name();
            let explicit_pkg = msg.args.get(1).and_then(Arg::as_name).unwrap_or("?");
            let wildcard_pkg = msg.args.get(2).and_then(Arg::as_name).unwrap_or("?");
            format!(
                "import of `{sym_name}` from package `{explicit_pkg}` conflicts with wildcard import from package `{wildcard_pkg}`"
            )
        }
        MessageId::WidthMismatch
        | MessageId::BitsWide
        | MessageId::UndeclaredType
        | MessageId::NotAType
        | MessageId::BitsNonDataType
        | MessageId::NotADataType
        | MessageId::UnsupportedTaggedUnion
        | MessageId::IllegalEnumBaseType
        | MessageId::EnumBaseDimsNotConstant
        | MessageId::EnumRangeBoundNotEvaluable
        | MessageId::EnumRangeCountNegative
        | MessageId::EnumRangeTooLarge
        | MessageId::EnumAssignFromNonEnum
        | MessageId::EnumAssignWrongEnum
        | MessageId::EnumTypeHere
        | MessageId::ConversionArgCategory
        | MessageId::ConversionWidthMismatch
        | MessageId::PackedUnionWidthMismatch
        | MessageId::ExpectedMemberWidth
        | MessageId::ModportDirectionViolation
        | MessageId::ModportRefUnsupported => render_type_message(msg),
        MessageId::WildcardLocalConflict => {
            let sym_name = name();
            let pkg = msg.args.get(1).and_then(Arg::as_name).unwrap_or("?");
            format!(
                "local declaration of `{sym_name}` conflicts with wildcard import from package `{pkg}`"
            )
        }
        MessageId::RealizedHere => {
            let sym_name = name();
            format!("wildcard import of `{sym_name}` realized here")
        }
        MessageId::WildcardImportHere => "wildcard import here".into(),
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
        _ => render_elab_message(msg),
    }
}

fn render_type_message(msg: &Message) -> String {
    let name = || msg.args.first().and_then(Arg::as_name).unwrap_or("?");
    match msg.id {
        MessageId::WidthMismatch => {
            let lhs_w = msg.args.first().and_then(Arg::as_width).unwrap_or(0);
            let rhs_w = msg.args.get(1).and_then(Arg::as_width).unwrap_or(0);
            format!("implicit truncation: {rhs_w}-bit value assigned to {lhs_w}-bit target")
        }
        MessageId::BitsWide => {
            let w = msg.args.first().and_then(Arg::as_width).unwrap_or(0);
            format!("{w} bits")
        }
        MessageId::UndeclaredType => format!("undeclared type `{}`", name()),
        MessageId::NotAType => format!("`{}` is not a type", name()),
        MessageId::BitsNonDataType => "$bits argument is not a data type".into(),
        MessageId::NotADataType => "not a data type".into(),
        MessageId::UnsupportedTaggedUnion => "tagged unions are not yet supported".into(),
        MessageId::IllegalEnumBaseType => {
            format!("enum base type `{}` is not an integral type", name())
        }
        MessageId::EnumBaseDimsNotConstant => {
            "enum base type has non-constant packed dimensions".into()
        }
        MessageId::EnumRangeBoundNotEvaluable => {
            "enum member range bound is not a constant expression".into()
        }
        MessageId::EnumRangeCountNegative => {
            let count = msg.args.first().and_then(Arg::as_name).unwrap_or("?");
            format!("enum member range count is negative ({count})")
        }
        MessageId::EnumRangeTooLarge => {
            let count = msg.args.first().and_then(Arg::as_name).unwrap_or("?");
            format!("enum member range count is too large ({count})")
        }
        MessageId::EnumAssignFromNonEnum => {
            let rhs_ty = name();
            let lhs_name = msg.args.get(1).and_then(Arg::as_name).unwrap_or("?");
            format!("cannot assign {rhs_ty} to enum `{lhs_name}` without an explicit cast")
        }
        MessageId::EnumAssignWrongEnum => {
            let rhs_name = name();
            let lhs_name = msg.args.get(1).and_then(Arg::as_name).unwrap_or("?");
            format!("cannot assign enum `{rhs_name}` to enum `{lhs_name}` without an explicit cast")
        }
        MessageId::EnumTypeHere => {
            format!("enum type `{}`", name())
        }
        MessageId::ConversionArgCategory | MessageId::ConversionWidthMismatch => {
            render_conversion_message(msg)
        }
        MessageId::PackedUnionWidthMismatch => {
            let field = name();
            let actual = msg.args.get(1).and_then(Arg::as_width).unwrap_or(0);
            let expected = msg.args.get(2).and_then(Arg::as_width).unwrap_or(0);
            format!("packed union member `{field}` is {actual} bits, expected {expected} bits")
        }
        MessageId::ExpectedMemberWidth => {
            let w = msg.args.first().and_then(Arg::as_width).unwrap_or(0);
            format!("expected width: {w} bits")
        }
        MessageId::ModportDirectionViolation => {
            let direction = name();
            let access = msg.args.get(1).and_then(Arg::as_name).unwrap_or("?");
            format!("modport member declared '{direction}' cannot be used in {access} context")
        }
        MessageId::ModportRefUnsupported => {
            "direction enforcement for 'ref' modport members is not yet supported".into()
        }
        _ => String::new(),
    }
}

fn render_conversion_message(msg: &Message) -> String {
    let name = || msg.args.first().and_then(Arg::as_name).unwrap_or("?");
    match msg.id {
        MessageId::ConversionArgCategory => {
            let expected = msg.args.get(1).and_then(Arg::as_name).unwrap_or("?");
            format!("{} requires {expected} argument", name())
        }
        MessageId::ConversionWidthMismatch => {
            let expected = msg.args.get(1).and_then(Arg::as_width).unwrap_or(0);
            let actual = msg.args.get(2).and_then(Arg::as_width).unwrap_or(0);
            format!(
                "{} requires {expected}-bit argument, got {actual}-bit",
                name()
            )
        }
        _ => String::new(),
    }
}

fn render_elab_message(msg: &Message) -> String {
    let name = || msg.args.first().and_then(Arg::as_name).unwrap_or("?");
    match msg.id {
        MessageId::UnresolvedModuleInst => format!("module `{}` not found", name()),
        MessageId::NotInstantiable => {
            format!("`{}` is not an instantiable design unit", name())
        }
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
        MessageId::UnknownParam => {
            let param = name();
            let module = msg.args.get(1).and_then(Arg::as_name).unwrap_or("?");
            format!("parameter `{param}` not found on module `{module}`")
        }
        MessageId::DuplicateParamOverride => {
            format!("parameter `{}` overridden more than once", name())
        }
        MessageId::TooManyPositionalParams => {
            let expected = msg.args.first().and_then(Arg::as_count).unwrap_or(0);
            let got = msg.args.get(1).and_then(Arg::as_count).unwrap_or(0);
            format!("too many positional parameter overrides: expected {expected}, got {got}")
        }
        MessageId::ParamNotConst => {
            format!(
                "parameter `{}` override is not a constant expression",
                name()
            )
        }
        MessageId::GenCondNotConst => "generate condition is not a constant expression".into(),
        MessageId::GenvarNotConst => "genvar expression is not constant".into(),
        MessageId::DuplicateGenBlockName => {
            format!("duplicate generate block name `{}`", name())
        }
        MessageId::GenerateIterationLimit => {
            let limit = msg.args.first().and_then(Arg::as_count).unwrap_or(0);
            format!("generate-for iteration limit ({limit}) reached")
        }
        _ => String::new(),
    }
}
