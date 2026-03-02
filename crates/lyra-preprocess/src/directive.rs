/// A recognized LRM Section 22 directive keyword.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DirectiveKeyword {
    BeginKeywords,
    Celldefine,
    DefaultNettype,
    Define,
    Else,
    Elsif,
    EndKeywords,
    Endcelldefine,
    Endif,
    Ifdef,
    Ifndef,
    Include,
    Line,
    NounconnectedDrive,
    Pragma,
    Resetall,
    Timescale,
    UnconnectedDrive,
    Undef,
}

/// Three-way classification of a directive token.
pub(crate) enum DirectiveClass<'a> {
    /// Backtick + valid identifier, not a known directive keyword.
    MacroInvoke { name: &'a str },
    /// Recognized LRM directive keyword.
    Keyword(DirectiveKeyword),
    /// Malformed or unrecognized form (not valid identifier after backtick).
    Other { raw: &'a str },
}

/// Classify a raw directive token text (including the backtick prefix).
pub(crate) fn classify_directive(raw: &str) -> DirectiveClass<'_> {
    let name = raw.strip_prefix('`').unwrap_or("");
    if name.is_empty() {
        return DirectiveClass::Other { raw };
    }

    let mut chars = name.chars();
    let first = chars.next().unwrap_or('\0');
    if !first.is_ascii_alphabetic() && first != '_' {
        return DirectiveClass::Other { raw };
    }
    for c in chars {
        if !c.is_ascii_alphanumeric() && c != '_' {
            return DirectiveClass::Other { raw };
        }
    }

    if let Some(kw) = match_keyword(name) {
        return DirectiveClass::Keyword(kw);
    }

    DirectiveClass::MacroInvoke { name }
}

fn match_keyword(name: &str) -> Option<DirectiveKeyword> {
    match name {
        "begin_keywords" => Some(DirectiveKeyword::BeginKeywords),
        "celldefine" => Some(DirectiveKeyword::Celldefine),
        "default_nettype" => Some(DirectiveKeyword::DefaultNettype),
        "define" => Some(DirectiveKeyword::Define),
        "else" => Some(DirectiveKeyword::Else),
        "elsif" => Some(DirectiveKeyword::Elsif),
        "end_keywords" => Some(DirectiveKeyword::EndKeywords),
        "endcelldefine" => Some(DirectiveKeyword::Endcelldefine),
        "endif" => Some(DirectiveKeyword::Endif),
        "ifdef" => Some(DirectiveKeyword::Ifdef),
        "ifndef" => Some(DirectiveKeyword::Ifndef),
        "include" => Some(DirectiveKeyword::Include),
        "line" => Some(DirectiveKeyword::Line),
        "nounconnected_drive" => Some(DirectiveKeyword::NounconnectedDrive),
        "pragma" => Some(DirectiveKeyword::Pragma),
        "resetall" => Some(DirectiveKeyword::Resetall),
        "timescale" => Some(DirectiveKeyword::Timescale),
        "unconnected_drive" => Some(DirectiveKeyword::UnconnectedDrive),
        "undef" => Some(DirectiveKeyword::Undef),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classify_known_directives() {
        assert!(matches!(
            classify_directive("`define"),
            DirectiveClass::Keyword(DirectiveKeyword::Define)
        ));
        assert!(matches!(
            classify_directive("`timescale"),
            DirectiveClass::Keyword(DirectiveKeyword::Timescale)
        ));
        assert!(matches!(
            classify_directive("`include"),
            DirectiveClass::Keyword(DirectiveKeyword::Include)
        ));
    }

    #[test]
    fn classify_macro_invoke() {
        match classify_directive("`FOO") {
            DirectiveClass::MacroInvoke { name } => assert_eq!(name, "FOO"),
            _ => panic!("expected MacroInvoke"),
        }
        match classify_directive("`my_macro") {
            DirectiveClass::MacroInvoke { name } => assert_eq!(name, "my_macro"),
            _ => panic!("expected MacroInvoke"),
        }
    }

    #[test]
    fn classify_malformed() {
        assert!(matches!(
            classify_directive("`"),
            DirectiveClass::Other { .. }
        ));
        assert!(matches!(
            classify_directive("`123"),
            DirectiveClass::Other { .. }
        ));
    }
}
