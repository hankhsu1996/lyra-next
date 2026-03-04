pub mod code;
mod diagnostic;
mod label;
pub mod message;

pub use code::DiagKey;
pub use diagnostic::{Diagnostic, DiagnosticOrigin, Severity};
pub use label::{Label, LabelKind, TextEdit};
pub use message::{Arg, Message, MessageId, render_message};
