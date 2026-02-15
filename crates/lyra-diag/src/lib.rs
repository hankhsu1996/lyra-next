mod code;
mod diagnostic;
mod label;
pub mod message;

pub use code::DiagnosticCode;
pub use diagnostic::{Diagnostic, Severity};
pub use label::{Label, LabelKind, TextEdit};
pub use message::{Arg, Message, MessageId, render_message};
