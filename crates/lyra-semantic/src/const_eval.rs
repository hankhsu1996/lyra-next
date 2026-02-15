/// Placeholder for constant expression evaluation.
///
/// Will evaluate parameter expressions, generate-loop bounds, etc.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstValue {
    Int(i64),
    Unknown,
}
