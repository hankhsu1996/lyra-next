/// Placeholder type representation for `SystemVerilog` types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    /// Bit-vector with width.
    Logic(u32),
    /// Integer type.
    Integer,
    /// Unknown / not yet resolved.
    Unknown,
}
