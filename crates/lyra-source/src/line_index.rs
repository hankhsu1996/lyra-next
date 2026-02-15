use crate::TextSize;

/// A line/column position, both 0-indexed.
///
/// `line` is the 0-based line number. `col` is the byte offset from the
/// start of that line (also 0-based).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineCol {
    pub line: u32,
    pub col: u32,
}

/// Maps byte offsets to line/column positions.
///
/// Built once per file text. Lines are 0-indexed, columns are byte offsets
/// within a line (also 0-indexed).
///
/// Line breaks are detected by `\n` (LF). For CRLF input, `\r` counts as a
/// regular byte in column math and `\n` starts the next line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineIndex {
    /// Byte offset of the start of each line. First entry is always 0.
    line_starts: Vec<TextSize>,
    /// Total length of the source text in bytes.
    len: TextSize,
}

impl LineIndex {
    /// Build a `LineIndex` by scanning `text` for newlines.
    ///
    /// Empty text has exactly 1 line with start offset 0.
    pub fn new(text: &str) -> Self {
        let mut line_starts = vec![TextSize::new(0)];
        let mut pos = TextSize::new(0);
        for b in text.bytes() {
            pos += TextSize::new(1);
            if b == b'\n' {
                line_starts.push(pos);
            }
        }
        Self {
            line_starts,
            len: TextSize::of(text),
        }
    }

    /// Convert a byte offset to a line/column position.
    ///
    /// Offsets beyond the end of text are clamped to the EOF position.
    pub fn line_col(&self, offset: TextSize) -> LineCol {
        let offset = std::cmp::min(offset, self.len);
        let line = self.line_starts.partition_point(|&start| start <= offset) - 1;
        let col = u32::from(offset) - u32::from(self.line_starts[line]);
        LineCol {
            line: line as u32,
            col,
        }
    }

    /// Convert a line/column position back to a byte offset.
    ///
    /// Returns `None` if `line` is out of range or `col` is past the end
    /// of the line.
    pub fn offset(&self, lc: LineCol) -> Option<TextSize> {
        let line = lc.line as usize;
        let start = *self.line_starts.get(line)?;
        let end = self.line_starts.get(line + 1).copied().unwrap_or(self.len);
        let raw = u32::from(start).checked_add(lc.col)?;
        let offset = TextSize::new(raw);
        if offset > end {
            return None;
        }
        Some(offset)
    }

    /// Number of lines in the source text.
    pub fn line_count(&self) -> usize {
        self.line_starts.len()
    }
}
