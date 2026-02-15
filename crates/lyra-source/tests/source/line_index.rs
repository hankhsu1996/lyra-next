use lyra_source::{LineCol, LineIndex, TextSize};

#[test]
fn empty_text_has_one_line() {
    let idx = LineIndex::new("");
    assert_eq!(idx.line_count(), 1);
    assert_eq!(idx.line_col(TextSize::new(0)), LineCol { line: 0, col: 0 });
}

#[test]
fn single_line_no_newline() {
    let idx = LineIndex::new("hello");
    assert_eq!(idx.line_count(), 1);
    assert_eq!(idx.line_col(TextSize::new(0)), LineCol { line: 0, col: 0 });
    assert_eq!(idx.line_col(TextSize::new(3)), LineCol { line: 0, col: 3 });
    assert_eq!(idx.line_col(TextSize::new(5)), LineCol { line: 0, col: 5 });
}

#[test]
fn multi_line() {
    let idx = LineIndex::new("aaa\nbbb\nccc");
    assert_eq!(idx.line_count(), 3);
    // Line 0: "aaa\n" starts at 0
    assert_eq!(idx.line_col(TextSize::new(0)), LineCol { line: 0, col: 0 });
    assert_eq!(idx.line_col(TextSize::new(2)), LineCol { line: 0, col: 2 });
    // Offset 3 is the '\n' itself -- still on line 0
    assert_eq!(idx.line_col(TextSize::new(3)), LineCol { line: 0, col: 3 });
    // Line 1: "bbb\n" starts at 4
    assert_eq!(idx.line_col(TextSize::new(4)), LineCol { line: 1, col: 0 });
    assert_eq!(idx.line_col(TextSize::new(6)), LineCol { line: 1, col: 2 });
    // Line 2: "ccc" starts at 8
    assert_eq!(idx.line_col(TextSize::new(8)), LineCol { line: 2, col: 0 });
    assert_eq!(idx.line_col(TextSize::new(10)), LineCol { line: 2, col: 2 });
}

#[test]
fn offset_at_newline_char() {
    // The newline byte belongs to the line before it
    let idx = LineIndex::new("ab\ncd\n");
    assert_eq!(idx.line_count(), 3);
    // '\n' at offset 2 is on line 0
    assert_eq!(idx.line_col(TextSize::new(2)), LineCol { line: 0, col: 2 });
    // '\n' at offset 5 is on line 1
    assert_eq!(idx.line_col(TextSize::new(5)), LineCol { line: 1, col: 2 });
    // Line 2 starts at 6 (empty trailing line)
    assert_eq!(idx.line_col(TextSize::new(6)), LineCol { line: 2, col: 0 });
}

#[test]
fn offset_roundtrip() {
    let text = "first\nsecond\nthird";
    let idx = LineIndex::new(text);
    for i in 0..text.len() {
        let offset = TextSize::new(i as u32);
        let lc = idx.line_col(offset);
        assert_eq!(idx.offset(lc), Some(offset), "roundtrip failed at {i}");
    }
}

#[test]
fn last_line_no_trailing_newline() {
    let idx = LineIndex::new("a\nb");
    assert_eq!(idx.line_count(), 2);
    assert_eq!(idx.line_col(TextSize::new(2)), LineCol { line: 1, col: 0 });
    assert_eq!(idx.line_col(TextSize::new(3)), LineCol { line: 1, col: 1 });
}

#[test]
fn out_of_range_offset_clamps_to_eof() {
    let idx = LineIndex::new("ab");
    // Offset 10 is well past end -- should clamp to EOF (line 0, col 2)
    assert_eq!(idx.line_col(TextSize::new(10)), LineCol { line: 0, col: 2 });
}

#[test]
fn offset_returns_none_for_out_of_range_line() {
    let idx = LineIndex::new("ab\ncd");
    assert!(idx.offset(LineCol { line: 5, col: 0 }).is_none());
}

#[test]
fn offset_returns_none_for_out_of_range_col() {
    let idx = LineIndex::new("ab\ncd");
    // Line 0 is "ab\n" (3 bytes including newline), col 4 is past end
    assert!(idx.offset(LineCol { line: 0, col: 4 }).is_none());
    // Line 1 is "cd" (2 bytes), col 3 is past end
    assert!(idx.offset(LineCol { line: 1, col: 3 }).is_none());
}

#[test]
fn offset_returns_none_for_col_overflow() {
    let idx = LineIndex::new("ab\ncd");
    // u32::MAX col would overflow when added to a nonzero line start
    assert!(
        idx.offset(LineCol {
            line: 1,
            col: u32::MAX
        })
        .is_none()
    );
}

#[test]
fn crlf_line_split_on_lf() {
    let idx = LineIndex::new("ab\r\ncd");
    assert_eq!(idx.line_count(), 2);
    // '\r' at offset 2 is on line 0 (col 2)
    assert_eq!(idx.line_col(TextSize::new(2)), LineCol { line: 0, col: 2 });
    // '\n' at offset 3 is still line 0 (col 3)
    assert_eq!(idx.line_col(TextSize::new(3)), LineCol { line: 0, col: 3 });
    // Line 1 starts at offset 4
    assert_eq!(idx.line_col(TextSize::new(4)), LineCol { line: 1, col: 0 });
}
