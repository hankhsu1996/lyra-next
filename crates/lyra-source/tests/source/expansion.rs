use lyra_source::{ExpansionFrame, ExpansionKind, FileId, FileLoc, Span, TextRange, TextSize};

#[test]
fn file_loc_equality() {
    let a = FileLoc {
        file: FileId(0),
        offset: TextSize::new(10),
    };
    let b = FileLoc {
        file: FileId(0),
        offset: TextSize::new(10),
    };
    let c = FileLoc {
        file: FileId(1),
        offset: TextSize::new(10),
    };
    assert_eq!(a, b);
    assert_ne!(a, c);
}

#[test]
fn file_loc_hash() {
    use std::collections::HashSet;
    let mut set = HashSet::new();
    let loc = FileLoc {
        file: FileId(0),
        offset: TextSize::new(5),
    };
    set.insert(loc);
    assert!(set.contains(&loc));
}

#[test]
fn expansion_frame_construction() {
    let call_site = Span {
        file: FileId(0),
        range: TextRange::new(TextSize::new(12), TextSize::new(28)),
    };
    let spelling = FileLoc {
        file: FileId(1),
        offset: TextSize::new(0),
    };
    let frame = ExpansionFrame {
        kind: ExpansionKind::Include,
        call_site,
        spelling,
    };
    assert_eq!(frame.kind, ExpansionKind::Include);
    assert_eq!(frame.call_site.file, FileId(0));
    assert_eq!(frame.spelling.file, FileId(1));
    assert_eq!(frame.spelling.offset, TextSize::new(0));
}
