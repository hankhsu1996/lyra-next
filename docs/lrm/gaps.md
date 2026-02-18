# LRM Gaps

Known gaps between LRM requirements and current engine capabilities. This is the work queue for LRM signoff. See `docs/lrm-signoff.md` for methodology.

When you discover a gap during `/lrm-add`, add an entry here. When you fix the gap and add the passing test, remove the entry. Both changes land in the same PR.

## Chapter 5: Lexical conventions

### 5.12 Attributes

The parser does not recognize attribute instances (`(* attr_name *)`, `(* attr_name = value *)`). Attributes can appear as prefixes on declarations, module items, statements, and port connections, and as suffixes on operators and function calls. Blocked by: parser support for `(* ... *)` syntax.

Test to add: `lrm_ch05_attributes`
