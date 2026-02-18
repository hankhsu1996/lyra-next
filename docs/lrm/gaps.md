# LRM Gaps

Known gaps between LRM requirements and current engine capabilities. This is the work queue for LRM signoff. See `docs/lrm-signoff.md` for methodology.

When you discover a gap during `/lrm-add`, add an entry here. When you fix the gap and add the passing test, remove the entry. Both changes land in the same PR.

## Ch 5: Lexical conventions

- **Escaped identifiers in declarations** -- parser does not accept `\name` as a variable/signal name in declarations. Blocked by: parser. Planned tests: `lrm_ch05_escaped_identifiers`.
- **`initial begin` / procedural blocks** -- parser does not accept `initial begin...end` in module body. Blocked by: parser. Planned tests: `lrm_ch05_system_names` (needs `$display`, `$finish` in procedural context).
