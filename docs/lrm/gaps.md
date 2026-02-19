# LRM Gaps

Known gaps between LRM requirements and current engine capabilities. This is the work queue for LRM signoff. See `docs/lrm-signoff.md` for methodology.

When you discover a gap during `/lrm-add`, add an entry here. When you fix the gap and add the passing test, remove the entry. Both changes land in the same PR.

## Chapter 5: Lexical conventions

### 5.10 Keyed struct pattern name resolution

Keys in keyed assignment patterns (`'{a:0, b:1}`) are parsed as `NameRef` nodes. The semantic builder collects them as use sites and the resolver reports false "unresolved name" errors because it does not recognize them as struct field references. Blocked by: semantic layer support for assignment pattern context.

Test affected: `lrm/ch05/structure_literals` (annotations pin the current false-positive errors)
