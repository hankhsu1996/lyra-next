# Quality Gaps

This file tracks non-LRM technical debt that risks determinism, incremental
stability, or architectural drift. These are not failing tests yet, but they
need explicit resolution and a ratchet to prevent regressions.

When a gap is identified, it should be described here and fixed in a
follow-up PR. The north star reference is in `docs/architecture.md`.

When a gap is fully resolved, remove it from this file. Only open work
belongs here -- git history has the record of what was fixed.

## Entries

1. File size warnings (blocks future growth)
   - Problem: 5 files exceed 800-line soft limit. One is within 60 lines of the 1200-line hard limit.
   - Files:
     1. `lyra-semantic/src/resolve.rs` -- 1141 lines (59 from hard limit)
     2. `lyra-ast/src/nodes.rs` -- 1090 lines
     3. `lyra-db/src/elab_queries.rs` -- 1065 lines
     4. `lyra-db/src/diagnostics.rs` -- 1058 lines
     5. `lyra-db/tests/expr_type/members.rs` -- 979 lines (test file, lower priority)
   - Enforcement: `tools/policy/check_lines.py` (L001 hard fail at 1200, L002 warning at 800).
