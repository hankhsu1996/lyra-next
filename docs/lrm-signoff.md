# LRM Signoff

This document defines what it means to "sign off" an IEEE 1800 LRM chapter, the workflow for getting there, and how gaps are tracked.

## What "Signed Off" Means

A chapter is **signed off** when:

- All chapter requirements that Lyra implements are covered by test cases.
- All failure cases produce the expected diagnostic codes and spans.
- All passing cases produce no unexpected diagnostics.
- At least one cross-feature interaction test exists per chapter.

A chapter is either **Not started**, **In progress**, or **Signed off**. There is no "unsupported" status -- chapters we have not begun are simply "Not started".

## What Counts as Coverage

Each signed-off chapter must have tests covering:

- **Core examples**: representative syntax and semantics from the LRM text.
- **Edge cases**: unusual but valid syntax, boundary conditions, corner cases called out in the LRM.
- **Error cases**: invalid syntax or semantics that must produce diagnostics.
- **Interactions**: at least one test combining the chapter's feature with another feature (e.g., typedef inside a package, array port on an instantiation).

## Test Infrastructure

LRM test cases live in `crates/lyra-tests/testdata/corpus/` as subdirectories. The existing corpus runner (`crates/lyra-tests/tests/corpus.rs`) picks them up automatically. Everything in this directory must pass on main.

Each test case is a directory containing one or more `.sv` files with inline annotations that encode expected diagnostics:

```sv
module top;
  int x = y;
  //        ^ error[lyra.semantic[1]]: unresolved name
endmodule
```

The annotation system supports:

- **File-level annotations**: `// ^ severity[code]: message` (caret points to the column on the previous line)
- **Span annotations**: `// ^~~~ severity[code]` (caret + tildes encode span length)
- **Unit-level annotations**: `// unit severity[code]: message` (for cross-file diagnostics)
- **`// ALLOW-EXTRA-DIAGS`**: opt-in directive for in-progress tests where not all diagnostics are pinned yet

A file with no annotations is a "pass" test -- the runner asserts zero diagnostics. A file with annotations is a "fail" test -- the runner asserts exactly those diagnostics and no others (unless `ALLOW-EXTRA-DIAGS` is set).

## Workflow

### Principle: always green on main

Tests never land on main while failing. If the engine cannot pass a test today, the test does not get committed. Instead, the gap goes into `docs/lrm/gaps.md` so it is not forgotten.

### Adding tests: `/lrm-add <chapter> <case-name>`

Scaffolds a test case in the corpus. Only add tests that pass with current engine capabilities. If you discover a gap (parser does not support a feature, wrong diagnostic, etc.), do not commit a failing test. Instead, add an entry to `docs/lrm/gaps.md` describing what is missing.

### Recording gaps: `docs/lrm/gaps.md`

The gap ledger is hand-maintained and committed. Every time you decide not to add a test because the engine cannot pass it yet, you must add a gaps.md entry. This is the key moment where forgetting happens -- the ledger prevents it.

Each entry records: what LRM feature is missing, what blocks it (parser, semantic, etc.), and what test names will be added once the fix lands.

When you fix an engine gap, add the now-passing tests to the corpus and remove the entry from gaps.md. Both changes land in the same PR.

### Policy: corpus changes require gaps.md review

If `testdata/corpus/lrm_ch*` changes in a PR, check whether `docs/lrm/gaps.md` should also change (new gaps discovered, or gaps resolved). This keeps the ledger in sync with reality.

### Signing off: `/lrm-signoff <chapter>`

The final audit. Reads the LRM chapter, checks corpus coverage against the chapter's requirements, and verifies that gaps.md has no remaining entries for this chapter. Only then does the chapter status move to "Signed off" in `docs/lrm/progress.md`.

### Typical session

```
/lrm-add 5 identifiers_keywords   -- works today, lands green
/lrm-add 5 number_literals        -- works today, lands green
  (escaped identifiers don't parse -- add entry to gaps.md)

... later, fix parser ...

/lrm-add 5 escaped_identifiers    -- now works, lands green
  (remove escaped identifiers entry from gaps.md)

/lrm-signoff 5                    -- audit, no gaps remain, mark signed off
```

### Recommended order

Start with chapters the existing pipeline already handles, then expand as capabilities land:

1. Ch 5 -- Lexical conventions (lexer)
2. Ch 22 -- Compiler directives (preprocessor)
3. Ch 6, 7 -- Data types, aggregate types (type skeleton)
4. Ch 23 -- Modules and hierarchy (name resolution)
5. Ch 26 -- Packages (cross-file resolution)
6. Ch 3 -- Design and verification building blocks
7. Remaining chapters as elaboration/simulation capabilities ship

## Tracking

- **Chapter status**: `docs/lrm/progress.md` -- the scoreboard.
- **Gap ledger**: `docs/lrm/gaps.md` -- the work queue.
- **Decision records**: `docs/lrm/chXX.md` -- created on demand when an LRM ambiguity requires an interpretation decision. Do not create empty templates.
