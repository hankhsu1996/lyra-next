# LRM Signoff

This document defines what it means to "sign off" an IEEE 1800 LRM chapter, the workflow for getting there, and how gaps are tracked.

## What "Signed Off" Means

A chapter is **signed off** when:

- All ownable (leaf) sections in `docs/lrm/sections.json` for the chapter have owner directories.
- All failure cases produce the expected diagnostic codes and spans.
- All passing cases produce no unexpected diagnostics.
- No entries remain in `docs/lrm/gaps.md` for the chapter.

A chapter is either **Not started**, **In progress**, or **Signed off**. There is no "unsupported" status -- chapters we have not begun are simply "Not started".

## Section-Keyed Model

The test corpus is keyed by LRM section IDs. `docs/lrm/sections.json` is the canonical index of all sections for chapters 5, 6, 7, 11, 20, 25, 26 (more added as coverage expands). Every heading from the LRM gets an entry, even non-testable ones (marked `ownable: false`).

A section is ownable iff it is a leaf (no children). Parent sections are never ownable. When a parent section has content beyond its children, a synthetic `.0` child captures it (e.g., `5.6.0` for simple identifier rules under `5.6`).

### Directory naming

Pattern: `{section}_{snake_case_label}/` under `lrm/chXX/`.

Examples:
- `ch05/5.3_white_space/`
- `ch05/5.6.0_simple_identifiers/`
- `ch26/26.3_referencing_data/cases/wildcard_import/`

### test.yaml format

```yaml
kind: lrm
lrm:
  section: "5.7.1"
```

Only `section` is required. No `chapter` or `title` fields -- those come from sections.json.

### cases/ nesting

When a leaf section has multiple independent tests needing separate compilation units, use `cases/`:

```
ch26/26.3_referencing_data/
  cases/
    explicit_import/
      test.yaml    # section: "26.3"
      main.sv
    wildcard_import/
      test.yaml    # section: "26.3"
      main.sv
```

Rules:
- No `test.yaml` directly under owner dir when `cases/` exists (no mixed mode).
- Every case's `section` must match the owner's section prefix.
- If no `cases/`: single `test.yaml` + `.sv` files directly in the dir.

## Test Infrastructure

LRM test cases live in `crates/lyra-tests/testdata/corpus/lrm/` as section-keyed directories. The corpus runner (`crates/lyra-tests/tests/corpus.rs`) discovers them, validates against sections.json, and reports coverage.

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

### Runner validation

The runner validates on every test run:
- All dirs under `lrm/chXX/` match section-keyed format (hard error for old-layout dirs)
- Section exists in sections.json and is ownable
- No duplicate section ownership
- Chapter dir matches section number
- test.yaml section matches owner dir prefix

Coverage summary is printed to stderr showing per-chapter owned/total leaf sections.

## Workflow

### Principle: always green on main

Tests never land on main while failing. If the engine cannot pass a test today, the test does not get committed. Instead, the gap goes into `docs/lrm/gaps.md` so it is not forgotten.

### Adding tests: `/lrm-add <chapter> <section>`

Scaffolds a test case in the corpus. Only add tests that pass with current engine capabilities. If you discover a gap, do not commit a failing test -- add an entry to `docs/lrm/gaps.md`.

### Recording gaps: `docs/lrm/gaps.md`

The gap ledger is hand-maintained and committed. Every time you decide not to add a test because the engine cannot pass it yet, you must add a gaps.md entry.

Each entry records: what LRM feature is missing, what blocks it, and what test names will be added once the fix lands.

When you fix an engine gap, add the now-passing tests to the corpus and remove the entry from gaps.md. Both changes land in the same PR.

### Signing off: `/lrm-signoff <chapter>`

The final audit. Checks sections.json for all ownable sections, verifies owner dirs exist, verifies gaps.md has no remaining entries. Signoff = all leaves owned + passing.

### Typical session

```
/lrm-add 5 5.6.1        -- works today, lands green
/lrm-add 5 5.7.1        -- works today, lands green
  (compiler directives don't work -- add entry to gaps.md)

... later, fix preprocessor ...

/lrm-add 5 5.6.4        -- now works, lands green
  (remove 5.6.4 entry from gaps.md)

/lrm-signoff 5           -- audit, no gaps remain, mark signed off
```

## Tracking

- **Section index**: `docs/lrm/sections.json` -- the canonical structure.
- **Chapter status**: `docs/lrm/progress.md` -- the scoreboard.
- **Gap ledger**: `docs/lrm/gaps.md` -- the work queue.
- **Decision records**: `docs/lrm/chXX.md` -- created on demand when an LRM ambiguity requires an interpretation decision.
