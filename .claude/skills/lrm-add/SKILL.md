---
name: lrm-add
description: Scaffold a new LRM test case
---

# LRM Add

Create a new LRM test case directory. Arguments: `<chapter> <case-name>` (e.g., `5 string_literals`, `23 port_connections`).

## Context

- **Existing corpus for this chapter:** !`find crates/lyra-tests/testdata/corpus/lrm/ -name test.yaml -printf '%h\n' 2>/dev/null | sort`
- **Current gaps:** !`cat docs/lrm/gaps.md`

## Instructions

Given a chapter number and case name from the user's argument:

### 1. Read the relevant LRM section

Read the relevant pages from the IEEE 1800 LRM PDF. Understand what the section requires.

### 2. Create the test directory

Directory: `crates/lyra-tests/testdata/corpus/lrm/ch{XX}/{case_name}/`

Where `XX` is the zero-padded chapter number and `case_name` is the user-provided name in snake_case. Directory names must not contain `__`.

### 3. Create test.yaml

Create `test.yaml` in the test directory with LRM metadata:

```yaml
kind: lrm
lrm:
  chapter: <N>
  section: "<N.M>"
  title: "<Section title>"
```

The `chapter` must match the `chXX` directory, and `section` must start with `"<chapter>."`.

### 4. Create the .sv file(s)

Create a `main.sv` (or appropriately named files for multi-file tests) with:
- A header comment citing the LRM section being tested
- Minimal SystemVerilog code exercising the feature
- Inline annotations for expected diagnostics (if testing error cases)

**Only include constructs the engine can handle today.** If a feature causes parse errors or wrong diagnostics, do not include it in the test.

### 5. Verify the test runs

```bash
cargo test -p lyra-tests corpus_snapshots
```

If snapshots need updating:

```bash
cargo insta accept
```

The test MUST pass. If it does not, scope the test down to what works.

### 6. Record any gaps discovered

If you had to omit LRM features because the engine cannot handle them, add entries to `docs/lrm/gaps.md`. Each entry must include:
- What LRM feature is missing
- What blocks it (parser, semantic analysis, etc.)
- What test names will be added once the fix lands

This is mandatory -- every deferred feature must have a gaps.md entry.

### 7. Report

Tell the user:
- The created test path and whether it passes
- Any gaps discovered and recorded in gaps.md
