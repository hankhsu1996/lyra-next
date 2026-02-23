---
name: lrm-add
description: Scaffold a new LRM test case
---

# LRM Add

Add LRM corpus test cases. Two usage modes:

- **Chapter audit:** `/lrm-add <chapter>` -- Audit the entire chapter, create tests for uncovered sections, gap what the engine cannot handle.
- **Single case:** `/lrm-add <chapter> <section>` -- Create one test case for a specific LRM leaf section.

## Argument parsing

The chapter argument is flexible. All of these mean chapter 5: `5`, `ch5`, `ch05`, `chapter 5`. Extract the integer and zero-pad it for directory names (`ch05`, `ch26`).

The section, if present, is the second argument as a dotted section number (e.g., `5.9.1`, `26.3`).

## Context

- **Section index (source of truth):** !`cat docs/lrm/sections.json`
- **Existing corpus:** !`find crates/lyra-tests/testdata/corpus/lrm/ -name test.yaml -printf '%h\n' 2>/dev/null | sort`
- **Current gaps:** !`cat docs/lrm/gaps.md`

## Section-keyed model

The test corpus is keyed by LRM section IDs. `docs/lrm/sections.json` is the canonical index of all sections. Every ownable (leaf) section can have exactly one owner directory.

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

Only `section` is required. No `chapter` or `title` fields.

### cases/ nesting

When a leaf section has multiple independent tests:

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

If `cases/` exists, no `test.yaml` directly under the owner dir. Every case's `section` must match the owner's section prefix.

### Synthetic .0 leaves

When a parent section has content beyond its children, a synthetic `.0` child exists in sections.json (e.g., `5.6.0` for simple identifier rules under 5.6).

## Coverage model

Load `docs/lrm/sections.json` as source of truth. An ownable section is covered iff an owner directory exists for it.

### No duplication

Every test must add NEW coverage. Before creating a test, check existing owner dirs for the chapter. Duplicating content across tests is a sign of bad organization.

### Leaf-section targeting

Tests must target ownable (leaf) sections in sections.json. The section in test.yaml must match the owner dir's section prefix exactly. Non-ownable sections (parents with children) cannot have owner dirs.

## Instructions

### Chapter-only mode

When only a chapter number is given, audit the chapter for coverage gaps and fill them.

1. **Load sections.json** -- find all ownable (leaf) sections for this chapter.
2. **Inventory existing owner dirs** -- list dirs under `lrm/chXX/` and extract section prefixes.
3. **Identify uncovered ownable sections** -- sections in sections.json that have no owner dir.
4. **For each uncovered section, decide: test or gap entry.**
   - If the engine handles the feature: create a test case (see below).
   - If the engine cannot handle it: add an entry to `docs/lrm/gaps.md`. Do NOT create a failing test.
5. **Report** a coverage table showing every ownable section and its status.

### Single-case mode

1. **Validate section** -- confirm it exists in sections.json and is ownable.
2. **Check if already owned** -- if an owner dir exists, create under `cases/` automatically.
3. **Create the test case** (see below).
4. **Gap anything** the engine cannot handle.
5. **Report** the test path and any gaps.

## Create a test case

Directory: `crates/lyra-tests/testdata/corpus/lrm/ch{XX}/{section}_{label}/`

If the section already has an owner dir, create under `cases/{case_name}/` instead.

**test.yaml:**
```yaml
kind: lrm
lrm:
  section: "<N.M.P>"
```

**main.sv** (or multiple .sv files for multi-file tests):
- Header comment citing the LRM section
- Minimal SystemVerilog code exercising the feature
- Inline annotations for expected diagnostics (`// ^ error[code]: message`)
- **Only include constructs the engine handles today.** If a feature causes parse errors or wrong diagnostics, omit it and gap it.

**Verify:**
```bash
cargo test -p lyra-tests --test corpus
```

If snapshots need updating: `cargo insta accept`, then read the snapshot to verify every diagnostic makes sense.

The test MUST pass. If it does not, scope down to what works.

## Record gaps

Add entries to `docs/lrm/gaps.md` for every LRM feature the engine cannot handle. Each entry needs:
- What LRM feature is missing
- What blocks it (parser, semantic analysis, etc.)
- What test name will be added once the fix lands

This is mandatory -- every deferred feature must have a gaps.md entry.
