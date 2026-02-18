---
name: lrm-signoff
description: Sign off an LRM chapter by auditing test coverage and updating the tracker
---

# LRM Signoff

Audit and sign off an LRM chapter. Argument: chapter number (e.g., `5`, `23`).

## Context

- **Existing corpus:** !`ls crates/lyra-tests/testdata/corpus/ | grep lrm_ch`
- **Current tracker:** !`cat docs/lrm/progress.md`
- **Current gaps:** !`cat docs/lrm/gaps.md`

## Instructions

Given a chapter number (from the user's argument):

### 1. Check gaps.md first

If `docs/lrm/gaps.md` has entries for this chapter, the chapter CANNOT be signed off. Report the remaining gaps and stop. The gaps must be fixed first.

### 2. Read the LRM chapter

Read the relevant pages from the IEEE 1800 LRM PDF. Identify:
- Key syntax forms and semantics rules
- Examples given in the LRM text
- Edge cases and boundary conditions
- Error conditions (things that must be rejected)

### 3. Inventory existing tests

List all `lrm_chXX_*` directories in `crates/lyra-tests/testdata/corpus/` for this chapter. Read each `.sv` file and its annotations to understand what is already covered.

### 4. Identify gaps

Compare the LRM requirements against existing test coverage. For each gap:
- If the engine can handle it today: create the test (must pass).
- If the engine cannot handle it: add an entry to `docs/lrm/gaps.md` and report it. The chapter cannot be signed off with open gaps.

### 5. Run the corpus

```bash
cargo test -p lyra-tests corpus_snapshots
```

Fix any annotation mismatches. Update snapshots if needed:

```bash
cargo insta accept
```

### 6. Sign off (only if no gaps remain)

Edit `docs/lrm/progress.md`:
- Set the chapter status to **Signed off**
- Fill in the test path column with the glob pattern (e.g., `lrm_ch05_*`)

### 7. Report

Tell the user:
- How many test cases existed vs how many were added
- What the chapter covers
- Any gaps that prevent signoff (if applicable)
- Any LRM ambiguities encountered (create `docs/lrm/chXX.md` if needed)
