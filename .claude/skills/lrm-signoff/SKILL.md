---
name: lrm-signoff
description: Sign off an LRM chapter by auditing test coverage and updating the tracker
---

# LRM Signoff

Audit and sign off an LRM chapter. Argument: chapter number (e.g., `5`, `23`).

## Context

- **Section index:** !`cat docs/lrm/sections.json`
- **Existing corpus:** !`find crates/lyra-tests/testdata/corpus/lrm/ -name test.yaml -printf '%h\n' 2>/dev/null | sort`
- **Current gaps:** !`cat docs/lrm/gaps.md`
- **Current tracker:** !`cat docs/lrm/progress.md`

## Instructions

Given a chapter number (from the user's argument):

### 1. Check gaps.md first

If `docs/lrm/gaps.md` has entries for this chapter, the chapter CANNOT be signed off. Report the remaining gaps and stop.

### 2. Load sections.json

Find all ownable (leaf) sections for this chapter. This is the complete checklist -- no LRM reading needed for structure.

### 3. Inventory existing owner dirs

List all dirs under `lrm/chXX/`. Each dir's section prefix identifies which leaf section it owns.

### 4. Identify uncovered sections

Ownable sections without owner dirs are uncovered. For each:
- If the engine can handle it today: create the test (must pass).
- If the engine cannot handle it: add an entry to `docs/lrm/gaps.md`. The chapter cannot be signed off with open gaps.

### 5. Run the corpus

```bash
cargo test -p lyra-tests --test corpus
```

Fix any annotation mismatches. Update snapshots if needed:

```bash
cargo insta accept
```

### 6. Sign off (only if no gaps remain)

A chapter is signable when: all ownable (leaf) sections have owner dirs, all tests pass, and no gaps.md entries remain for this chapter.

Edit `docs/lrm/progress.md`:
- Set the chapter status to **Signed off**
- Fill in the test path column with the glob pattern (e.g., `lrm/ch05/*`)

### 7. Report

Tell the user:
- Leaf section coverage table from sections.json (owned/total)
- How many test cases existed vs how many were added
- Any gaps that prevent signoff (if applicable)
