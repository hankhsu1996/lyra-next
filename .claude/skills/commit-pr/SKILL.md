---
name: commit-pr
description: Commit changes and create a pull request in one step
disable-model-invocation: true
---

# Commit and PR

Commit changes and create a PR in one step. This combines the commit and pr skills.

---

## Part 1: Commit

### STOP: Check Branch First

**You are NOT allowed to commit on main.** Before doing ANYTHING else:

1. Check the current branch in the Context section below
2. If on `main`, infer an appropriate branch name from the changes (see Branch Rules)
3. Create the branch with `git switch -c <branch-name>` BEFORE any other steps

Do NOT proceed with formatting or staging until you are on a feature branch.

### Context

- **Current branch:** !`git branch --show-current`
- **Git status:** !`git status --short`
- **Staged diff:** !`git diff --cached`
- **Unstaged diff:** !`git diff`

### Pre-commit Checks

Before committing, format and check everything:

1. **Rust formatting:**

   ```bash
   cargo fmt
   ```

2. **Compile check:**

   ```bash
   cargo check
   ```

3. **Linting:**

   ```bash
   cargo clippy
   ```

4. **Tests:**

   ```bash
   cargo test
   ```

5. **Policy checks** - First get merge base, then run checks:

   ```bash
   git merge-base origin/main HEAD
   # Use the returned SHA in subsequent commands
   python3 tools/policy/check_ascii.py --diff-base <SHA>
   python3 tools/policy/check_errors.py --diff-base <SHA>
   ```

   If merge base fails (no remote yet), run without `--diff-base`:

   ```bash
   python3 tools/policy/check_ascii.py
   python3 tools/policy/check_errors.py
   ```

   If any fail, fix violations before committing.

### Commit Format

```
<Summary starting with verb, 50 chars or less>

- Bullet under 60 chars
- Another bullet if needed (2-5 total)
```

Bullet points should be **concise** (under 60 chars each) and describe **what changed**, not background context.

**ASCII only.** No special Unicode characters.

**CRITICAL: Do NOT add attribution.** No "Generated with Claude Code", no "Co-Authored-By", no author credits. These duplicate badly when squash-merging. The commit message should ONLY contain the summary line and bullet points.

**IMPORTANT: Describe the outcome, not the process.** The commit message reflects what changed, not how you got there.

### Branch Rules

**Branch name format:** `<type>/<short-description>`

- **Types:** `feature`, `bugfix`, `refactor`, `release`, `chore`, `docs`
- Use kebab-case: `aaa-bbb-ccc`
- Keep short (~5 words max)

**IMPORTANT: Name for the primary feature, not the recent task.** Look at the full diff and identify what the main deliverable is.

**Examples:**

- `feature/user-auth`
- `bugfix/null-pointer-crash`
- `refactor/split-codegen` (behavior-preserving restructuring)
- `chore/update-deps` (CI changes go here)
- `docs/api-reference`

### Commit Instructions

1. **Check branch first** - See "STOP: Check Branch First" section above. Do NOT skip this.
2. Format all files (`cargo fmt`)
3. Run all checks (check, clippy, test, policy) - fix any violations before proceeding
4. **Check git status again** - Formatters may modify files beyond your original changeset. Run `git status --short` to see all modified files before staging.
5. Stage files with `git add <files>` (do NOT use `git add -A`)
6. Run `git commit` as a separate command (do NOT chain with add)

**Note:** Never use `git commit --amend` if the previous commit has been pushed. If `git status` shows "Your branch is up to date with origin", the last commit is pushed - create a new commit instead.

---

## Part 2: PR

### PR Context

- **Commits on this branch:** !`git log --oneline main..HEAD`
- **Full diff from main:** !`git diff --merge-base origin/main HEAD --stat`
- **Commits behind main:** !`git fetch origin main --quiet && git rev-list --count HEAD..origin/main`

### PR Format

**Title:**

- Start with verb, capitalized
- Do NOT use colon format like "Fix: xxx"

**Body:**

Always start with `## Summary` as a paragraph (not bullet points) describing what the PR does.

After Summary, add sections only if they add value. All sections are optional:

- `## Design` - For non-trivial design decisions, architectural reasoning
- `## Testing` - Only if testing approach is non-obvious or worth highlighting
- Other sections as appropriate for the PR

Simple fixes may need only Summary. Don't force sections that have nothing meaningful to say.

**What makes a good PR description:**

- **Design rationale**: Explain the approach and why. If minimal, explain why existing infrastructure was sufficient.
- **What didn't change**: Often more informative than listing what did. Shows architectural understanding.
- **Alternatives explored**: If complexity was considered and rejected, mention it briefly.
- **Why it works**: If something works with little code, explain the underlying reason.

**Formatting:**

- Summary: paragraph, not bullet points
- Other sections: bullet points, checkboxes, or prose as appropriate
- Use h3 subsections within sections if content is substantial

**Adapt to PR type:**

- **Feature PRs**: Summary + Design with rationale
- **Bug fix PRs**: Summary + root cause analysis
- **Chore/docs PRs**: Summary only, keep brief

**Avoid:**

- Bullet points in Summary
- Listing files changed (GitHub shows this)
- Internal planning concepts ("Phase 1", "Step 2")

**ASCII only.** No Unicode, no emojis.

**CRITICAL: Do NOT add attribution.** No "Generated with Claude Code", no "Co-Authored-By", no author credits.

### PR Instructions

1. If commits behind main > 0, rebase first: `git rebase origin/main`, then re-run `cargo fmt`. If formatting changed, amend the last commit with the fix.
2. **Read the full diff** (`git diff origin/main..HEAD`) before writing the PR description. The `--stat` above is not sufficient - you must see the actual code changes.
3. Push: `git push -u origin <branch>`
4. Create PR: `gh pr create --title "..." --body "..."`
5. Enable auto-merge: `gh pr merge --auto --squash` (skip if user says no auto-merge)
6. Return the PR URL to the user
