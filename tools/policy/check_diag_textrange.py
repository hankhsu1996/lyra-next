#!/usr/bin/env python3
"""Check that TextRange does not leak into semantic diagnostic types.

Rules:
  D001: No TextRange type references in lyra-semantic source files
  D002: No text_range() method calls in lyra-semantic source files

The semantic layer stores stable anchors (Site, NameSpan, TokenSpan) in
diagnostics. TextRange computation belongs in the lowering layer (lyra-db).

Usage:
  python3 tools/policy/check_diag_textrange.py                          # All files
  python3 tools/policy/check_diag_textrange.py --diff-base origin/main  # Changed files
  python3 tools/policy/check_diag_textrange.py --staged                 # Staged files
"""

import argparse
import re
import subprocess
import sys
from pathlib import Path

TARGET_PREFIX = "crates/lyra-semantic/src/"

# D001: TextRange in code (imports, type annotations, expressions).
# Matches whole-word TextRange, not inside doc comments.
RE_TEXTRANGE = re.compile(r'\bTextRange\b')

# D002: text_range() method calls.
# Builder modules and producer modules that construct diagnostic anchors
# legitimately call text_range() to extract spans from CST nodes.
RE_TEXT_RANGE_CALL = re.compile(r'\btext_range\s*\(')
D002_ALLOWED = frozenset({
    "builder.rs",
    "builder_items.rs",
    "builder_stmt.rs",
    "builder_types.rs",
    "builder_order.rs",
    "type_check.rs",
})


def get_repo_root() -> Path:
    result = subprocess.run(
        ["git", "rev-parse", "--show-toplevel"],
        capture_output=True, text=True, check=True
    )
    return Path(result.stdout.strip())


def get_git_files(args: list[str]) -> list[str]:
    result = subprocess.run(
        ["git", "diff", "--name-only", "--diff-filter=ACMRT"] + args,
        capture_output=True, text=True, check=True
    )
    return [f for f in result.stdout.strip().split("\n") if f]


def get_all_files(repo_root: Path) -> list[str]:
    result = subprocess.run(
        ["git", "ls-files"],
        capture_output=True, text=True, check=True, cwd=repo_root
    )
    return [f for f in result.stdout.strip().split("\n") if f]


def should_check_file(filepath: str) -> bool:
    if not filepath.startswith(TARGET_PREFIX):
        return False
    if not filepath.endswith(".rs"):
        return False
    if "/tests/" in filepath or "/tests.rs" in filepath:
        return False
    if filepath.endswith("_tests.rs"):
        return False
    return True


def check_file(filepath: str, repo_root: Path) -> list[str]:
    errors = []
    full_path = repo_root / filepath
    module = filepath.removeprefix(TARGET_PREFIX)

    try:
        content = full_path.read_text(encoding="utf-8", errors="replace")
    except OSError as e:
        return [f"{filepath}: failed to read: {e}"]

    lines = content.splitlines()
    check_d002 = module not in D002_ALLOWED

    for lineno_0, line in enumerate(lines):
        lineno = lineno_0 + 1
        stripped = line.lstrip()

        # Skip comment-only lines
        if stripped.startswith("//"):
            continue

        # D001: no TextRange type references
        if RE_TEXTRANGE.search(line):
            errors.append(
                f"{filepath}:{lineno}: D001 TextRange reference"
            )
        # D002: no text_range() method calls (builder/producer modules exempt)
        if check_d002 and RE_TEXT_RANGE_CALL.search(line):
            errors.append(
                f"{filepath}:{lineno}: D002 text_range() call"
            )

    return errors


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Check TextRange does not leak into semantic diagnostics"
    )
    parser.add_argument(
        "--diff-base", help="Check files changed since git ref"
    )
    parser.add_argument(
        "--staged", action="store_true", help="Check staged files"
    )
    args = parser.parse_args()

    repo_root = get_repo_root()

    if args.staged:
        files = get_git_files(["--cached"])
    elif args.diff_base:
        files = get_git_files([args.diff_base])
    else:
        files = get_all_files(repo_root)

    files = [f for f in files if should_check_file(f)]

    if not files:
        print("No lyra-semantic files to check")
        return 0

    all_errors = []
    for filepath in sorted(set(files)):
        if (repo_root / filepath).exists():
            all_errors.extend(check_file(filepath, repo_root))

    if all_errors:
        print("Diagnostic TextRange policy violations:\n")
        for error in all_errors:
            print(f"  {error}")
        print(f"\nTotal: {len(all_errors)} violations")
        print("\nRules:")
        print("  D001: No TextRange type references in lyra-semantic files")
        print("  D002: No text_range() method calls in lyra-semantic files")
        return 1

    print(f"Checked {len(files)} files, no violations")
    return 0


if __name__ == "__main__":
    sys.exit(main())
