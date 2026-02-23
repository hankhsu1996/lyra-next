#!/usr/bin/env python3
"""Check CST layering policy: no rowan traversal in lyra-semantic except allowlisted modules.

Rules:
  C001: No SyntaxNode/SyntaxToken/SyntaxElement imports in lyra-semantic
  C002: No CST traversal method calls (.children(), .descendants(), etc.)

The semantic layer should consume typed AST accessors, not raw CST traversal.
Files that currently need CST access are allowlisted and tracked as tech debt.

Usage:
  python3 tools/policy/check_cst_layering.py                          # All files
  python3 tools/policy/check_cst_layering.py --diff-base origin/main  # Changed files
  python3 tools/policy/check_cst_layering.py --staged                 # Staged files
"""

import argparse
import re
import subprocess
import sys
from pathlib import Path

# Only check files under this path (relative to repo root).
TARGET_PREFIX = "crates/lyra-semantic/src/"

# Allowlisted modules that may use CST traversal.
#
# Tier 1 -- Builder phase (permanent: their job is to walk the CST).
# Tier 2 -- Extraction helpers (permanent: thin wrappers over CST tokens).
# Tier 3 -- Current users (tech debt: should migrate to typed accessors).
#
# When a tier-3 module is cleaned up, remove it from this set.
ALLOWED_MODULES = frozenset({
    # Tier 1: builder phase
    "builder.rs",
    "builder_items.rs",
    "builder_stmt.rs",
    "builder_types.rs",
    "builder_order.rs",
    # Tier 2: extraction helpers
    "syntax_helpers.rs",
    "expr_helpers.rs",
    # Tier 3: tech debt (track for removal)
    "type_extract.rs",
    "type_infer/mod.rs",
    "type_infer/range.rs",
    "type_check.rs",
    "const_eval.rs",
    "system_functions.rs",
    "system_call_view.rs",
    "literal.rs",
    "lhs.rs",
    "record.rs",
    "builtin_methods.rs",
})

# C001: importing rowan / SyntaxNode / SyntaxToken / SyntaxElement
RE_CST_IMPORT = re.compile(
    r'\buse\s+(?:lyra_parser|rowan).*\b(?:SyntaxNode|SyntaxToken|SyntaxElement)\b'
)

# C002: CST traversal method calls
RE_CST_TRAVERSAL = re.compile(
    r'\.'
    r'(?:children|descendants|first_child|last_child'
    r'|children_with_tokens|first_token|last_token'
    r'|descendants_with_tokens'
    r')\s*\('
)


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
    # Test files are exempt.
    if "/tests/" in filepath or "/tests.rs" in filepath:
        return False
    if filepath.endswith("_tests.rs"):
        return False
    return True


def _module_name(filepath: str) -> str:
    """Extract module-relative path from the full filepath.

    For example, 'crates/lyra-semantic/src/type_infer/mod.rs' -> 'type_infer/mod.rs'.
    """
    return filepath.removeprefix(TARGET_PREFIX)


def is_allowed(filepath: str) -> bool:
    return _module_name(filepath) in ALLOWED_MODULES


# Test detection for files that embed #[cfg(test)] blocks.
RE_TEST_ATTR = re.compile(r'#\[(cfg\(test\)|test)\]')


def is_in_test_context(lines: list[str], lineno: int) -> bool:
    for i in range(lineno - 1, -1, -1):
        if RE_TEST_ATTR.search(lines[i]):
            return True
    return False


def check_file(filepath: str, repo_root: Path) -> list[str]:
    errors = []
    full_path = repo_root / filepath

    try:
        content = full_path.read_text(encoding="utf-8", errors="replace")
    except OSError as e:
        return [f"{filepath}: failed to read: {e}"]

    lines = content.splitlines()

    for lineno_0, line in enumerate(lines):
        lineno = lineno_0 + 1
        stripped = line.lstrip()

        # Skip comments.
        if stripped.startswith("//"):
            continue

        # Skip test code.
        if is_in_test_context(lines, lineno_0):
            continue

        if RE_CST_IMPORT.search(line):
            errors.append(
                f"{filepath}:{lineno}: C001 CST type import in semantic layer"
            )

        if RE_CST_TRAVERSAL.search(line):
            errors.append(
                f"{filepath}:{lineno}: C002 CST traversal call in semantic layer"
            )

    return errors


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Check CST layering policy in lyra-semantic"
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
        if is_allowed(filepath):
            continue
        if (repo_root / filepath).exists():
            all_errors.extend(check_file(filepath, repo_root))

    if all_errors:
        print("CST layering violations:\n")
        for error in all_errors:
            print(f"  {error}")
        print(f"\nTotal: {len(all_errors)} violations")
        print("\nRules:")
        print("  C001: No SyntaxNode/SyntaxToken/SyntaxElement imports")
        print("  C002: No .children()/.descendants()/etc. traversal calls")
        print("  Allowlisted modules: see ALLOWED_MODULES in this script")
        return 1

    checked = len([f for f in files if not is_allowed(f)])
    print(f"Checked {checked} files ({len(files)} total, "
          f"{len(files) - checked} allowlisted), no violations")
    return 0


if __name__ == "__main__":
    sys.exit(main())
