#!/usr/bin/env python3
"""Check CST layering policy: no Rowan traversal in lyra-semantic except in allowlisted modules.

Rules:
  C001: No CST type imports (SyntaxNode/SyntaxToken/SyntaxElement) in lyra-semantic
  C002: No CST traversal calls (.children(), .descendants(), etc.)

The semantic layer should consume typed AST accessors, not raw CST traversal.
Files that currently need CST access are temporarily allowlisted and tracked as tech debt.

Tier-3 modules have ceiling enforcement: the violation count must not exceed the
ceiling. When a module reaches 0, remove it from both ALLOWED_MODULES and
CST_CALL_CEILINGS.

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
# Tier 3 -- Residual CST usage (tech debt: should migrate to typed accessors).
#
# When a tier-3 module reaches 0 violations, remove from both sets.
ALLOWED_MODULES = frozenset({
    # Tier 1: builder phase
    "builder.rs",
    "builder_items.rs",
    "builder_stmt.rs",
    "builder_types.rs",
    "builder_order.rs",
    # Tier 2: SyntaxNode in public API signatures only (no CST traversal)
    "type_extract.rs",
    "record.rs",
    # Tier 3: residual CST usage with ceiling enforcement
    "type_infer/mod.rs",
    "type_infer/range.rs",
    "type_check.rs",
    "const_eval.rs",
    "system_functions.rs",
    "literal.rs",
    "lhs.rs",
    "builtin_methods.rs",
})

# Ceiling enforcement for tier-3 modules.
#
# Maps module name -> max allowed C002 (traversal) violation count.
# C001 (imports) are not counted -- modules legitimately use SyntaxNode
# in their signatures. Only actual CST traversal calls are tracked.
# If a module exceeds its ceiling, the check fails. If it drops below,
# update the ceiling to the new count (ratchet down).
# Tier-1 modules are not tracked here (no ceiling).
CST_CALL_CEILINGS = {
    "type_check.rs": 6,
}

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
RE_CFG_TEST = re.compile(r'#\[cfg\(test\)\]')
RE_TEST_FN = re.compile(r'#\[test\]')


def compute_test_lines(lines: list[str]) -> set[int]:
    """Compute the set of 0-based line indices that are inside test code.

    Handles #[cfg(test)] mod ... { } blocks by tracking brace depth,
    and individual #[test] fn blocks.
    """
    test_lines: set[int] = set()
    i = 0
    n = len(lines)
    while i < n:
        line = lines[i]
        if RE_CFG_TEST.search(line):
            # Found #[cfg(test)]; scan forward for opening brace
            start = i
            depth = 0
            found_open = False
            for j in range(i, n):
                test_lines.add(j)
                depth += lines[j].count('{') - lines[j].count('}')
                if not found_open and '{' in lines[j]:
                    found_open = True
                if found_open and depth <= 0:
                    i = j + 1
                    break
            else:
                i = n
            continue
        if RE_TEST_FN.search(line):
            # Individual #[test] fn: mark until closing brace
            test_lines.add(i)
            depth = 0
            found_open = False
            for j in range(i + 1, n):
                test_lines.add(j)
                depth += lines[j].count('{') - lines[j].count('}')
                if not found_open and '{' in lines[j]:
                    found_open = True
                if found_open and depth <= 0:
                    i = j + 1
                    break
            else:
                i = n
            continue
        i += 1
    return test_lines


def count_violations(filepath: str, repo_root: Path) -> tuple[list[str], int, int]:
    """Check a file for CST violations.

    Returns (list of violation messages, C001 count, C002 count).
    """
    errors = []
    full_path = repo_root / filepath

    try:
        content = full_path.read_text(encoding="utf-8", errors="replace")
    except OSError as e:
        return [f"{filepath}: failed to read: {e}"], 0, 0

    lines = content.splitlines()
    c001 = 0
    c002 = 0
    test_lines = compute_test_lines(lines)

    for lineno_0, line in enumerate(lines):
        lineno = lineno_0 + 1
        stripped = line.lstrip()

        # Skip comments.
        if stripped.startswith("//"):
            continue

        # Skip test code.
        if lineno_0 in test_lines:
            continue

        if RE_CST_IMPORT.search(line):
            errors.append(
                f"{filepath}:{lineno}: C001 CST type import in semantic layer"
            )
            c001 += 1

        if RE_CST_TRAVERSAL.search(line):
            errors.append(
                f"{filepath}:{lineno}: C002 CST traversal call in semantic layer"
            )
            c002 += 1

    return errors, c001, c002


def self_test() -> int:
    """Run built-in tests for the policy script's own logic."""
    failures = 0

    # Test 1: code after #[cfg(test)] block is NOT exempt
    lines_1 = [
        '#[cfg(test)]',
        'mod tests {',
        '    fn foo() { node.children(); }',
        '}',
        'fn bar() { node.children(); }',
    ]
    tl = compute_test_lines(lines_1)
    if 4 in tl:
        print("FAIL: line after #[cfg(test)] block was exempt")
        failures += 1
    if 2 not in tl:
        print("FAIL: line inside #[cfg(test)] block was not exempt")
        failures += 1

    # Test 2: nested braces inside test module
    lines_2 = [
        '#[cfg(test)]',
        'mod tests {',
        '    fn foo() {',
        '        if true { node.children(); }',
        '    }',
        '}',
        'fn after() { node.children(); }',
    ]
    tl2 = compute_test_lines(lines_2)
    if 3 not in tl2:
        print("FAIL: nested brace line inside test was not exempt")
        failures += 1
    if 6 in tl2:
        print("FAIL: line after nested test block was exempt")
        failures += 1

    # Test 3: #[test] fn is exempt
    lines_3 = [
        'fn prod() { node.children(); }',
        '#[test]',
        'fn test_foo() {',
        '    node.children();',
        '}',
        'fn prod2() { node.children(); }',
    ]
    tl3 = compute_test_lines(lines_3)
    if 0 in tl3:
        print("FAIL: production fn before #[test] was exempt")
        failures += 1
    if 3 not in tl3:
        print("FAIL: line inside #[test] fn was not exempt")
        failures += 1
    if 5 in tl3:
        print("FAIL: production fn after #[test] fn was exempt")
        failures += 1

    # Test 4: empty file
    tl4 = compute_test_lines([])
    if tl4:
        print("FAIL: empty file produced test lines")
        failures += 1

    if failures:
        print(f"\n{failures} self-test(s) FAILED")
        return 1
    print("All self-tests passed")
    return 0


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
    parser.add_argument(
        "--self-test", action="store_true", help="Run built-in self-tests"
    )
    args = parser.parse_args()

    if args.self_test:
        return self_test()

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
    ceiling_errors = []
    for filepath in sorted(set(files)):
        if not (repo_root / filepath).exists():
            continue
        mod_name = _module_name(filepath)
        if is_allowed(filepath):
            # Ceiling enforcement for tier-3 modules (C002 only).
            # C001 imports are expected in modules that handle SyntaxNode
            # in their signatures; only actual traversal calls count.
            if mod_name in CST_CALL_CEILINGS:
                _, _, c002 = count_violations(filepath, repo_root)
                ceiling = CST_CALL_CEILINGS[mod_name]
                if c002 > ceiling:
                    ceiling_errors.append(
                        f"{filepath}: {c002} C002 violations exceeds "
                        f"ceiling {ceiling}"
                    )
        else:
            errors, _, _ = count_violations(filepath, repo_root)
            all_errors.extend(errors)

    failed = False

    if all_errors:
        print("CST layering violations:\n")
        for error in all_errors:
            print(f"  {error}")
        print(f"\nTotal: {len(all_errors)} violations")
        print("\nRules:")
        print("  C001: No SyntaxNode/SyntaxToken/SyntaxElement imports")
        print("  C002: No .children()/.descendants()/etc. traversal calls")
        print("  Allowlisted modules: see ALLOWED_MODULES in this script")
        failed = True

    if ceiling_errors:
        if all_errors:
            print()
        print("CST ceiling violations:\n")
        for error in ceiling_errors:
            print(f"  {error}")
        print("\nUpdate CST_CALL_CEILINGS or reduce CST usage.")
        failed = True

    if failed:
        return 1

    checked = len([f for f in files if not is_allowed(f)])
    ceilinged = len([
        f for f in files
        if is_allowed(f) and _module_name(f) in CST_CALL_CEILINGS
    ])
    print(f"Checked {checked} files, {ceilinged} ceiling-enforced "
          f"({len(files)} total, "
          f"{len(files) - checked} allowlisted), no violations")
    return 0


if __name__ == "__main__":
    sys.exit(main())
