#!/usr/bin/env python3
"""Check CST layering policy: no raw CST traversal in lyra-semantic or lyra-db.

Rules:
  C001: No CST traversal calls (.children(), .descendants(), etc.)
  C002: No manual Expression/ParenExpr wrapper matching

SyntaxNode/SyntaxToken imports are not checked -- many modules legitimately
use these types in function signatures. The real enforcement is C001 (actual
CST traversal calls) and C002 (manual wrapper peeling).

Consumer layers (lyra-semantic, lyra-db) should use typed AST accessors from
lyra-ast, not raw CST traversal. Use `Expr::peel()` instead of matching on
`SyntaxKind::Expression` or `SyntaxKind::ParenExpr`. Builder-phase modules
are permanently allowlisted since their job is to walk the CST via direct
dot-calls.

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

# Checked crates and their allowlisted modules. Each entry maps a source
# prefix to a frozenset of module-relative paths (forward-slash, relative to
# the crate's src/ directory) that are exempt from C001/C002.
#
# Permanent entries need no annotation. Transitional entries must have a
# TODO(<tag>) comment so they can be grepped and cleaned up.
CHECKED_CRATES: dict[str, frozenset[str]] = {
    "crates/lyra-semantic/src/": frozenset({
        "builder.rs",
        "builder_items.rs",
        "builder_stmt.rs",
        "builder_types.rs",
        "builder_order.rs",
    }),
    "crates/lyra-db/src/": frozenset(),
}

# Derive a human-readable crate label from the prefix for error messages.
_CRATE_LABELS: dict[str, str] = {
    prefix: prefix.split("/")[1] for prefix in CHECKED_CRATES
}

# C001: CST traversal method calls
RE_CST_TRAVERSAL = re.compile(
    r'\.'
    r'(?:children|descendants|first_child|last_child'
    r'|children_with_tokens|first_token|last_token'
    r'|descendants_with_tokens'
    r')\s*\('
)

# C002: Manual wrapper-kind matching (use Expr::peel() instead)
RE_WRAPPER_MATCH = re.compile(
    r'SyntaxKind::(?:Expression|ParenExpr)\b'
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


# Prefixes sorted longest-first so the most specific match wins.
_SORTED_PREFIXES = sorted(CHECKED_CRATES.keys(), key=len, reverse=True)


def _matching_crate(filepath: str) -> str | None:
    """Return the crate prefix if filepath belongs to a checked crate."""
    for prefix in _SORTED_PREFIXES:
        if filepath.startswith(prefix):
            return prefix
    return None


def should_check_file(filepath: str) -> bool:
    if _matching_crate(filepath) is None:
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
    prefix = _matching_crate(filepath)
    if prefix:
        return filepath.removeprefix(prefix)
    return filepath


def is_allowed(filepath: str) -> bool:
    prefix = _matching_crate(filepath)
    if prefix is None:
        return False
    return _module_name(filepath) in CHECKED_CRATES[prefix]


# Test detection for files that embed #[cfg(test)] blocks.
RE_CFG_TEST = re.compile(r'#\[cfg\(test\)\]')
RE_TEST_FN = re.compile(r'#\[test\]')


def _find_item_extent(lines: list[str], start: int) -> int:
    """Find the last line of the item starting at `start`.

    Scans forward from `start`. If a `{` is found before a bare `;`,
    tracks brace depth and returns the line where depth reaches 0.
    If a `;` is found first (non-braced item), returns that line.
    If the file ends without either, returns `start` (defensive).
    """
    n = len(lines)
    depth = 0
    found_open = False
    for j in range(start, n):
        line_j = lines[j]
        # Bare semicolon before any brace -> single item (e.g. `use foo;`)
        if not found_open and ';' in line_j and '{' not in line_j:
            return j
        depth += line_j.count('{') - line_j.count('}')
        if not found_open and '{' in line_j:
            found_open = True
        if found_open and depth <= 0:
            return j
    return start


def compute_test_lines(lines: list[str]) -> set[int]:
    """Compute the set of 0-based line indices that are inside test code.

    Handles #[cfg(test)] mod ... { } blocks by tracking brace depth,
    and individual #[test] fn blocks. Non-braced items (e.g.
    `#[cfg(test)] use foo;`) exempt only the item itself.
    """
    test_lines: set[int] = set()
    i = 0
    n = len(lines)
    while i < n:
        line = lines[i]
        if RE_CFG_TEST.search(line):
            end = _find_item_extent(lines, i)
            for j in range(i, end + 1):
                test_lines.add(j)
            i = end + 1
            continue
        if RE_TEST_FN.search(line):
            end = _find_item_extent(lines, i)
            for j in range(i, end + 1):
                test_lines.add(j)
            i = end + 1
            continue
        i += 1
    return test_lines


def check_violations(filepath: str, repo_root: Path) -> list[str]:
    """Check a file for C001/C002 violations.

    Returns list of violation messages.
    """
    errors = []
    full_path = repo_root / filepath
    prefix = _matching_crate(filepath)
    crate_label = _CRATE_LABELS.get(prefix, "unknown") if prefix else "unknown"

    try:
        content = full_path.read_text(encoding="utf-8", errors="replace")
    except OSError as e:
        return [f"{filepath}: failed to read: {e}"]

    lines = content.splitlines()
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

        if RE_CST_TRAVERSAL.search(line):
            errors.append(
                f"{filepath}:{lineno}: C001 CST traversal call ({crate_label})"
            )

        if RE_WRAPPER_MATCH.search(line):
            errors.append(
                f"{filepath}:{lineno}: C002 manual wrapper match ({crate_label})"
            )

    return errors


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

    # Test 5: #[cfg(test)] on non-braced item (use) must not exempt rest of file
    lines_5 = [
        '#[cfg(test)]',
        'use some_crate::test_util;',
        'fn prod() { node.children(); }',
    ]
    tl5 = compute_test_lines(lines_5)
    if 0 not in tl5 or 1 not in tl5:
        print("FAIL: #[cfg(test)] use item lines were not exempt")
        failures += 1
    if 2 in tl5:
        print("FAIL: production fn after #[cfg(test)] use was exempt")
        failures += 1

    # Test 6: #[cfg(test)] on same line as non-braced item
    lines_6 = [
        '#[cfg(test)] use foo::bar;',
        'fn prod() { node.children(); }',
    ]
    tl6 = compute_test_lines(lines_6)
    if 0 not in tl6:
        print("FAIL: inline #[cfg(test)] use was not exempt")
        failures += 1
    if 1 in tl6:
        print("FAIL: production fn after inline #[cfg(test)] use was exempt")
        failures += 1

    # Test 7: #[cfg(test)] block followed by #[cfg(test)] use
    lines_7 = [
        '#[cfg(test)]',
        'mod tests {',
        '    fn foo() {}',
        '}',
        '#[cfg(test)]',
        'use test_util::helper;',
        'fn prod() { node.children(); }',
    ]
    tl7 = compute_test_lines(lines_7)
    if 6 in tl7:
        print("FAIL: production fn after block+use was exempt")
        failures += 1
    if 4 not in tl7 or 5 not in tl7:
        print("FAIL: #[cfg(test)] use after block was not exempt")
        failures += 1

    # Test 8 (C002): SyntaxKind::Expression in match arm -> should fire
    c003_bad = [
        'match node.kind() {',
        '    SyntaxKind::Expression => { wrapper.inner() }',
        '    SyntaxKind::ParenExpr => { paren.inner() }',
        '    _ => {}',
        '}',
    ]
    tl8 = compute_test_lines(c003_bad)
    for idx, line in enumerate(c003_bad):
        if idx in tl8:
            continue
        if RE_WRAPPER_MATCH.search(line):
            break
    else:
        print("FAIL: C002 did not catch SyntaxKind::Expression match")
        failures += 1

    # Test 9 (C002): SyntaxKind::Literal should NOT fire
    c003_ok = [
        'match node.kind() {',
        '    SyntaxKind::Literal => eval_literal(node),',
        '    SyntaxKind::BinExpr => eval_bin(node),',
        '    _ => {}',
        '}',
    ]
    for line in c003_ok:
        if RE_WRAPPER_MATCH.search(line):
            print(f"FAIL: C002 false positive on: {line.strip()}")
            failures += 1

    # Test 10 (C002): wrapper match inside #[cfg(test)] -> should NOT fire
    c003_test = [
        '#[cfg(test)]',
        'mod tests {',
        '    fn foo() { match k { SyntaxKind::Expression => {} _ => {} } }',
        '}',
    ]
    tl10 = compute_test_lines(c003_test)
    for idx, line in enumerate(c003_test):
        if idx in tl10:
            continue
        if RE_WRAPPER_MATCH.search(line):
            print("FAIL: C002 fired inside #[cfg(test)] block")
            failures += 1

    if failures:
        print(f"\n{failures} self-test(s) FAILED")
        return 1
    print("All self-tests passed")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Check CST layering policy in lyra-semantic and lyra-db"
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
        print("No files to check")
        return 0

    all_errors = []
    for filepath in sorted(set(files)):
        if not (repo_root / filepath).exists():
            continue
        if not is_allowed(filepath):
            errors = check_violations(filepath, repo_root)
            all_errors.extend(errors)

    if all_errors:
        print("CST layering violations:\n")
        for error in all_errors:
            print(f"  {error}")
        print(f"\nTotal: {len(all_errors)} violations")
        print("\nRules:")
        print("  C001: No .children()/.descendants()/etc. CST traversal")
        print("  C002: No SyntaxKind::Expression/ParenExpr matching "
              "(use Expr::peel())")
        print("Allowlisted modules: see CHECKED_CRATES in this script")
        return 1

    checked = len([f for f in files if not is_allowed(f)])
    allowlisted = len(files) - checked
    print(f"Checked {checked} files ({len(files)} total, "
          f"{allowlisted} allowlisted), no violations")
    return 0


if __name__ == "__main__":
    sys.exit(main())
