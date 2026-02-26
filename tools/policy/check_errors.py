#!/usr/bin/env python3
"""Check Rust error-handling policy compliance.

Rules:
  R001: No .unwrap() or .expect() in library code (tests are exempt)
  R002: No panic!() in library code (tests are exempt)
  R003: No unsafe without a // SAFETY: comment on the preceding line
  R004: No dbg!() or println!() (use structured diagnostics)
  R005: No section-header comments (e.g. // --- Foo --- or // ### Foo ###)

Usage:
  python3 tools/policy/check_errors.py                          # All files
  python3 tools/policy/check_errors.py --diff-base origin/main  # Changed files
  python3 tools/policy/check_errors.py --staged                 # Staged files
"""

import argparse
import re
import subprocess
import sys
from pathlib import Path

INCLUDE_PATHS = ("crates/",)
EXTENSIONS = frozenset({".rs"})

# Patterns
RE_UNWRAP = re.compile(r'\.unwrap\s*\(|\.expect\s*\(\s*"')
RE_PANIC = re.compile(r'\bpanic!\s*\(')
RE_UNSAFE = re.compile(r'\bunsafe\b')
RE_SAFETY_COMMENT = re.compile(r'//\s*SAFETY:')
RE_DBG = re.compile(r'\bdbg!\s*\(')
RE_PRINTLN = re.compile(r'\bprintln!\s*\(')
RE_SECTION_HEADER = re.compile(
    r'//\s*'           # comment start
    r'(?:'
    r'[-=~#*]{2,}\s+'  # leading decorators (2+): -- , ==, ##, etc.
    r'.+'              # content
    r'\s+[-=~#*]{2,}'  # trailing decorators
    r'|'
    r'[-=~#*]{3,}'     # or just a line of decorators (3+)
    r')'
)

# Test detection: lines inside #[cfg(test)] mod or #[test] fn
RE_CFG_TEST = re.compile(r'#\[cfg\(test\)\]')
RE_TEST_FN = re.compile(r'#\[test\]')


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
    path = Path(filepath)
    return (
        path.suffix in EXTENSIONS
        and any(filepath.startswith(inc) for inc in INCLUDE_PATHS)
        and "/tests/" not in filepath
        and not _is_test_filename(path.name)
    )


def _is_test_filename(name: str) -> bool:
    """Recognize test-only source files by naming convention.

    Files named ``tests.rs`` or ``*_tests.rs`` are always test code
    (loaded via ``#[cfg(test)] mod tests;`` or similar in a parent
    module).  Exempt them from R001/R002 the same way ``/tests/``
    directories are exempted.
    """
    return name == "tests.rs" or name.endswith("_tests.rs")


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

    Handles `#[cfg(test)] mod ... { }` blocks by tracking brace depth,
    and individual `#[test] fn` blocks. Non-braced items (e.g.
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


def check_file(filepath: str, repo_root: Path) -> list[str]:
    errors = []
    full_path = repo_root / filepath

    try:
        content = full_path.read_text(encoding="utf-8", errors="replace")
    except OSError as e:
        return [f"{filepath}: failed to read: {e}"]

    lines = content.splitlines()
    test_lines = compute_test_lines(lines)

    for lineno_0, line in enumerate(lines):
        lineno = lineno_0 + 1
        stripped = line.lstrip()

        # Skip lines inside comments (crude: starts with //)
        if stripped.startswith("//"):
            # R005: section-header comments
            if RE_SECTION_HEADER.match(stripped):
                errors.append(
                    f"{filepath}:{lineno}: R005 section-header comment style banned")
            continue

        in_test = lineno_0 in test_lines

        # R001: .unwrap() / .expect() in library code
        if RE_UNWRAP.search(line) and not in_test:
            errors.append(
                f"{filepath}:{lineno}: R001 .unwrap()/.expect() banned in library code")

        # R002: panic!() in library code
        if RE_PANIC.search(line) and not in_test:
            errors.append(
                f"{filepath}:{lineno}: R002 panic!() banned in library code")

        # R003: unsafe without SAFETY comment
        if RE_UNSAFE.search(line):
            prev_line = lines[lineno_0 - 1] if lineno_0 > 0 else ""
            if not RE_SAFETY_COMMENT.search(prev_line):
                errors.append(
                    f"{filepath}:{lineno}: R003 unsafe block without // SAFETY: comment")

        # R004: dbg!() or println!()
        if RE_DBG.search(line):
            errors.append(f"{filepath}:{lineno}: R004 dbg!() banned")
        if RE_PRINTLN.search(line):
            errors.append(f"{filepath}:{lineno}: R004 println!() banned")

    return errors


def self_test() -> int:
    """Run built-in tests for test-context detection."""
    failures = 0

    # Test 1: prod code AFTER #[cfg(test)] block must still be checked
    lines_1 = [
        'fn prod_before() { x.unwrap(); }',
        '#[cfg(test)]',
        'mod tests {',
        '    fn test_foo() { x.unwrap(); }',
        '}',
        'fn prod_after() { x.unwrap(); }',
    ]
    tl = compute_test_lines(lines_1)
    if 0 in tl:
        print("FAIL test1: prod code before #[cfg(test)] was exempt")
        failures += 1
    if 3 not in tl:
        print("FAIL test1: code inside #[cfg(test)] was not exempt")
        failures += 1
    if 5 in tl:
        print("FAIL test1: prod code after #[cfg(test)] block was exempt")
        failures += 1

    # Test 2: #[cfg(test)] use exempts only that item
    lines_2 = [
        '#[cfg(test)]',
        'use some_crate::test_util;',
        'fn prod() { x.unwrap(); }',
    ]
    tl2 = compute_test_lines(lines_2)
    if 0 not in tl2 or 1 not in tl2:
        print("FAIL test2: #[cfg(test)] use lines were not exempt")
        failures += 1
    if 2 in tl2:
        print("FAIL test2: prod fn after #[cfg(test)] use was exempt")
        failures += 1

    # Test 3: #[test] fn exempts only the function body
    lines_3 = [
        'fn prod() { x.unwrap(); }',
        '#[test]',
        'fn test_foo() {',
        '    x.unwrap();',
        '}',
        'fn prod2() { x.unwrap(); }',
    ]
    tl3 = compute_test_lines(lines_3)
    if 0 in tl3:
        print("FAIL test3: prod fn before #[test] was exempt")
        failures += 1
    if 3 not in tl3:
        print("FAIL test3: line inside #[test] fn was not exempt")
        failures += 1
    if 5 in tl3:
        print("FAIL test3: prod fn after #[test] fn was exempt")
        failures += 1

    # Test 4: nested braces inside #[cfg(test)]
    lines_4 = [
        '#[cfg(test)]',
        'mod tests {',
        '    fn foo() {',
        '        if true { x.unwrap(); }',
        '    }',
        '}',
        'fn prod() { x.unwrap(); }',
    ]
    tl4 = compute_test_lines(lines_4)
    if 3 not in tl4:
        print("FAIL test4: nested brace line inside test was not exempt")
        failures += 1
    if 6 in tl4:
        print("FAIL test4: prod fn after nested test block was exempt")
        failures += 1

    if failures:
        print(f"\n{failures} self-test(s) FAILED")
        return 1
    print("All self-tests passed")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description="Check Rust error policy")
    parser.add_argument(
        "--diff-base", help="Check files changed since git ref")
    parser.add_argument("--staged", action="store_true",
                        help="Check staged files")
    parser.add_argument("--self-test", action="store_true",
                        help="Run built-in self-tests")
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
        if (repo_root / filepath).exists():
            all_errors.extend(check_file(filepath, repo_root))

    if all_errors:
        print("Error policy violations:\n")
        for error in all_errors:
            print(f"  {error}")
        print(f"\nTotal: {len(all_errors)} violations")
        print("\nRules:")
        print("  R001: No .unwrap()/.expect() in library code (use Result/Option)")
        print("  R002: No panic!() in library code (use Result/Option)")
        print("  R003: Every unsafe block needs a // SAFETY: comment above it")
        print("  R004: No dbg!() or println!() (use structured diagnostics)")
        print("  R005: No section-header comments (// --- Foo --- etc.)")
        return 1

    print(f"Checked {len(files)} files, no violations")
    return 0


if __name__ == "__main__":
    sys.exit(main())
