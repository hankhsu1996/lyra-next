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
import sys
from pathlib import Path

from policy_base import (
    Finding, add_scope_args, compute_test_lines, get_repo_root,
    report_findings, resolve_files,
)

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
    r'//\s*'
    r'(?:'
    r'[-=~#*]{2,}\s+'
    r'.+'
    r'\s+[-=~#*]{2,}'
    r'|'
    r'[-=~#*]{3,}'
    r')'
)

RULES = {
    "R001": "No .unwrap()/.expect() in library code (use Result/Option)",
    "R002": "No panic!() in library code (use Result/Option)",
    "R003": "Every unsafe block needs a // SAFETY: comment above it",
    "R004": "No dbg!() or println!() (use structured diagnostics)",
    "R005": "No section-header comments (// --- Foo --- etc.)",
}


def should_check_file(filepath):
    path = Path(filepath)
    return (
        path.suffix in EXTENSIONS
        and any(filepath.startswith(inc) for inc in INCLUDE_PATHS)
        and "/tests/" not in filepath
        and not _is_test_filename(path.name)
    )


def _is_test_filename(name):
    """Recognize test-only source files by naming convention.

    Files named tests.rs or *_tests.rs are always test code
    (loaded via #[cfg(test)] mod tests; or similar in a parent
    module). Exempt them from R001/R002 the same way /tests/
    directories are exempted.
    """
    return name == "tests.rs" or name.endswith("_tests.rs")


def check_file(filepath, repo_root):
    findings = []
    full_path = repo_root / filepath

    try:
        content = full_path.read_text(encoding="utf-8", errors="replace")
    except OSError as e:
        return [Finding("error", "R000", filepath, 0, f"failed to read: {e}")]

    lines = content.splitlines()
    test_lines = compute_test_lines(lines)

    for lineno_0, line in enumerate(lines):
        lineno = lineno_0 + 1
        stripped = line.lstrip()

        if stripped.startswith("//"):
            if RE_SECTION_HEADER.match(stripped):
                findings.append(Finding(
                    "error", "R005", filepath, lineno,
                    "section-header comment style banned"))
            continue

        in_test = lineno_0 in test_lines

        if RE_UNWRAP.search(line) and not in_test:
            findings.append(Finding(
                "error", "R001", filepath, lineno,
                ".unwrap()/.expect() banned in library code"))

        if RE_PANIC.search(line) and not in_test:
            findings.append(Finding(
                "error", "R002", filepath, lineno,
                "panic!() banned in library code"))

        if RE_UNSAFE.search(line):
            prev_line = lines[lineno_0 - 1] if lineno_0 > 0 else ""
            if not RE_SAFETY_COMMENT.search(prev_line):
                findings.append(Finding(
                    "error", "R003", filepath, lineno,
                    "unsafe block without // SAFETY: comment"))

        if RE_DBG.search(line):
            findings.append(Finding(
                "error", "R004", filepath, lineno, "dbg!() banned"))
        if RE_PRINTLN.search(line):
            findings.append(Finding(
                "error", "R004", filepath, lineno, "println!() banned"))

    return findings


def self_test():
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


def main():
    parser = argparse.ArgumentParser(description="Check Rust error policy")
    add_scope_args(parser)
    parser.add_argument("--self-test", action="store_true",
                        help="Run built-in self-tests")
    args = parser.parse_args()

    if args.self_test:
        return self_test()

    repo_root = get_repo_root()
    files = resolve_files(args, repo_root, should_check_file)

    if not files:
        print("No files to check")
        return 0

    all_findings = []
    for filepath in files:
        all_findings.extend(check_file(filepath, repo_root))

    return report_findings(all_findings, RULES, len(files))


if __name__ == "__main__":
    sys.exit(main())
