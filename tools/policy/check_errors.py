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
RE_TEST_ATTR = re.compile(r'#\[(cfg\(test\)|test)\]')


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
    )


def is_in_test_context(lines: list[str], lineno: int) -> bool:
    """Check whether lineno is inside a #[cfg(test)] module or #[test] fn.

    Scans backwards looking for #[cfg(test)] or #[test]. Simple and
    conservative: any occurrence above the violation line counts.
    """
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

        # Skip lines inside comments (crude: starts with //)
        if stripped.startswith("//"):
            # R005: section-header comments
            if RE_SECTION_HEADER.match(stripped):
                errors.append(
                    f"{filepath}:{lineno}: R005 section-header comment style banned")
            continue

        # Skip string contents (crude: ignore lines that are mostly strings)

        # R001: .unwrap() / .expect() in library code
        if RE_UNWRAP.search(line) and not is_in_test_context(lines, lineno_0):
            errors.append(
                f"{filepath}:{lineno}: R001 .unwrap()/.expect() banned in library code")

        # R002: panic!() in library code
        if RE_PANIC.search(line) and not is_in_test_context(lines, lineno_0):
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


def main() -> int:
    parser = argparse.ArgumentParser(description="Check Rust error policy")
    parser.add_argument(
        "--diff-base", help="Check files changed since git ref")
    parser.add_argument("--staged", action="store_true",
                        help="Check staged files")
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
