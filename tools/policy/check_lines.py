#!/usr/bin/env python3
"""Check file line-count policy compliance.

Rules:
  L001: No file over 1200 lines (hard limit)
  L002: File over 800 lines (warning -- split recommended)

Usage:
  python3 tools/policy/check_lines.py                          # All files
  python3 tools/policy/check_lines.py --diff-base origin/main  # Changed files
  python3 tools/policy/check_lines.py --staged                 # Staged files
"""

import argparse
import subprocess
import sys
from pathlib import Path

INCLUDE_PATHS = ("crates/",)
EXTENSIONS = frozenset({".rs"})

HARD_LIMIT = 1200
WARN_LIMIT = 800


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
    )


def check_file(filepath: str, repo_root: Path) -> tuple[list[str], list[str]]:
    errors = []
    warnings = []
    full_path = repo_root / filepath

    try:
        content = full_path.read_text(encoding="utf-8", errors="replace")
    except OSError as e:
        return [f"{filepath}: failed to read: {e}"], []

    line_count = content.count("\n")
    # Count final line if file doesn't end with newline
    if content and not content.endswith("\n"):
        line_count += 1

    if line_count > HARD_LIMIT:
        errors.append(
            f"{filepath}: L001 {line_count} lines (hard limit {HARD_LIMIT})")
    elif line_count > WARN_LIMIT:
        warnings.append(
            f"{filepath}: L002 {line_count} lines (split recommended at {WARN_LIMIT})")

    return errors, warnings


def main() -> int:
    parser = argparse.ArgumentParser(description="Check file line-count policy")
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
    all_warnings = []
    for filepath in sorted(set(files)):
        if (repo_root / filepath).exists():
            errors, warnings = check_file(filepath, repo_root)
            all_errors.extend(errors)
            all_warnings.extend(warnings)

    if all_warnings:
        print("Line-count warnings:\n")
        for warning in all_warnings:
            print(f"  {warning}")
        print()

    if all_errors:
        print("Line-count violations:\n")
        for error in all_errors:
            print(f"  {error}")
        print(f"\nTotal: {len(all_errors)} violations")
        print("\nRules:")
        print(f"  L001: No file over {HARD_LIMIT} lines (hard limit)")
        print(f"  L002: File over {WARN_LIMIT} lines (split recommended)")
        return 1

    print(f"Checked {len(files)} files, no violations")
    return 0


if __name__ == "__main__":
    sys.exit(main())
