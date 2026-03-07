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
import sys
from pathlib import Path

from policy_base import (
    Finding, add_scope_args, get_repo_root, report_findings, resolve_files,
)

INCLUDE_PATHS = ("crates/",)
EXTENSIONS = frozenset({".rs"})

HARD_LIMIT = 1200
WARN_LIMIT = 800

RULES = {
    "L001": f"No file over {HARD_LIMIT} lines (hard limit)",
    "L002": f"File over {WARN_LIMIT} lines (split recommended)",
}


def should_check_file(filepath):
    path = Path(filepath)
    return (
        path.suffix in EXTENSIONS
        and any(filepath.startswith(inc) for inc in INCLUDE_PATHS)
    )


def check_file(filepath, repo_root):
    findings = []
    full_path = repo_root / filepath

    try:
        content = full_path.read_text(encoding="utf-8", errors="replace")
    except OSError as e:
        return [Finding("error", "L001", filepath, 0, f"failed to read: {e}")]

    line_count = content.count("\n")
    if content and not content.endswith("\n"):
        line_count += 1

    if line_count > HARD_LIMIT:
        findings.append(Finding(
            "error", "L001", filepath, 0,
            f"{line_count} lines (hard limit {HARD_LIMIT})"))
    elif line_count > WARN_LIMIT:
        findings.append(Finding(
            "warning", "L002", filepath, 0,
            f"{line_count} lines (split recommended at {WARN_LIMIT})"))

    return findings


def main():
    parser = argparse.ArgumentParser(
        description="Check file line-count policy")
    add_scope_args(parser)
    args = parser.parse_args()

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
