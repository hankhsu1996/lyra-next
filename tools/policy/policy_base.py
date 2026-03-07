#!/usr/bin/env python3
"""Shared infrastructure for policy check scripts.

Provides common scope resolution, finding model, test-exemption logic,
and reporting for all policy check scripts in tools/policy/.
"""

import argparse
import re
import subprocess
from dataclasses import dataclass
from pathlib import Path


@dataclass
class Finding:
    """A single policy finding (error or warning)."""

    level: str  # "error" or "warning"
    rule: str  # e.g. "R001", "L002"
    filepath: str
    line: int  # 0 for file-level findings
    message: str


def get_repo_root() -> Path:
    """Get the repository root directory."""
    result = subprocess.run(
        ["git", "rev-parse", "--show-toplevel"],
        capture_output=True, text=True, check=True,
    )
    return Path(result.stdout.strip())


def add_scope_args(parser: argparse.ArgumentParser) -> None:
    """Add mutually exclusive scope flags to an argument parser."""
    scope = parser.add_mutually_exclusive_group()
    scope.add_argument(
        "--diff-base", help="Check files changed since git ref")
    scope.add_argument(
        "--staged", action="store_true", help="Check staged files")
    scope.add_argument(
        "--all", action="store_true",
        help="Check all tracked files (default when no scope given)")


def resolve_files(args, repo_root, filter_fn):
    """Resolve the list of files to check based on scope flags.

    Returns a sorted, deduplicated list of repo-relative paths that
    pass filter_fn and exist on disk.
    """
    if getattr(args, "staged", False):
        raw = _git_diff_files(["--cached"])
    elif getattr(args, "diff_base", None):
        raw = _git_diff_files([args.diff_base])
    else:
        raw = _all_tracked_files(repo_root)
    return sorted(set(
        f for f in raw
        if filter_fn(f) and (repo_root / f).exists()
    ))


def _git_diff_files(extra_args):
    result = subprocess.run(
        ["git", "diff", "--name-only", "--diff-filter=ACMRT"] + extra_args,
        capture_output=True, text=True, check=True,
    )
    return [f for f in result.stdout.strip().split("\n") if f]


def _all_tracked_files(repo_root):
    result = subprocess.run(
        ["git", "ls-files"],
        capture_output=True, text=True, check=True, cwd=repo_root,
    )
    return [f for f in result.stdout.strip().split("\n") if f]


def report_findings(findings, rules, checked_count):
    """Print findings and return exit code (1 if errors, 0 otherwise)."""
    errors = [f for f in findings if f.level == "error"]
    warnings = [f for f in findings if f.level == "warning"]

    if warnings:
        print("Warnings:\n")
        for w in warnings:
            _print_finding(w)
        print()

    if errors:
        print("Violations:\n")
        for e in errors:
            _print_finding(e)
        print(f"\nTotal: {len(errors)} violation(s)")

    if rules and (errors or warnings):
        print("\nRules:")
        for code, desc in sorted(rules.items()):
            print(f"  {code}: {desc}")

    if not errors and not warnings:
        print(f"Checked {checked_count} files, no findings")

    return 1 if errors else 0


def _print_finding(f):
    if f.line > 0:
        print(f"  {f.filepath}:{f.line}: {f.rule} {f.message}")
    else:
        print(f"  {f.filepath}: {f.rule} {f.message}")


# Test exemption helpers

_RE_CFG_TEST = re.compile(r'#\[cfg\(test\)\]')
_RE_TEST_FN = re.compile(r'#\[test\]')


def compute_test_lines(lines):
    """Compute 0-based line indices that are inside test code.

    Handles #[cfg(test)] mod ... { } blocks by tracking brace depth,
    and individual #[test] fn blocks. Non-braced items (e.g.
    #[cfg(test)] use foo;) exempt only the item itself.
    """
    test_lines = set()
    i = 0
    n = len(lines)
    while i < n:
        line = lines[i]
        if _RE_CFG_TEST.search(line) or _RE_TEST_FN.search(line):
            end = _find_item_extent(lines, i)
            for j in range(i, end + 1):
                test_lines.add(j)
            i = end + 1
        else:
            i += 1
    return test_lines


def _find_item_extent(lines, start):
    """Find the last line of the item starting at start.

    Scans forward. If { is found before bare ;, tracks brace depth
    and returns the line where depth reaches 0. If ; is found first,
    returns that line. If file ends without either, returns start.
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
