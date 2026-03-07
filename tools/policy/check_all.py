#!/usr/bin/env python3
"""Run all policy checks.

Computes the diff base automatically (merge-base of origin/main and HEAD)
and runs every check script in tools/policy/.

Usage:
  python3 tools/policy/check_all.py                          # Auto diff-base
  python3 tools/policy/check_all.py --diff-base origin/main  # Explicit base
  python3 tools/policy/check_all.py --staged                 # Staged files
  python3 tools/policy/check_all.py --all                    # All files
"""

import argparse
import subprocess
import sys
from pathlib import Path

CHECKS = [
    "check_errors.py",
    "check_ascii.py",
    "check_cst_layering.py",
    "check_lines.py",
    "check_diag_textrange.py",
]


def resolve_diff_base() -> str:
    """Compute default diff base via git merge-base origin/main HEAD."""
    result = subprocess.run(
        ["git", "merge-base", "origin/main", "HEAD"],
        capture_output=True, text=True,
    )
    if result.returncode != 0:
        return ""
    return result.stdout.strip()


def main() -> int:
    parser = argparse.ArgumentParser(description="Run all policy checks")
    scope = parser.add_mutually_exclusive_group()
    scope.add_argument(
        "--diff-base", help="Check files changed since git ref")
    scope.add_argument("--staged", action="store_true",
                       help="Check staged files")
    scope.add_argument("--all", action="store_true",
                       help="Check all files (no diff filtering)")
    args = parser.parse_args()

    policy_dir = Path(__file__).parent

    # Build the scope args passed to each check script.
    if args.staged:
        scope_args = ["--staged"]
    elif args.all:
        scope_args = []
    elif args.diff_base:
        scope_args = ["--diff-base", args.diff_base]
    else:
        base = resolve_diff_base()
        if not base:
            print(
                "warning: could not resolve merge-base (no origin/main?), "
                "falling back to checking ALL files",
                file=sys.stderr,
            )
            scope_args = []
        else:
            scope_args = ["--diff-base", base]

    failed = []
    for check in CHECKS:
        print(f"--- {check} ---")
        script = policy_dir / check
        result = subprocess.run(
            [sys.executable, str(script)] + scope_args,
            capture_output=False,
        )
        if result.returncode != 0:
            failed.append(check)

    if failed:
        print(f"\n{len(failed)} check(s) failed: {', '.join(failed)}")
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
