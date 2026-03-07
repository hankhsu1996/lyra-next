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

import subprocess
import sys
from pathlib import Path

from policy_base import add_scope_args

CHECKS = [
    "check_errors.py",
    "check_ascii.py",
    "check_cst_layering.py",
    "check_lines.py",
    "check_diag_textrange.py",
]


def resolve_diff_base():
    """Compute default diff base via git merge-base origin/main HEAD."""
    result = subprocess.run(
        ["git", "merge-base", "origin/main", "HEAD"],
        capture_output=True, text=True,
    )
    if result.returncode != 0:
        return ""
    return result.stdout.strip()


def build_scope_args(args):
    """Convert parsed args to CLI flags for child scripts."""
    if args.staged:
        return ["--staged"]
    if getattr(args, "all", False):
        return ["--all"]
    if args.diff_base:
        return ["--diff-base", args.diff_base]
    base = resolve_diff_base()
    if not base:
        print(
            "warning: could not resolve merge-base (no origin/main?), "
            "falling back to checking ALL files",
            file=sys.stderr,
        )
        return ["--all"]
    return ["--diff-base", base]


def main():
    import argparse
    parser = argparse.ArgumentParser(description="Run all policy checks")
    add_scope_args(parser)
    args = parser.parse_args()

    policy_dir = Path(__file__).parent
    scope_args = build_scope_args(args)

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
