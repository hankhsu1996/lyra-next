#!/usr/bin/env python3
"""Check that TextRange does not leak into semantic diagnostic types.

Rules:
  D001: No TextRange type references in lyra-semantic files
  D002: No text_range() method calls in lyra-semantic files

The semantic layer stores stable anchors (Site, NameSpan, TokenSpan) in
diagnostics. TextRange computation belongs in the lowering layer (lyra-db).
Producer modules that construct diagnostic anchors are allowlisted.

Usage:
  python3 tools/policy/check_diag_textrange.py                          # All files
  python3 tools/policy/check_diag_textrange.py --diff-base origin/main  # Changed files
  python3 tools/policy/check_diag_textrange.py --staged                 # Staged files
"""

import argparse
import re
import sys

from policy_base import (
    Finding, add_scope_args, get_repo_root, report_findings, resolve_files,
)

TARGET_PREFIX = "crates/lyra-semantic/src/"

# D001: TextRange in code (imports, type annotations, expressions).
RE_TEXTRANGE = re.compile(r'\bTextRange\b')

# D002: text_range() method calls.
RE_TEXT_RANGE_CALL = re.compile(r'\btext_range\s*\(')

# Modules that legitimately use TextRange as part of anchor construction.
D001_ALLOWED = frozenset({
    "builder.rs",
    "builder_items.rs",
    "builder_stmt.rs",
    "builder_types.rs",
    "builder_order.rs",
    "type_check.rs",
    "type_check_expr.rs",
    "streaming/check.rs",
})

D002_ALLOWED = frozenset({
    "builder.rs",
    "builder_items.rs",
    "builder_stmt.rs",
    "builder_types.rs",
    "builder_order.rs",
    "type_check.rs",
    "type_check_expr.rs",
    "streaming/check.rs",
})

RULES = {
    "D001": "No TextRange type references in lyra-semantic files",
    "D002": "No text_range() method calls in lyra-semantic files",
}


def should_check_file(filepath):
    if not filepath.startswith(TARGET_PREFIX):
        return False
    if not filepath.endswith(".rs"):
        return False
    if "/tests/" in filepath or "/tests.rs" in filepath:
        return False
    if filepath.endswith("_tests.rs"):
        return False
    return True


def check_file(filepath, repo_root):
    findings = []
    full_path = repo_root / filepath
    module = filepath.removeprefix(TARGET_PREFIX)

    try:
        content = full_path.read_text(encoding="utf-8", errors="replace")
    except OSError as e:
        return [Finding("error", "D001", filepath, 0, f"failed to read: {e}")]

    lines = content.splitlines()
    check_d001 = module not in D001_ALLOWED
    check_d002 = module not in D002_ALLOWED

    for lineno_0, line in enumerate(lines):
        lineno = lineno_0 + 1
        stripped = line.lstrip()

        if stripped.startswith("//"):
            continue

        if check_d001 and RE_TEXTRANGE.search(line):
            findings.append(Finding(
                "error", "D001", filepath, lineno,
                "TextRange reference"))

        if check_d002 and RE_TEXT_RANGE_CALL.search(line):
            findings.append(Finding(
                "error", "D002", filepath, lineno,
                "text_range() call"))

    return findings


def main():
    parser = argparse.ArgumentParser(
        description="Check TextRange does not leak into semantic diagnostics")
    add_scope_args(parser)
    args = parser.parse_args()

    repo_root = get_repo_root()
    files = resolve_files(args, repo_root, should_check_file)

    if not files:
        print("No lyra-semantic files to check")
        return 0

    all_findings = []
    for filepath in files:
        all_findings.extend(check_file(filepath, repo_root))

    return report_findings(all_findings, RULES, len(files))


if __name__ == "__main__":
    sys.exit(main())
