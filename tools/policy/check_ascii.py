#!/usr/bin/env python3
"""Check ASCII-only policy compliance.

Rules:
  A001: No non-ASCII characters in text files

This enforces ASCII-only content to avoid encoding issues and ensure
consistency (no Unicode arrows, emojis, curly quotes, etc.).

Usage:
  python3 tools/policy/check_ascii.py                          # All files
  python3 tools/policy/check_ascii.py --diff-base origin/main  # Changed files
  python3 tools/policy/check_ascii.py --staged                 # Staged files
"""

import argparse
import sys
from pathlib import Path

from policy_base import (
    Finding, add_scope_args, get_repo_root, report_findings, resolve_files,
)

EXTENSIONS = frozenset({
    ".rs", ".toml", ".md", ".yaml", ".yml", ".json", ".txt", ".sh", ".py",
})
SPECIAL_FILES = frozenset({"Makefile", "Dockerfile"})

RULES = {
    "A001": "Only ASCII characters (0x00-0x7F) allowed in text files",
}


def should_check_file(filepath):
    path = Path(filepath)
    return path.suffix in EXTENSIONS or path.name in SPECIAL_FILES


def check_file(filepath, repo_root):
    findings = []
    full_path = repo_root / filepath

    try:
        content = full_path.read_bytes()
    except OSError as e:
        return [Finding("error", "A001", filepath, 0, f"failed to read: {e}")]

    lines = content.split(b'\n')
    for lineno, line in enumerate(lines, 1):
        for col, byte in enumerate(line, 1):
            if byte > 127:
                try:
                    char_bytes = bytes([byte])
                    remaining = line[col:]
                    for i in range(min(3, len(remaining))):
                        if remaining[i] & 0xC0 == 0x80:
                            char_bytes += bytes([remaining[i]])
                        else:
                            break
                    char = char_bytes.decode('utf-8', errors='replace')
                except Exception:
                    char = '?'
                findings.append(Finding(
                    "error", "A001", filepath, lineno,
                    f"non-ASCII character '{char}' (0x{byte:02x})"))
                break  # Only report first non-ASCII per line

    return findings


def main():
    parser = argparse.ArgumentParser(description="Check ASCII-only policy")
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
