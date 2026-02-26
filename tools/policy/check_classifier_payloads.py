#!/usr/bin/env python3
"""Check that classifier Kind enums in lyra-ast contain only lightweight payloads.

Rules:
  P001: Kind enum variant contains a banned typed wrapper payload.

Scans only `pub enum` whose name ends with `Kind` in `crates/lyra-ast/src/`.
Banned payloads: TypeSpec, NameRef, QualifiedName, DottedName, SyntaxNode, SyntaxElement.
Allowed payloads: SyntaxToken, Expr, Option<Expr>, POD types.

Usage:
  python3 tools/policy/check_classifier_payloads.py
  python3 tools/policy/check_classifier_payloads.py --self-test
"""

import argparse
import re
import subprocess
import sys
from pathlib import Path

TARGET_PREFIX = "crates/lyra-ast/src/"

BANNED_TYPES = frozenset({
    "TypeSpec",
    "NameRef",
    "QualifiedName",
    "DottedName",
    "SyntaxNode",
    "SyntaxElement",
})

RE_PUB_ENUM_KIND = re.compile(r'pub\s+enum\s+(\w+Kind)\b')
RE_VARIANT_TYPE = re.compile(r'\b([A-Z]\w+)\b')


def get_repo_root() -> Path:
    result = subprocess.run(
        ["git", "rev-parse", "--show-toplevel"],
        capture_output=True, text=True, check=True
    )
    return Path(result.stdout.strip())


def check_file(filepath: Path) -> list[str]:
    """Check a file for banned payloads in Kind enums."""
    errors = []
    try:
        content = filepath.read_text(encoding="utf-8", errors="replace")
    except OSError:
        return []

    lines = content.splitlines()
    in_kind_enum = False
    enum_name = ""
    brace_depth = 0

    for lineno_0, line in enumerate(lines):
        lineno = lineno_0 + 1
        stripped = line.strip()

        if stripped.startswith("//"):
            continue

        m = RE_PUB_ENUM_KIND.search(line)
        if m:
            in_kind_enum = True
            enum_name = m.group(1)
            brace_depth = 0

        if in_kind_enum:
            brace_depth += line.count('{') - line.count('}')
            if brace_depth <= 0 and '{' in content[:sum(len(l) + 1 for l in lines[:lineno_0 + 1])]:
                if brace_depth <= 0 and any('{' in l for l in lines[:lineno_0 + 1]):
                    pass
            for word in RE_VARIANT_TYPE.findall(line):
                if word in BANNED_TYPES:
                    errors.append(
                        f"{filepath}:{lineno}: P001 {enum_name} variant "
                        f"contains banned payload type `{word}`"
                    )
            if brace_depth <= 0 and '{' in ''.join(lines[max(0, lineno_0 - 20):lineno_0 + 1]):
                in_kind_enum = False

    return errors


def check_kind_enum_content(content: str) -> list[str]:
    """Check string content for banned payloads in Kind enums (for self-test)."""
    errors = []
    lines = content.splitlines()
    in_kind_enum = False
    enum_name = ""
    brace_depth = 0

    for lineno_0, line in enumerate(lines):
        lineno = lineno_0 + 1
        stripped = line.strip()
        if stripped.startswith("//"):
            continue

        m = RE_PUB_ENUM_KIND.search(line)
        if m:
            in_kind_enum = True
            enum_name = m.group(1)
            brace_depth = 0

        if in_kind_enum:
            brace_depth += line.count('{') - line.count('}')
            for word in RE_VARIANT_TYPE.findall(line):
                if word in BANNED_TYPES:
                    errors.append(
                        f"line {lineno}: P001 {enum_name} variant "
                        f"contains banned payload type `{word}`"
                    )
            if brace_depth <= 0 and any('{' in l for l in lines[:lineno_0 + 1]):
                in_kind_enum = False

    return errors


def self_test() -> int:
    """Run built-in self-tests."""
    failures = 0

    # Test 1: Kind enum with banned TypeSpec payload -> should fail
    bad = """\
pub enum UnpackedDimKind {
    Assoc { ts: TypeSpec },
    Unsized,
}
"""
    errs = check_kind_enum_content(bad)
    if not errs:
        print("FAIL: Kind enum with TypeSpec payload was not caught")
        failures += 1

    # Test 2: Non-Kind enum with NameRef -> should pass (not scanned)
    ok = """\
pub enum TypeNameRef {
    Simple(NameRef),
    Qualified(QualifiedName),
}
"""
    errs = check_kind_enum_content(ok)
    if errs:
        print(f"FAIL: Non-Kind enum was incorrectly flagged: {errs}")
        failures += 1

    # Test 3: Kind enum with allowed types -> should pass
    allowed = """\
pub enum UnpackedDimKind {
    Queue { bound: Option<Expr> },
    Size { expr: Expr },
    Wildcard,
}
"""
    errs = check_kind_enum_content(allowed)
    if errs:
        print(f"FAIL: Kind enum with allowed types was flagged: {errs}")
        failures += 1

    if failures:
        print(f"\n{failures} self-test(s) FAILED")
        return 1
    print("All self-tests passed")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Check classifier payload policy in lyra-ast"
    )
    parser.add_argument(
        "--self-test", action="store_true", help="Run built-in self-tests"
    )
    args = parser.parse_args()

    if args.self_test:
        return self_test()

    repo_root = get_repo_root()
    target_dir = repo_root / TARGET_PREFIX

    if not target_dir.exists():
        print(f"Target directory not found: {target_dir}")
        return 1

    all_errors = []
    for rs_file in sorted(target_dir.glob("**/*.rs")):
        errs = check_file(rs_file)
        all_errors.extend(errs)

    if all_errors:
        print("Classifier payload violations:\n")
        for error in all_errors:
            print(f"  {error}")
        print(f"\nTotal: {len(all_errors)} violations")
        print("\nKind enums must not contain TypeSpec, NameRef, "
              "QualifiedName, DottedName, SyntaxNode, or SyntaxElement payloads.")
        return 1

    print("No classifier payload violations")
    return 0


if __name__ == "__main__":
    sys.exit(main())
