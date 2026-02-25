#!/usr/bin/env python3
"""Check that definition-namespace items are not created as Symbols.

Rules:
  N001: No SymbolKind::{Module,Package,Interface,Program,Primitive,Config}
        construction in builder code (push_symbol call sites).

Definition-namespace items (module, package, interface, program, primitive,
config) must use push_def_entry, not push_symbol. The builder's push_symbol
has a runtime guard that emits InternalError if a def-namespace kind sneaks
through, but this script catches it at review time.

Usage:
  python3 tools/policy/check_def_namespace_symbols.py
  python3 tools/policy/check_def_namespace_symbols.py --diff-base origin/main
  python3 tools/policy/check_def_namespace_symbols.py --staged
"""

import argparse
import re
import subprocess
import sys
from pathlib import Path

# Only check builder files where symbols are constructed
TARGET_FILES = frozenset({
    "crates/lyra-semantic/src/builder.rs",
    "crates/lyra-semantic/src/builder_items.rs",
    "crates/lyra-semantic/src/builder_types.rs",
})

DEF_NAMESPACE_KINDS = [
    "Module", "Package", "Interface", "Program", "Primitive", "Config",
]

# Match SymbolKind::<def-namespace-kind> in push_symbol call contexts.
# This catches both `kind: SymbolKind::Module` and `SymbolKind::Module`.
RE_DEF_NS_SYMBOL = re.compile(
    r'SymbolKind::(' + '|'.join(DEF_NAMESPACE_KINDS) + r')\b'
)

# Lines in push_symbol's debug_assert and check_name_site_invariants
# are allowed -- they guard against misuse.
GUARD_PATTERNS = [
    "debug_assert!",
    "check_name_site_invariants",
    "expected_name_kind",
    "emit_internal_error",
    "def-namespace kind",
    "routed to push_symbol",
]


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


def is_in_guard_context(lines: list[str], lineno_0: int) -> bool:
    """Check if the line is within a guard block (debug_assert, invariant check)."""
    for offset in range(-20, 21):
        idx = lineno_0 + offset
        if 0 <= idx < len(lines):
            for pattern in GUARD_PATTERNS:
                if pattern in lines[idx]:
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

        # Skip comments
        if stripped.startswith("//"):
            continue

        if RE_DEF_NS_SYMBOL.search(line):
            # Allow guard/invariant-check contexts
            if is_in_guard_context(lines, lineno_0):
                continue
            errors.append(
                f"{filepath}:{lineno}: N001 def-namespace SymbolKind "
                f"construction outside guard context"
            )

    return errors


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Check def-namespace items use push_def_entry, not push_symbol"
    )
    parser.add_argument(
        "--diff-base", help="Check files changed since git ref"
    )
    parser.add_argument(
        "--staged", action="store_true", help="Check staged files"
    )
    args = parser.parse_args()

    repo_root = get_repo_root()

    if args.staged:
        files = get_git_files(["--cached"])
    elif args.diff_base:
        files = get_git_files([args.diff_base])
    else:
        files = get_all_files(repo_root)

    files = [f for f in files if f in TARGET_FILES]

    if not files:
        print("No builder files to check")
        return 0

    all_errors = []
    for filepath in sorted(set(files)):
        if (repo_root / filepath).exists():
            all_errors.extend(check_file(filepath, repo_root))

    if all_errors:
        print("Def-namespace symbol policy violations:\n")
        for error in all_errors:
            print(f"  {error}")
        print(f"\nTotal: {len(all_errors)} violations")
        print("\nRule:")
        print("  N001: Definition-namespace items (module, package, interface,")
        print("        program, primitive, config) must use push_def_entry,")
        print("        not push_symbol. SymbolKind::{Module,...} should only")
        print("        appear in guard/invariant-check contexts.")
        return 1

    print(f"Checked {len(files)} builder files, no violations")
    return 0


if __name__ == "__main__":
    sys.exit(main())
