#!/usr/bin/env python3
"""Check CST layering policy: no raw CST traversal in lyra-semantic or lyra-db.

Rules:
  C001: No CST traversal calls (.children(), .descendants(), etc.)
  C002: No manual Expression/ParenExpr wrapper matching
  C003: No SyntaxKind discrimination (use typed AST accessors)
  C004: No SyntaxNode/SyntaxToken in lyra-semantic non-builder code
  C005: No .syntax() calls in lyra-semantic non-builder code

Consumer layers (lyra-semantic, lyra-db) should use typed AST accessors from
lyra-ast, not raw CST traversal. Use `Expr::peeled()` instead of matching on
`SyntaxKind::Expression` or `SyntaxKind::ParenExpr`. Builder-phase modules
are permanently allowlisted since their job is to walk the CST via direct
dot-calls.

Usage:
  python3 tools/policy/check_cst_layering.py                          # All files
  python3 tools/policy/check_cst_layering.py --diff-base origin/main  # Changed files
  python3 tools/policy/check_cst_layering.py --staged                 # Staged files
"""

import argparse
import re
import subprocess
import sys
from pathlib import Path

# Per-rule allowlists for each checked crate. Modules listed here are exempt
# from the specified rule only.
#
# Permanent entries need no annotation. Transitional entries must have a
# TODO(<tag>) comment so they can be grepped and cleaned up.

C001_ALLOWED: dict[str, frozenset[str]] = {
    "crates/lyra-semantic/src/": frozenset({
        "builder.rs",
        "builder_items.rs",
        "builder_stmt.rs",
        "builder_types.rs",
        "builder_order.rs",
    }),
    "crates/lyra-db/src/": frozenset(),
}

C002_ALLOWED: dict[str, frozenset[str]] = {
    "crates/lyra-semantic/src/": frozenset({
        "builder.rs",
        "builder_items.rs",
        "builder_stmt.rs",
        "builder_types.rs",
        "builder_order.rs",
    }),
    "crates/lyra-db/src/": frozenset(),
}

C003_ALLOWED: dict[str, frozenset[str]] = {
    "crates/lyra-semantic/src/": frozenset({
        # Permanent: builder code walks CST by design
        "builder.rs",
        "builder_items.rs",
        "builder_stmt.rs",
        "builder_types.rs",
    }),
    "crates/lyra-db/src/": frozenset(),
}

# C004: Ban raw SyntaxNode/SyntaxToken names in lyra-semantic non-builder code.
# lyra-db is allowed to use SyntaxNode (it orchestrates CST-to-typed conversion).
C004_ALLOWED: dict[str, frozenset[str]] = {
    "crates/lyra-semantic/src/": frozenset({
        "builder.rs",
        "builder_items.rs",
        "builder_stmt.rs",
        "builder_types.rs",
        "builder_order.rs",
    }),
}

# C005: Ban .syntax() calls in lyra-semantic non-builder code.
# lyra-db and lyra-ast may call .syntax() freely.
C005_ALLOWED: dict[str, frozenset[str]] = {
    "crates/lyra-semantic/src/": frozenset({
        "builder.rs",
        "builder_items.rs",
        "builder_stmt.rs",
        "builder_types.rs",
        "builder_order.rs",
    }),
}

# Combined set of all checked crate prefixes.
CHECKED_CRATES = (
    set(C001_ALLOWED) | set(C002_ALLOWED)
    | set(C003_ALLOWED) | set(C004_ALLOWED)
    | set(C005_ALLOWED)
)

# Derive a human-readable crate label from the prefix for error messages.
_CRATE_LABELS: dict[str, str] = {
    prefix: prefix.split("/")[1] for prefix in CHECKED_CRATES
}

# C001: CST traversal method calls
RE_CST_TRAVERSAL = re.compile(
    r'\.'
    r'(?:children|descendants|first_child|last_child'
    r'|children_with_tokens|first_token|last_token'
    r'|descendants_with_tokens'
    r')\s*\('
)

# C002: Manual wrapper-kind matching (use Expr::peeled() instead)
RE_WRAPPER_MATCH = re.compile(
    r'SyntaxKind::(?:Expression|ParenExpr)\b'
)

# C003: SyntaxKind discrimination
RE_SYNTAX_KIND_USE = re.compile(r'SyntaxKind::')
RE_SYNTAX_KIND_IMPORT = re.compile(r'\buse\s+[\w:]+::SyntaxKind\b')

# C004: Raw SyntaxNode/SyntaxToken usage (imports + qualified paths).
# Matches `::SyntaxNode`, `{SyntaxNode`, `, SyntaxNode` (group import).
# Bare unqualified uses are caught transitively via the import line.
RE_SYNTAX_NODE_TOKEN = re.compile(
    r'(?:::|\{|,)\s*(?:SyntaxNode|SyntaxToken)\b'
)

# C005: .syntax() calls (raw CST escape hatch).
# Handles optional whitespace: `.syntax ()`, `.syntax(  )`.
RE_DOT_SYNTAX = re.compile(r'\.syntax\s*\(\s*\)')


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


# Prefixes sorted longest-first so the most specific match wins.
_SORTED_PREFIXES = sorted(CHECKED_CRATES, key=len, reverse=True)


def _matching_crate(filepath: str) -> str | None:
    """Return the crate prefix if filepath belongs to a checked crate."""
    for prefix in _SORTED_PREFIXES:
        if filepath.startswith(prefix):
            return prefix
    return None


def should_check_file(filepath: str) -> bool:
    if _matching_crate(filepath) is None:
        return False
    if not filepath.endswith(".rs"):
        return False
    # Test files are exempt.
    if "/tests/" in filepath or "/tests.rs" in filepath:
        return False
    if filepath.endswith("_tests.rs"):
        return False
    return True


def _module_name(filepath: str) -> str:
    """Extract module-relative path from the full filepath.

    For example, 'crates/lyra-semantic/src/type_infer/mod.rs' -> 'type_infer/mod.rs'.
    """
    prefix = _matching_crate(filepath)
    if prefix:
        return filepath.removeprefix(prefix)
    return filepath


def _is_allowed(filepath: str, allowlist: dict[str, frozenset[str]]) -> bool:
    prefix = _matching_crate(filepath)
    if prefix is None:
        return False
    return _module_name(filepath) in allowlist.get(prefix, frozenset())


# Test detection for files that embed #[cfg(test)] blocks.
RE_CFG_TEST = re.compile(r'#\[cfg\(test\)\]')
RE_TEST_FN = re.compile(r'#\[test\]')


def _find_item_extent(lines: list[str], start: int) -> int:
    """Find the last line of the item starting at `start`.

    Scans forward from `start`. If a `{` is found before a bare `;`,
    tracks brace depth and returns the line where depth reaches 0.
    If a `;` is found first (non-braced item), returns that line.
    If the file ends without either, returns `start` (defensive).
    """
    n = len(lines)
    depth = 0
    found_open = False
    for j in range(start, n):
        line_j = lines[j]
        # Bare semicolon before any brace -> single item (e.g. `use foo;`)
        if not found_open and ';' in line_j and '{' not in line_j:
            return j
        depth += line_j.count('{') - line_j.count('}')
        if not found_open and '{' in line_j:
            found_open = True
        if found_open and depth <= 0:
            return j
    return start


def compute_test_lines(lines: list[str]) -> set[int]:
    """Compute the set of 0-based line indices that are inside test code.

    Handles #[cfg(test)] mod ... { } blocks by tracking brace depth,
    and individual #[test] fn blocks. Non-braced items (e.g.
    `#[cfg(test)] use foo;`) exempt only the item itself.
    """
    test_lines: set[int] = set()
    i = 0
    n = len(lines)
    while i < n:
        line = lines[i]
        if RE_CFG_TEST.search(line):
            end = _find_item_extent(lines, i)
            for j in range(i, end + 1):
                test_lines.add(j)
            i = end + 1
            continue
        if RE_TEST_FN.search(line):
            end = _find_item_extent(lines, i)
            for j in range(i, end + 1):
                test_lines.add(j)
            i = end + 1
            continue
        i += 1
    return test_lines


def strip_comments(lines: list[str]) -> list[str]:
    """Strip Rust comments from lines, preserving line count.

    Handles // line comments and /* */ block comments (including
    multi-line). Returns a list of the same length with comment
    content replaced by spaces.
    """
    result = []
    in_block = False
    for line in lines:
        out = []
        i = 0
        n = len(line)
        while i < n:
            if in_block:
                end = line.find("*/", i)
                if end == -1:
                    out.append(" " * (n - i))
                    i = n
                else:
                    out.append(" " * (end - i + 2))
                    i = end + 2
                    in_block = False
            elif i + 1 < n and line[i] == '/' and line[i + 1] == '/':
                out.append(" " * (n - i))
                i = n
            elif i + 1 < n and line[i] == '/' and line[i + 1] == '*':
                in_block = True
                out.append("  ")
                i += 2
            else:
                out.append(line[i])
                i += 1
        result.append("".join(out))
    return result


def check_violations(filepath: str, repo_root: Path) -> list[str]:
    """Check a file for C001-C005 violations.

    Returns list of violation messages.
    """
    errors = []
    full_path = repo_root / filepath
    prefix = _matching_crate(filepath)
    crate_label = _CRATE_LABELS.get(prefix, "unknown") if prefix else "unknown"

    c001_ok = _is_allowed(filepath, C001_ALLOWED)
    c002_ok = _is_allowed(filepath, C002_ALLOWED)
    c003_ok = _is_allowed(filepath, C003_ALLOWED)
    # C004 only applies to crates listed in C004_ALLOWED (lyra-semantic).
    c004_applies = prefix is not None and prefix in C004_ALLOWED
    c004_ok = not c004_applies or _is_allowed(filepath, C004_ALLOWED)
    # C005 only applies to crates listed in C005_ALLOWED (lyra-semantic).
    c005_applies = prefix is not None and prefix in C005_ALLOWED
    c005_ok = not c005_applies or _is_allowed(filepath, C005_ALLOWED)

    try:
        content = full_path.read_text(encoding="utf-8", errors="replace")
    except OSError as e:
        return [f"{filepath}: failed to read: {e}"]

    lines = content.splitlines()
    test_lines = compute_test_lines(lines)
    stripped_lines = strip_comments(lines)

    for lineno_0, stripped in enumerate(stripped_lines):
        lineno = lineno_0 + 1

        # Skip test code.
        if lineno_0 in test_lines:
            continue

        if not c001_ok and RE_CST_TRAVERSAL.search(stripped):
            errors.append(
                f"{filepath}:{lineno}: C001 CST traversal call ({crate_label})"
            )

        if not c002_ok and RE_WRAPPER_MATCH.search(stripped):
            errors.append(
                f"{filepath}:{lineno}: C002 manual wrapper match ({crate_label})"
            )

        if not c003_ok:
            if RE_SYNTAX_KIND_USE.search(stripped) or RE_SYNTAX_KIND_IMPORT.search(stripped):
                errors.append(
                    f"{filepath}:{lineno}: C003 SyntaxKind discrimination"
                    f" ({crate_label})"
                )

        if not c004_ok and RE_SYNTAX_NODE_TOKEN.search(stripped):
            errors.append(
                f"{filepath}:{lineno}: C004 raw SyntaxNode/SyntaxToken"
                f" ({crate_label})"
            )

        if not c005_ok and RE_DOT_SYNTAX.search(stripped):
            errors.append(
                f"{filepath}:{lineno}: C005 .syntax() call"
                f" ({crate_label})"
            )

    return errors


def self_test() -> int:
    """Run built-in tests for the policy script's own logic."""
    failures = 0

    # Test 1: code after #[cfg(test)] block is NOT exempt
    lines_1 = [
        '#[cfg(test)]',
        'mod tests {',
        '    fn foo() { node.children(); }',
        '}',
        'fn bar() { node.children(); }',
    ]
    tl = compute_test_lines(lines_1)
    if 4 in tl:
        print("FAIL: line after #[cfg(test)] block was exempt")
        failures += 1
    if 2 not in tl:
        print("FAIL: line inside #[cfg(test)] block was not exempt")
        failures += 1

    # Test 2: nested braces inside test module
    lines_2 = [
        '#[cfg(test)]',
        'mod tests {',
        '    fn foo() {',
        '        if true { node.children(); }',
        '    }',
        '}',
        'fn after() { node.children(); }',
    ]
    tl2 = compute_test_lines(lines_2)
    if 3 not in tl2:
        print("FAIL: nested brace line inside test was not exempt")
        failures += 1
    if 6 in tl2:
        print("FAIL: line after nested test block was exempt")
        failures += 1

    # Test 3: #[test] fn is exempt
    lines_3 = [
        'fn prod() { node.children(); }',
        '#[test]',
        'fn test_foo() {',
        '    node.children();',
        '}',
        'fn prod2() { node.children(); }',
    ]
    tl3 = compute_test_lines(lines_3)
    if 0 in tl3:
        print("FAIL: production fn before #[test] was exempt")
        failures += 1
    if 3 not in tl3:
        print("FAIL: line inside #[test] fn was not exempt")
        failures += 1
    if 5 in tl3:
        print("FAIL: production fn after #[test] fn was exempt")
        failures += 1

    # Test 4: empty file
    tl4 = compute_test_lines([])
    if tl4:
        print("FAIL: empty file produced test lines")
        failures += 1

    # Test 5: #[cfg(test)] on non-braced item (use) must not exempt rest of file
    lines_5 = [
        '#[cfg(test)]',
        'use some_crate::test_util;',
        'fn prod() { node.children(); }',
    ]
    tl5 = compute_test_lines(lines_5)
    if 0 not in tl5 or 1 not in tl5:
        print("FAIL: #[cfg(test)] use item lines were not exempt")
        failures += 1
    if 2 in tl5:
        print("FAIL: production fn after #[cfg(test)] use was exempt")
        failures += 1

    # Test 6: #[cfg(test)] on same line as non-braced item
    lines_6 = [
        '#[cfg(test)] use foo::bar;',
        'fn prod() { node.children(); }',
    ]
    tl6 = compute_test_lines(lines_6)
    if 0 not in tl6:
        print("FAIL: inline #[cfg(test)] use was not exempt")
        failures += 1
    if 1 in tl6:
        print("FAIL: production fn after inline #[cfg(test)] use was exempt")
        failures += 1

    # Test 7: #[cfg(test)] block followed by #[cfg(test)] use
    lines_7 = [
        '#[cfg(test)]',
        'mod tests {',
        '    fn foo() {}',
        '}',
        '#[cfg(test)]',
        'use test_util::helper;',
        'fn prod() { node.children(); }',
    ]
    tl7 = compute_test_lines(lines_7)
    if 6 in tl7:
        print("FAIL: production fn after block+use was exempt")
        failures += 1
    if 4 not in tl7 or 5 not in tl7:
        print("FAIL: #[cfg(test)] use after block was not exempt")
        failures += 1

    # Test 8 (C002): SyntaxKind::Expression in match arm -> should fire
    c003_bad = [
        'match node.kind() {',
        '    SyntaxKind::Expression => { wrapper.inner() }',
        '    SyntaxKind::ParenExpr => { paren.inner() }',
        '    _ => {}',
        '}',
    ]
    tl8 = compute_test_lines(c003_bad)
    for idx, line in enumerate(c003_bad):
        if idx in tl8:
            continue
        if RE_WRAPPER_MATCH.search(line):
            break
    else:
        print("FAIL: C002 did not catch SyntaxKind::Expression match")
        failures += 1

    # Test 9 (C002): SyntaxKind::Literal should NOT fire
    c003_ok = [
        'match node.kind() {',
        '    SyntaxKind::Literal => eval_literal(node),',
        '    SyntaxKind::BinExpr => eval_bin(node),',
        '    _ => {}',
        '}',
    ]
    for line in c003_ok:
        if RE_WRAPPER_MATCH.search(line):
            print(f"FAIL: C002 false positive on: {line.strip()}")
            failures += 1

    # Test 10 (C002): wrapper match inside #[cfg(test)] -> should NOT fire
    c003_test = [
        '#[cfg(test)]',
        'mod tests {',
        '    fn foo() { match k { SyntaxKind::Expression => {} _ => {} } }',
        '}',
    ]
    tl10 = compute_test_lines(c003_test)
    for idx, line in enumerate(c003_test):
        if idx in tl10:
            continue
        if RE_WRAPPER_MATCH.search(line):
            print("FAIL: C002 fired inside #[cfg(test)] block")
            failures += 1

    # Test 11 (C003): SyntaxKind::Literal should fire C003 (not just C002)
    for line in c003_ok:
        if RE_SYNTAX_KIND_USE.search(line):
            break
    else:
        print("FAIL: C003 did not catch SyntaxKind::Literal")
        failures += 1

    # Test 12 (C003): `use lyra_lexer::SyntaxKind;` should fire C003
    if not RE_SYNTAX_KIND_IMPORT.search('use lyra_lexer::SyntaxKind;'):
        print("FAIL: C003 did not catch SyntaxKind import")
        failures += 1

    # Test 13 (comment stripping): inline comment stripped
    stripped_13 = strip_comments(['let x = foo; // SyntaxKind::Bar'])
    if RE_SYNTAX_KIND_USE.search(stripped_13[0]):
        print("FAIL: SyntaxKind in inline comment was not stripped")
        failures += 1

    # Test 14 (comment stripping): block comment stripped
    stripped_14 = strip_comments(['let x = /* SyntaxKind::Foo */ bar;'])
    if RE_SYNTAX_KIND_USE.search(stripped_14[0]):
        print("FAIL: SyntaxKind in block comment was not stripped")
        failures += 1

    # Test 15 (comment stripping): multi-line block comment
    stripped_15 = strip_comments([
        'let x = /* start',
        'SyntaxKind::Foo',
        '*/ bar;',
    ])
    if RE_SYNTAX_KIND_USE.search(stripped_15[1]):
        print("FAIL: SyntaxKind in multi-line block comment was not stripped")
        failures += 1
    if 'bar' not in stripped_15[2]:
        print("FAIL: code after block comment end was stripped")
        failures += 1

    # Test 16 (comment stripping): code before comment preserved
    stripped_16 = strip_comments(['SyntaxKind::Foo; // comment'])
    if not RE_SYNTAX_KIND_USE.search(stripped_16[0]):
        print("FAIL: code before inline comment was incorrectly stripped")
        failures += 1

    # Test 17 (tightened import regex): broad pattern should not match
    if RE_SYNTAX_KIND_IMPORT.search('// use lyra_lexer::SyntaxKind;'):
        # OK in raw regex -- but strip_comments handles this.
        pass
    if RE_SYNTAX_KIND_IMPORT.search('use foo::SyntaxKindExt;'):
        print("FAIL: C003 import regex matched SyntaxKindExt")
        failures += 1

    # Test 18 (C004): qualified SyntaxNode should fire
    if not RE_SYNTAX_NODE_TOKEN.search('let n: lyra_parser::SyntaxNode = x;'):
        print("FAIL: C004 did not catch qualified SyntaxNode")
        failures += 1

    # Test 19 (C004): import of SyntaxToken should fire
    if not RE_SYNTAX_NODE_TOKEN.search('use lyra_parser::SyntaxToken;'):
        print("FAIL: C004 did not catch SyntaxToken import")
        failures += 1

    # Test 20 (C004): unrelated identifier should not fire
    if RE_SYNTAX_NODE_TOKEN.search('let syntax_node_count = 42;'):
        print("FAIL: C004 false positive on syntax_node_count")
        failures += 1

    # Test 21 (C004): group import should fire
    if not RE_SYNTAX_NODE_TOKEN.search('use lyra_parser::{SyntaxNode, Parse};'):
        print("FAIL: C004 did not catch group import of SyntaxNode")
        failures += 1

    # Test 22 (C004): group import second position should fire
    if not RE_SYNTAX_NODE_TOKEN.search('use lyra_parser::{Parse, SyntaxToken};'):
        print("FAIL: C004 did not catch second-position group import")
        failures += 1

    # Test 23 (C004): bare unqualified ref should not fire (import catches)
    if RE_SYNTAX_NODE_TOKEN.search('fn foo(node: &SyntaxNode) {}'):
        print("FAIL: C004 false positive on bare unqualified SyntaxNode")
        failures += 1

    # Test 24 (C004): string literal should not fire
    if RE_SYNTAX_NODE_TOKEN.search('"SyntaxNode is a CST handle"'):
        print("FAIL: C004 false positive on string literal")
        failures += 1

    # Test 25 (C005): .syntax() call should fire
    if not RE_DOT_SYNTAX.search('let n = expr.syntax();'):
        print("FAIL: C005 did not catch .syntax() call")
        failures += 1

    # Test 26 (C005): .syntax() in method chain should fire
    if not RE_DOT_SYNTAX.search('map.erased_ast_id(node.syntax())'):
        print("FAIL: C005 did not catch .syntax() in method chain")
        failures += 1

    # Test 27 (C005): syntax() without dot should not fire
    if RE_DOT_SYNTAX.search('fn syntax() -> &SyntaxNode {'):
        print("FAIL: C005 false positive on fn syntax()")
        failures += 1

    # Test 28 (C005): .syntax_node() should not fire
    if RE_DOT_SYNTAX.search('node.syntax_node()'):
        print("FAIL: C005 false positive on .syntax_node()")
        failures += 1

    # Test 29 (C005): .syntax () with space before paren should fire
    if not RE_DOT_SYNTAX.search('let n = expr.syntax ();'):
        print("FAIL: C005 did not catch .syntax () with space")
        failures += 1

    # Test 30 (C005): .syntax(  ) with inner spaces should fire
    if not RE_DOT_SYNTAX.search('node.syntax(  )'):
        print("FAIL: C005 did not catch .syntax(  ) with inner spaces")
        failures += 1

    if failures:
        print(f"\n{failures} self-test(s) FAILED")
        return 1
    print("All self-tests passed")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Check CST layering policy in lyra-semantic and lyra-db"
    )
    parser.add_argument(
        "--diff-base", help="Check files changed since git ref"
    )
    parser.add_argument(
        "--staged", action="store_true", help="Check staged files"
    )
    parser.add_argument(
        "--self-test", action="store_true", help="Run built-in self-tests"
    )
    args = parser.parse_args()

    if args.self_test:
        return self_test()

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
        if not (repo_root / filepath).exists():
            continue
        all_errors.extend(check_violations(filepath, repo_root))

    if all_errors:
        print("CST layering violations:\n")
        for error in all_errors:
            print(f"  {error}")
        print(f"\nTotal: {len(all_errors)} violations")
        print("\nRules:")
        print("  C001: No .children()/.descendants()/etc. CST traversal")
        print("  C002: No SyntaxKind::Expression/ParenExpr matching "
              "(use Expr::peeled())")
        print("  C003: No SyntaxKind discrimination "
              "(use typed AST accessors)")
        print("  C004: No raw SyntaxNode/SyntaxToken in lyra-semantic "
              "(use typed wrappers)")
        print("  C005: No .syntax() calls in lyra-semantic "
              "(add typed accessor in lyra-ast)")
        print("Allowlisted modules: see C00x_ALLOWED in this script")
        return 1

    checked = len(files)
    print(f"Checked {checked} files, no violations")
    return 0


if __name__ == "__main__":
    sys.exit(main())
