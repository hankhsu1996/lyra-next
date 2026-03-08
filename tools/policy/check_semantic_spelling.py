#!/usr/bin/env python3
"""Policy check: no raw identifier token text as semantic names.

Semantic-name construction sites must use `semantic_spelling()` from
`lyra-ast`, not `SmolStr::new(tok.text())` on identifier tokens.

This check greps for the raw pattern in semantic crates to prevent
regression of the escaped-identifier normalization fix (LRM 5.6.1).

Test code is excluded from the check.
"""

import subprocess
import sys
import os

# Crate source directories to check
CHECKED_DIRS = [
    "crates/lyra-semantic/src",
    "crates/lyra-db/src",
]

def main():
    # Accept common policy-check args; only --staged is meaningful here.
    # --diff-base and --all are accepted but ignored (always checks all
    # files in CHECKED_DIRS since the check is fast).
    staged_only = "--staged" in sys.argv
    staged_files = set()

    if staged_only:
        result = subprocess.run(
            ["git", "diff", "--cached", "--name-only"],
            capture_output=True, text=True,
        )
        staged_files = set(result.stdout.strip().split("\n")) if result.stdout.strip() else set()

    violations = []

    for checked_dir in CHECKED_DIRS:
        for root, _dirs, files in os.walk(checked_dir):
            for fname in files:
                if not fname.endswith(".rs"):
                    continue
                path = os.path.join(root, fname)
                rel = os.path.relpath(path)

                if staged_only and rel not in staged_files:
                    continue

                with open(path) as f:
                    lines = f.readlines()

                in_test = False
                for lineno, line in enumerate(lines, 1):
                    stripped = line.strip()
                    # Skip comments
                    if stripped.startswith("//"):
                        continue
                    # Skip test blocks
                    if "#[cfg(test)]" in stripped:
                        in_test = True
                    if in_test:
                        continue

                    # Check for the raw pattern
                    if "SmolStr::new(" in stripped and ".text())" in stripped:
                        violations.append(f"{rel}:{lineno}: {stripped}")

    if violations:
        print("ERROR: Raw token text used as semantic name.")
        print("Use semantic_spelling() or FieldExpr::member_lookup_name() instead.")
        print()
        for v in violations:
            print(f"  {v}")
        print()
        print(f"{len(violations)} violation(s) found.")
        sys.exit(1)
    else:
        print("OK: No raw identifier token text in semantic names.")
        sys.exit(0)


if __name__ == "__main__":
    main()
