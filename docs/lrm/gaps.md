# LRM Gaps

Known gaps between LRM requirements and current engine capabilities. This is the work queue for LRM signoff. See `docs/lrm-signoff.md` for methodology.

When you discover a gap during `/lrm-add`, add an entry here. When you fix the gap and add the passing test, remove the entry. Both changes land in the same PR.

## Chapter 26 -- Packages

### 26.3: Enum literal import semantics

Importing an enum type does not import its enumeration literals (LRM 26.3 teeth_t example). Importing `teeth_t` from package `q` does not make `FALSE` visible -- a bare reference to `FALSE` still resolves to `p::FALSE` via a wildcard import of `p`. Not tested. Blocked by: semantic (enum literal visibility tracking). Test: `lrm/ch26/enum_literal_import`.

### 26.3: Wildcard import scoping across generate blocks

LRM 26.3 Examples 1-4 define complex lexical scoping rules for wildcard imports: imports in outer scopes affect inner generate blocks, positional (before/after) matters, and function/task calls search to end of scope. Not tested, likely not implemented. Blocked by: semantic (positional wildcard import resolution). Tests: `lrm/ch26/wildcard_scope_blocks`.

### 26.3: Later local declaration conflicts with wildcard-imported name

"If a wildcard imported symbol is made locally visible in a scope, any later locally visible declaration of the same name in that scope shall be illegal" (LRM 26.3 Example 1 line 5). Error case not tested. Blocked by: semantic (conflict detection between wildcard import and later local decl). Test: `lrm/ch26/wildcard_local_conflict`.

### 26.6: Export makes import a reference (later local decl illegal)

An export of an unreferenced candidate for import counts as a reference, importing the declaration. A subsequent local declaration of the same name is then illegal (LRM p6 example). Error case not tested. Blocked by: semantic (export-triggers-import semantics). Test: `lrm/ch26/export_triggers_import`.

### 26.7: std package contents (Annex G)

The std built-in package should contain process, mailbox, and semaphore classes per Annex G. Not tested. Blocked by: class support (Ch 8). Test: deferred until class support lands.
