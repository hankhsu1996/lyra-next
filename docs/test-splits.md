# Test file splits

Tracking file for `lyra-db/src/tests/` modules that need splitting into directory modules. See `CLAUDE.local.md` "Test File Organization" for the convention.

Rule: single files under 800 lines stay flat; over 800 becomes a directory module with domain submodules.

## Completed

### `elab.rs` (1219 lines -> `elab/`)

Split into: `mod.rs` (helpers), `signature.rs`, `ports.rs`, `instances.rs`, `params.rs`, `generate.rs`, `identity.rs`, `interface.rs`

## Pending

### `type_of.rs` (1153 lines)

Recommended split:
- `mod.rs` -- helpers (find_symbol, get_type, get_type_raw)
- `basic.rs` -- logic, int, wire, port, param base types
- `typedef.rs` -- typedef expansion, chains, cycles, dims merge
- `aggregate.rs` -- enum, struct, union types and their members
- `interface.rs` -- interface-typed port/variable/identity tests
- `dims.rs` -- unpacked dims, real/string arrays, multi-dim

### `expr_type.rs` (1120 lines)

Recommended split:
- `mod.rs` -- helpers (expr_type_of_first_param, bv constructors)
- `literals.rs` -- int, sized, unsized, real, string literals
- `operators.rs` -- prefix, binary, shift, relational, equality, logical, power, conditional
- `indexing.rs` -- concat, replic, unpacked index, packed bitselect, range
- `calls.rs` -- function/task calls, system calls, package calls, callable sig
- `members.rs` -- struct/union/enum field access

### `resolve.rs` (847 lines)

Recommended split:
- `mod.rs` -- (no shared helpers needed, just submodule declarations)
- `basic.rs` -- port, net, var, block scope, shadowing, multi-declarator
- `imports.rs` -- explicit/wildcard imports, ambiguity, local shadows, qualified names
- `typedef.rs` -- typedef resolution, namespace coexistence, package typedef
- `diagnostics.rs` -- unresolved, duplicate, not-a-type diagnostics

### `mod.rs` (581 lines)

Extract ~400 lines of DB infrastructure/incremental tests into `infra.rs`. Keep shared helpers and smoke tests in `mod.rs`.
