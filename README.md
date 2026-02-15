# Lyra Next

Lyra Next is a Rust-based SystemVerilog semantic platform -- a shared foundation for tooling such as language servers, linters, formatters, compilers, simulators, and synthesis flows.

Instead of building each tool independently, Lyra Next provides a single incremental semantic model of the codebase that all tools can query.

## What this project is

Lyra Next is not a compiler frontend, not a simulator, and not an LSP.

It is a semantic database for SystemVerilog:

- Lossless CST (trivia-preserving syntax tree)
- Normalized typed AST
- Symbol and scope resolution with stable IDs
- Type system and constant evaluation foundations
- Structured diagnostics with precise spans and fix-its
- Incremental query engine (only recompute what changed)

Tools built on top of Lyra Next share the same semantic model instead of reimplementing their own.

## Relationship to Lyra (C++)

The original Lyra project is a SystemVerilog compiler written in C++.

Lyra Next explores a new architecture in Rust focused on:

- Stronger semantic layering
- Incremental correctness by design
- Safer memory model
- A reusable platform for the broader tooling ecosystem

Over time, Lyra Next is expected to become the primary implementation of Lyra.

## Current status

This repository is in the early scaffold stage.

Implemented:
- Workspace structure
- Core crate layout
- Arena wrapper
- Source model primitives
- Salsa database wiring
- Rowan CST foundation

Not implemented yet:
- Full lexer
- SystemVerilog grammar
- Name resolution
- Type system
- Elaboration

Expect rapid architectural changes.

## Building

```bash
cargo check
cargo test
```

Rust toolchain is pinned via `rust-toolchain.toml`.

## Contributing

This project is currently in active design phase.

If you want to contribute:

* Read `docs/architecture.md`
* Look for issues labeled `good-first-layer`
* Discuss larger design ideas before implementing

## License

MIT License. See [LICENSE](LICENSE) for details.
