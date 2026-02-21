# Lyra Next

Lyra Next is a SystemVerilog semantic analysis engine written in Rust, targeting IEEE 1800-2023. It builds a complete, incremental understanding of a SystemVerilog codebase and exposes semantics as a reusable core for future tools such as simulators, synthesizers, and language servers.

## What it provides

- Hand-written recursive-descent parser producing a lossless CST
- Normalized typed AST with stable node identities
- Symbol and scope resolution across files and packages
- Type and constant evaluation for widths, dimensions, and parameters
- Structured diagnostics with labels and fix-its
- Incremental query database powered by Salsa

## Design

The architecture is built around fast feedback on large projects. A few principles shape every decision:

- **Stable identities.** Every definition gets an ID that survives incremental edits. Queries compose over IDs, not borrowed references.
- **Strict layering.** Parse, index, resolve, type-check, and evaluate are separate query stages with no back-edges.
- **Pure query boundaries.** Each stage is a deterministic function of its inputs, enabling caching, parallelism, and reproducible results.

See `docs/architecture.md` for the full design.

## Relationship to Lyra (C++)

The original Lyra project is a SystemVerilog compiler written in C++.

Lyra Next is a ground-up rewrite in Rust focused on incremental correctness, semantic layering, and a safer memory model. Over time, Lyra Next is expected to become the primary implementation of Lyra.

## Building

```bash
cargo check
cargo test
```

Rust toolchain is pinned via `rust-toolchain.toml`.

## Contributing

Design docs and development workflow are in `docs/`. Discuss larger design ideas before implementing.

## License

MIT License. See [LICENSE](LICENSE) for details.
