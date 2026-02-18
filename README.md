# Lyra Next

Lyra Next is a SystemVerilog semantic analysis engine written in Rust, targeting IEEE 1800-2023. It provides a complete, incremental understanding of SystemVerilog source code.

## What it does

- Hand-written recursive-descent parser producing a lossless CST
- Normalized typed AST with stable node identities
- Symbol and scope resolution across files and packages
- Type system with constant evaluation for dimension bounds
- Structured diagnostics with labels and fix-its
- Incremental query engine (Salsa): only recompute what changed

## Relationship to Lyra (C++)

The original Lyra project is a SystemVerilog compiler written in C++.

Lyra Next is a ground-up rewrite in Rust focused on:

- Stronger semantic layering
- Incremental correctness by design
- Safer memory model

Over time, Lyra Next is expected to become the primary implementation of Lyra.

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
