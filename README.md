# Lyra Next

Lyra Next is a Rust-based SystemVerilog parser and semantic analyzer. It provides an incremental semantic model of the codebase that language servers, linters, formatters, compilers, and simulators can share, instead of each tool reimplementing its own.

- Hand-written recursive-descent parser producing a lossless CST
- Normalized typed AST
- Symbol and scope resolution with stable IDs
- Type system and constant evaluation foundations
- Structured diagnostics with precise spans and fix-its
- Incremental query engine (only recompute what changed)

## Relationship to Lyra (C++)

The original Lyra project is a SystemVerilog compiler written in C++.

Lyra Next explores a new architecture in Rust focused on:

- Stronger semantic layering
- Incremental correctness by design
- Safer memory model
- A reusable platform for the broader tooling ecosystem

Over time, Lyra Next is expected to become the primary implementation of Lyra.

## Building

```bash
cargo check
cargo test
```

Rust toolchain is pinned via `rust-toolchain.toml`.

## Contributing

Design docs and the project roadmap live in `docs/`. Read through them before making changes, and discuss larger design ideas before implementing.

## License

MIT License. See [LICENSE](LICENSE) for details.
