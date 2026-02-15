# Lyra Next

Lyra Next is a Rust-based SystemVerilog parser and semantic analyzer. It provides an incremental semantic model of the codebase that language servers, linters, formatters, compilers, and simulators can share, instead of each tool reimplementing its own.

- Hand-written recursive-descent parser producing a lossless CST
- Normalized typed AST
- Symbol and scope resolution with stable IDs
- Type system and constant evaluation foundations
- Structured diagnostics with precise spans and fix-its
- Incremental query engine (only recompute what changed)

This is a from-scratch Rust rewrite of [Lyra](https://github.com/nicovank/Lyra), a SystemVerilog compiler originally written in C++.

## Building

```bash
cargo test
```

Rust toolchain is pinned via `rust-toolchain.toml`.

## Contributing

Design docs and the project roadmap live in `docs/`.

## License

MIT License. See [LICENSE](LICENSE) for details.
