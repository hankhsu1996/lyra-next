# Lyra Next

SystemVerilog tooling is fragmented. Every language server, linter, simulator, and compiler implements its own parser and semantic analysis.

Lyra Next is a unified SystemVerilog toolchain built in Rust. A single incremental semantic core handles parsing, name resolution, type checking, and diagnostics. Tools like the LSP, linter, formatter, and simulator are all clients of that core.

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
