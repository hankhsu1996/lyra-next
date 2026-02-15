# Lyra Next

SystemVerilog tooling is fragmented. Every language server, linter, simulator, and compiler implements its own parser and semantic analysis.

Lyra Next is a unified SystemVerilog toolchain built in Rust. A single incremental semantic core handles parsing, name resolution, type checking, and diagnostics. Tools like the LSP, linter, formatter, and simulator are all clients of that core.

The semantic core provides:

- Lossless parsing that preserves every token, including whitespace and comments
- Name resolution and scope analysis
- Type checking and constant evaluation
- Structured diagnostics with precise source locations
- Incremental analysis that only recomputes what changed

This is a from-scratch rewrite of [Lyra](https://github.com/nicovank/Lyra), which was originally built in C++.

## Building

```bash
cargo test
```

Rust toolchain is pinned via `rust-toolchain.toml`.

## Contributing

Design docs and the project roadmap live in `docs/`.

## License

MIT License. See [LICENSE](LICENSE) for details.
