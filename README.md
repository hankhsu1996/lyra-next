# Lyra Next

Lyra Next is a Rust-based SystemVerilog semantic platform -- a shared foundation for tooling such as language servers, linters, formatters, compilers, simulators, and synthesis flows.

Instead of building each tool independently, Lyra Next provides a single incremental semantic model of the codebase that all tools can query.

## What this project is

Lyra Next is a semantic database for SystemVerilog:

- Hand-written recursive-descent parser producing a lossless CST
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

## Building

```bash
cargo check
cargo test
```

Rust toolchain is pinned via `rust-toolchain.toml`.

## Documentation

- `docs/roadmap.md` -- milestone plan (M0--M5)
- `docs/architecture.md` -- crate layering and design principles
- `docs/working_method.md` -- how features go from LRM text to merged code

## Contributing

If you want to contribute:

- Read `docs/working_method.md` for the development workflow
- Look at `docs/roadmap.md` to see what's next
- Discuss larger design ideas before implementing

## License

MIT License. See [LICENSE](LICENSE) for details.
