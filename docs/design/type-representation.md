# Type Representation Contract

Normative rules for the `Ty` type model and its formatting.

## Canonical `Ty` Invariants

### Unpacked dimensions: `Ty::Array` only

Unpacked dimensions are represented exclusively via `Ty::Array { elem, dim }` nesting. No other type variant stores unpacked dimensions.

A multi-dimensional declaration wraps outermost-first:

```
logic [7:0] x [2][3]
```

becomes:

```
Array(elem = Array(elem = Integral(logic [7:0]), dim = Size(3)), dim = Size(2))
```

The outermost (leftmost in source) dimension is the outermost `Array` wrapper. Peeling one `Array` layer removes the outermost unpacked dimension.

### Packed dimensions and signedness

Packed dimensions and signedness overrides live inside `Integral`:

```rust
Integral { keyword, signed, packed: Box<[PackedDim]> }
```

Only integral types have packed dimensions. Non-integral types (`Real`, `String`, `Chandle`, `Event`, `Void`) carry no packed modifiers.

### Aggregate identity

`Ty::Enum(EnumId)` and `Ty::Struct(StructId)` carry an ID that identifies the aggregate definition. The ID includes a `FileId` and an ordinal within the defining scope. Names are not stored in the type -- name lookup requires access to the def index.

IDs are internal-only. Ordinal churn is accepted for now. Revisit when finer-grained intra-file invalidation or external persistence is needed.

## Printing Contract

### `Ty::pretty()` -- pure, always lossless

`Ty::pretty()` in `lyra-semantic` is the baseline formatter. It is:

- **Pure**: no database or external state needed.
- **Lossless**: includes all packed dims, unpacked dims, and signedness overrides. No information is dropped.
- **Enum/struct-unaware**: prints bare keywords (`enum`, `struct`) without names, because names require DB access.

All `pretty()` methods on `SymbolType`, `ExprType`, `BitVecType`, and `TypeAtResult` delegate to `Ty::pretty()` and are equally lossless.

### `TyFmt` -- enriched formatting with DB access

`TyFmt` in `lyra-db` wraps a database reference and a compilation unit. It provides enriched output that resolves aggregate names:

- `TyFmt::ty(&self, &Ty) -> SmolStr` -- lossless, with names (e.g. `enum color_t`, `struct packed pixel_t`).
- `TyFmt::symbol_type(&self, &SymbolType) -> SmolStr` -- same enrichment for symbol types.

Diagnostics and `type_at` queries in the DB layer use `TyFmt` when DB access is available. Pure-context code (tests, `lyra-semantic` internals) uses `Ty::pretty()` directly.

### No lossy printers in core

There is no "compact" or "abbreviated" formatting mode. If one is needed in the future, it must be explicitly opt-in and must never be used by diagnostics.

## Extraction: no silent dropping

Type extraction from AST declarations follows two phases:

1. **`build_base_ty`**: constructs the base `Ty` with packed dims and signedness for integrals; passes through non-integral types as-is.
2. **`wrap_unpacked`**: wraps any `Ty` with `Ty::Array` layers for unpacked dimensions.

Every extraction path (variables, nets, ports, typedefs, parameters) calls both phases. Non-integral types (`real x[4]`, `string s[2]`) get their unpacked dims via the same `wrap_unpacked` path as integrals.

If a type form is unsupported, extraction produces `Ty::Error` with a diagnostic -- never silently drops modifiers or dimensions.
