# Architectural Gaps

Known design tensions that should be addressed when they become blocking.

## Ty vs DataTy split

Priority: Medium (promote to High when starting virtual interface / modport constraints).

`Ty` carries both data types (integral, real, enum, record, string, chandle) and non-data-type semantic classifications (interface, void, event). Data-type-only APIs (`bit_width_total`, integral coercion, sizing, casts) need increasingly many `Ty::Interface => None/Error` guard branches.

Adding `InterfaceIdentity::Generic` makes the gap more visible -- generic interface ports are a meaningful type that participates in port signatures but has no width, no coercion rules, and no data-type operations.

Future refactor: introduce `DataTy` (LRM data-type subset), move data-type-only APIs to it, and let `Ty` become a sum of `Data(DataTy) | InterfacePort(InterfaceType) | ...`. This eliminates guard branches by making non-data kinds structurally unreachable in data-type pipelines.
