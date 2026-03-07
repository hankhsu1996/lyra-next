// LRM 3.12.1: export is only valid inside a package.
// The parser rejects it at file scope with cascading errors.
// ALLOW-EXTRA-DIAGS
export pkg::foo;
// @export error[lyra.parse.error]: expected top-level declaration
