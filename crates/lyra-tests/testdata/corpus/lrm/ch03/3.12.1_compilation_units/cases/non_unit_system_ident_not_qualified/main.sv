// LRM 3.12.1: Non-$unit system identifiers are not qualified name roots.
// ALLOW-EXTRA-DIAGS
module top;
  int x = $display::foo;
// @[::] error[lyra.parse.error]: expected `;`
endmodule
