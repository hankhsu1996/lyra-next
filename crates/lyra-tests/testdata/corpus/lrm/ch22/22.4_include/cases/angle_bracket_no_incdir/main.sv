// LRM 22.4: angle-bracket `include does not search includer directory
// With no configured include dirs, the sibling file is not found
`include <defs.svh>

// ALLOW-EXTRA-DIAGS
module m;
  int x;
endmodule
