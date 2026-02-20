// LRM 26.4: Using packages in module headers
//
// Header-level import is not yet supported by the parser.
// This test covers the workaround: importing inside the module body.
// The actual header import syntax (module M import P::*; (...)) is
// tracked in gaps.md.

package A;
  typedef logic [7:0] byte_t;
endpackage

// Body import as workaround for header import
module M(input logic [7:0] data);
  import A::byte_t;
  byte_t local_data;
  assign local_data = data;
endmodule
