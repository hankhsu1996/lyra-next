// LRM 26.6: Export merge precedence
//
// Verify deterministic precedence when exports and local defs overlap:
//   1. Local defs always win over exports
//   2. Explicit exports win over wildcard exports
//   3. Package wildcard wins over all-wildcard

// Source packages
package Src;
  typedef logic [7:0] byte_t;
  parameter int SIZE = 8;
endpackage

// Local def shadows export: package has its own byte_t AND exports Src::byte_t.
// The local definition must be the visible one.
package LocalWins;
  typedef logic [15:0] byte_t;
  import Src::*;
  export Src::*;
endpackage

module check_local_wins;
  import LocalWins::byte_t;
  byte_t data;
endmodule

// Explicit export: only SIZE is re-exported, not byte_t
package ExplicitOnly;
  import Src::*;
  export Src::SIZE;
endpackage

module check_explicit_only;
  import ExplicitOnly::SIZE;
  localparam int S = SIZE;
endmodule

// All-wildcard re-exports everything imported
package AllWild;
  import Src::*;
  export *::*;
endpackage

module check_all_wild;
  import AllWild::*;
  byte_t aw_data;
  localparam int AW = SIZE;
endmodule
