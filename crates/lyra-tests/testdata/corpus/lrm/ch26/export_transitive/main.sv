// LRM 26.6: Transitive export chain
//
// A re-exports from B, B re-exports from C. A consumer that
// imports from A should see C's symbols through the chain.

// Origin package
package C;
  typedef logic [7:0] byte_t;
  parameter int DEPTH = 4;
endpackage

// Middle package: imports from C, re-exports
package B;
  import C::*;
  export C::*;
endpackage

// Top package: imports from B, re-exports
package A;
  import B::*;
  export B::*;
endpackage

// Consumer imports from A, sees C's symbols transitively
module consumer;
  import A::*;
  byte_t data;
  localparam int D = DEPTH;
endmodule
