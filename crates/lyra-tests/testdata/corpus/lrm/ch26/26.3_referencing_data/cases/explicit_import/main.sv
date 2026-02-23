// LRM 26.3: Explicit import (import pkg::identifier)

package P;
  typedef logic [7:0] byte_t;
  parameter int WIDTH = 16;
endpackage

// Explicit import of a type
module use_explicit_type;
  import P::byte_t;
  byte_t data;
endmodule

// Explicit import of a parameter
module use_explicit_param;
  import P::WIDTH;
  logic [WIDTH-1:0] bus;
endmodule

// Error: import nonexistent member
module bad_member;
  import P::nonexistent;
  //    ^ error[lyra.semantic[4]]: member `nonexistent` not found in package `P`
endmodule
