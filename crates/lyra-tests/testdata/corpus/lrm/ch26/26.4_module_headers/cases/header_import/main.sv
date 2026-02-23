// LRM 26.4: Using packages in module headers
//
// Package import declarations may appear in the module header,
// between the module name and the port list.

package A;
  typedef logic [7:0] byte_t;
  parameter int WIDTH = 8;
endpackage

// Header-level wildcard import (LRM 26.4)
module M
  import A::*;
(input byte_t data);
  logic [WIDTH-1:0] bus;
endmodule

// Header-level explicit import
module N
  import A::byte_t;
(input byte_t data);
  byte_t local_data;
endmodule
