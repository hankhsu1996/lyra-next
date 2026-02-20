// LRM 26.4: Header import in program declarations
//
// The LRM specifies that package import declarations may appear in
// program headers, between the program name and the port list.

package ProgPkg;
  typedef logic [7:0] byte_t;
  parameter int WIDTH = 8;
endpackage

// Program with header-level wildcard import
program test_prog
  import ProgPkg::*;
(input logic clk);
  byte_t data;
endprogram

// Program with header-level explicit import
program test_explicit
  import ProgPkg::byte_t;
(input logic clk);
  byte_t payload;
endprogram
