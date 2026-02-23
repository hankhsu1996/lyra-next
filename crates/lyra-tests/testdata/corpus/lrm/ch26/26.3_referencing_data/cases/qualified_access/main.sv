// LRM 26.3: Qualified package access using :: operator

package ValPkg;
  typedef logic [7:0] byte_t;
  parameter int SIZE = 4;
  int count = 0;
endpackage

// Qualified access to a parameter value in an expression
module qualified_param;
  localparam int N = ValPkg::SIZE;
endmodule

// Qualified type in variable declaration (LRM 26.3)
module qualified_type;
  ValPkg::byte_t data;
endmodule
