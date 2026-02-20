// LRM 26.3: Qualified package access using :: operator

package ValPkg;
  parameter int SIZE = 4;
  int count = 0;
endpackage

// Qualified access to a parameter value in an expression
module qualified_param;
  localparam int N = ValPkg::SIZE;
endmodule
