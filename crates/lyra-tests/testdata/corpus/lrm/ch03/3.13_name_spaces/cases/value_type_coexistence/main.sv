// LRM 3.13: Name spaces
//
// The value namespace and type namespace are independent.
// The same identifier can exist as both a typedef (type)
// and a variable (value) without conflict.

module value_type_coexist;
  typedef logic [7:0] foo;
  logic foo;
endmodule
