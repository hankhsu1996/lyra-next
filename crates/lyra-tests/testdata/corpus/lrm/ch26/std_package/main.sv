// LRM 26.7: The std built-in package
//
// Every compilation unit implicitly imports std::*.
// A user-defined package named "std" takes precedence
// over the built-in empty std package.

// Module with no imports -- should not produce errors
// about unresolved std package
module plain_module;
  logic x;
endmodule

// User-defined std overrides built-in
package std;
  typedef logic [7:0] byte_t;
endpackage

module use_user_std;
  import std::byte_t;
  byte_t data;
endmodule
