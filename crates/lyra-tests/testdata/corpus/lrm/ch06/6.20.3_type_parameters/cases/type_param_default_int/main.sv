// LRM 6.20.3: Type parameter with keyword default
//
// A type parameter with a built-in type default resolves correctly.
// The variable declared with the type parameter gets the default type.

module type_param_default_int #(parameter type T = int) ();
  T x;
endmodule
