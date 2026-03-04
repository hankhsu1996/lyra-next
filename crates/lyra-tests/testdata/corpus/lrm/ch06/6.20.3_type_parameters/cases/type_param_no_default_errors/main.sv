// LRM 6.20.3: Type parameter without default
//
// A type parameter with no default type produces a diagnostic.

module type_param_no_default #(parameter type T) ();
//                                            ^ error[lyra.semantic.type_param_no_default]: type parameter `T` requires a default type
  T x;
endmodule
