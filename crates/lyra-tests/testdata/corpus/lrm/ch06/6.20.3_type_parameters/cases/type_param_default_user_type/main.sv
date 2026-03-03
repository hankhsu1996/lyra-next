// LRM 6.20.3: Type parameter with user-defined type default
//
// A type parameter defaulting to a typedef resolves through the
// normal type alias pipeline.

module type_param_user_type;
  typedef logic [7:0] byte_t;
  parameter type T = byte_t;
  T x;
endmodule
