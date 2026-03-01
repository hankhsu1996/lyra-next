// LRM 6.19: Enum members share the enclosing scope with other
// declarations. Colliding names are duplicate-definition errors
// regardless of declaration order.

module enum_member_dup_scope;

  // Local variable declared before enum with same member name
  logic IDLE;
  typedef enum { IDLE, RUNNING } state_a_t;
  //             ^ error[lyra.semantic[2]]: duplicate definition of `IDLE`

  // Enum declared before local variable with same member name
  typedef enum { OFF, ON } toggle_t;
  logic OFF;
  //    ^ error[lyra.semantic[2]]: duplicate definition of `OFF`

  // Two enums with a colliding member name
  typedef enum { X, Y } alpha_t;
  typedef enum { X, Z } beta_t;
  //             ^ error[lyra.semantic[2]]: duplicate definition of `X`

endmodule
