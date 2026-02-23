// LRM 6.19: enum base type declarations with keyword types.
// Default is int; explicit keyword base types with optional
// signing and packed dimensions are supported.
module test_enum_base;
  // Default base type (int, per LRM 6.19)
  typedef enum { A, B, C } default_t;
  default_t d;

  // Explicit int base
  typedef enum int { X, Y, Z } int_t;
  int_t i;

  // Explicit bit vector with packed dimension
  typedef enum logic [2:0] { S0, S1, S2 } state_t;
  state_t s;

  // Explicit integer atom base (byte)
  typedef enum byte { LOW = 0, HIGH = 1 } level_t;
  level_t l;

  // Signed/unsigned qualifiers on base type
  typedef enum int unsigned { P, Q } unsigned_t;
  unsigned_t u;

  initial begin
    d = A;
    i = X;
    s = S0;
    l = LOW;
    u = P;
  end
endmodule
