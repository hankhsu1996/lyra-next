// LRM 6.19: Enum base type resolution
//
// Keyword bases, default base, and named (typedef) bases are resolved
// to determine variant width, signedness, and 2-state/4-state behavior.

module enum_named_base;

  // Keyword base: logic [7:0] -> 8-bit unsigned 4-state
  enum logic [7:0] { K_A, K_B } kw_enum;

  // Keyword base: int -> 32-bit signed 2-state
  enum int { I_X, I_Y } int_enum;

  // Default base (int): 32-bit signed 2-state
  enum { D_P, D_Q } default_enum;

  // Keyword base: bit [3:0] -> 4-bit unsigned 2-state
  enum bit [3:0] { B_A, B_B } bit_enum;

  // Named base: typedef in same module
  typedef logic [7:0] byte_t;
  enum byte_t { N_A, N_B } named_enum;

  // Qualified base: typedef from another package
  enum pkg::nibble_t { Q_X, Q_Y } qual_enum;

endmodule
