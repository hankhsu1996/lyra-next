// LRM 6.11.3: Signed and unsigned integer types
//
// Each integer type has a default signedness per Table 6-8.
// The signed/unsigned keyword explicitly overrides the default.

module signed_unsigned;

  // Signed by default: byte, shortint, int, longint
  byte     b_default;
  shortint si_default;
  int      i_default;
  longint  li_default;

  // Override signed types to unsigned
  byte unsigned     b_uns;
  shortint unsigned si_uns;
  int unsigned      i_uns;
  longint unsigned  li_uns;

  // Unsigned by default: bit, logic, reg
  bit   bt_default;
  logic lg_default;
  reg   rg_default;

  // Override unsigned types to signed
  bit signed   bt_sgn;
  logic signed lg_sgn;
  reg signed   rg_sgn;

  // Signed/unsigned on vectors
  bit signed [7:0]    sv_signed;
  logic unsigned [7:0] lv_unsigned;

endmodule
