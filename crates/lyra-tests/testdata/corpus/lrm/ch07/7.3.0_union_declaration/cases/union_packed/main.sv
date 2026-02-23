// LRM 7.3: Unions -- packed union declaration and member access
//
// Tests packed union typedef and field selection.

module union_packed;

  typedef union packed {
    logic [7:0] high;
    logic [7:0] low;
  } overlay_t;

  overlay_t ov;
  logic [7:0] result_high;
  assign result_high = ov.high;

endmodule
