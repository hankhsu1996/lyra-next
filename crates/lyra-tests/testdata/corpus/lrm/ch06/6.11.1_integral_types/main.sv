// LRM 6.11: Integer data types
//
// Table 6-8 defines all integer data types with their widths,
// state counts, and default signedness.
// 6.11.1 defines integral types.
// 6.11.2 defines 2-state vs 4-state.
// 6.11.3 defines signed vs unsigned defaults and overrides.

module integer_types;

  // 2-state types -- no x/z values (LRM 6.11.2)
  byte       b;       // 8-bit signed
  shortint   si;      // 16-bit signed
  int        i;       // 32-bit signed
  longint    li;      // 64-bit signed
  bit        bt;      // 1-bit unsigned (scalar)
  bit [7:0]  bv;      // 8-bit unsigned (vector)

  // 4-state types -- x/z values (LRM 6.11.2)
  logic      lg;      // 1-bit unsigned (scalar)
  logic [7:0] lgv;    // 8-bit unsigned (vector)
  reg        rg;      // 1-bit unsigned (scalar)
  reg [7:0]  rgv;     // 8-bit unsigned (vector)
  integer    intg;    // 32-bit signed
  time       t;       // 64-bit unsigned

  // Explicit signedness override (LRM 6.11.3)
  int unsigned ui;
  int signed   ssi;
  bit signed [3:0] sb;
  logic unsigned [7:0] ulg;
  byte unsigned ub;

  // Literal assignments
  assign i = 42;
  assign li = 100;
  assign b = 8'hFF;
  assign si = 16'h1234;

  // Vector declarations (LRM 6.9)
  logic [3:0] v;
  logic signed [3:0] signed_vec;
  logic [-1:4] neg_range;

  // var keyword form (LRM 6.8)
  var byte vb;
  var int vi;
  var logic [15:0] vw;

endmodule
