// LRM 26.3: Wildcard import (import pkg::*)

package Colors;
  typedef enum logic [1:0] {
    RED   = 2'b00,
    GREEN = 2'b01,
    BLUE  = 2'b10
  } color_t;
  parameter int NUM_COLORS = 3;
endpackage

// Wildcard import makes all package items available
module use_wildcard;
  import Colors::*;
  color_t pixel;
  localparam int N = NUM_COLORS;
endmodule
