// LRM 6.20.3: Localparam type
//
// localparam type works the same as parameter type for default resolution.

module type_param_localparam;
  localparam type T = logic [3:0];
  T x;
endmodule
