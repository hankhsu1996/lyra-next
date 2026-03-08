`default_nettype none
module m;
  assign y = x;
  // @y error[lyra.semantic.implicit_net_forbidden]: implicit net creation disabled by `default_nettype none` for `y`
  // @x error[lyra.semantic.unresolved_name]: unresolved name `x`
endmodule
