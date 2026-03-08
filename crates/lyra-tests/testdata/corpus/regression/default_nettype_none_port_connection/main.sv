`default_nettype none
module sub(input a);
endmodule

module top;
  sub u(.a(w));
  // @w error[lyra.semantic.implicit_net_forbidden]: implicit net creation disabled by `default_nettype none` for `w`
endmodule
