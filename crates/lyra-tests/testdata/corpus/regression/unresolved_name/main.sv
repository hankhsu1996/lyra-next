module top;
  assign y = x;
  // @x error[lyra.semantic.unresolved_name]: unresolved name `x`
endmodule
