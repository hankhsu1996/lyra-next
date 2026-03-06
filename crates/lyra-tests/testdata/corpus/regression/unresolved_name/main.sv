module top;
  assign y = x;
  // @y error[lyra.semantic.unresolved_name]: unresolved name `y`
  // @x error[lyra.semantic.unresolved_name]: unresolved name `x`
endmodule
