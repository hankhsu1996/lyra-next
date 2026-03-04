module top;
  assign y = x;
  //    ^ error[lyra.semantic.unresolved_name]: unresolved name `y`
  //        ^ error[lyra.semantic.unresolved_name]: unresolved name `x`
endmodule
