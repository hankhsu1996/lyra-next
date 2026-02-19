module top;
  assign y = x;
  //    ^ error[lyra.semantic[1]]: unresolved name `y`
  //        ^ error[lyra.semantic[1]]: unresolved name `x`
endmodule
