module sub #(parameter W = 1) (input a);
endmodule

module top;
  wire x;
  sub #(.W(UNDEF)) u(.a(x));
  // @UNDEF error[lyra.semantic.unresolved_name]: unresolved name `UNDEF`
endmodule
