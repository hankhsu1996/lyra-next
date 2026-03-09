// LRM 12.6.1 -- casez with matches is illegal

module casez_matches;
  int x;
  always_comb begin
    casez (x) matches
// @casez error[lyra.semantic.case_matches_requires_plain_case]
      0: ;
      default: ;
    endcase
  end
endmodule
