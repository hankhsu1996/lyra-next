// LRM 12.6.1 -- casex with matches is illegal

module casex_matches;
  int x;
  always_comb begin
    casex (x) matches
// @casex error[lyra.semantic.case_matches_requires_plain_case]
      0: ;
      default: ;
    endcase
  end
endmodule
