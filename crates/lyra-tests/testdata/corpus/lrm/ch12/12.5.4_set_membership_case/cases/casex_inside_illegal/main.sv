// LRM 12.5.4 -- casex with inside is illegal

module casex_inside;
  logic [2:0] sel;
  always_comb begin
    casex (sel) inside
// @casex error[lyra.semantic.case_inside_requires_plain_case]
      1: ;
      default: ;
    endcase
  end
endmodule
