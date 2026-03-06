// LRM 12.5.4 -- casez with inside is illegal

module casez_inside;
  logic [2:0] sel;
  always_comb begin
    casez (sel) inside
// @casez error[lyra.semantic.case_inside_requires_plain_case]
      1: ;
      default: ;
    endcase
  end
endmodule
