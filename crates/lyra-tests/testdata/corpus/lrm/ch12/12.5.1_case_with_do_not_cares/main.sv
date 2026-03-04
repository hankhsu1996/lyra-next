// LRM 12.5.1 -- Case statement with do-not-cares
//
// casez treats z/?/high-impedance bits as don't-care in comparison.
// casex treats x/z/?/high-impedance bits as don't-care.

module case_with_do_not_cares;
  logic [3:0] opcode;
  logic [7:0] result;

  // casez: z and ? are don't-care
  initial begin
    casez (opcode)
      4'b1???: result = 8'hF0;
      4'b01??: result = 8'h0F;
      4'b001?: result = 8'h03;
      default: result = 8'h00;
    endcase
  end

  // casex: x and z are don't-care
  initial begin
    casex (opcode)
      4'b1xxx: result = 8'hF0;
      4'b01xx: result = 8'h0F;
      default: result = 8'h00;
    endcase
  end

  // casez with z literal in pattern
  initial begin
    casez (opcode)
      4'b1zzz: result = 8'hF0;
      4'b01zz: result = 8'h0F;
      default: result = 8'h00;
    endcase
  end
endmodule
