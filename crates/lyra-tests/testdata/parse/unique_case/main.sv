module decoder(input logic [1:0] sel, output logic [3:0] out);
  always_comb begin
    unique case (sel)
      0: out = 1;
      1: out = 2;
      2: out = 4;
      default: out = 8;
    endcase
  end
endmodule
