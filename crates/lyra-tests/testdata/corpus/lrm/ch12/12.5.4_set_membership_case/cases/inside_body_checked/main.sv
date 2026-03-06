// LRM 12.5.4 -- branch bodies in case inside are semantically visited

module inside_body_checked;
  logic [3:0] sel;
  logic [7:0] wide;
  logic [3:0] narrow;
  always_comb begin
    case (sel) inside
      1: narrow = wide;
// @narrow warning[lyra.type.width_mismatch]
      default: ;
    endcase
  end
endmodule
