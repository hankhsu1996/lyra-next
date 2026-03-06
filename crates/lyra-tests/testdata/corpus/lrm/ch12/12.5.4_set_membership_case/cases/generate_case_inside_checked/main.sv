// LRM 12.5.4 -- case inside body semantics checked in always block

module case_inside_always;
  logic [3:0] sel;
  logic [3:0] narrow;
  logic [7:0] wide;
  always_comb begin
    case (sel) inside
      1: narrow = wide;
// @narrow warning[lyra.type.width_mismatch]
      default: ;
    endcase
  end
endmodule
