// LRM 12.5.4 -- value-range expressions are semantically visited

module inside_ranges_typechecked;
  logic [7:0] sel;
  logic [7:0] lo, hi;
  always_comb begin
    case (sel) inside
      8'd1, 8'd2:   ;
      [lo:hi]:       ;
      default:       ;
    endcase
  end
endmodule
