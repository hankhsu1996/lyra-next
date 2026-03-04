module case_inside_test;
  logic [2:0] status;
  always_comb begin
    priority case (status) inside
      1, 3:          ;
      [4:7]:         ;
      default:       ;
    endcase
  end
endmodule
