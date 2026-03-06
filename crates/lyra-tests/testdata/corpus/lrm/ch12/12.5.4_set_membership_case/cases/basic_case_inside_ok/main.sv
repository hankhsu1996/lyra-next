// LRM 12.5.4 -- legal case inside, no diagnostics expected

module basic_case_inside;
  logic [2:0] status;
  always_comb begin
    priority case (status) inside
      1, 3:          ;
      [4:7]:         ;
      default:       ;
    endcase
  end
endmodule
