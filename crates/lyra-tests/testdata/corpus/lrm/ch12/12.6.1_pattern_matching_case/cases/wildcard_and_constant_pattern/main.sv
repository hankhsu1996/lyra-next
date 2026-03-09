// LRM 12.6.1 -- wildcard and constant patterns in case matches

module wildcard_and_constant;
  int x;
  int result;

  always_comb begin
    case (x) matches
      0: result = 0;
      1: result = 1;
      .*: result = -1;
    endcase
  end
endmodule
