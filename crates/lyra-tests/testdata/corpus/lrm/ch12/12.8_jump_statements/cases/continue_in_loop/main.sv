// LRM 12.8 -- continue statement in a loop

module continue_in_loop;
  integer i;
  initial begin
    for (i = 0; i < 4; i = i + 1) begin
      continue;
    end
  end
endmodule
