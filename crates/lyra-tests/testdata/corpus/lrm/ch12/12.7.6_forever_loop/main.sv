// LRM 12.7.6 -- Forever loop statements
//
// The forever loop repeats the body indefinitely.
// Typically used with timing controls or break (when supported).

module forever_loop;
  logic clk;

  // Forever with begin-end
  initial begin
    clk = 1'b0;
    forever begin
      clk = ~clk;
    end
  end
endmodule
