// LRM 12.7.4 -- While loop statements
//
// The while loop tests the condition before executing the body.

module while_loop;
  integer i;

  // Basic while
  initial begin
    i = 0;
    while (i < 10)
      i = i + 1;
  end

  // While with begin-end
  initial begin
    i = 100;
    while (i > 0) begin
      i = i - 1;
    end
  end

  // While with compound condition
  logic done;
  initial begin
    i = 0;
    done = 1'b0;
    while (i < 10 && !done) begin
      i = i + 1;
    end
  end
endmodule
