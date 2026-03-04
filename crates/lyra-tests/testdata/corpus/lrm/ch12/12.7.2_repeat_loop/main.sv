// LRM 12.7.2 -- Repeat loop statements
//
// The repeat loop executes a statement a fixed number of times.

module repeat_loop;
  integer count;

  // Basic repeat
  initial begin
    count = 0;
    repeat (10)
      count = count + 1;
  end

  // Repeat with begin-end
  initial begin
    count = 0;
    repeat (5) begin
      count = count + 2;
    end
  end

  // Repeat with expression
  logic [3:0] n;
  initial begin
    n = 4'd3;
    count = 0;
    repeat (n)
      count = count + 1;
  end
endmodule
