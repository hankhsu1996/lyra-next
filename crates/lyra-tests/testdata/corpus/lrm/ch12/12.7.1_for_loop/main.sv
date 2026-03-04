// LRM 12.7.1 -- For loop statements
//
// The for loop has initialization, condition, and step expressions.

module for_loop;
  integer i;
  logic [7:0] arr [0:3];

  // Basic for loop
  initial begin
    for (i = 0; i < 4; i = i + 1)
      arr[i] = 8'd0;
  end

  // For loop with begin-end
  initial begin
    for (i = 3; i >= 0; i = i - 1) begin
      arr[i] = 8'd0;
    end
  end

  // Nested for loops
  integer j;
  initial begin
    for (i = 0; i < 2; i = i + 1)
      for (j = 0; j < 2; j = j + 1)
        arr[0] = 8'd0;
  end
endmodule
