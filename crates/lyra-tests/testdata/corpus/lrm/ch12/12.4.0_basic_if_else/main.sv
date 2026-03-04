// LRM 12.4.0 -- Basic if-else statement
//
// An if-else statement selects execution based on a condition expression.
// The condition is treated as a boolean: zero is false, nonzero is true.

module basic_if_else;
  logic [7:0] a, b, result;

  // Simple if with else
  initial begin
    if (a > b)
      result = a;
    else
      result = b;
  end

  // If without else
  initial begin
    if (a == 8'd0)
      result = 8'd0;
  end

  // If with begin-end block
  initial begin
    if (a != b) begin
      result = a + b;
    end else begin
      result = 8'd0;
    end
  end
endmodule
