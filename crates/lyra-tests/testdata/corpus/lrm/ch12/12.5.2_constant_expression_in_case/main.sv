// LRM 12.5.2 -- Constant expression in case statement
//
// The case expression can be a constant, with variable expressions
// as case items. This reverses the typical pattern.

module constant_expression_in_case;
  logic [2:0] a, b, c;
  logic [7:0] result;

  // Constant 1'b1 as case expression, boolean expressions as items
  initial begin
    case (1'b1)
      (a == 3'd0): result = 8'd0;
      (b == 3'd1): result = 8'd1;
      (c == 3'd2): result = 8'd2;
      default: result = 8'hFF;
    endcase
  end

  // Constant 1'b0 as case expression
  initial begin
    case (1'b0)
      (a > b): result = 8'd0;
      (b > c): result = 8'd1;
      default: result = 8'hFF;
    endcase
  end
endmodule
