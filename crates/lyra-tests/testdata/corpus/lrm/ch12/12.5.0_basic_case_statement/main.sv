// LRM 12.5.0 -- Basic case statement
//
// The case statement selects a branch based on exact match between
// the case expression and one of the case item expressions.
// Multiple expressions per item are comma-separated. The default
// item matches when no other item matches.

module basic_case_statement;
  logic [2:0] sel;
  logic [7:0] result;

  // Simple case with default
  initial begin
    case (sel)
      3'd0: result = 8'd0;
      3'd1: result = 8'd1;
      3'd2: result = 8'd2;
      default: result = 8'hFF;
    endcase
  end

  // Multiple expressions per case item
  initial begin
    case (sel)
      3'd0, 3'd1: result = 8'd0;
      3'd2, 3'd3, 3'd4: result = 8'd1;
      default: result = 8'hFF;
    endcase
  end

  // Case with begin-end blocks
  initial begin
    case (sel)
      3'd0: begin
        result = 8'd0;
      end
      3'd1: begin
        result = 8'd1;
      end
      default: begin
        result = 8'hFF;
      end
    endcase
  end

  // Case without default
  initial begin
    case (sel)
      3'd0: result = 8'd0;
      3'd1: result = 8'd1;
    endcase
  end
endmodule
