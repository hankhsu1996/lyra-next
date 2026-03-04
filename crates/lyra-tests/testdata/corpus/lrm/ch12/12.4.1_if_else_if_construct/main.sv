// LRM 12.4.1 -- if-else-if construct
//
// A chain of if-else-if conditions tests multiple alternatives in order.
// The first matching branch executes; remaining branches are skipped.

module if_else_if_construct;
  logic [3:0] sel;
  logic [7:0] result;

  // Multi-way if-else-if chain
  initial begin
    if (sel == 4'd0)
      result = 8'h00;
    else if (sel == 4'd1)
      result = 8'h11;
    else if (sel == 4'd2)
      result = 8'h22;
    else if (sel == 4'd3)
      result = 8'h33;
    else
      result = 8'hFF;
  end

  // Nested if-else-if with begin-end
  initial begin
    if (sel[3]) begin
      result = 8'hF0;
    end else if (sel[2]) begin
      result = 8'h0F;
    end else begin
      result = 8'h00;
    end
  end
endmodule
