// LRM 12.4.2.0 -- unique-if, unique0-if, and priority-if semantics
//
// unique-if: no overlap among conditions; violation if no match or multiple match.
// unique0-if: no overlap; violation only if multiple match (no match is OK).
// priority-if: evaluated in order; violation if no match.

module unique_priority_if;
  logic [2:0] a;
  logic [7:0] result;

  // unique-if: all conditions mutually exclusive
  initial begin
    unique if (a == 3'd0)
      result = 8'd0;
    else if (a == 3'd1)
      result = 8'd1;
    else if (a == 3'd2)
      result = 8'd2;
    else
      result = 8'hFF;
  end

  // unique0-if: no-match is acceptable
  initial begin
    unique0 if (a == 3'd0)
      result = 8'd0;
    else if (a == 3'd1)
      result = 8'd1;
  end

  // priority-if: order matters, first match wins
  initial begin
    priority if (a[2])
      result = 8'hF0;
    else if (a[1])
      result = 8'h0F;
    else
      result = 8'h00;
  end
endmodule
