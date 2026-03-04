// LRM 12.5.3.0 -- unique-case, unique0-case, and priority-case semantics
//
// unique case: items must be mutually exclusive.
// unique0 case: like unique but no-match is acceptable.
// priority case: items evaluated in order; first match wins.

module unique_priority_case;
  logic [2:0] sel;
  logic [7:0] result;

  // unique case
  initial begin
    unique case (sel)
      3'd0: result = 8'd0;
      3'd1: result = 8'd1;
      3'd2: result = 8'd2;
      default: result = 8'hFF;
    endcase
  end

  // unique0 case: no-match is acceptable
  initial begin
    unique0 case (sel)
      3'd0: result = 8'd0;
      3'd1: result = 8'd1;
    endcase
  end

  // priority case: order matters
  initial begin
    priority case (sel)
      3'd0: result = 8'd0;
      3'd1: result = 8'd1;
      default: result = 8'hFF;
    endcase
  end

  // unique casez
  initial begin
    unique casez (sel)
      3'b1??: result = 8'hF0;
      3'b01?: result = 8'h0F;
      3'b001: result = 8'h03;
      default: result = 8'h00;
    endcase
  end
endmodule
