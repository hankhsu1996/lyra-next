// LRM 12.8 -- break outside loop is illegal

module break_outside_loop;
  initial begin
    break;
// @break error[lyra.semantic.break_outside_loop]: `break` statement outside of a loop
    if (1) begin
      break;
// @break error[lyra.semantic.break_outside_loop]: `break` statement outside of a loop
    end
  end
endmodule
