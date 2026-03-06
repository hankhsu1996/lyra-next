// LRM 12.8 -- continue outside loop is illegal

module continue_outside_loop;
  initial begin
    continue;
// @continue error[lyra.semantic.continue_outside_loop]: `continue` statement outside of a loop
  end
endmodule
