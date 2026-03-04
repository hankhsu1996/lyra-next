// LRM 12.8 -- continue outside loop is illegal

module continue_outside_loop;
  initial begin
    continue;
//  ^ error[lyra.semantic[19]]: `continue` statement outside of a loop
  end
endmodule
