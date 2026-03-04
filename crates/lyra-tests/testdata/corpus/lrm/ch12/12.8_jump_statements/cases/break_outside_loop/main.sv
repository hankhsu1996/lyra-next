// LRM 12.8 -- break outside loop is illegal

module break_outside_loop;
  initial begin
    break;
//  ^ error[lyra.semantic[18]]: `break` statement outside of a loop
    if (1) begin
      break;
//    ^ error[lyra.semantic[18]]: `break` statement outside of a loop
    end
  end
endmodule
