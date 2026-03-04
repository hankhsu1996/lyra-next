// LRM 12.8 -- return outside function/task is illegal

module return_outside_callable;
  initial begin
    return;
//  ^ error[lyra.semantic.return_outside_callable]: `return` statement outside of a function or task
  end
endmodule
