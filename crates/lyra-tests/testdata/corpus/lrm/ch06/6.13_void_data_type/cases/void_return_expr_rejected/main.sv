// LRM 6.13: Void function call cannot be used as a value.

module void_return_expr_rejected;

  function void f();
  endfunction

  int x = f();
  //     ^ error[lyra.type[32]]: void expression cannot be used as a value

endmodule
