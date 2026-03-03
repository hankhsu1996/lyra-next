// LRM 6.13: Void function call in statement context is legal.

module void_return_stmt_ok;

  function void do_nothing();
  endfunction

  initial begin
    do_nothing();
  end

endmodule
