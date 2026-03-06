// LRM 12.8 -- returning a value from void function/task is illegal

module return_value_in_void;
  function void foo();
    return 1;
// @return error[lyra.semantic.return_value_in_void]: cannot return a value from a void function or task
  endfunction

  task bar();
    return 1;
// @return error[lyra.semantic.return_value_in_void]: cannot return a value from a void function or task
  endtask
endmodule
