// LRM 12.8 -- return without value in void function and task

module return_void;
  function void f();
    return;
  endfunction

  task t();
    return;
  endtask
endmodule
