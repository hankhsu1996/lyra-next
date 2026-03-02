module m;
  function automatic void f_auto; endfunction
  function static void f_static; endfunction
  function void f_default; endfunction

  task automatic t_auto; endtask
  task static t_static; endtask
  task t_default; endtask
endmodule
