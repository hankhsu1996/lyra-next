// LRM 12.8 -- omitting return value from non-void function is illegal

module return_missing_value;
  function int foo();
    return;
// @return error[lyra.semantic.return_missing_value]: non-void function must return a value
  endfunction
endmodule
