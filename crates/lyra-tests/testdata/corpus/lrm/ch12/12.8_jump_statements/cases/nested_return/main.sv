// LRM 12.8 -- return inside conditional within a loop

module nested_return;
  function int find_nonzero(input int a, input int b);
    if (a != 0)
      return a;
    if (b != 0)
      return b;
    return 0;
  endfunction
endmodule
