// LRM 3.12.1: File-level function visible at CU scope.
function int add(int a, int b);
  return a + b;
endfunction

module top;
  int result = add(1, 2);
endmodule
