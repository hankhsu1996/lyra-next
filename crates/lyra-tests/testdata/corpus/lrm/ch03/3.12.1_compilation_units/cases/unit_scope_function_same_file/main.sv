// LRM 3.12.1: $unit:: for CU-scope function call.
function int add(int a, int b);
  return a + b;
endfunction

module top;
  int result = $unit::add(1, 2);
endmodule
