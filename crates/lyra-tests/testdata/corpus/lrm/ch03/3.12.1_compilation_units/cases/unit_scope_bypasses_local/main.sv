// LRM 3.12.1: $unit:: bypasses local scope and accesses CU scope.
int file_var = 1;

module top;
  int file_var = 2;
  int x = $unit::file_var;
endmodule
