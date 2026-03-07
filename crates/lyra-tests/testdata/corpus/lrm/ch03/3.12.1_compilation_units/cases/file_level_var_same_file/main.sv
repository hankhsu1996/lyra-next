// LRM 3.12.1: File-level variable declaration visible at CU scope.
int file_var = 42;

module top;
  int local_var = file_var;
endmodule
