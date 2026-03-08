// LRM 3.12.1: $unit:: explicit scoped name prefix for CU-scope variable.
int cu_var = 42;

module top;
  int x = $unit::cu_var;
endmodule
