// LRM 3.12.1: $unit:: for CU-scope typedef in type position.
typedef logic [7:0] byte_t;

module top;
  $unit::byte_t x;
endmodule
