// LRM 3.12.1: $unit:: with nonexistent name produces member_not_found.
module top;
  int x = $unit::nonexistent;
// @$unit error[lyra.semantic.member_not_found]: member `nonexistent` not found in package `$unit`
endmodule
