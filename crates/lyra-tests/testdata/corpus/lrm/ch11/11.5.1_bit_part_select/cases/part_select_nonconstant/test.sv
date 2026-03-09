// LRM 11.5.1: fixed part-select bounds must be constant integer expressions
module part_select_nonconstant;
  logic [7:0] vec;
  integer i, j;

  // Legal: constant bounds
  logic [3:0] ok;
  assign ok = vec[3:0];

  // Illegal: variable upper bound
  logic [3:0] bad1;
  assign bad1 = vec[i:0];
  // @vec error[lyra.type.fixed_part_select_non_constant]: fixed part-select bounds must be constant integer expressions

  // Illegal: variable lower bound
  logic [3:0] bad2;
  assign bad2 = vec[7:j];
  // @vec error[lyra.type.fixed_part_select_non_constant]: fixed part-select bounds must be constant integer expressions

  // Illegal: both variable
  logic [3:0] bad3;
  assign bad3 = vec[i:j];
  // @vec error[lyra.type.fixed_part_select_non_constant]: fixed part-select bounds must be constant integer expressions
endmodule
