// LRM 7.5: Array assignment compatibility
//
// Tests structural assignment compatibility between array types.

module array_assignment_compat;

  // Same-kind assignments (no diagnostic)
  int dyn_a [];
  int dyn_b [];
  int fixed_a [5];
  int fixed_b [5];
  initial begin
    dyn_a = dyn_b;
    dyn_a = fixed_a;
    fixed_a = fixed_b;
  end

  // Error: fixed size mismatch
  int fixed_c [3];
  initial begin
    fixed_a = fixed_c;
  //         ^ error[lyra.type[24]]
  end

  // Error: element type mismatch
  logic [7:0] elem_mis [];
  initial begin
    dyn_a = elem_mis;
  //       ^ error[lyra.type[24]]
  end

endmodule
