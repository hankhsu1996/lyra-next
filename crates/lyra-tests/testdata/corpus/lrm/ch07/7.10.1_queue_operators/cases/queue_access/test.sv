// LRM 7.10.1: Queue indexing and slicing
module queue_access;
  int q [$];
  int x;

  // Queue element select by index
  assign x = q[0];

  // Queue element select with $ (last element)
  assign x = q[$];

  // Queue slice with fixed range
  int q2 [$];
  assign q2 = q[0:2];

  // Queue slice with $ as upper bound
  assign q2 = q[0:$];

  // Queue slice with $ as lower bound
  assign q2 = q[$:0];
endmodule
