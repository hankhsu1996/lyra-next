module bounded_queues_valid;
  int q1 [$:1];
  logic [7:0] q2 [$:15];
  parameter int DEPTH = 10;
  int q3 [$:DEPTH];
endmodule
