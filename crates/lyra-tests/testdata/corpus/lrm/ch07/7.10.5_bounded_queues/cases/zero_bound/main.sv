module bounded_queues_zero;
  int q1 [$:0];
  //        ^ error[lyra.type[37]]: queue bound must be a positive integer, got 0
endmodule
