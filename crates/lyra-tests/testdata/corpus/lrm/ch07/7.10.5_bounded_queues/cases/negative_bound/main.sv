module bounded_queues_negative;
  int q1 [$:-1];
  //        ^ error[lyra.type.queue_bound_not_positive]: queue bound must be a positive integer, got -1
endmodule
