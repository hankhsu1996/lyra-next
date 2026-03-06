module bounded_queues_non_const;
  int n;
  int q [$:n];
  // @n error[lyra.type.queue_bound_not_const]: queue bound must be a constant positive integer
endmodule
