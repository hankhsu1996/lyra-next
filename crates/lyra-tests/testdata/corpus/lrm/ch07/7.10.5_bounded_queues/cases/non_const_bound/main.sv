module bounded_queues_non_const;
  int n;
  int q [$:n];
  //       ^ error[lyra.type[36]]: queue bound must be a constant positive integer
endmodule
