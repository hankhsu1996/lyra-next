// LRM 7.10.1: Indexed part-select not allowed on queues
module queue_part_select;
  int q [$];
  int q2 [$];

  // Indexed-plus part-select on queue is illegal
  assign q2 = q[0+:2];
  // @q error[lyra.type.queue_part_select_not_allowed]: indexed part-select (`+:` / `-:`) is not allowed on queues

  // Indexed-minus part-select on queue is illegal
  assign q2 = q[0-:2];
  // @q error[lyra.type.queue_part_select_not_allowed]: indexed part-select (`+:` / `-:`) is not allowed on queues
endmodule
