// LRM 7.10.1: Incompatible operand types in queue concatenation
module queue_concat_incompat;
  int q [$];
  real r;
  int fixed_arr [3];

  // real is not compatible with int queue element type
  initial q = {q, r};
  // @[{] error[lyra.type.queue_concat_incompat]: queue concatenation operand has incompatible element type

  // fixed-size array is not a valid queue concat operand
  initial q = {q, fixed_arr};
  // @[{] error[lyra.type.queue_concat_incompat]: queue concatenation operand has incompatible element type

  // queue-queue with non-equivalent element types (different width)
  byte bq [$];
  initial q = {q, bq};
  // @[{] error[lyra.type.queue_concat_incompat]: queue concatenation operand has incompatible element type

  // queue-queue order should not matter
  initial q = {bq, q};
  // @[{] error[lyra.type.queue_concat_incompat]: queue concatenation operand has incompatible element type
endmodule
