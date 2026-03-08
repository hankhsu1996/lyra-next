// LRM 7.10.1: $ is only valid as a queue index or slice bound
module dollar_misuse;
  int q [$];
  int arr [10];
  int x;

  // $ as direct assignment RHS
  assign x = $;
  // @[$] error[lyra.type.dollar_outside_queue_context]: `$` can only be used as a queue index or slice bound

  // $ as non-queue array index
  assign x = arr[$];
  // @arr error[lyra.type.dollar_outside_queue_context]: `$` can only be used as a queue index or slice bound

  // $ in arithmetic expression
  assign x = $ + 1;
  // @[$] error[lyra.type.dollar_outside_queue_context]: `$` can only be used as a queue index or slice bound

  // $ in logical negation
  assign x = !$;
  // @[!] error[lyra.type.dollar_outside_queue_context]: `$` can only be used as a queue index or slice bound

  // $ in ternary condition
  assign x = $ ? 1 : 0;
  // @[$] error[lyra.type.dollar_outside_queue_context]: `$` can only be used as a queue index or slice bound

  // $ in ternary arm
  assign x = 1 ? $ : 0;
  // @[1] error[lyra.type.dollar_outside_queue_context]: `$` can only be used as a queue index or slice bound
endmodule
