module bounded_queues_struct_union;

  // Valid: positive bound in struct member
  struct {
    int q [$:10];
  } s_ok;

  // Invalid: zero bound in struct member
  struct {
    int q [$:0];
    // @[0] error[lyra.type.queue_bound_not_positive]: queue bound must be a positive integer, got 0
  } s_zero;

  // Invalid: negative bound in struct member
  struct {
    int q [$:-1];
    // @[-] error[lyra.type.queue_bound_not_positive]: queue bound must be a positive integer, got -1
  } s_neg;

  // Invalid: zero bound in nested struct member
  struct {
    struct {
      int q [$:0];
      // @[0] error[lyra.type.queue_bound_not_positive]: queue bound must be a positive integer, got 0
    } inner;
  } s_nested;

  // Invalid: zero bound in union member
  union {
    int q [$:0];
    // @q error[lyra.semantic.illegal_union_member_type]: queue member `q` is not allowed in untagged unions
    // @[0] error[lyra.type.queue_bound_not_positive]: queue bound must be a positive integer, got 0
  } u_zero;

endmodule
