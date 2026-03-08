// LRM 7.10.1: Empty concat requires queue or dynamic array target
module empty_concat_error;
  int x;

  // Empty concat assigned to non-array target
  assign x = {};
  // @[{] error[lyra.type.empty_concat_requires_context]: empty concatenation `{}` requires a queue or dynamic array target
endmodule
