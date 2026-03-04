// LRM 6.20.6: Const variable without initializer
//
// A const variable must have an initializer.

module const_missing_init;

  const int x;
  //        ^ error[lyra.type.const_missing_init]: const variable must have an initializer

endmodule
