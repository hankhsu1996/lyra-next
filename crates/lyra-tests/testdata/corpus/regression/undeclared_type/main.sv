module m; unknown_t x; int my_var; my_var y; endmodule
// @unknown_t error[lyra.type.undeclared_type]: undeclared type `unknown_t`
// @2:my_var error[lyra.type.not_a_type]: `my_var` is not a type
