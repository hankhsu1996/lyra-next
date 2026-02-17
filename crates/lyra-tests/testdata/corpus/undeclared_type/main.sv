// ALLOW-EXTRA-DIAGS
module m; unknown_t x; int my_var; my_var y; endmodule
//       ^ error[lyra.type[2]]: undeclared type `unknown_t`
//                                ^ error[lyra.type[3]]: `my_var` is not a type
