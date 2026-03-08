// Non-ANSI port expression under `default_nettype none (LRM 6.10).
// `a` has no explicit net declaration, so implicit net is forbidden.
`default_nettype none
module top(a);
  // @a error[lyra.semantic.implicit_net_forbidden]: implicit net creation disabled by `default_nettype none` for `a`
  input a;
endmodule
