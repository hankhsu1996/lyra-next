// LRM 25.5.4: Modport expressions
// Tests .port_name(expression) syntax in modport declarations

interface bus;
  logic [7:0] r;
  logic [3:0] x;
  logic flag;

  // Mixed bare and expression ports in the same modport
  modport view_a(input .A(r[3:0]), output .B(r[7:4]), input flag);

  // Expression port with bare name target
  modport view_b(input .X(x), output .Y(r));

  // Empty parens
  modport view_c(input .E());

  // Non-lvalue expression target
  modport view_d(output .N(2));

  // Different modports exposing different slices of the same signal
  modport lo(input .data(r[3:0]));
  modport hi(input .data(r[7:4]));
endinterface

// Legal: read from input expression port
module read_expr_port(bus.view_a i);
  logic v;
  always_comb v = i.flag;
endmodule

// Legal: write to output expression port with lvalue target
module write_expr_port(bus.view_b i);
  always_comb i.Y = 8'hFF;
endmodule

// Illegal: write to input expression port
module write_to_input_expr(bus.view_a i);
  always_comb i.A = 4'b0;
  //            ^ error[lyra.type[10]]: modport member declared 'input' cannot be used in write context
endmodule

// Illegal: read from output expression port
module read_from_output_expr(bus.view_a i);
  logic [3:0] v;
  always_comb v = i.B;
  //                ^ error[lyra.type[10]]: modport member declared 'output' cannot be used in read context
endmodule

// Illegal: any access to empty-parens port
module read_empty_port(bus.view_c i);
  logic v;
  always_comb v = i.E;
  //                ^ error[lyra.type[14]]: modport port has no connection (empty parens)
endmodule

// Illegal: write through non-lvalue expression target
module write_nonlvalue(bus.view_d i);
  always_comb i.N = 1;
  //            ^ error[lyra.type[15]]: modport expression target is not assignable
endmodule
