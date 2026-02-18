// LRM 5.12: Attributes

// Prefix on module declaration
(* optimize_power *) module attr_test;

  // Prefix on variable declarations
  (* fsm_state *) logic [7:0] state1;
  (* fsm_state=1 *) logic [3:0] state2;

  // Multiple attribute instances stacked
  (* a *) (* b=3 *) logic [7:0] multi;

  // Multiple specs in one instance
  (* full_case, parallel_case *) logic dummy;

  // Attribute value with expression (multiplication)
  (* weight = 2 * 3 *) int weighted;

  // Prefix on always block
  (* keep *) always_comb begin
    state1 = 8'h00;
  end

  // Prefix on continuous assign
  (* synthesis_off *) assign state2 = 4'b0;

  // Operator attribute (infix)
  logic [7:0] a, b, c;
  assign c = a + (* mode = "cla" *) b;

  // Ternary attribute
  logic sel, d;
  assign d = sel ? (* no_glitch *) a[0] : b[0];

  // Wildcard sensitivity (NOT an attribute)
  always @(*) begin
    dummy = state1[0];
  end

  // Spaced wildcard sensitivity
  always @( * ) begin
    dummy = state1[1];
  end

endmodule
