// LRM 6.3.2.1: Charge strength
//
// The trireg net type accepts an optional charge strength: small, medium, or large.
// Syntax: trireg ( charge_kw ) [signing] [packed_dims] declarator ;

module charge_strength;

  trireg (small) a;
  trireg (medium) [7:0] b;
  trireg (large) signed [3:0] c;

endmodule
