// LRM 6.3.2.2: Drive strength on net declarations
//
// Net declarations accept an optional drive strength: ( strength0 , strength1 )
// or ( strength1 , strength0 ).

module drive_strength_net;

  wire (strong0, pull1) a;
  tri (supply0, strong1) b;
  trireg (weak0, highz1) c;

  // Reversed order: strength1 first, strength0 second
  wire (pull1, strong0) d;
  tri (weak1, supply0) e;

endmodule
