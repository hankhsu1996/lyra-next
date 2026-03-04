// LRM 6.6.8: Generic interconnect
//
// interconnect [signing] {packed_dimension} identifier {unpacked_dimension} ;
//
// Interconnect nets do not allow strength specifications.

module interconnect_basic;
  interconnect a;
  interconnect signed [3:0] b;
  interconnect [7:0] c;
  interconnect d, e;
  interconnect f [3:0];
  interconnect \my_ic ;
endmodule

interface interconnect_intf;
  interconnect g;
endinterface

module interconnect_strength_rejected;
  interconnect (strong1, pull0) x;
  //           ^ error[lyra.parse.error]: strength specification is not allowed on interconnect nets
endmodule
