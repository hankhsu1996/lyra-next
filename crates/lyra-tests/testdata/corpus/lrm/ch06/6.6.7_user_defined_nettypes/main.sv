// LRM 6.6.7: User-defined nettypes
//
// nettype <data_type> <identifier> [with <resolve_function>] ;
// nettype <nettype_identifier> <identifier> ;

package net_pkg;
  nettype logic my_net;
  nettype logic [7:0] my_byte_net;
  nettype logic my_resolved_net with my_resolve;
endpackage

module nettype_decls;
  nettype integer my_int_net;
  nettype real my_real_net;
  nettype logic \my_escaped_net ;
endmodule
