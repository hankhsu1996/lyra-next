// LRM 6.6.7: User-defined nettypes
//
// nettype <data_type> <identifier> [with <resolve_function>] ;
// nettype <nettype_identifier> <identifier> ;

package net_pkg;
  nettype logic my_net;
  nettype logic [7:0] my_byte_net;
  nettype logic my_resolved_net with my_resolve;
  nettype my_net my_alias;
endpackage

module nettype_decls;
  nettype integer my_int_net;
  nettype real my_real_net;
  nettype logic \my_escaped_net ;
endmodule

module nettype_use;
  import net_pkg::*;
  my_net x;
  my_byte_net y;
  my_alias z;
endmodule
