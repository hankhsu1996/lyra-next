// LRM 26.5: Search order rules for package imports

package P;
  typedef enum logic {FALSE, TRUE} bool_t;
  parameter int C = 42;
endpackage

package Q;
  parameter int C = 0;
endpackage

// Local declaration shadows wildcard import
module local_shadows_wildcard;
  import P::*;
  int C;
  assign C = 1;
endmodule

// Explicit import takes priority
module explicit_import_priority;
  import P::C;
  localparam int X = C;
endmodule

// Wildcard import from two packages -- no conflict unless same name used
module two_wildcards_no_conflict;
  import P::*;
  import Q::*;
  bool_t flag;
endmodule

// Wildcard import ambiguity -- same name from two packages
module two_wildcards_ambiguity;
  import P::*;
  import Q::*;
  localparam int X = C;
  //                ^ error[lyra.semantic[5]]: name `C` is ambiguous: imported from packages `P`, `Q`
endmodule
