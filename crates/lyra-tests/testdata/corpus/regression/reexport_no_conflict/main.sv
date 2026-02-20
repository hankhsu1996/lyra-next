// Regression: no conflict when both resolve to same GlobalDefId through re-export
package P;
  parameter int C = 42;
endpackage
package Q;
  import P::*;
  export P::*;
endpackage

module m;
  import Q::*;
  import P::C;
endmodule
