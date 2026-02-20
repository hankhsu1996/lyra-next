// LRM 26.6: Importing the same declaration through multiple export paths
// is not a conflict. Both P1 and P4 expose the same original x.
package P1;
  parameter int x = 1;
endpackage
package P4;
  import P1::x;
  export P1::x;
endpackage

// Consumer re-exports x from both P1 and P4 -- same underlying declaration.
package Consumer;
  import P1::x;
  export P1::x;
  import P4::x;
  export P4::x;
endpackage

module m;
  import Consumer::*;
  parameter int y = x;
endmodule
