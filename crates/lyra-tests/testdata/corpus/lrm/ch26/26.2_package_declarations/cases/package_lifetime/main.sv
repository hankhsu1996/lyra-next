// LRM 26.2: Package lifetime keyword
//
// Package declarations accept an optional lifetime keyword (automatic or static).
// The default lifetime is static.

// Package with automatic lifetime
package automatic AutoPkg;
  typedef logic [7:0] byte_t;
  parameter int WIDTH = 8;
endpackage

// Package with static lifetime
package static StaticPkg;
  parameter int DEPTH = 16;
endpackage

// Package with no lifetime (default static)
package DefaultPkg;
  parameter int SIZE = 4;
endpackage

// Consumer verifies all three packages are accessible
module consumer;
  import AutoPkg::byte_t;
  import StaticPkg::DEPTH;
  import DefaultPkg::SIZE;
  byte_t data;
  localparam int D = DEPTH;
  localparam int S = SIZE;
endmodule
