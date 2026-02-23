// LRM 26.6: Export may precede corresponding import
//
// "A package export may precede a corresponding package import" (LRM 26.6).
// The export and import are order-independent within the package body.

package Src;
  typedef logic [7:0] byte_t;
  parameter int SIZE = 8;
endpackage

// Export appears before the import it re-exports
package Reexporter;
  export Src::byte_t;
  export Src::SIZE;
  import Src::byte_t;
  import Src::SIZE;
endpackage

// Consumer verifies the re-exported names are visible
module consumer;
  import Reexporter::*;
  byte_t data;
  localparam int S = SIZE;
endmodule
