// LRM 26.6: Exporting imported names
//
// Export declarations make imported names visible to importers
// of the exporting package.

// Inner package with definitions
package Inner;
  typedef logic [7:0] byte_t;
  parameter int SIZE = 8;
endpackage

// Explicit export: re-export a specific name
package ExplicitExporter;
  import Inner::byte_t;
  import Inner::SIZE;
  export Inner::byte_t;
endpackage

// Package wildcard export: re-export all from a package
package WildcardExporter;
  import Inner::*;
  export Inner::*;
endpackage

// All-wildcard export: re-export everything imported
package AllExporter;
  import Inner::*;
  export *::*;
endpackage

// Consumer of explicit export
module use_explicit;
  import ExplicitExporter::byte_t;
  byte_t data;
endmodule

// Consumer of wildcard export
module use_wildcard;
  import WildcardExporter::*;
  byte_t wdata;
  localparam int W = SIZE;
endmodule

// Consumer of all-wildcard export
module use_all;
  import AllExporter::*;
  byte_t adata;
  localparam int A = SIZE;
endmodule
