// LRM 26.4: Multi-import in single header
//
// A single import declaration in a module header can contain
// comma-separated import items (LRM 26.4 example).

package PkgA;
  typedef logic [7:0] byte_t;
endpackage

package PkgB;
  parameter int SIZE = 16;
endpackage

// Single import with comma-separated items from different packages
module multi_hdr
  import PkgA::byte_t, PkgB::SIZE;
(input byte_t data);
  logic [SIZE-1:0] bus;
endmodule

// Multiple separate header imports (for comparison)
module separate_hdr
  import PkgA::byte_t;
  import PkgB::SIZE;
(input byte_t data);
  logic [SIZE-1:0] bus;
endmodule
