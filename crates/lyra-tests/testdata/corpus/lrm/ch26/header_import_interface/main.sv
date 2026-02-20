// LRM 26.4: Header import in interface declarations
//
// The LRM specifies that package import declarations may appear in
// interface headers, between the interface name and the port list.

package IfcPkg;
  typedef logic [7:0] byte_t;
  parameter int WIDTH = 8;
endpackage

// Interface with header-level wildcard import
interface bus_if
  import IfcPkg::*;
(input logic clk);
  byte_t data;
endinterface

// Interface with header-level explicit import
interface data_if
  import IfcPkg::byte_t;
(input logic clk);
  byte_t payload;
endinterface
