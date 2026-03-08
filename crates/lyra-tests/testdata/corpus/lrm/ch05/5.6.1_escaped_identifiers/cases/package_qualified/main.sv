// LRM 5.6.1: Escaped identifiers in package-qualified names.
// Package name and member name normalization across escaped/plain forms.

package \mypkg ;
  parameter int WIDTH = 8;
endpackage

module package_qualified;
  int w;
  initial w = mypkg::WIDTH;
endmodule
