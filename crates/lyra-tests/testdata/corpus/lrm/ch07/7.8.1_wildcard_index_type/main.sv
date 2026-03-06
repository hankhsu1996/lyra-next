// LRM 7.8.1 -- Wildcard index type
//
// Associative arrays with wildcard index [*] accept any integral index.
// Tests declaration, indexing, and compatible built-in methods.

module wildcard_index;

  // Declaration with different element types
  int aa [*];
  logic [7:0] bb [*];

  // Integral indexing in continuous assignment context
  int result;
  assign result = aa[0];

  // Built-in methods: size, num
  int sz;
  assign sz = aa.size();

  int n;
  assign n = bb.num();

  // Procedural context: indexing with various integral widths
  initial begin
    aa[0] = 1;
    aa[100] = 2;
    aa[8'hFF] = 3;
    bb[32'hDEAD] = 8'hAB;

    // delete with no argument (clear all)
    aa.delete();
  end

endmodule
