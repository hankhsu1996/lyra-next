// LRM 7.8 -- Associative array declarations
//
// Tests associative array type representation with typed and wildcard keys.

module assoc_array_decl;

  // Wildcard key
  int aa_wild [*];

  // String key
  int aa_str [string];

  // Integer key
  logic [7:0] aa_int [int];

  // .size()/.num() return int
  int sz;
  assign sz = aa_wild.size();

  int n;
  assign n = aa_str.num();

  // .delete() in statement context
  initial begin
    aa_wild.delete();
    aa_str.delete();
  end

endmodule
