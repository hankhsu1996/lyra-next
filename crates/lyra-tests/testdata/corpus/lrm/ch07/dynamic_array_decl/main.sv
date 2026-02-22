// LRM 7.5 -- Dynamic array declarations
//
// Tests dynamic array type representation and .size()/.delete() methods.

module dynamic_array_decl;

  // Basic dynamic array
  int dyn_int [];

  // Dynamic array of vectors
  logic [7:0] dyn_vec [];

  // .size() returns int
  int sz;
  assign sz = dyn_int.size();

  // .delete() in statement context
  initial begin
    dyn_int.delete();
    dyn_vec.delete();
  end

endmodule
