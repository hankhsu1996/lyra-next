// LRM 12.7.3 -- Foreach with package-qualified field access
//
// The array reference `p::obj.arr` resolves through a package-scoped
// variable with a struct field. The var list `[i]` iterates over the
// remaining dimension. No diagnostics expected.

package p;

  typedef struct { int arr [3]; } obj_t;
  obj_t obj;

endpackage

module package_qualified_field;

  int tmp;

  initial begin
    foreach (p::obj.arr[i]) begin
      tmp = i;
    end
  end

endmodule
