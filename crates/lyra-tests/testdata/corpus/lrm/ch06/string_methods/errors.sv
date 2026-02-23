// LRM 6.16 -- String method error cases

module string_method_errors;

  string s;

  initial begin
    s.foo();
    //^ error[lyra.type[16]]: unknown method `foo` on this type
    s.len(1);
    //^ error[lyra.type[16]]: wrong number of arguments to method `len`
  end

endmodule
