// LRM 6.16 -- String method error cases

module string_method_errors;

  string s;

  initial begin
    s.foo();
    // @foo error[lyra.type.method_call_error]: unknown method `foo` on this type
    s.len(1);
    // @len error[lyra.type.method_call_error]: wrong number of arguments to method `len`
  end

endmodule
