// LRM 6.13: Void cannot be used as a variable type.

module void_object_decl_rejected;

  void x;
  //   ^ error[lyra.type[31]]: void cannot be used as an object type

endmodule
