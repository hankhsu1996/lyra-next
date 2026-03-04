// LRM 6.13: Void cannot be used as a variable type.

module void_object_decl_rejected;

  void x;
  //   ^ error[lyra.type.void_object_type]: void cannot be used as an object type

endmodule
