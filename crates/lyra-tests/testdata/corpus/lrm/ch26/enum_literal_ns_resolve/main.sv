// Qualified enum literal resolves in value namespace

module ns_resolve_test;
  logic v;
  assign v = r::X;
endmodule
