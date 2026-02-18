// LRM 5.6: Identifiers, keywords, and system names
// ALLOW-EXTRA-DIAGS

// 5.6 Simple identifiers: letters, digits, $, _
// First character must be letter or underscore, not digit or $
module identifiers_basic;
  int shiftreg_a;
  int busa_index;
  int error_condition;
  int merge_ab;
  int _bus3;
  int n$657;
  int _;
  int A_B_C;
  int a123;
endmodule

// 5.6.2 Keywords are lowercase only; identifiers are case sensitive
module case_sensitivity;
  int abc;
  int ABC;
  int Abc;
endmodule
