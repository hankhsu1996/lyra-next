// Static callable with unqualified locals (default to static)
module m_static;
  function static void f_static;
    int x;
  endfunction

  task static t_static;
    int y;
  endtask
endmodule

// Automatic callable with unqualified locals (default to automatic)
module m_auto;
  function automatic void f_auto;
    int a;
  endfunction

  task automatic t_auto;
    int b;
  endtask
endmodule

// Container-level automatic causing unqualified callable to become automatic
module automatic m_cont;
  function void f_inherited;
    int c;
  endfunction
endmodule

// Explicit local override inside automatic callable
module m_override;
  function automatic void f_mixed;
    static int s;
    int d;
  endfunction
endmodule

// Nested block inheriting callable lifetime context
module m_nested;
  function automatic void f_nested;
    begin
      int e;
    end
  endfunction
endmodule

// For / foreach automatic loop variables
module m_loops;
  int arr[3];
  initial begin
    for (int i = 0; i < 3; i = i + 1) begin
    end
    foreach (arr[j]) begin
    end
  end
endmodule

// Illegal automatic variable declaration in non-procedural container context
module m_illegal;
  automatic int z;
  // @automatic error[lyra.decl.automatic_var_non_procedural]: automatic variable declarations are only allowed in procedural contexts
endmodule
