// LRM 6.16 -- String data type built-in methods
//
// All 18 methods in valid contexts: value methods in expressions,
// void methods as statements.

module string_methods;

  string s, t;
  integer n;
  real r;

  // Value methods in expression context
  int    v_len  = s.len();
  byte   v_getc = s.getc(v_len);
  string v_up   = s.toupper();
  string v_lo   = s.tolower();
  int    v_cmp  = s.compare(t);
  int    v_icmp = s.icompare(t);
  string v_sub  = s.substr(v_len, v_len);

  // ato* family returns integer (4-state)
  integer v_atoi   = s.atoi();
  integer v_atohex = s.atohex();
  integer v_atooct = s.atooct();
  integer v_atobin = s.atobin();

  // atoreal returns real
  real v_atoreal = s.atoreal();

  // Void methods in statement context
  initial begin
    s.putc(v_len, v_getc);
    s.itoa(n);
    s.hextoa(n);
    s.octtoa(n);
    s.bintoa(n);
    s.realtoa(r);
  end

endmodule
