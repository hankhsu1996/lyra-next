interface simple_bus;
  logic req, gnt;
endinterface

module memMod(interface a, interface b);
endmodule

module top;
  simple_bus sb1();
  simple_bus sb2();
  memMod m(.a(sb1), .b(sb2));
endmodule
