// LRM 25.10: Function not listed in modport is restricted

interface iface;
  function int hidden_fn();
    hidden_fn = 1;
  endfunction
  modport reader();
endinterface

module consumer(iface.reader bus);
  int x;
  always_comb x = bus.hidden_fn();
  // @hidden_fn error[lyra.type.member_not_in_modport]: member `hidden_fn` is not accessible through this modport
endmodule
