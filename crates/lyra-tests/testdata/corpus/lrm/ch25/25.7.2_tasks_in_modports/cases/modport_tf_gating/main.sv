// LRM 25.7.2: Only imported TF members are accessible through modport.
// Non-imported tasks/functions are gated by modport restrictions.

interface simple_bus;
  logic [7:0] data;

  task Read(input logic [7:0] addr);
  endtask

  task Write(input logic [7:0] addr, input logic [7:0] wdata);
  endtask

  task Reset();
  endtask

  modport master(
    input data,
    import Read
  );
endinterface

module consumer(simple_bus.master bus);
  initial begin
    bus.Read(8'hAA);
    bus.Write(8'h10, 8'hFF);
    // @Write error[lyra.type.member_not_in_modport]: member `Write` is not accessible through this modport
    bus.Reset();
    // @Reset error[lyra.type.member_not_in_modport]: member `Reset` is not accessible through this modport
  end
endmodule
