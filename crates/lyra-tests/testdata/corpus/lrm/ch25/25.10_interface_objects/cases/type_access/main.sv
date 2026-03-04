// LRM 25.10: Types defined inside an interface are accessible through
// modport-qualified ports via dotted name in type position.

interface bus_iface;
  typedef logic [7:0] byte_t;
  typedef enum logic {READ, WRITE} dir_t;
  logic [7:0] data;
  modport master(output data);
endinterface

module consumer(bus_iface.master bus);
  typedef bus.byte_t local_byte;
  typedef bus.dir_t  local_dir;
  local_byte x;
  local_dir d;
endmodule
