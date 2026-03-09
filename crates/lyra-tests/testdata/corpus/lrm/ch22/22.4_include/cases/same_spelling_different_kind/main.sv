// LRM 22.4: same spelling resolves differently by include kind
// Quoted finds the sibling file; angle-bracket finds the -I dir file
`include "defs.svh"
`include <defs.svh>

module m;
  local_type x;
  global_type y;
endmodule
