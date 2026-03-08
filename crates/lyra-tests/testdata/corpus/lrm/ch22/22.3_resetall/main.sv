// LRM 22.3: `resetall is legal at top level
`resetall

module m;
  `resetall
  // @[`resetall] error[lyra.preprocess.resetall_inside_design_element]: `resetall is not allowed inside a design element
endmodule

interface i;
  `resetall
  // @[`resetall] error[lyra.preprocess.resetall_inside_design_element]: `resetall is not allowed inside a design element
endinterface

program p;
  `resetall
  // @[`resetall] error[lyra.preprocess.resetall_inside_design_element]: `resetall is not allowed inside a design element
endprogram

package pkg;
  `resetall
  // @[`resetall] error[lyra.preprocess.resetall_inside_design_element]: `resetall is not allowed inside a design element
endpackage

primitive prim(output o, input a);
  `resetall
  // @[`resetall] error[lyra.preprocess.resetall_inside_design_element]: `resetall is not allowed inside a design element
  table
    0 : 0;
    1 : 1;
  endtable
endprimitive

config cfg;
  `resetall
  // @[`resetall] error[lyra.preprocess.resetall_inside_design_element]: `resetall is not allowed inside a design element
  design m;
endconfig
