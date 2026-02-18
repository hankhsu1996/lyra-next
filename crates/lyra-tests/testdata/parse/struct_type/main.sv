module m;
  typedef struct packed { logic [7:0] data; logic valid; } pkt_t;
  struct { int x; int y; } point;
  typedef union { int i; logic [31:0] bits; } word_t;
endmodule
