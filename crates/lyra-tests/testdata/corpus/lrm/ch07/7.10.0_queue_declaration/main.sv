// LRM 7.10 -- Queue declarations
//
// Tests queue type representation, bounded queues, and queue methods.

module queue_decl;

  // Unbounded queue
  int q_int [$];

  // Bounded queue
  logic [7:0] q_bounded [$:255];

  // .size() returns int
  int sz;
  assign sz = q_int.size();

  // .pop_front()/.pop_back() return element type
  int pf;
  assign pf = q_int.pop_front();
  int pb;
  assign pb = q_int.pop_back();

  // Void methods in statement context
  initial begin
    q_int.push_back(42);
    q_int.push_front(0);
    q_int.insert(0, 1);
    q_int.delete();
    q_bounded.delete();
  end

endmodule
