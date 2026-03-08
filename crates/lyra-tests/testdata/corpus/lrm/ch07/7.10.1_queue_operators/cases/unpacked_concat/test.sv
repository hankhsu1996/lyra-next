// LRM 7.10.1: Queue concatenation (unpacked array concatenation)
module unpacked_concat;
  int q [$];
  int q1 [$];
  int q2 [$];
  int item;

  // Concatenate queue with element
  initial q = {q1, item};

  // Concatenate element with queue
  initial q = {item, q2};

  // Concatenate two queues
  initial q = {q1, q2};

  // Concatenate queue slice with element
  initial q = {q1[0:1], item};

  // Empty concat assigned to queue
  initial q = {};

  // Widening-compatible: shortint element into int queue
  shortint si;
  initial q = {q1, si};

  // Widening-compatible: byte element into int queue
  byte b;
  initial q = {b, q2};
endmodule
