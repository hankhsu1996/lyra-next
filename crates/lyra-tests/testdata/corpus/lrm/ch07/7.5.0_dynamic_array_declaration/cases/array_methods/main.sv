// LRM 7.5, 7.9, 7.10 -- Array built-in methods
//
// Tests .size(), .delete(), .push_back(), .pop_front(), etc. on
// dynamic arrays, queues, and associative arrays.

module array_methods;

  // Dynamic array
  int d[];
  int d_sz = d.size();

  // Queue
  int q[$];
  int q_sz  = q.size();
  int q_pf  = q.pop_front();
  int q_pb  = q.pop_back();

  // Associative array with typed key
  int aa[string];
  string s;
  int aa_sz = aa.size();
  int aa_n  = aa.num();
  int aa_e  = aa.exists("hello");
  int aa_f  = aa.first(s);
  int aa_l  = aa.last(s);
  int aa_nx = aa.next(s);
  int aa_pv = aa.prev(s);

  // Associative array with wildcard key -- .size/.num/.delete allowed
  int ww[*];
  int ww_sz = ww.size();
  int ww_n  = ww.num();

  // Void methods in statement context
  initial begin
    d.delete();
    q.push_back(1);
    q.push_front(2);
    q.insert(0, 3);
    q.delete();
    q.delete(0);
    aa.delete();
    aa.delete("key");
  end

endmodule
