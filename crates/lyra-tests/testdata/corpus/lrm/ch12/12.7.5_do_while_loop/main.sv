// LRM 12.7.5 -- Do-while loop statements
//
// The do-while loop executes the body first, then tests the condition.

module do_while_loop;
  integer i;

  // Basic do-while
  initial begin
    i = 0;
    do i = i + 1; while (i < 3);
  end

  // Do-while with begin-end body
  initial begin
    i = 0;
    do begin
      i = i + 1;
      i = i + 2;
    end while (i < 10);
  end

  // Do-while with compound condition
  logic done;
  initial begin
    i = 0;
    done = 1'b0;
    do i = i + 1; while (i < 10 && !done);
  end
endmodule
