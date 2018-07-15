-To demonstrate how shell process restarts

-Remove process_flag(trap_exit, true) in module 'errh.erl' in function 'start'.

-Compile with `erl -make` and run shell with  `erl -pa ebin/`

Evaluate the following:

1> self().
2> errh:start().
3> errh:request(1).
4> self().
 --> same pid as in 1>
5> errh:request(one).
  --> notice a crash message
6> self().
  --> different Pid than in 1> and 4>.

