rosemary@SCUBA:[223]~/..ring/naiive$ ls
rosemary@SCUBA:[223]~/..ring/naiive$ mkdir src
rosemary@SCUBA:[223]~/..ring/naiive$ mkdir testr
rosemary@SCUBA:[223]~/..ring/naiive$ rm -r testr/
rosemary@SCUBA:[223]~/..ring/naiive$ mkdir test
rosemary@SCUBA:[223]~/..ring/naiive$ mkdir ebin
rosemary@SCUBA:[223]~/..ring/naiive$ cp ../../
distributed.systems/ iot/                 ring/                
enjoy.rosetta.code/  otp/                 
rosemary@SCUBA:[223]~/..ring/naiive$ cp ../../iot/
readme.md  sp_ub/     
rosemary@SCUBA:[223]~/..ring/naiive$ cp ../../iot/sp_ub/
deps/      ebin/      Emakefile  include/   src/       
rosemary@SCUBA:[223]~/..ring/naiive$ cp ../../iot/sp_ub/Emakefile .
rosemary@SCUBA:[223]~/..ring/naiive$ vim Emakefile 
rosemary@SCUBA:[223]~/..ring/naiive$ ls
ebin  Emakefile  src  test
rosemary@SCUBA:[223]~/..ring/naiive$ emacs test/ring_tests.erl
rosemary@SCUBA:[222]~/..ring/naiive$ erl -make
Recompile: test/ring_tests
rosemary@SCUBA:[222]~/..ring/naiive$ erl -pa ebin/
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> eunit:test(ring).
undefined
*** test module not found ***
**ring

=======================================================
  Failed: 0.  Skipped: 0.  Passed: 0.
One or more tests were cancelled.
error
2> eunit:test(ring_tests).
ring_tests: ring_must_halt_test (module 'ring_tests')...*failed*
in function ring:start/2
  called as start(3,hello)
in call from ring_tests:ring_must_halt_test/0 (test/ring_tests.erl, line 8)
in call from ring_tests:ring_must_halt_test/0
**error:undef
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 0.
error
3> q().
ok
4> rosemary@SCUBA:[221]~/..ring/naiive$ emacs src/ring.erl
^Z
[1]+  Stopped                 emacs src/ring.erl
rosemary@SCUBA:[222]~/..ring/naiive$ bg
[1]+ emacs src/ring.erl &
rosemary@SCUBA:[222]~/..ring/naiive$ erl -make
Recompile: src/ring
src/ring.erl:4: Warning: variable 'M' is unused
src/ring.erl:4: Warning: variable 'N' is unused
rosemary@SCUBA:[222]~/..ring/naiive$ erl -pa ebin/
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> eunit:test(ring).
  Test passed.
ok
2> q().
ok
3> rosemary@SCUBA:[221]~/..ring/naiive$ emacs test/ring_tests.erl 
^Z
[2]+  Stopped                 emacs test/ring_tests.erl
rosemary@SCUBA:[222]~/..ring/naiive$ bg
[2]+ emacs test/ring_tests.erl &
rosemary@SCUBA:[222]~/..ring/naiive$ erl -make
Recompile: src/ring
src/ring.erl:4: Warning: variable 'M' is unused
src/ring.erl:4: Warning: variable 'N' is unused
Recompile: test/ring_tests
test/ring_tests.erl:13: undefined macro 'assertMath/2'
rosemary@SCUBA:[222]~/..ring/naiive$ erl -make
Recompile: test/ring_tests
test/ring_tests.erl:12: function spawn_node/0 undefined
rosemary@SCUBA:[222]~/..ring/naiive$ erl -make
Recompile: src/ring
src/ring.erl:2: function spawn_node/0 undefined
src/ring.erl:4: Warning: variable 'M' is unused
src/ring.erl:4: Warning: variable 'N' is unused
rosemary@SCUBA:[222]~/..ring/naiive$ erl -make
Recompile: src/ring
src/ring.erl:2: function spawn_node/0 undefined
src/ring.erl:4: Warning: variable 'M' is unused
src/ring.erl:4: Warning: variable 'N' is unused
rosemary@SCUBA:[222]~/..ring/naiive$ erl -make
Recompile: src/ring
src/ring.erl:2: function spawn_node/0 undefined
rosemary@SCUBA:[222]~/..ring/naiive$ erl -make
Recompile: src/ring
Recompile: test/ring_tests
rosemary@SCUBA:[222]~/..ring/naiive$ erl -pa ebin/
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> eunit:test(ring).
ring_tests: ring_spawn_node_test...*failed*
in function ring_tests:'-ring_spawn_node_test/0-fun-0-'/1 (test/ring_tests.erl, line 13)
**error:{assertMatch,[{module,ring_tests},
              {line,13},
              {expression,"NodePid"},
              {pattern,"Pid when is_pid ( Pid )"},
              {value,ok}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 1.
error
2> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
3> eunit:test(ring).
  2 tests passed.
ok
4> eunit:test(ring_tests).
  2 tests passed.
ok
5> c(ring_tests).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:19: function send_message/3 undefined
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:20: function fetch_message/1 undefined
error
6> c(ring).               
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:10: Warning: variable 'Message' is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:10: Warning: variable 'ReceiverPid2' is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:10: Warning: variable 'SenderPid1' is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:13: Warning: variable 'ReceiverPid' is unused
{ok,ring}
7> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
8> eunit:test(ring_tests).
ring_tests: node_send_message_to_node_test...*failed*
in function ring_tests:'-node_send_message_to_node_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 21)
**error:{assertEqual,[{module,ring_tests},
              {line,21},
              {expression,"Received"},
              {expected,hello},
              {value,ok}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
9> c(ring_tests).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
10> c(ring_tests).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
11> c(ring).               
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:10: Warning: variable 'Message' is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:10: Warning: variable 'ReceiverPid2' is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:10: Warning: variable 'SenderPid1' is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:13: Warning: variable 'ReceiverPid' is unused
{ok,ring}
12> eunit:test(ring_tests).
  All 3 tests passed.
ok
13> c(ring).               
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:13: Warning: variable 'ReceiverPid' is unused
{ok,ring}
14> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
15> eunit:test(ring_tests).
  All 3 tests passed.
ok
16> c(ring_tests).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
17> c(ring).               
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
18> eunit:test(ring_tests).
  All 3 tests passed.
ok
19> eunit:test(ring).      
  All 3 tests passed.
ok
20> c(ring).               
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
21> eunit:test(ring).
ring_tests: node_send_message_to_node_test...*timed out*
undefined
=======================================================
  Failed: 0.  Skipped: 0.  Passed: 0.
One or more tests were cancelled.
error
22> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:7: Warning: variable 'SendNode' is unused
{ok,ring}
23> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
24> eunit:test(ring).

=ERROR REPORT==== 1-May-2018::18:30:13 ===
Error in process <0.284.0> with exit value:
{function_clause,[{ring,node_proc,
                        [[]],
                        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl"},
                         {line,7}]}]}

=ERROR REPORT==== 1-May-2018::18:30:13 ===
Error in process <0.285.0> with exit value:
{function_clause,[{ring,node_proc,
                        [[]],
                        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl"},
                         {line,7}]}]}
ring_tests: node_send_message_to_node_test...*timed out*
undefined
=======================================================
  Failed: 0.  Skipped: 0.  Passed: 0.
One or more tests were cancelled.
error
25> eunit:test(ring).

=ERROR REPORT==== 1-May-2018::18:31:21 ===
Error in process <0.292.0> with exit value:
{function_clause,[{ring,node_proc,
                        [[]],
                        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl"},
                         {line,7}]}]}

=ERROR REPORT==== 1-May-2018::18:31:21 ===
Error in process <0.293.0> with exit value:
{function_clause,[{ring,node_proc,
                        [[]],
                        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl"},
                         {line,7}]}]}
ring_tests: node_send_message_to_node_test...*timed out*
undefined
=======================================================
  Failed: 0.  Skipped: 0.  Passed: 0.
One or more tests were cancelled.
error
26> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:18: function any/0 undefined
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:18: function atom/0 undefined
error
27> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
28> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
29> eunit:test(ring).

=ERROR REPORT==== 1-May-2018::18:34:54 ===
Error in process <0.314.0> with exit value:
{function_clause,[{ring,node_proc,
                        [{state,undefined,undefined}],
                        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl"},
                         {line,9}]}]}

=ERROR REPORT==== 1-May-2018::18:34:54 ===
Error in process <0.315.0> with exit value:
{function_clause,[{ring,node_proc,
                        [{state,undefined,undefined}],
                        [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl"},
                         {line,9}]}]}
ring_tests: node_send_message_to_node_test...*timed out*
undefined
=======================================================
  Failed: 0.  Skipped: 0.  Passed: 0.
One or more tests were cancelled.
error
30> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
31> eunit:test(ring).
  All 3 tests passed.
ok
32> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
33> eunit:test(ring).
  All 3 tests passed.
ok
34> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
35> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
36> eunit:test(ring).
ring_tests: node_send_quit_message_to_node_test...*failed*
in function ring_tests:'-node_send_quit_message_to_node_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 33)
**error:{assert,[{module,ring_tests},
         {line,33},
         {expression,"is_process_alive ( NodePid2 )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
37> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:17: Warning: variable 'From' is unused
{ok,ring}
38> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
39> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
40> eunit:test(ring).
ring_tests: node_send_quit_message_to_node_test...*failed*
in function ring_tests:'-node_send_quit_message_to_node_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 33)
**error:{assert,[{module,ring_tests},
         {line,33},
         {expression,"is_process_alive ( NodePid2 )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
41> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
42> eunit:test(ring).
  All 4 tests passed.
ok
43> length([]).
0
44> length([1,23]).
2
45> c(ring_tests_.
* 1: syntax error before: '.'
45> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
46> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
47> eunit:test(ring).
ring_tests: ring_start_n_nodes_test...*failed*
in function erlang:length/1
  called as length(ok)
in call from ring_tests:'-ring_start_n_nodes_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 9)
**error:badarg
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 3.
error
48> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
49> r(ring_tests).
** exception error: undefined shell command r/1
50> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
51> eunit:test(ring).
  All 4 tests passed.
ok
52> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
53> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
54> eunit:test(ring).
  All 4 tests passed.
ok
55> h([1,2,3]).
** exception error: undefined shell command h/1
56> head([1,2,3]).
** exception error: undefined shell command head/1
57> hd([1,2,3]).  
1
58> tl([3,4,5]).
[4,5]
59> hd(lists:reverse([4,5])).
5
60> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:45: variable 'MessageFromFirstNode' is unbound
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:42: Warning: variable 'MessagFromFirstNode' is unused
error
61> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl 
{ok,ring_tests}
62> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
63> eunit:test(ring).
  All 5 tests passed.
ok
64> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl 
{ok,ring_tests}
65> eunit:test(ring).
  All 5 tests passed.
ok
66> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl 
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:45: variable 'ReplyNodeNth' is unbound
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:43: Warning: variable 'ReplyFromLastNode' is unused
error
67> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl 
{ok,ring_tests}
68> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_first_node_in_the_ring_Has_Pid_of_Nearest_Node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_first_node_in_the_ring_Has_Pid_of_Nearest_Node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 45)
**error:{assertMatch,[{module,ring_tests},
              {line,45},
              {expression,"Reply"},
              {pattern,"{ fetch_message , NodePid , Message } when NodePid /= Node1st"},
              {value,{initial_message,<0.728.0>}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
69> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl 
{ok,ring_tests}
70> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_first_node_in_the_ring_Has_Pid_of_Nearest_Node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_first_node_in_the_ring_Has_Pid_of_Nearest_Node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 45)
**error:{assertMatch,[{module,ring_tests},
              {line,45},
              {expression,"Reply"},
              {pattern,"{ fetch_message , NodePid , Message } when NodePid == Node1st"},
              {value,{initial_message,<0.766.0>}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
71> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:40: Warning: the result of the expression is ignored (suppress the warning by assigning the expression to the _ variable)
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:40: Warning: variable 'Node1st' is unused
{ok,ring_tests}
72> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_first_node_in_the_ring_Has_Pid_of_Nearest_Node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_first_node_in_the_ring_Has_Pid_of_Nearest_Node_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 45)
**error:{assertMatch,[{module,ring_tests},
              {line,45},
              {expression,"Reply"},
              {pattern,"{ fetch_message , NodePid , Message } when NodePid == self ( )"},
              {value,{initial_message,<0.804.0>}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
73> c(ring). 
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:16: Warning: function node_proc/1 is unused
{ok,ring}
74> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
75> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_first_node_in_the_ring_Has_Pid_of_Nearest_Node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_first_node_in_the_ring_Has_Pid_of_Nearest_Node_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 45)
**error:{assertMatch,[{module,ring_tests},
              {line,45},
              {expression,"Reply"},
              {pattern,"{ fetch_message , NodePid , Message } when NodePid == self ( )"},
              {value,{initial_message,<0.835.0>}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
76> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:40: Warning: the result of the expression is ignored (suppress the warning by assigning the expression to the _ variable)
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:40: Warning: variable 'Node1st' is unused
{ok,ring_tests}
77> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
78> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
79> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_first_node_in_the_ring_Has_Pid_of_Nearest_Node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_first_node_in_the_ring_Has_Pid_of_Nearest_Node_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 44)
**error:{assertMatch,[{module,ring_tests},
              {line,44},
              {expression,"Reply"},
              {pattern,"{ fetch_message , NodePid , Message } when NodePid == self ( )"},
              {value,{initial_message,<0.895.0>}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
80> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
81> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_first_node_in_the_ring_Has_Pid_of_Nearest_Node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_first_node_in_the_ring_Has_Pid_of_Nearest_Node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 45)
**error:{assertMatch,[{module,ring_tests},
              {line,45},
              {expression,"Reply"},
              {pattern,"{ fetch_message , NodePid , Message } when NodePid == Node1st"},
              {value,{initial_message,<0.933.0>}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
82> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:45: variable 'SendNpde' is unbound
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:45: Warning: variable 'SendNode' is unused
error
83> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
84> eunit:test(ring).
  All 5 tests passed.
ok
85> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:44: syntax error before: Reply
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:42: Warning: variable 'LastNode' is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:43: Warning: variable 'Nearest_to_LastNode' is unused
error
86> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:44: syntax error before: Reply
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:42: Warning: variable 'LastNode' is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:43: Warning: variable 'Nearest_to_LastNode' is unused
error
87> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:44: syntax error before: Reply
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:42: Warning: variable 'LastNode' is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:43: Warning: variable 'Nearest_to_LastNode' is unused
error
88> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:43: Warning: variable 'Nearest_to_LastNode' is unused
{ok,ring_tests}
89> eunit:test(ring).
  All 5 tests passed.
ok
90> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
91> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_first_node_in_the_ring_Has_Pid_of_Nearest_Node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_first_node_in_the_ring_Has_Pid_of_Nearest_Node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 47)
**error:{assertMatch,[{module,ring_tests},
              {line,47},
              {expression,"Reply"},
              {pattern,"{ _Message , SendNode } when SendNode == Nearest_to_LastNode"},
              {value,{initial_message,<0.1111.0>}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
92> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:43: Warning: variable 'Nearest_to_LastNode' is unused
{ok,ring_tests}
93> eunit:test(ring).
  All 5 tests passed.
ok
94> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
95> eunit:test(ring).
  All 5 tests passed.
ok
96> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:43: Warning: variable 'Nearest_to_LastNode' is unused
{ok,ring_tests}
97> eunit:test(ring).
  All 5 tests passed.
ok
98> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
99> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_first_node_in_the_ring_Has_Pid_of_Nearest_Node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_first_node_in_the_ring_Has_Pid_of_Nearest_Node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 47)
**error:{assertMatch,[{module,ring_tests},
              {line,47},
              {expression,"Reply"},
              {pattern,"{ _Message , SendNode } when SendNode == NearestLastNode"},
              {value,{initial_message,<0.1251.0>}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
100> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
101> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:17: variable 'Nodes' is unbound
error
102> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
103> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
104> c(ring).      
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:17: variable 'Nodes' is unbound
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:14: Warning: variable 'Nodew' is unused
error
105> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
106> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_first_node_in_the_ring_Has_Pid_of_Nearest_Node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_first_node_in_the_ring_Has_Pid_of_Nearest_Node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 47)
**error:{assertMatch,[{module,ring_tests},
              {line,47},
              {expression,"Reply"},
              {pattern,"{ _Message , SendNode } when SendNode == NearestLastNode"},
              {value,{undefined,undefined}}]}
  output:<<"">>

ring_tests: start_n_nodes_test...*failed*
in function ring_tests:'-start_n_nodes_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 11)
**error:{assertEqual,[{module,ring_tests},
              {line,11},
              {expression,"length ( Nodes )"},
              {expected,3},
              {value,4}]}
  output:<<"">>

=======================================================
  Failed: 2.  Skipped: 0.  Passed: 3.
error
107> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
108> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_first_node_in_the_ring_Has_Pid_of_Nearest_Node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_first_node_in_the_ring_Has_Pid_of_Nearest_Node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 47)
**error:{assertMatch,[{module,ring_tests},
              {line,47},
              {expression,"Reply"},
              {pattern,"{ _Message , SendNode } when SendNode == NearestLastNode"},
              {value,{undefined,undefined}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
109> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
110> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
111> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 46)
**error:{assertMatch,[{module,ring_tests},
              {line,46},
              {expression,"Reply"},
              {pattern,"{ _Message , SendNode } when SendNode == NearestLastNode"},
              {value,{undefined,undefined}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
112> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
113> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 46)
**error:{assertMatch,[{module,ring_tests},
              {line,46},
              {expression,"Reply"},
              {pattern,"{ _Message , SendNode } when SendNode == NearestLastNode"},
              {value,{undefined,undefined}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
114> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
115> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 46)
**error:{assertMatch,[{module,ring_tests},
              {line,46},
              {expression,"Reply"},
              {pattern,"{ _Message , SendNode } when SendNode == NearestLastNode"},
              {value,{undefined,undefined}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
116> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
117> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test/0-fun-2-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 48)
**error:{assertMatch,[{module,ring_tests},
              {line,48},
              {expression,"Reply"},
              {pattern,"{ _Message , SendNode } when SendNode == NearestLastNode"},
              {value,{undefined,undefined}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
118> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
119> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 47)
**error:{assertMatch,[{module,ring_tests},
              {line,47},
              {expression,"Reply"},
              {pattern,"{ _Message , SendNode } when SendNode == NearestLastNode"},
              {value,{undefined,undefined}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
120> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
121> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 47)
**error:{assertMatch,[{module,ring_tests},
              {line,47},
              {expression,"Reply"},
              {pattern,"{ _Message , SendNode } when SendNode == NearestLastNode"},
              {value,{undefined,undefined}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
122> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
123> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:48: syntax error before: 
error
124> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
125> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 47)
in call from ring_tests:fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 47)
**error:{assertMatch,[{module,ring_tests},
              {line,47},
              {expression,"Reply"},
              {pattern,"{ _Message , SendNode } when SendNode == NearestLastNode"},
              {value,{undefined,undefined}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
126> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:42: Warning: variable 'NearestLastNode' is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:44: Warning: variable 'Reply' is unused
{ok,ring_tests}
127> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 48)
**error:{assertEqual,[{module,ring_tests},
              {line,48},
              {expression,"Nodes"},
              {expected,[sad,as,das,da,sd]},
              {value,[<0.1686.0>,<0.1685.0>,<0.1684.0>]}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
128> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:48: syntax error before: 
error
129> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 48)
**error:{assertEqual,[{module,ring_tests},
              {line,48},
              {expression,"Nodes"},
              {expected,[sad,as,das,da,sd]},
              {value,[<0.1723.0>,<0.1722.0>,<0.1721.0>]}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
130> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
131> eunit:test(ring).
  All 5 tests passed.
ok
132> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
133> eunit:test(ring).
  All 5 tests passed.
ok
134> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
135> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
136> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 47)
**error:{assertMatch,[{module,ring_tests},
              {line,47},
              {expression,"Reply"},
              {pattern,"{ _Message , SendNode } when SendNode == NearestLastNode"},
              {value,{undefined,undefined}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
137> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:47: variable '_Message' is unbound
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:47: variable '_SendNode' is unbound
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:42: Warning: variable 'NearestLastNode' is unused
error
138> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 47)
**error:{assertMatch,[{module,ring_tests},
              {line,47},
              {expression,"Reply"},
              {pattern,"{ _Message , SendNode } when SendNode == NearestLastNode"},
              {value,{undefined,undefined}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
139> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:47: variable 'SendNode' is unbound
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:42: Warning: variable 'NearestLastNode' is unused
error
140> eunit:test(ring).
ring_tests: fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test...*failed*
in function ring_tests:'-fetch_message_of_last_node_is_the_message_sent_by_the_nearest_node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 47)
**error:{assertMatch,[{module,ring_tests},
              {line,47},
              {expression,"Reply"},
              {pattern,"{ _Message , SendNode } when SendNode == NearestLastNode"},
              {value,{undefined,undefined}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
141> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:42: Warning: variable 'NearestLastNode' is unused
{ok,ring_tests}
142> eunit:test(ring).
ring_tests: fetched_message_in_the_last_node_is_the_message_sent_by_the_nearest_node_test...*failed*
in function ring_tests:'-fetched_message_in_the_last_node_is_the_message_sent_by_the_nearest_node_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 47)
**error:{assertEqual,[{module,ring_tests},
              {line,47},
              {expression,"Reply"},
              {expected,{as,asd}},
              {value,{undefined,undefined}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
143> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
144> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
145> eunit:test(ring).
ring_tests: fetched_message_in_the_last_node_is_the_message_sent_by_the_nearest_node_test...*failed*
in function ring_tests:'-fetched_message_in_the_last_node_is_the_message_sent_by_the_nearest_node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 47)
**error:{assertMatch,[{module,ring_tests},
              {line,47},
              {expression,"Reply"},
              {pattern,"{ _Message , SendNode } when SendNode == NearestLastNode"},
              {value,{undefined,undefined}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
146> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
147> c(ring).      
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
148> eunit:test(ring).
  All 5 tests passed.
ok
149> eunit:test(ring).
  All 5 tests passed.
ok
150> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
151> eunit:test(ring).
  All 5 tests passed.
ok
152> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
153> eunit:test(ring).
  All 5 tests passed.
ok
154> eunit:test(ring).
  All 5 tests passed.
ok
155> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
156> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
157> c(ring).      
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
158> eunit:test(ring).
ring_tests: node_send_quit_message_to_node_test...*failed*
in function ring_tests:'-node_send_quit_message_to_node_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 35)
**error:{assert,[{module,ring_tests},
         {line,35},
         {expression,"is_process_alive ( NodePid2 )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
159> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
160> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
161> eunit:test(ring).
  All 5 tests passed.
ok
162> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:43: variable 'FirstNode' is unbound
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:50: Warning: variable 'ReplySecond' is unused
error
163> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:50: Warning: variable 'ReplySecond' is unused
{ok,ring_tests}
164> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
165> eunit:test(ring).
  All 5 tests passed.
ok
166> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
167> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
168> eunit:test(ring).
  All 5 tests passed.
ok
169> q().
ok
170> rosemary@SCUBA:[224]~/..ring/naiive$ 


