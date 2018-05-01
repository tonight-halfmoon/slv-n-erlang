rosemary@SCUBA:[223]~/..ring/naiive$ ls
ebin  Emakefile  history.erl  src  test
rosemary@SCUBA:[223]~/..ring/naiive$ erl -make
Recompile: src/ring
src/ring.erl:17: function send_message/2 undefined
rosemary@SCUBA:[223]~/..ring/naiive$ erl -make
Recompile: src/ring
rosemary@SCUBA:[223]~/..ring/naiive$ erl -pa ebin/
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> eunit:test(ring).

=ERROR REPORT==== 1-May-2018::21:55:05 ===
Error in process <0.79.0> with exit value:
{badarg,[{ring,node_proc,1,[{file,"src/ring.erl"},{line,36}]}]}
ring_tests: fetched_message_in_next_node_is_sent_from_the_adjacent_fromer_node_test...*failed*
in function ring_tests:'-fetched_message_in_next_node_is_sent_from_the_adjacent_fromer_node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 48)
in call from ring_tests:fetched_message_in_next_node_is_sent_from_the_adjacent_fromer_node_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 48)
**error:{assertMatch,[{module,ring_tests},
              {line,48},
              {expression,"Reply"},
              {pattern,"{ _Message , SendNode } when SendNode == NearestLastNode"},
              {value,{initial_message,<0.83.0>}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
2> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
3> eunit:test(ring).

=ERROR REPORT==== 1-May-2018::21:55:56 ===
Error in process <0.117.0> with exit value:
{badarg,[{ring,node_proc,1,[{file,"src/ring.erl"},{line,36}]}]}
ring_tests: fetched_message_in_next_node_is_sent_from_the_adjacent_fromer_node_test...*failed*
in function ring_tests:'-fetched_message_in_next_node_is_sent_from_the_adjacent_fromer_node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 48)
in call from ring_tests:fetched_message_in_next_node_is_sent_from_the_adjacent_fromer_node_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 48)
**error:{assertMatch,[{module,ring_tests},
              {line,48},
              {expression,"Reply"},
              {pattern,"{ _Message , SendNode } when SendNode == NearestLastNode"},
              {value,{initial_message,<0.121.0>}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
4> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
5> eunit:test(ring).

=ERROR REPORT==== 1-May-2018::21:56:10 ===
Error in process <0.155.0> with exit value:
{badarg,[{ring,node_proc,1,[{file,"src/ring.erl"},{line,36}]}]}
ring_tests: fetched_message_in_next_node_is_sent_from_the_adjacent_fromer_node_test...*failed*
in function ring_tests:'-fetched_message_in_next_node_is_sent_from_the_adjacent_fromer_node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 48)
in call from ring_tests:fetched_message_in_next_node_is_sent_from_the_adjacent_fromer_node_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 48)
**error:{assertMatch,[{module,ring_tests},
              {line,48},
              {expression,"Reply"},
              {pattern,"{ _Message , SendNode } when SendNode == NearestLastNode"},
              {value,{initial_message,<0.159.0>}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
6> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:42: Warning: variable 'NearestLastNode' is unused
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:44: Warning: the result of the expression is ignored (suppress the warning by assigning the expression to the _ variable)
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:44: Warning: variable 'SecondNode' is unused
{ok,ring_tests}
7> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:52: variable 'SendNode' is unbound
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:42: Warning: variable 'NearestLastNode' is unused
error
8> eunit:test(ring).

=ERROR REPORT==== 1-May-2018::21:57:43 ===
Error in process <0.209.0> with exit value:
{badarg,[{ring,node_proc,1,[{file,"src/ring.erl"},{line,36}]}]}
ring_tests: fetched_message_in_next_node_is_sent_from_the_adjacent_fromer_node_test...*failed*
in function ring_tests:'-fetched_message_in_next_node_is_sent_from_the_adjacent_fromer_node_test/0-fun-1-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 52)
**error:{assertMatch,[{module,ring_tests},
              {line,52},
              {expression,"ReplySecond"},
              {pattern,"{ Message , SendNode } when SendNode == FirstNode"},
              {value,{undefined,undefined}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
9> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:52: variable 'SendNode' is unbound
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:42: Warning: variable 'NearestLastNode' is unused
error
10> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
11> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
12> eunit:test(ring).

=ERROR REPORT==== 1-May-2018::21:59:44 ===
Error in process <0.280.0> with exit value:
{badarg,[{ring,node_proc,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl"},
                {line,36}]}]}
ring_tests: fetched_message_in_next_node_is_sent_from_the_adjacent_fromer_node_test...*failed*
in function ring_tests:'-fetched_message_in_next_node_is_sent_from_the_adjacent_fromer_node_test/0-fun-0-'/2 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 46)
**error:{assertMatch,[{module,ring_tests},
              {line,46},
              {expression,"Reply"},
              {pattern,"{ _Message , SendNode } when SendNode == FirstNode"},
              {value,{initial_message,<0.283.0>}}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
error
13> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
14> eunit:test(ring).

=ERROR REPORT==== 1-May-2018::22:01:39 ===
Error in process <0.318.0> with exit value:
{badarg,[{ring,node_proc,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl"},
                {line,36}]}]}
  All 5 tests passed.
ok
15> 
15> eunit:test(ring).

=ERROR REPORT==== 1-May-2018::22:01:43 ===
Error in process <0.339.0> with exit value:
{badarg,[{ring,node_proc,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl"},
                {line,36}]}]}
  All 5 tests passed.
ok
16> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
17> c(ring).      
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:41: syntax error before: '.'
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:44: function node_proc/1 undefined
error
18> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
19> eunit:test(ring).

=ERROR REPORT==== 1-May-2018::22:04:35 ===
Error in process <0.410.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,node_proc,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl"},
                {line,36}]}]}
  All 5 tests passed.
ok
20> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
21> eunit:test(ring).

=ERROR REPORT==== 1-May-2018::22:05:08 ===
Error in process <0.448.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,node_proc,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl"},
                {line,36}]}]}
  All 5 tests passed.
ok
22> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:36: syntax error before: ','
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:45: function node_proc/1 undefined
error
23> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:36: syntax error before: '&'
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:45: function node_proc/1 undefined
error
24> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:36: syntax error before: '&'
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:45: function node_proc/1 undefined
error
25> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
26> eunit:test(ring).

=ERROR REPORT==== 1-May-2018::22:05:41 ===
Error in process <0.534.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,node_proc,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl"},
                {line,36}]}]}
  All 5 tests passed.
ok
27> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
28> eunit:test(ring).
  All 5 tests passed.
ok
29> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl:47: Warning: variable 'SendNode' is unused
{ok,ring}
30> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
31> eunit:test(ring).
  All 5 tests passed.
ok
32> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
33> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:50: syntax error before: 
error
34> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
35> eunit:test(ring).
  All 5 tests passed.
ok
36> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
37> eunit:test(ring).
  All 5 tests passed.
ok
38> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
39> eunit:test(ring).
  All 5 tests passed.
ok
40> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
41> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
42> eunit:test(ring).
  All 5 tests passed.
ok
43> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
44> eunit:test(ring).
ring_tests: terminate_gracefully_when_ring_nodes_receive_a_quit_message_test...*failed*
in function ring_tests:'-terminate_gracefully_when_ring_nodes_receive_a_quit_message_test/0-fun-0-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 60)
in call from ring_tests:terminate_gracefully_when_ring_nodes_receive_a_quit_message_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 60)
**error:{assert,[{module,ring_tests},
         {line,60},
         {expression,"is_process_alive ( LastNode )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 5.
error
45> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
46> eunit:test(ring).
ring_tests: terminate_gracefully_when_ring_nodes_receive_a_quit_message_test...*failed*
in function ring_tests:'-terminate_gracefully_when_ring_nodes_receive_a_quit_message_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 61)
in call from ring_tests:terminate_gracefully_when_ring_nodes_receive_a_quit_message_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 61)
**error:{assert,[{module,ring_tests},
         {line,61},
         {expression,"is_process_alive ( hd ( Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 5.
error
47> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
48> eunit:test(ring).
ring_tests: terminate_gracefully_when_ring_nodes_receive_a_quit_message_test...*failed*
in function ring_tests:'-terminate_gracefully_when_ring_nodes_receive_a_quit_message_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 61)
in call from ring_tests:terminate_gracefully_when_ring_nodes_receive_a_quit_message_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 61)
**error:{assert,[{module,ring_tests},
         {line,61},
         {expression,"is_process_alive ( hd ( Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 5.
error
49> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:60: badly formed argument for macro 'assertNot'
error
50> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl:54: variable 'Message' is unbound
error
51> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
52> eunit:test(ring).
ring_tests: terminate_gracefully_when_ring_nodes_receive_a_quit_message_test...*failed*
in function ring_tests:'-terminate_gracefully_when_ring_nodes_receive_a_quit_message_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 61)
in call from ring_tests:terminate_gracefully_when_ring_nodes_receive_a_quit_message_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 61)
**error:{assert,[{module,ring_tests},
         {line,61},
         {expression,"is_process_alive ( lists : nth ( 2 , Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 5.
error
53> c(ring).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
54> eunit:test(ring).
ring_tests: terminate_gracefully_when_ring_nodes_receive_a_quit_message_test...*failed*
in function ring_tests:'-terminate_gracefully_when_ring_nodes_receive_a_quit_message_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 61)
in call from ring_tests:terminate_gracefully_when_ring_nodes_receive_a_quit_message_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 61)
**error:{assert,[{module,ring_tests},
         {line,61},
         {expression,"is_process_alive ( lists : nth ( 2 , Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 5.
error
55> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
56> eunit:test(ring).
ring_tests: terminate_gracefully_when_ring_nodes_receive_a_quit_message_test...*failed*
in function ring_tests:'-terminate_gracefully_when_ring_nodes_receive_a_quit_message_test/0-fun-1-'/1 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 61)
in call from ring_tests:terminate_gracefully_when_ring_nodes_receive_a_quit_message_test/0 (/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl, line 61)
**error:{assert,[{module,ring_tests},
         {line,61},
         {expression,"is_process_alive ( lists : nth ( 2 , Nodes ) )"},
         {expected,false},
         {value,true}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 5.
error
57> c(ring_tests).   
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
58> c(ring).      
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
59> eunit:test(ring).
  All 6 tests passed.
ok
60> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
61> eunit:test(ring).

=ERROR REPORT==== 1-May-2018::22:32:32 ===
Error in process <0.1218.0> with exit value:
{badarg,[{erlang,is_process_alive,[undefined],[]},
         {ring,node_proc,1,
               [{file,"/home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl"},
                {line,36}]}]}
  All 6 tests passed.
ok
62> c(ring).         
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/src/ring.erl
{ok,ring}
63> c(ring_tests).
Recompiling /home/rosemary/programming/erlang/slv-n-erlang/concurrent/ring/naiive/test/ring_tests.erl
{ok,ring_tests}
64> eunit:test(ring).
  All 6 tests passed.
ok
65> q().
ok
74> rosemary@SCUBA:[227]~/..ring/naiive$ 


