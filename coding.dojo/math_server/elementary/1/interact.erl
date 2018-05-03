~/../elementary$ erl -make
Recompile: src/server
rosemary@SCUBA:[215]~/../elementary$ erl -pa ebin/
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> 1> eunit:test(server). 
  2 tests passed.
ok

2> MathServerPid = server:start().
<0.63.0>
3> server:call(MathServerPid, {request, self(), [{circle, 3}]}).
28.274333882308138
4> q().                                                              
ok

