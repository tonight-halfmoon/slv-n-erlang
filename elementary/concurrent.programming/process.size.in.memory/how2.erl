rosemary@SCUBA:~/..cess.size.in.memory.js:0.ps:190_$ erl
Erlang/OTP 20 [erts-9.0.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.0.1  (abort with ^G)
1> Fun = fun() -> receive after infinity -> ok end end.
#Fun<erl_eval.20.99386804>
2> {_,Bytes} = process_info(spawn(Fun), memory).
{memory,2712}
3> Bytes div erlang:system_info(wordsize).
339
4> 
