Erlang/OTP 19 [erts-8.3.5.5] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V8.3.5.5  (abort with ^G)
1> c(standard_functions).
standard_functions.erl:6: syntax error before: '<-'
standard_functions.erl:3: function map/2 undefined
error
2> c(standard_functions).
{ok,standard_functions}
3> standard_functions:map(fun(X) -> X +1 end, [1,2,3,4,5]).
[2,3,4,5,6]
4> standard_functions:filter(fun(X) when X > 1 -> true end, [1,2,3,4,5]).
** exception error: no function clause matching erl_eval:'-inside-an-interpreted-fun-'(1) 
5> standard_functions:filter(fun(X) when X > 1 -> true: (X) -> false end, [1,2,3,4,5]).
* 1: syntax error before: '->'
5> standard_functions:filter(fun(X) when X > 1 -> true; (X) -> false end, [1,2,3,4,5]).
[2,3,4,5]
6> standard_functions:append( [[1,2,3,4,5], [a,b,c,v], a]).
** exception error: no function clause matching standard_functions:'-append/1-lc$^1/1-1-'(a) (standard_functions.erl, line 8)
     in function  standard_functions:'-append/1-lc$^1/1-1-'/2 (standard_functions.erl, line 8)
     in call from standard_functions:'-append/1-lc$^1/1-1-'/2 (standard_functions.erl, line 8)
7> standard_functions:append( [[1,2,3,4,5], [a,b,c,v]]).
[1,2,3,4,5,a,b,c,v]
8> 